pub mod alloc;
pub mod alloc_util;
pub mod constants;
pub mod ctx;
mod location;
mod vec;

use crate::alloc::{ConstantMemoryRegion, FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::{
    is_map, is_vec, layout_struct, layout_tuple, layout_tuple_elements, reserve_space_for_type,
    type_size_and_alignment,
};
use crate::constants::ConstantsManager;
use crate::ctx::Context;
use seq_map::SeqMap;
use swamp_script_node::Node;
use swamp_script_semantic::intr::IntrinsicFunction;
use swamp_script_semantic::{
    AnonymousStructLiteral, ArgumentExpressionOrLocation, BinaryOperator, BinaryOperatorKind,
    BooleanExpression, CompoundOperatorKind, ConstantId, ConstantRef, EnumLiteralData, Expression,
    ExpressionKind, ForPattern, Function, Guard, InternalFunctionDefinitionRef, InternalFunctionId,
    InternalMainExpression, Iterable, Literal, Match, MutOrImmutableExpression, NormalPattern,
    Pattern, Postfix, PostfixKind, RangeMode, SingleLocationExpression,
    SingleMutLocationExpression, StructInstantiation, UnaryOperator, UnaryOperatorKind,
    VariableRef, WhenBinding,
};
use swamp_script_types::{AnonymousStructType, EnumVariantType, Signature, StructTypeField, Type};
use swamp_vm_instr_build::{InstructionBuilder, PatchPosition};
use swamp_vm_types::{
    BOOL_SIZE, BinaryInstruction, CountU16, FrameMemoryAddress, FrameMemoryAddressIndirectPointer,
    FrameMemorySize, HEAP_PTR_ALIGNMENT, HEAP_PTR_SIZE, HeapMemoryAddress, INT_SIZE,
    InstructionPosition, MemoryAlignment, MemoryOffset, MemorySize, PTR_SIZE,
    TempFrameMemoryAddress, VEC_ITERATOR_ALIGNMENT, VEC_ITERATOR_SIZE,
};
use tracing::{error, info, trace};

#[derive(Debug)]
pub enum ErrorKind {
    IllegalCompoundAssignment,
    VariableNotUnique,
    IllegalCollection,
    NotAnIterableCollection,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub node: Node,
}

pub struct SlicePairInfo {
    pub addr: TempFrameMemoryAddress,
    pub key_size: MemorySize,
    pub value_size: MemorySize,
    pub element_count: CountU16,
    pub element_size: MemorySize,
}

pub struct FunctionInfo {
    pub starts_at_ip: InstructionPosition,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct FunctionFixup {
    pub patch_position: PatchPosition,
    pub fn_id: InternalFunctionId,
    //pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct ConstantInfo {
    pub ip: InstructionPosition,
    pub constant_ref: ConstantRef,
}

pub struct CodeGenState {
    builder: InstructionBuilder,
    constants: ConstantsManager,
    constant_offsets: SeqMap<ConstantId, ConstantMemoryRegion>,
    constant_functions: SeqMap<ConstantId, ConstantInfo>,
    function_infos: SeqMap<InternalFunctionId, FunctionInfo>,
    function_fixups: Vec<FunctionFixup>,
}

pub struct GenOptions {
    pub is_halt_function: bool,
}

impl CodeGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: InstructionBuilder::default(),
            constants: ConstantsManager::new(),
            constant_offsets: SeqMap::default(),
            function_infos: SeqMap::default(),
            constant_functions: SeqMap::default(),
            function_fixups: vec![],
        }
    }

    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.builder.instructions
    }
    pub fn create_function_sections(&self) -> SeqMap<InstructionPosition, String> {
        let mut lookups = SeqMap::new();
        for (_func_id, function_info) in &self.function_infos {
            let description = format!(
                "{}",
                function_info.internal_function_definition.assigned_name
            );
            lookups
                .insert(function_info.starts_at_ip.clone(), description)
                .unwrap()
        }

        lookups
    }
    #[must_use]
    pub fn builder(&self) -> &InstructionBuilder {
        &self.builder
    }
    pub fn constant_functions(&self) -> &SeqMap<ConstantId, ConstantInfo> {
        &self.constant_functions
    }
    pub(crate) fn add_call(&mut self, internal_fn: &InternalFunctionDefinitionRef, comment: &str) {
        let call_comment = &format!("calling {} ({})", internal_fn.assigned_name, comment);

        if let Some(found) = self.function_infos.get(&internal_fn.program_unique_id) {
            self.builder.add_call(&found.starts_at_ip, call_comment);
        } else {
            let patch_position = self.builder.add_call_placeholder(call_comment);
            self.function_fixups.push(FunctionFixup {
                patch_position,
                fn_id: internal_fn.program_unique_id,
            });
        }
    }
    #[must_use]
    pub fn comments(&self) -> &[String] {
        &self.builder.comments
    }

    pub fn finalize(&mut self) {
        for function_fixup in &self.function_fixups {
            let func = self.function_infos.get(&function_fixup.fn_id).unwrap();
            self.builder.patch_call(
                PatchPosition(InstructionPosition(function_fixup.patch_position.0.0)),
                &func.starts_at_ip,
            );
        }
    }

    #[must_use]
    pub fn take_instructions_and_constants(self) -> (Vec<BinaryInstruction>, Vec<u8>) {
        (self.builder.instructions, self.constants.take_data())
    }

    pub fn gen_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        options: &GenOptions,
    ) -> Result<(), Error> {
        assert_ne!(internal_fn_def.program_unique_id, 0);
        self.function_infos
            .insert(
                internal_fn_def.program_unique_id,
                FunctionInfo {
                    starts_at_ip: self.builder.position(),
                    internal_function_definition: internal_fn_def.clone(),
                },
            )
            .unwrap();

        let mut function_generator = FunctionCodeGen::new(self);

        let ctx = Context::new(FrameMemoryRegion::new(
            FrameMemoryAddress(0),
            MemorySize(512),
        ));

        function_generator.layout_variables(
            &internal_fn_def.function_scope_state,
            &internal_fn_def.signature.return_type,
        );

        let ExpressionKind::Block(block_expressions) = &internal_fn_def.body.kind else {
            panic!("function body should be a block")
        };

        if let ExpressionKind::IntrinsicCallEx(found_intrinsic_fn, _non_instantiated_arguments) =
            &block_expressions[0].kind
        {
            // Intentionally do nothing
        } else {
            function_generator.gen_expression(&internal_fn_def.body, &ctx)?;
        }

        self.finalize_function(options);

        Ok(())
    }

    pub fn finalize_function(&mut self, options: &GenOptions) {
        if options.is_halt_function {
            self.builder.add_hlt("");
        } else {
            self.builder.add_ret("");
        }
    }

    pub fn gen_constants_in_order(&mut self, constants: &[ConstantRef]) -> Result<(), Error> {
        for constant in constants {
            let (size, _alignment) = type_size_and_alignment(&constant.resolved_type);
            let ip = self.builder.position();
            {
                let mut function_generator = FunctionCodeGen::new(self);

                let empty_ctx = Context::new(FrameMemoryRegion::new(FrameMemoryAddress(0), size));
                function_generator.gen_expression(&constant.expr, &empty_ctx)?;
                self.finalize_function(&GenOptions {
                    is_halt_function: true,
                });
            }

            let constant_info = ConstantInfo {
                ip,
                //memory_offset:
                constant_ref: constant.clone(),
            };

            self.constant_functions
                .insert(constant.id, constant_info)
                .unwrap();
        }

        Ok(())
    }

    /// # Errors
    ///
    pub fn gen_main_function(
        &mut self,
        main: &InternalMainExpression,
        options: &GenOptions,
    ) -> Result<(), Error> {
        let mut function_generator = FunctionCodeGen::new(self);

        function_generator.layout_variables(&main.function_scope_state, &main.expression.ty);
        let empty_ctx = Context::new(FrameMemoryRegion::default());
        function_generator.gen_expression(&main.expression, &empty_ctx)?;
        self.finalize_function(options);
        Ok(())
    }
}

pub struct FunctionCodeGen<'a> {
    state: &'a mut CodeGenState,
    variable_offsets: SeqMap<usize, FrameMemoryRegion>,
    frame_size: FrameMemorySize,
    extra_frame_allocator: ScopeAllocator,
    temp_allocator: ScopeAllocator,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(state: &'a mut CodeGenState) -> Self {
        Self {
            state,
            variable_offsets: SeqMap::default(),
            frame_size: FrameMemorySize(0),
            extra_frame_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
            temp_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
        }
    }
}

impl FunctionCodeGen<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn gen_single_intrinsic_call(
        &mut self,
        intrinsic_fn: &IntrinsicFunction,
        self_addr: Option<FrameMemoryRegion>,
        arguments: &[ArgumentExpressionOrLocation],
        ctx: &Context,
    ) -> Result<(), Error> {
        if arguments.is_empty() {
            return Ok(());
        }
        info!(?intrinsic_fn, "generate specific call for intrinsic");
        match intrinsic_fn {
            // Fixed
            IntrinsicFunction::FloatRound => todo!(),
            IntrinsicFunction::FloatFloor => todo!(),
            IntrinsicFunction::FloatSqrt => todo!(),
            IntrinsicFunction::FloatSign => todo!(),
            IntrinsicFunction::FloatAbs => todo!(),
            IntrinsicFunction::FloatRnd => todo!(),
            IntrinsicFunction::FloatCos => todo!(),
            IntrinsicFunction::FloatSin => todo!(),
            IntrinsicFunction::FloatAcos => todo!(),
            IntrinsicFunction::FloatAsin => todo!(),
            IntrinsicFunction::FloatAtan2 => todo!(),
            IntrinsicFunction::FloatMin => todo!(),
            IntrinsicFunction::FloatMax => todo!(),
            IntrinsicFunction::FloatClamp => todo!(),
            // Int
            IntrinsicFunction::IntAbs => todo!(),
            IntrinsicFunction::IntRnd => todo!(),
            IntrinsicFunction::IntMax => todo!(),
            IntrinsicFunction::IntMin => todo!(),
            IntrinsicFunction::IntClamp => todo!(),
            IntrinsicFunction::IntToFloat => todo!(),

            // String
            IntrinsicFunction::StringLen => todo!(),

            // Vec
            IntrinsicFunction::VecFromSlice => {
                let slice_variable = &arguments[0];
                let slice_region = self.gen_for_access_or_location_ex(slice_variable)?;
                let (element_size, element_alignment) =
                    type_size_and_alignment(&slice_variable.ty());
                self.state.builder.add_vec_from_slice(
                    ctx.addr(),
                    slice_region.addr,
                    element_size,
                    CountU16(slice_region.size.0 / element_size.0),
                    "create vec from slice",
                );
                Ok(())
            }
            IntrinsicFunction::VecPush => todo!(),
            IntrinsicFunction::VecPop => todo!(),
            IntrinsicFunction::VecRemoveIndex => todo!(),
            IntrinsicFunction::VecClear => todo!(),
            IntrinsicFunction::VecCreate => todo!(),
            IntrinsicFunction::VecSubscript => todo!(),
            IntrinsicFunction::VecSubscriptMut => todo!(),
            IntrinsicFunction::VecIter => todo!(),
            IntrinsicFunction::VecIterMut => todo!(),
            IntrinsicFunction::VecSelfPush => todo!(),
            IntrinsicFunction::VecSelfExtend => todo!(),

            // Map
            IntrinsicFunction::MapCreate => todo!(),
            IntrinsicFunction::MapFromSlicePair => {
                let slice_pair_argument = &arguments[0];
                let ArgumentExpressionOrLocation::Expression(expr) = slice_pair_argument else {
                    panic!();
                };

                let ExpressionKind::Literal(some_lit) = &expr.kind else {
                    panic!();
                };

                let Literal::SlicePair(slice_type, expression_pairs) = some_lit else {
                    panic!();
                };

                let slice_pair_info = self.gen_slice_pair_literal(slice_type, expression_pairs);
                self.state.builder.add_map_new_from_slice(
                    ctx.addr(),
                    slice_pair_info.addr.to_addr(),
                    slice_pair_info.key_size,
                    slice_pair_info.value_size,
                    slice_pair_info.element_count,
                    "create map from temporary slice pair",
                );

                Ok(())
            }
            IntrinsicFunction::MapHas => todo!(),
            IntrinsicFunction::MapRemove => {
                let ArgumentExpressionOrLocation::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.gen_intrinsic_map_remove(self_addr.unwrap(), key_argument, ctx)
            }
            IntrinsicFunction::MapIter => todo!(),
            IntrinsicFunction::MapIterMut => todo!(),
            IntrinsicFunction::MapLen => todo!(),
            IntrinsicFunction::MapIsEmpty => todo!(),
            IntrinsicFunction::MapSubscript => todo!(),
            IntrinsicFunction::MapSubscriptSet => todo!(),
            IntrinsicFunction::MapSubscriptMut => todo!(),
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => todo!(),

            // Sparse
            IntrinsicFunction::SparseCreate => todo!(),
            IntrinsicFunction::SparseFromSlice => todo!(),
            IntrinsicFunction::SparseIter => todo!(),
            IntrinsicFunction::SparseIterMut => todo!(),
            IntrinsicFunction::SparseSubscript => todo!(),
            IntrinsicFunction::SparseSubscriptMut => todo!(),
            IntrinsicFunction::SparseHas => todo!(),
            IntrinsicFunction::SparseRemove => todo!(),

            // Grid
            IntrinsicFunction::GridCreate => todo!(),
            IntrinsicFunction::GridFromSlice => todo!(),
            IntrinsicFunction::GridSubscript => todo!(),
            IntrinsicFunction::GridSubscriptMut => todo!(),

            IntrinsicFunction::Float2Magnitude => todo!(),

            IntrinsicFunction::SparseAdd => todo!(),
            IntrinsicFunction::VecLen => todo!(),
            IntrinsicFunction::VecIsEmpty => todo!(),
            IntrinsicFunction::SparseNew => todo!(),
        }
    }

    fn gen_intrinsic_map_remove(
        &mut self,
        map_region: FrameMemoryRegion,
        key_expr: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let key_region = self.gen_expression_for_access(key_expr)?;

        self.state
            .builder
            .add_map_remove(map_region.addr, key_region.addr, "");

        Ok(())
    }

    pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
        let (size, alignment) = type_size_and_alignment(ty);
        allocator.reserve(size, alignment)
    }

    /// # Errors
    ///
    pub fn layout_variables(
        &mut self,
        variables: &Vec<VariableRef>,
        return_type: &Type,
    ) -> Result<(), Error> {
        let mut allocator = ScopeAllocator::new(FrameMemoryRegion::new(
            FrameMemoryAddress(0),
            MemorySize(1024),
        ));
        let _current_offset = Self::reserve(return_type, &mut allocator);

        let mut enter_comment = "variables:\n".to_string();

        for var_ref in variables {
            let var_target = Self::reserve(&var_ref.resolved_type, &mut allocator);
            trace!(?var_ref.assigned_name, ?var_target, "laying out");
            enter_comment += &format!(
                "  ${:04X}:{} {}\n",
                var_target.addr.0, var_target.size.0, var_ref.assigned_name
            );
            self.variable_offsets
                .insert(var_ref.unique_id_within_function, var_target)
                .map_err(|_| self.create_err(ErrorKind::VariableNotUnique, &var_ref.name))?;
        }

        let extra_frame_size = MemorySize(80);
        let extra_target = FrameMemoryRegion::new(allocator.addr(), extra_frame_size);
        self.extra_frame_allocator = ScopeAllocator::new(extra_target);
        self.frame_size = allocator.addr().as_size().add(extra_frame_size);

        self.state
            .builder
            .add_enter(self.frame_size, &enter_comment);

        self.temp_allocator = ScopeAllocator::new(FrameMemoryRegion::new(
            FrameMemoryAddress(self.frame_size.0),
            MemorySize(1024),
        ));

        Ok(())
    }

    pub fn temp_memory_region_for_type(&mut self, ty: &Type, comment: &str) -> FrameMemoryRegion {
        let new_target_info = reserve_space_for_type(ty, &mut self.temp_allocator);
        trace!(?new_target_info, "creating temporary space");
        new_target_info
    }

    pub fn temp_space_for_type(&mut self, ty: &Type, comment: &str) -> Context {
        Context::new(self.temp_memory_region_for_type(ty, comment))
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn gen_expression_for_access(
        &mut self,
        expr: &Expression,
    ) -> Result<FrameMemoryRegion, Error> {
        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                info!(?var_ref, "variable access");
                let frame_address = self
                    .variable_offsets
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                return Ok(*frame_address);
            }

            ExpressionKind::Literal(lit) => match lit {
                Literal::Slice(slice_type, expressions) => {
                    return self.gen_slice_literal(slice_type, expressions);
                }
                Literal::SlicePair(slice_pair_type, pairs) => {
                    let info = self.gen_slice_pair_literal(slice_pair_type, pairs);
                    return Ok(FrameMemoryRegion::new(
                        info.addr.0,
                        MemorySize(info.element_count.0 * info.element_size.0),
                    ));
                }
                _ => {}
            },
            _ => {}
        };

        let temp_ctx = self.temp_space_for_type(&expr.ty, "expression");

        self.gen_expression(expr, &temp_ctx)?;

        Ok(temp_ctx.target())
    }

    pub(crate) fn extra_frame_space_for_type(&mut self, ty: &Type) -> Context {
        let target = Self::reserve(ty, &mut self.extra_frame_allocator);
        Context::new(target)
    }

    pub fn gen_expression(&mut self, expr: &Expression, ctx: &Context) -> Result<(), Error> {
        match &expr.kind {
            ExpressionKind::InterpolatedString(_) => todo!(),

            ExpressionKind::ConstantAccess(constant_ref) => {
                self.gen_constant_access(constant_ref, ctx)
            }
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                self.gen_tuple_destructuring(variables, tuple_types, tuple_expression)
            }
            ExpressionKind::Range(start, end, mode) => self.gen_range(start, end, mode, ctx),

            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                self.gen_assignment(target_mut_location_expr, source_expr)
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                self.gen_variable_access(variable_ref, ctx)
            }
            ExpressionKind::InternalFunctionAccess(function) => {
                self.internal_function_access(function, ctx)
            }
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(operator) => self.gen_unary_operator(operator, ctx),
            ExpressionKind::PostfixChain(start, chain) => self.gen_postfix_chain(start, chain, ctx),
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.gen_variable_definition(variable, expression, ctx)
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                self.gen_variable_reassignment(variable, expression, ctx)
            }
            ExpressionKind::StructInstantiation(struct_literal) => {
                self.gen_struct_literal(struct_literal, ctx)
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                self.gen_anonymous_struct_literal(anon_struct, ctx)
            }
            ExpressionKind::Literal(basic_literal) => self.gen_literal(basic_literal, ctx),
            ExpressionKind::Option(maybe_option) => {
                self.gen_option_expression(maybe_option.as_deref(), ctx)
            }
            ExpressionKind::ForLoop(a, b, c) => self.gen_for_loop(a, b, c),
            ExpressionKind::WhileLoop(condition, expression) => {
                self.gen_while_loop(condition, expression, ctx)
            }
            ExpressionKind::Block(expressions) => self.gen_block(expressions, ctx),
            ExpressionKind::Match(match_expr) => self.gen_match(match_expr, ctx),
            ExpressionKind::Guard(guards) => self.gen_guard(guards, ctx),
            ExpressionKind::If(conditional, true_expr, false_expr) => {
                self.gen_if(conditional, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.gen_when(bindings, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                self.compound_assignment(target_location, operator_kind, source_expr, ctx)
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.gen_intrinsic_call_ex(intrinsic_fn, arguments, ctx)
            }
            // --------- Not high prio
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::FunctionValueCall(_, _, _) => todo!(),

            // --------- TO BE REMOVED
            ExpressionKind::IntrinsicFunctionAccess(_) => todo!(), // TODO: IntrinsicFunctionAccess should be reduced away in analyzer
            ExpressionKind::ExternalFunctionAccess(_) => todo!(), // TODO: ExternalFunctionAccess should be reduced away in analyzer
        }
    }

    fn gen_unary_operator(
        &mut self,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> Result<(), Error> {
        match &unary_operator.kind {
            UnaryOperatorKind::Not => {}
            UnaryOperatorKind::Negate => match (&unary_operator.left.ty) {
                Type::Int => {
                    let left_source = self.gen_expression_for_access(&unary_operator.left)?;
                    self.state
                        .builder
                        .add_neg_i32(ctx.addr(), left_source.addr, "negate i32");
                }

                Type::Float => {
                    let left_source = self.gen_expression_for_access(&unary_operator.left)?;
                    self.state
                        .builder
                        .add_neg_f32(ctx.addr(), left_source.addr, "negate f32");
                }
                _ => todo!(),
            },
        }

        Ok(())
    }

    fn gen_binary_operator(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<(), Error> {
        match (&binary_operator.left.ty, &binary_operator.right.ty) {
            (Type::Int, Type::Int) => self.gen_binary_operator_i32(binary_operator, ctx)?,
            (Type::Bool, Type::Bool) => self.gen_binary_operator_bool(binary_operator)?,
            _ => todo!(),
        }

        Ok(())
    }

    fn gen_binary_operator_i32(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<(), Error> {
        let left_source = self.gen_expression_for_access(&binary_operator.left)?;
        let right_source = self.gen_expression_for_access(&binary_operator.right)?;

        match binary_operator.kind {
            BinaryOperatorKind::Add => {
                self.state.builder.add_add_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    "i32 add",
                );
            }

            BinaryOperatorKind::Subtract => todo!(),
            BinaryOperatorKind::Multiply => {
                self.state.builder.add_mul_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    "i32 add",
                );
            }
            BinaryOperatorKind::Divide => todo!(),
            BinaryOperatorKind::Modulo => todo!(),
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            BinaryOperatorKind::LessThan => {
                self.state
                    .builder
                    .add_lt_i32(left_source.addr(), right_source.addr(), "i32 lt");
            }
            BinaryOperatorKind::LessEqual => todo!(),
            BinaryOperatorKind::GreaterThan => {
                self.state
                    .builder
                    .add_gt_i32(left_source.addr(), right_source.addr(), "i32 gt");
            }
            BinaryOperatorKind::GreaterEqual => todo!(),
            BinaryOperatorKind::RangeExclusive => todo!(),
        }

        Ok(())
    }

    fn gen_binary_operator_bool(&mut self, binary_operator: &BinaryOperator) -> Result<(), Error> {
        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                // this updates the z flag
                self.gen_boolean_access(&binary_operator.left);

                let jump_after_patch = self
                    .state
                    .builder
                    .add_jmp_if_equal_placeholder("skip rhs `or` expression");

                // this updates the z flag
                self.gen_boolean_access(&binary_operator.right);

                self.state.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                // this updates the z flag
                self.gen_boolean_access(&binary_operator.left);

                let jump_after_patch = self
                    .state
                    .builder
                    .add_jmp_if_not_equal_placeholder("skip rhs `and` expression");

                // this updates the z flag
                self.gen_boolean_access(&binary_operator.right);

                self.state.builder.patch_jump_here(jump_after_patch);
            }
            _ => {
                panic!("unknown operator")
            }
        }

        Ok(())
    }

    fn gen_condition_context(
        &mut self,
        condition: &BooleanExpression,
    ) -> Result<(Context, PatchPosition), Error> {
        let condition_ctx = self.extra_frame_space_for_type(&Type::Bool);
        self.gen_expression(&condition.expression, &condition_ctx)?;

        let jump_on_false_condition = self
            .state
            .builder
            .add_jmp_if_not_equal_placeholder("jump boolean condition false");

        Ok((condition_ctx, jump_on_false_condition))
    }

    fn gen_boolean_access(&mut self, condition: &Expression) {
        let _frame_memory_region = self.gen_expression_for_access(&condition);
        /*
        // HACK:
        if frame_memory_region.size.0 == 1 {
            self.state.builder.add_tst8(
                frame_memory_region.addr,
                "convert to boolean expression (update z flag)",
            );
        }

         */
    }

    fn gen_boolean_expression(&mut self, condition: &BooleanExpression) {
        self.gen_boolean_access(&condition.expression);
    }

    fn gen_if(
        &mut self,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition)?;

        // True expression just takes over our target
        self.gen_expression(true_expr, ctx)?;

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self
                .state
                .builder
                .add_jump_placeholder("condition is false skip");

            // If the expression was false, it should continue here
            self.state.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.gen_expression(false_expr, ctx)?;

            self.state.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.state.builder.patch_jump_here(jump_on_false_condition);
        }

        Ok(())
    }

    fn gen_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.state.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition)?;

        // Expression is only for side effects
        let mut unit_ctx = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.gen_expression(expression, &mut unit_ctx)?;

        // Always jump to the condition again to see if it is true
        self.state
            .builder
            .add_jmp(ip_for_condition, "jmp to while condition");

        self.state.builder.patch_jump_here(jump_on_false_condition);

        Ok(())
    }

    fn gen_location_argument(
        &mut self,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        let region = self.gen_lvalue_address(argument)?;

        self.state
            .builder
            .add_mov(ctx.addr(), region.addr, region.size, comment);

        Ok(())
    }

    fn gen_variable_assignment(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name));

        let init_ctx =
            ctx.with_target(*target_relative_frame_pointer, "variable assignment target");

        self.gen_mut_or_immute(mut_or_immutable_expression, &init_ctx)
    }

    fn gen_assignment(
        &mut self,
        lhs: &SingleMutLocationExpression,
        rhs: &Expression,
    ) -> Result<(), Error> {
        let lhs_addr = self.gen_lvalue_address(&lhs.0)?;
        let access = self.gen_expression_for_access(rhs)?;

        self.state
            .builder
            .add_mov(lhs_addr.addr, access.addr, access.size, "assignment");

        Ok(())
    }

    fn gen_variable_definition(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.gen_variable_assignment(variable, mut_or_immutable_expression, ctx)
    }

    fn gen_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &Box<MutOrImmutableExpression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.gen_variable_assignment(variable, mut_or_immutable_expression, ctx)
    }

    fn copy_back_mutable_arguments(
        &mut self,
        signature: &Signature,
        maybe_self: Option<FrameMemoryRegion>,
        arguments: &Vec<ArgumentExpressionOrLocation>,
    ) -> Result<(), Error> {
        let arguments_memory_region = self.infinite_above_frame_size();
        let mut arguments_allocator = ScopeAllocator::new(arguments_memory_region);

        let _argument_addr = Self::reserve(&signature.return_type, &mut arguments_allocator);

        let mut parameters = signature.parameters.clone();
        if let Some(found_self) = maybe_self {
            let source_region =
                Self::reserve(&parameters[0].resolved_type, &mut arguments_allocator);
            self.state.builder.add_mov(
                found_self.addr,
                source_region.addr,
                source_region.size,
                "copy back to <self>",
            );
            parameters.remove(0);
        }
        for (parameter, argument) in parameters.iter().zip(arguments) {
            let source_region = Self::reserve(&parameter.resolved_type, &mut arguments_allocator);
            if !parameter.is_mutable {
                continue;
            }

            if let ArgumentExpressionOrLocation::Location(found_location) = argument {
                let argument_target = self.gen_lvalue_address(found_location)?;
                self.state.builder.add_mov(
                    argument_target.addr,
                    source_region.addr,
                    source_region.size,
                    &format!(
                        "copy back mutable argument {}",
                        found_location.starting_variable.assigned_name
                    ),
                );
            } else {
                panic!("internal error. argument is mut but not a location")
            }
        }
        Ok(())
    }
    fn gen_arguments(
        &mut self,
        signature: &Signature,
        self_region: Option<FrameMemoryRegion>,
        arguments: &Vec<ArgumentExpressionOrLocation>,
    ) -> FrameMemoryRegion {
        let arguments_memory_region = self.infinite_above_frame_size();
        let mut arguments_allocator = ScopeAllocator::new(arguments_memory_region);

        let _argument_addr = Self::reserve(&signature.return_type, &mut arguments_allocator);

        let mut argument_targets = Vec::new();
        let mut argument_comments = Vec::new();

        for type_for_parameter in &signature.parameters {
            let argument_target =
                Self::reserve(&type_for_parameter.resolved_type, &mut arguments_allocator);
            let arg_ctx = Context::new(argument_target);
            argument_targets.push(arg_ctx);
            argument_comments.push(format!("argument {}", type_for_parameter.name));
        }

        if let Some(push_self) = self_region {
            self.state.builder.add_mov(
                argument_targets[0].addr(),
                push_self.addr,
                push_self.size,
                "<self>",
            );
            argument_targets.remove(0);
        }

        for ((argument_target_ctx, argument_expr_or_loc), argument_comment) in argument_targets
            .iter()
            .zip(arguments)
            .zip(argument_comments)
        {
            self.gen_argument(
                argument_expr_or_loc,
                &argument_target_ctx,
                &argument_comment,
            );
        }

        let last_addr = argument_targets[argument_targets.len() - 1]
            .addr()
            .add(argument_targets[argument_targets.len() - 1].target_size());
        FrameMemoryRegion {
            addr: argument_targets[0].addr(),
            size: MemorySize(last_addr.0 - argument_targets[0].addr().0),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn gen_postfix_chain(
        &mut self,
        start_expression: &Expression,
        chain: &[Postfix],
        ctx: &Context,
    ) -> Result<(), Error> {
        if let ExpressionKind::InternalFunctionAccess(internal_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
                    if let Some(intrinsic_fn) = single_intrinsic_fn(&internal_fn.body) {
                        self.gen_single_intrinsic_call(intrinsic_fn, None, args, ctx);
                    } else {
                        self.gen_arguments(&internal_fn.signature, None, args);
                        self.state
                            .add_call(internal_fn, &format!("frame size: {}", self.frame_size)); // will be fixed up later
                        let (return_size, _alignment) =
                            type_size_and_alignment(&internal_fn.signature.return_type);
                        if return_size.0 != 0 {
                            self.state.builder.add_mov(
                                ctx.addr(),
                                self.infinite_above_frame_size().addr,
                                return_size,
                                "copy the ret value to destination",
                            );
                        }
                        self.copy_back_mutable_arguments(&internal_fn.signature, None, args);
                    }

                    return Ok(());
                }
            }
        }

        if let ExpressionKind::ExternalFunctionAccess(external_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
                    let total_region = self.gen_arguments(&external_fn.signature, None, args);
                    self.state.builder.add_host_call(
                        external_fn.id as u16,
                        total_region.size,
                        &format!("call external '{}'", external_fn.assigned_name),
                    );
                    let (return_size, _alignment) =
                        type_size_and_alignment(&external_fn.signature.return_type);
                    if return_size.0 != 0 {
                        self.state.builder.add_mov(
                            ctx.addr(),
                            self.infinite_above_frame_size().addr,
                            return_size,
                            "copy the ret value to destination",
                        );
                    }

                    return Ok(());
                }
            }
        }

        let mut start_source = self.gen_expression_for_access(start_expression)?;

        for element in chain {
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let (memory_offset, memory_size, _max_alignment) =
                        Self::get_struct_field_offset(
                            &anonymous_struct.field_name_sorted_fields,
                            *field_index,
                        );
                    info!(
                        ?field_index,
                        ?memory_offset,
                        ?memory_size,
                        "lookup struct field",
                    );
                    start_source = FrameMemoryRegion::new(
                        start_source.addr.advance(memory_offset),
                        memory_size,
                    );
                }
                PostfixKind::MemberCall(function_to_call, arguments) => {
                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            if let Some(intrinsic_fn) = single_intrinsic_fn(&internal_fn.body) {
                                self.gen_single_intrinsic_call(
                                    intrinsic_fn,
                                    Some(start_source),
                                    arguments,
                                    ctx,
                                );
                            } else {
                                self.gen_arguments(
                                    &internal_fn.signature,
                                    Some(start_source),
                                    arguments,
                                );
                                self.state.add_call(
                                    internal_fn,
                                    &format!("frame size: {}", self.frame_size),
                                ); // will be fixed up later

                                let (return_size, _alignment) =
                                    type_size_and_alignment(&internal_fn.signature.return_type);
                                if return_size.0 != 0 {
                                    self.state.builder.add_mov(
                                        ctx.addr(),
                                        self.infinite_above_frame_size().addr,
                                        return_size,
                                        "copy the return value to destination",
                                    );
                                }

                                self.copy_back_mutable_arguments(
                                    &internal_fn.signature,
                                    Some(start_source),
                                    arguments,
                                );
                            }
                        }
                        Function::External(external_fn) => {
                            //self.state.builder.add_host_call(external_fn.id);
                        }
                    }
                }
                PostfixKind::FunctionCall(arguments) => {
                    //self.gen_arguments(arguments);
                    //self.state.add_call(start_expression)
                }
                PostfixKind::OptionUnwrap => todo!(),
                PostfixKind::NoneCoalesce(_) => todo!(),
                PostfixKind::IntrinsicCall(_, _) => todo!(),
            }
        }

        Ok(())
    }

    fn gen_tuple(&mut self, expressions: &[Expression], ctx: &Context) -> Result<(), Error> {
        let mut scope = ScopeAllocator::new(ctx.target());

        for expr in expressions {
            let (memory_size, alignment) = type_size_and_alignment(&expr.ty);
            let start_addr = scope.allocate(memory_size, alignment);
            let element_region = FrameMemoryRegion::new(start_addr, memory_size);
            let element_ctx = Context::new(element_region);
            self.gen_expression(expr, &element_ctx)?;
        }

        Ok(())
    }

    fn get_struct_field_offset(
        fields: &SeqMap<String, StructTypeField>,
        index_to_find: usize,
    ) -> (MemoryOffset, MemorySize, MemoryAlignment) {
        let mut offset = 0;

        for (index, (_name, field)) in fields.iter().enumerate() {
            let (struct_field_size, struct_field_align) =
                type_size_and_alignment(&field.field_type);
            if index == index_to_find {
                return (MemoryOffset(offset), struct_field_size, struct_field_align);
            }

            offset += struct_field_size.0;
        }

        panic!("field not found");
    }

    fn gen_anonymous_struct(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        base_context: &Context,
    ) -> Result<(), Error> {
        for (field_index, expression) in source_order_expressions {
            let (field_memory_offset, field_size, _field_alignment) = Self::get_struct_field_offset(
                &anon_struct_type.field_name_sorted_fields,
                *field_index,
            );
            let field_ctx = base_context.with_offset(field_memory_offset, field_size);
            self.gen_expression(expression, &field_ctx)?;
        }

        Ok(())
    }

    fn gen_literal(&mut self, literal: &Literal, ctx: &Context) -> Result<(), Error> {
        match literal {
            Literal::IntLiteral(int) => {
                self.state.builder.add_ld32(ctx.addr(), *int, "int literal");
            }
            Literal::FloatLiteral(fixed_point) => {
                self.state
                    .builder
                    .add_ld32(ctx.addr(), fixed_point.inner(), "float literal");
            }
            Literal::NoneLiteral => {
                self.state.builder.add_ld8(ctx.addr(), 0, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.state
                    .builder
                    .add_ld8(ctx.addr(), u8::from(*truthy), "bool literal");
            }

            Literal::EnumVariantLiteral(enum_type, a, b) => {
                self.state.builder.add_ld8(
                    ctx.addr(),
                    a.common().container_index,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );

                let starting_offset = MemoryOffset(1);

                let (data_size, data_alignment) = match a {
                    EnumVariantType::Struct(enum_variant_struct) => {
                        layout_struct(&enum_variant_struct.anon_struct)
                    }
                    EnumVariantType::Tuple(tuple_type) => layout_tuple(&tuple_type.fields_in_order),
                    EnumVariantType::Nothing(_) => (MemorySize(0), MemoryAlignment::U8),
                };

                let skip_octets: usize = data_alignment.into();
                let skip = MemorySize(skip_octets as u16);
                let inner_addr = ctx.addr().add(skip);
                let region = FrameMemoryRegion::new(inner_addr, data_size);
                let inner_ctx = Context::new(region);

                //layout_union(a)
                match b {
                    EnumLiteralData::Nothing => {}
                    EnumLiteralData::Tuple(expressions) => {
                        self.gen_tuple(expressions, &inner_ctx)?;
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        if let EnumVariantType::Struct(variant_struct_type) = a {
                            self.gen_anonymous_struct(
                                &variant_struct_type.anon_struct,
                                sorted_expressions,
                                &inner_ctx,
                            )?;
                        }
                    }
                }
            }
            Literal::TupleLiteral(_tuple_type, expressions) => self.gen_tuple(expressions, ctx)?,
            Literal::StringLiteral(str) => {
                self.gen_string_literal(str, ctx);
            }
            Literal::Slice(ty, expressions) => {
                //self.gen_slice_literal(ty, expressions, ctx)
                todo!()
            }
            Literal::SlicePair(ty, expression_pairs) => {
                todo!()
            }
        }

        Ok(())
    }

    fn gen_string_literal(&mut self, string: &str, ctx: &Context) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();

        let data_ptr = self
            .state
            .constants
            .allocate(string_bytes, MemoryAlignment::U8);

        let mem_size = MemorySize(string_byte_count as u16);

        self.state.builder.add_string_from_constant_slice(
            ctx.addr(),
            data_ptr,
            mem_size,
            "create string",
        );
        // self.gen_vec_immediate(data_ptr, mem_size, mem_size, "string", ctx);
    }

    /*
    fn gen_vec_immediate(
        &mut self,
        data_ptr: MemoryAddress,
        len: MemorySize,
        capacity: MemorySize,
        comment_prefix: &str,
        ctx: &Context,
    ) {
        self.state
            .builder
            .add_ld_u16(ctx.addr(), len.0, &format!("{} len", comment_prefix));

        self.state.builder.add_ld_u16(
            ctx.addr().add(MemorySize(2)),
            capacity.0,
            &format!("{} capacity", comment_prefix),
        );

        self.state.builder.add_ld_u16(
            ctx.addr().add(MemorySize(4)),
            data_ptr.0,
            &format!("{} ptr", comment_prefix),
        );
    }


     */
    fn gen_option_expression(
        &mut self,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        if let Some(found_value) = maybe_option {
            self.state.builder.add_ld8(ctx.addr(), 1, "option Some tag"); // 1 signals `Some`
            let (inner_size, inner_alignment) = type_size_and_alignment(&found_value.ty);
            let one_offset_ctx = ctx.with_offset(inner_alignment.into(), inner_size);

            self.gen_expression(found_value, &one_offset_ctx)?; // Fills in more of the union
        } else {
            self.state.builder.add_ld8(ctx.addr(), 0, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }

        Ok(())
    }

    fn gen_for_loop_vec(
        &mut self,
        for_pattern: &ForPattern,
        collection_expr: &MutOrImmutableExpression,
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        let collection_region = self.gen_for_access_or_location(collection_expr)?;

        let temp_iterator_region = self
            .temp_allocator
            .allocate(MemorySize(VEC_ITERATOR_SIZE), VEC_ITERATOR_ALIGNMENT);
        self.state.builder.add_vec_iter_init(
            temp_iterator_region,
            FrameMemoryAddressIndirectPointer(collection_region.addr),
            "initialize vec iterator",
        );

        let loop_ip = self.state.builder.position();

        let placeholder_position = match for_pattern {
            ForPattern::Single(variable) => {
                let target_variable = self
                    .variable_offsets
                    .get(&variable.unique_id_within_function)
                    .unwrap();
                self.state.builder.add_vec_iter_next_placeholder(
                    temp_iterator_region,
                    target_variable.addr,
                    "move to next or jump over",
                )
            }
            ForPattern::Pair(variable_a, variable_b) => {
                let target_variable_a = self
                    .variable_offsets
                    .get(&variable_a.unique_id_within_function)
                    .unwrap();
                let target_variable_b = self
                    .variable_offsets
                    .get(&variable_b.unique_id_within_function)
                    .unwrap();
                self.state.builder.add_vec_iter_next_pair_placeholder(
                    temp_iterator_region,
                    target_variable_a.addr,
                    target_variable_b.addr,
                    "move to next or jump over",
                )
            }
        };

        Ok((loop_ip, placeholder_position))
    }

    fn gen_for_loop_map(
        &mut self,
        for_pattern: &ForPattern,
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        self.state.builder.add_map_iter_init(
            FrameMemoryAddress(0x80),
            FrameMemoryAddressIndirectPointer(FrameMemoryAddress(0xffff)),
            "initialize map iterator",
        );

        let jump_ip = self.state.builder.position();

        match for_pattern {
            ForPattern::Single(_) => {
                self.state.builder.add_map_iter_next(
                    FrameMemoryAddress(0x80),
                    FrameMemoryAddress(0x16),
                    InstructionPosition(256),
                    "move to next or jump over",
                );
            }
            ForPattern::Pair(_, _) => {
                self.state.builder.add_map_iter_next_pair(
                    FrameMemoryAddress(0x80),
                    FrameMemoryAddress(0x16),
                    FrameMemoryAddress(0x16),
                    InstructionPosition(256),
                    "move to next or jump over",
                );
            }
        }

        Ok((jump_ip, PatchPosition(InstructionPosition(0))))
    }

    fn gen_for_loop(
        &mut self,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        closure: &Box<Expression>,
    ) -> Result<(), Error> {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        let collection_type = &iterable.resolved_expression.expression_or_location.ty();
        let (jump_ip, placeholder_position) = match collection_type {
            Type::String => {
                todo!();
            }
            Type::NamedStruct(_vec) => {
                if let Some(found_info) = is_vec(collection_type) {
                    self.gen_for_loop_vec(for_pattern, &iterable.resolved_expression)?
                } else if let Some(found_info) = is_map(collection_type) {
                    self.gen_for_loop_map(for_pattern)?
                } else {
                    return Err(self.create_err(
                        ErrorKind::NotAnIterableCollection,
                        iterable.resolved_expression.node(),
                    ));
                }
            }
            _ => {
                return Err(self.create_err(
                    ErrorKind::IllegalCollection,
                    iterable.resolved_expression.node(),
                ));
            }
        };

        match for_pattern {
            ForPattern::Single(value_variable) => {}
            ForPattern::Pair(key_variable, value_variable) => {}
        }

        let unit_expr = self.temp_space_for_type(&Type::Unit, "for loop body");
        self.gen_expression(closure, &unit_expr)?;

        self.state
            .builder
            .add_jmp(jump_ip, "jump to next iteration");
        // advance iterator pointer
        // jump to check if iterator pointer has reached its end
        self.state.builder.patch_jump_here(placeholder_position);

        Ok(())
    }

    fn gen_for_loop_for_vec(
        &mut self,
        element_type: &Type,
        vector_expr: Expression,
        ctx: &mut Context,
    ) -> Result<(), Error> {
        // get the vector that is referenced
        let vector_ctx = self.temp_space_for_type(&vector_expr.ty, "vector space");
        self.gen_expression(&vector_expr, &vector_ctx)

        /*
        let value_var_addr = match for_pattern {
            ForPattern::Single(value_variable) => self
                .variable_offsets
                .get(&value_variable.unique_id_within_function)
                .expect("Variable not found"),
            ForPattern::Pair(_, _) => {
                panic!("Cannot use key-value pattern with vectors");
            }
        };

         */

        /*
        let element_size = type_size(element_type);
               // Temporary for the counter
               let counter_addr = ctx.allocate_temp(MemorySize(2)); // u16 counter
               self.state
                   .builder
                   .add_ld_u16(counter_addr, 0, "temporary counter");

               let loop_start_pos = self.state.builder.position();

               // vector length
               let length_addr = ctx.allocate_temp(MemorySize(2));
               self.state.builder.add_mov(
                   length_addr,
                   vector_ctx.addr().add(MemorySize(VECTOR_LENGTH_OFFSET)),
                   MemorySize(2),
                   "vector length",
               );

               // Compare counter < length
               let compare_result_addr = ctx.allocate_temp(MemorySize(1)); // boolean result
               self.state.builder.add_lt_u16(
                   compare_result_addr,
                   counter_addr,
                   length_addr,
                   "counter < length",
               );

               // Exit loop if counter >= length
               let exit_jump = self
                   .state
                   .builder
                   .add_conditional_jump_placeholder(compare_result_addr, "counter >= length exit");

               let data_ptr_addr = ctx.allocate_temp(MemorySize(2));
               self.state.builder.add_mov(
                   data_ptr_addr,
                   vector_ctx.addr().add(MemorySize(VECTOR_DATA_PTR_OFFSET)),
                   MemorySize(PTR_SIZE),
                   "copy vector data ptr",
               );


        */
        /*
        let offset_addr = ctx.allocate_temp(2);
        self.state.builder.add_mul_u16(
            offset_addr,
            counter_addr,
            element_size
        );

        self.state.builder.add_ld_indirect(
            *value_var_addr,     // Destination: loop variable
            data_ptr_addr,       // Base: vector's data pointer
            offset_addr,         // Offset: counter * element_size
            element_size         // Size to copy
        );

        let mut body_ctx = ctx.temp_space_for_type(&Type::Unit);
        self.gen_expression(body, &mut body_ctx);

        self.state.builder.add_inc_u16(counter_addr);

        self.state.builder.add_jmp_to_position(loop_start_pos);

        let end_pos = self.state.builder.current_position();
        self.state.builder.patch_jump(exit_jump, end_pos);

         */
    }

    fn gen_block(&mut self, expressions: &[Expression], ctx: &Context) -> Result<(), Error> {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let temp_context = self.temp_space_for_type(&Type::Unit, "block target");
                self.gen_expression(expr, &temp_context)?;
            }
            self.gen_expression(last, ctx)?;
        }

        Ok(())
    }

    fn get_variable_region(&self, variable: &VariableRef) -> (FrameMemoryRegion, MemoryAlignment) {
        let frame_address = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap();
        let (_size, align) = type_size_and_alignment(&variable.resolved_type);

        (*frame_address, align)
    }

    fn gen_variable_access(&mut self, variable: &VariableRef, ctx: &Context) -> Result<(), Error> {
        let (region, alignment) = self.get_variable_region(variable);
        self.state.builder.add_mov(
            ctx.addr(),
            region.addr,
            region.size,
            &format!(
                "variable access '{}' ({})",
                variable.assigned_name,
                ctx.comment()
            ),
        );

        Ok(())
    }

    fn referenced_or_not_type(ty: &Type) -> Type {
        if let Type::MutableReference(inner_type) = ty {
            *inner_type.clone()
        } else {
            ty.clone()
        }
    }

    fn compound_assignment(
        &mut self,
        target_location: &SingleMutLocationExpression,
        op: &CompoundOperatorKind,
        source: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let target_location = self.gen_lvalue_address(&target_location.0)?;

        let source_info = self.gen_expression_for_access(source)?;

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.gen_compound_assignment_i32(&target_location, op, &source_info);
            }
            Type::Float => {
                self.gen_compound_assignment_f32(&target_location, op, &source_info);
            }
            Type::String => todo!(),
            _ => return Err(self.create_err(ErrorKind::IllegalCompoundAssignment, &source.node)),
        }

        Ok(())
    }

    fn gen_compound_assignment_i32(
        &mut self,
        target: &FrameMemoryRegion,
        op: &CompoundOperatorKind,
        source_ctx: &FrameMemoryRegion,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.state.builder.add_add_i32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    "+=  (i32)",
                );
            }
            CompoundOperatorKind::Sub => todo!(),
            CompoundOperatorKind::Mul => todo!(),
            CompoundOperatorKind::Div => todo!(),
            CompoundOperatorKind::Modulo => todo!(),
        }
    }

    fn gen_compound_assignment_f32(
        &mut self,
        target: &FrameMemoryRegion,
        op: &CompoundOperatorKind,
        source_ctx: &FrameMemoryRegion,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.state.builder.add_add_f32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    "+=  (f32)",
                );
            }
            CompoundOperatorKind::Sub => todo!(),
            CompoundOperatorKind::Mul => todo!(),
            CompoundOperatorKind::Div => todo!(),
            CompoundOperatorKind::Modulo => todo!(),
        }
    }

    fn internal_function_access(
        &mut self,
        internal: &InternalFunctionDefinitionRef,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.state.builder.add_ld_u16(
            ctx.addr(),
            internal.program_unique_id,
            &format!("function access '{}'", internal.assigned_name),
        );
        Ok(())
    }

    fn infinite_above_frame_size(&self) -> FrameMemoryRegion {
        FrameMemoryRegion::new(FrameMemoryAddress(self.frame_size.0), MemorySize(1024))
    }

    fn gen_struct_literal(
        &mut self,
        struct_literal: &StructInstantiation,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.gen_struct_literal_helper(
            &struct_literal.struct_type_ref.anon_struct_type,
            &struct_literal.source_order_expressions,
            ctx,
        )
    }

    fn gen_anonymous_struct_literal(
        &mut self,
        anon_struct_literal: &AnonymousStructLiteral,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.gen_struct_literal_helper(
            &anon_struct_literal.anonymous_struct_type,
            &anon_struct_literal.source_order_expressions,
            ctx,
        )
    }

    fn gen_struct_literal_helper(
        &mut self,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        ctx: &Context,
    ) -> Result<(), Error> {
        let struct_type = Type::AnonymousStruct(struct_type_ref.clone());
        let (whole_struct_size, whole_struct_alignment) = type_size_and_alignment(&struct_type);
        if ctx.target_size().0 != whole_struct_size.0 {
            info!("problem");
        }
        assert_eq!(ctx.target_size().0, whole_struct_size.0);

        for (field_index, expression) in source_order_expressions {
            let (field_offset, field_size, field_alignment) =
                struct_field_offset(*field_index, struct_type_ref);
            //info!(?field_offset, ?field_index, "field offset");
            let new_address = ctx.addr().advance(field_offset);
            let field_ctx = Context::new(FrameMemoryRegion::new(new_address, field_size));
            self.gen_expression(expression, &field_ctx)?;
        }

        Ok(())
    }

    fn gen_slice_literal(
        &mut self,
        ty: &Type,
        expressions: &Vec<Expression>,
    ) -> Result<FrameMemoryRegion, Error> {
        let (element_size, element_alignment) = type_size_and_alignment(ty);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);

        let start_frame_address_to_transfer = self
            .temp_allocator
            .allocate(total_slice_size, element_alignment);
        for (index, expr) in expressions.iter().enumerate() {
            let memory_offset = MemoryOffset((index as u16) * element_size.0);
            let region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset),
                element_size,
            );
            let element_ctx = Context::new(region);
            self.gen_expression(expr, &element_ctx)?;
        }

        Ok(FrameMemoryRegion::new(
            start_frame_address_to_transfer,
            total_slice_size,
        ))
    }

    fn gen_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
    ) -> SlicePairInfo {
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let (key_size, key_alignment) = type_size_and_alignment(key_type);
        let (value_size, value_alignment) = type_size_and_alignment(value_type);
        let (element_size, tuple_alignment) = type_size_and_alignment(&constructed_tuple);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);

        let start_frame_address_to_transfer = self
            .temp_allocator
            .allocate(total_slice_size, tuple_alignment);

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {
            let memory_offset = MemoryOffset((index as u16) * element_size.0);
            let key_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset),
                element_size,
            );
            let key_ctx = Context::new(key_region);
            self.gen_expression(key_expr, &key_ctx);

            let value_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset.add(key_size, key_alignment)),
                value_size,
            );
            let value_ctx = Context::new(value_region);
            self.gen_expression(value_expr, &value_ctx);
        }

        SlicePairInfo {
            addr: TempFrameMemoryAddress(start_frame_address_to_transfer),
            key_size,
            value_size,
            element_count: CountU16(element_count),
            element_size,
        }
    }

    fn gen_slice_helper(
        &mut self,
        start_temp_frame_address_to_transfer: FrameMemoryAddress,
        element_count: u16,
        element_size: MemorySize,
        ctx: &Context,
    ) {
        let total_slice_size = MemorySize(element_size.0 * element_count);
        let vec_len_addr = ctx.addr().advance(MemoryOffset(0));
        self.state
            .builder
            .add_ld_u16(vec_len_addr, element_count, "slice len");

        let vec_capacity_addr = ctx.addr().advance(MemoryOffset(2));
        self.state
            .builder
            .add_ld_u16(vec_capacity_addr, element_count, "slice capacity");

        let vec_element_size_addr = ctx.addr().advance(MemoryOffset(4));
        self.state
            .builder
            .add_ld_u16(vec_element_size_addr, element_size.0, "slice element size");

        /*
        let allocated_vec_address = ctx.addr().advance(MemoryOffset(6));
        self.state
        .builder
        add_alloc(allocated_vec_address, total_slice_size, "slice literal");

        self.state.builder.add_stx(
            allocated_vec_address,
            MemoryOffset(0),
            start_temp_frame_address_to_transfer,
            total_slice_size,
            "copy from slice continuous temporary frame memory to allocated vec ptr heap area",
        );

         */
    }

    fn gen_intrinsic_call_ex(
        &mut self,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &Vec<ArgumentExpressionOrLocation>,
        ctx: &Context,
    ) -> Result<(), Error> {
        //        info!(?intrinsic_fn, "generating intrinsic call");

        match intrinsic_fn {
            // Fixed
            IntrinsicFunction::FloatRound => todo!(),
            IntrinsicFunction::FloatFloor => todo!(),
            IntrinsicFunction::FloatSqrt => todo!(),
            IntrinsicFunction::FloatSign => todo!(),
            IntrinsicFunction::FloatAbs => todo!(),
            IntrinsicFunction::FloatRnd => todo!(),
            IntrinsicFunction::FloatCos => todo!(),
            IntrinsicFunction::FloatSin => todo!(),
            IntrinsicFunction::FloatAcos => todo!(),
            IntrinsicFunction::FloatAsin => todo!(),
            IntrinsicFunction::FloatAtan2 => todo!(),
            IntrinsicFunction::FloatMin => todo!(),
            IntrinsicFunction::FloatMax => todo!(),
            IntrinsicFunction::FloatClamp => todo!(),

            // i32
            IntrinsicFunction::IntAbs => todo!(),
            IntrinsicFunction::IntRnd => todo!(),
            IntrinsicFunction::IntMax => todo!(),
            IntrinsicFunction::IntMin => todo!(),
            IntrinsicFunction::IntClamp => todo!(),
            IntrinsicFunction::IntToFloat => todo!(),

            // String
            IntrinsicFunction::StringLen => todo!(),

            // Vector
            IntrinsicFunction::VecFromSlice => self.gen_intrinsic_vec_from_slice(arguments, ctx),
            IntrinsicFunction::VecPush => todo!(),
            IntrinsicFunction::VecPop => todo!(),
            IntrinsicFunction::VecRemoveIndex => todo!(),
            IntrinsicFunction::VecClear => todo!(),
            IntrinsicFunction::VecCreate => {
                self.gen_intrinsic_vec_create(arguments);
                Ok(())
            }
            IntrinsicFunction::VecSubscript => todo!(),
            IntrinsicFunction::VecSubscriptMut => todo!(),
            IntrinsicFunction::VecIter => todo!(), // intentionally disregard, since it is never called
            IntrinsicFunction::VecIterMut => todo!(), // intentionally disregard, since it is never called
            IntrinsicFunction::VecLen => todo!(),
            IntrinsicFunction::VecIsEmpty => todo!(),
            IntrinsicFunction::VecSelfPush => todo!(),
            IntrinsicFunction::VecSelfExtend => todo!(),

            // Map
            IntrinsicFunction::MapCreate => todo!(),
            IntrinsicFunction::MapFromSlicePair => todo!(),
            IntrinsicFunction::MapHas => todo!(),
            IntrinsicFunction::MapRemove => todo!(),
            IntrinsicFunction::MapIter => todo!(),
            IntrinsicFunction::MapIterMut => todo!(),
            IntrinsicFunction::MapLen => todo!(),
            IntrinsicFunction::MapIsEmpty => todo!(),
            IntrinsicFunction::MapSubscript => todo!(),
            IntrinsicFunction::MapSubscriptSet => todo!(),
            IntrinsicFunction::MapSubscriptMut => todo!(),
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => todo!(),

            // Sparse
            IntrinsicFunction::SparseAdd => todo!(),
            IntrinsicFunction::SparseNew => todo!(),
            IntrinsicFunction::SparseCreate => todo!(),
            IntrinsicFunction::SparseFromSlice => todo!(),
            IntrinsicFunction::SparseIter => todo!(),
            IntrinsicFunction::SparseIterMut => todo!(),
            IntrinsicFunction::SparseSubscript => todo!(),
            IntrinsicFunction::SparseSubscriptMut => todo!(),
            IntrinsicFunction::SparseHas => todo!(),
            IntrinsicFunction::SparseRemove => todo!(),

            // Grid
            IntrinsicFunction::GridCreate => todo!(),
            IntrinsicFunction::GridFromSlice => todo!(),
            IntrinsicFunction::GridSubscript => todo!(),
            IntrinsicFunction::GridSubscriptMut => todo!(),

            // Other
            IntrinsicFunction::Float2Magnitude => todo!(),
        };

        Ok(())
    }

    fn gen_intrinsic_vec_create(&self, arguments: &Vec<ArgumentExpressionOrLocation>) {
        for arg in arguments {
            info!(?arg, "argument");
        }
    }

    fn gen_intrinsic_vec_from_slice(
        &mut self,
        arguments: &[ArgumentExpressionOrLocation],
        ctx: &Context,
    ) -> Result<(), Error> {
        if let ArgumentExpressionOrLocation::Expression(found_expr) = &arguments[0] {
            let memory = self.gen_expression_for_access(found_expr)?;
            self.state.builder.add_vec_from_slice(
                ctx.addr(),
                memory.addr,
                MemorySize(0),
                CountU16(0),
                "create vec",
            );
        } else {
            panic!("vec_from_slice");
        }

        Ok(())
    }

    fn gen_match(&mut self, match_expr: &Match, ctx: &Context) -> Result<(), Error> {
        let region_to_match = self.gen_for_access_or_location(&match_expr.expression)?;

        let mut jump_to_exit_placeholders = Vec::new();

        let arm_len_to_consider = if match_expr.contains_wildcard() {
            match_expr.arms.len()
        } else {
            match_expr.arms.len()
        };
        for (index, arm) in match_expr.arms.iter().enumerate() {
            let is_last = index == arm_len_to_consider - 1;

            //  Each arm must set the CPU zero flag
            let maybe_guard = match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => None,
                    NormalPattern::EnumPattern(enum_variant, maybe_patterns) => {
                        self.state.builder.add_eq_u8_immediate(
                            region_to_match.addr,
                            enum_variant.common().container_index,
                            "check for enum variant",
                        );
                        maybe_guard.as_ref()
                    }
                    NormalPattern::Literal(_) => {
                        todo!()
                    }
                },
                Pattern::Wildcard(_) => {
                    // Wildcard is always true, so no comparison code is needed here at all
                    None
                }
            };

            let did_add_comparison = !matches!(arm.pattern, Pattern::Wildcard(_));

            let maybe_skip_added = if did_add_comparison {
                Some(
                    self.state
                        .builder
                        .add_jmp_if_not_equal_placeholder("placeholder for enum match"),
                )
            } else {
                None
            };

            let maybe_guard_skip = if let Some(guard) = maybe_guard {
                self.gen_boolean_expression(guard);
                // z flag should have been updated now

                Some(
                    self.state
                        .builder
                        .add_jmp_if_not_equal_placeholder("placeholder for skip guard"),
                )
            } else {
                None
            };

            self.gen_expression(&arm.expression, ctx)?;

            if !is_last {
                let jump_to_exit_placeholder =
                    self.state.builder.add_jump_placeholder("jump to exit");
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
            }

            if let Some(skip) = maybe_skip_added {
                self.state.builder.patch_jump_here(skip);
            }
            if let Some(guard_skip) = maybe_guard_skip {
                self.state.builder.patch_jump_here(guard_skip);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.state.builder.patch_jump_here(placeholder);
        }

        Ok(())
    }

    fn gen_guard(&mut self, guards: &Vec<Guard>, ctx: &Context) -> Result<(), Error> {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                self.gen_boolean_expression(condition); // update z flag
                let skip_expression_patch = self
                    .state
                    .builder
                    .add_jmp_if_not_equal_placeholder("guard condition");
                self.gen_expression(&guard.result, ctx)?;
                let jump_to_exit_placeholder =
                    self.state.builder.add_jump_placeholder("jump to exit");
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.state.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.gen_expression(&guard.result, ctx)?;
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.state.builder.patch_jump_here(placeholder);
        }

        Ok(())
    }

    fn gen_when(
        &mut self,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            let (variable_region, _alignment) = self.get_variable_region(&binding.variable);

            let old_variable_region = self.gen_for_access_or_location(&binding.expr)?;

            self.state
                .builder
                .add_tst8(old_variable_region.addr, "check binding");
            let patch = self
                .state
                .builder
                .add_jmp_if_not_equal_placeholder("jump if none");
            all_false_jumps.push(patch);
        }

        // if we are here all bindings are `Some`
        for binding in bindings {
            let (variable_region, alignment) = self.get_variable_region(&binding.variable);

            if binding.has_expression() {
                let var_ctx = Context::new(variable_region);
                self.gen_mut_or_immute(&binding.expr, &var_ctx)?;
            } else {
                let ArgumentExpressionOrLocation::Expression(variable_access_expression) =
                    &binding.expr.expression_or_location
                else {
                    panic!("must be expression");
                };
                let old_variable_region =
                    self.gen_expression_for_access(variable_access_expression)?;
                let alignment_offset: MemoryOffset = alignment.into();
                let some_value_region = FrameMemoryRegion::new(
                    old_variable_region.addr.advance(alignment_offset),
                    MemorySize(variable_region.size.0),
                );
                self.state.builder.add_movlp(
                    variable_region.addr,
                    some_value_region.addr,
                    some_value_region.size,
                    "move from Some to value",
                );
            }
        }

        self.gen_expression(true_expr, ctx)?;
        let maybe_jump_over_false = if let Some(_else_expr) = maybe_false_expr {
            Some(
                self.state
                    .builder
                    .add_jump_placeholder("jump over false section"),
            )
        } else {
            None
        };

        for false_jump_patch in all_false_jumps {
            self.state.builder.patch_jump_here(false_jump_patch);
        }

        if let Some(else_expr) = maybe_false_expr {
            self.gen_expression(else_expr, ctx);
            self.state
                .builder
                .patch_jump_here(maybe_jump_over_false.unwrap());
        }

        Ok(())
    }

    fn gen_range(
        &mut self,
        start: &Expression,
        end: &Expression,
        mode: &RangeMode,
        ctx: &Context,
    ) -> Result<(), Error> {
        let start_ctx = ctx.with_offset(MemoryOffset(0), MemorySize(INT_SIZE));
        self.gen_expression(start, &start_ctx)?;

        let end_ctx = ctx.with_offset(MemoryOffset(INT_SIZE), MemorySize(INT_SIZE));
        self.gen_expression(end, &end_ctx)?;

        let mode_ctx = ctx.with_offset(MemoryOffset(INT_SIZE + INT_SIZE), MemorySize(BOOL_SIZE));
        let val = match &mode {
            RangeMode::Inclusive => 1u8,
            RangeMode::Exclusive => 0u8,
        };
        self.state
            .builder
            .add_ld8(mode_ctx.addr(), val, "range mode");

        Ok(())
    }

    fn create_err(&mut self, kind: ErrorKind, node: &Node) -> Error {
        error!(?kind, "encountered error");
        Error {
            kind,
            node: node.clone(),
        }
    }

    fn gen_tuple_destructuring(
        &mut self,
        target_variables: &Vec<VariableRef>,
        tuple_type: &Vec<Type>,
        source_tuple_expression: &Expression,
    ) -> Result<(), Error> {
        let source_region = self.gen_expression_for_access(source_tuple_expression)?;

        let (total_size, _max_alignment, element_offsets) = layout_tuple_elements(tuple_type);
        assert_eq!(total_size.0, source_region.size.0);

        for (target_variable, (element_offset, element_size)) in
            target_variables.iter().zip(element_offsets)
        {
            if target_variable.is_unused {
            } else {
                let (target_region, _variable_alignment) =
                    self.get_variable_region(target_variable);
                assert_eq!(target_region.size.0, element_size.0);

                let source_element_region = FrameMemoryRegion::new(
                    source_region.addr.advance(element_offset),
                    element_size,
                );
                self.state.builder.add_mov(
                    target_region.addr,
                    source_element_region.addr,
                    source_element_region.size,
                    &format!(
                        "destructuring to variable {}",
                        target_variable.assigned_name
                    ),
                );
            }
        }

        Ok(())
    }

    fn gen_constant_access(
        &mut self,
        constant_reference: &ConstantRef,
        ctx: &Context,
    ) -> Result<(), Error> {
        let constant_region = self
            .state
            .constant_offsets
            .get(&constant_reference.id)
            .unwrap();
        assert_eq!(constant_region.size.0, ctx.target_size().0);

        self.state.builder.add_ld_constant(
            ctx.addr(),
            constant_region.addr,
            constant_region.size,
            &format!("load constant '{}'", constant_reference.assigned_name),
        );

        Ok(())
    }
}

fn single_intrinsic_fn(body: &Expression) -> Option<&IntrinsicFunction> {
    let ExpressionKind::Block(block_expressions) = &body.kind else {
        panic!("function body should be a block")
    };

    if let ExpressionKind::IntrinsicCallEx(found_intrinsic_fn, _non_instantiated_arguments) =
        &block_expressions[0].kind
    {
        Some(found_intrinsic_fn)
    } else {
        None
    }
}

fn struct_field_offset(
    index_to_look_for: usize,
    anon_struct_type: &AnonymousStructType,
) -> (MemoryOffset, MemorySize, MemoryAlignment) {
    let mut offset = MemoryOffset(0);
    for (field_index, (_name, field)) in
        anon_struct_type.field_name_sorted_fields.iter().enumerate()
    {
        let (field_size, field_alignment) = type_size_and_alignment(&field.field_type);
        let field_start_offset = offset.space(field_size, field_alignment);
        if field_index == index_to_look_for {
            return (field_start_offset, field_size, field_alignment);
        }
    }

    panic!("field index is wrong")
}
