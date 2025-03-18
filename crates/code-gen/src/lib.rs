pub mod alloc;
pub mod alloc_util;
pub mod constants;
pub mod ctx;
mod vec;

use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::{
    layout_struct, layout_tuple, layout_union, reserve_space_for_type, type_size_and_alignment,
};
use crate::ctx::Context;
use crate::vec::{VECTOR_DATA_PTR_OFFSET, VECTOR_LENGTH_OFFSET};
use seq_map::SeqMap;
use std::ops::Deref;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, EnumLiteralData, Expression, ExpressionKind, ForPattern, Function,
    FunctionScopeState, InternalFunctionDefinition, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, Iterable, Literal, LocationAccessKind,
    MutOrImmutableExpression, Postfix, PostfixKind, SingleLocationExpression,
    SingleMutLocationExpression, StructInstantiation, VariableRef,
};
use swamp_script_types::{AnonymousStructType, EnumVariantType, Signature, StructTypeField, Type};

use crate::constants::ConstantsManager;
use swamp_script_semantic::intr::IntrinsicFunction;
use swamp_vm_instr_build::{InstructionBuilder, PTR_SIZE, PatchPosition, VEC_SIZE};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemorySize, InstructionPosition, MemoryAddress,
    MemoryAlignment, MemoryOffset, MemorySize,
};
use tracing::{info, trace};

pub struct FunctionInfo {
    pub starts_at_ip: InstructionPosition,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct FunctionFixup {
    pub patch_position: PatchPosition,
    pub fn_id: InternalFunctionId,
    //pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct CodeGenState {
    builder: InstructionBuilder,
    constants: ConstantsManager,
    function_infos: SeqMap<InternalFunctionId, FunctionInfo>,
    function_fixups: Vec<FunctionFixup>,
}

impl CodeGenState {
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
}

impl CodeGenState {
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
}

impl CodeGenState {
    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.builder.instructions
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
}

pub struct GenOptions {
    pub is_halt_function: bool,
}
impl Default for CodeGenState {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: InstructionBuilder::default(),
            constants: ConstantsManager::new(),
            function_infos: SeqMap::default(),
            function_fixups: vec![],
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
    ) {
        //        info!(?internal_fn_def.assigned_name, ?internal_fn_def.program_unique_id, "generating function");
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

        let mut function_generator = FunctionCodeGen::new(self, internal_fn_def.program_unique_id);

        let mut ctx = Context::new(FrameMemoryRegion::new(
            FrameMemoryAddress(0),
            MemorySize(512),
        ));

        function_generator.layout_variables(
            &internal_fn_def.function_scope_state,
            &internal_fn_def.signature.return_type,
        );

        function_generator.gen_expression(&internal_fn_def.body, &mut ctx);

        self.finalize_function(options);
    }

    pub fn finalize_function(&mut self, options: &GenOptions) {
        if options.is_halt_function {
            self.builder.add_hlt("");
        } else {
            self.builder.add_ret("");
        }
    }

    pub fn gen_main_function(&mut self, main: &InternalMainExpression, options: &GenOptions) {
        let mut function_generator = FunctionCodeGen::new(self, main.program_unique_id);

        function_generator.layout_variables(&main.function_scope_state, &main.expression.ty);
        let mut empty_ctx = Context::new(FrameMemoryRegion::default()); //"main return (unit)");
        function_generator.gen_expression(&main.expression, &mut empty_ctx);
        self.finalize_function(options);
    }
}

pub struct FunctionCodeGen<'a> {
    state: &'a mut CodeGenState,
    variable_offsets: SeqMap<usize, FrameMemoryRegion>,
    frame_size: FrameMemorySize,
    extra_frame_allocator: ScopeAllocator,
    temp_allocator: ScopeAllocator,
    fn_id: InternalFunctionId,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(state: &'a mut CodeGenState, fn_id: InternalFunctionId) -> Self {
        Self {
            fn_id,
            state,
            variable_offsets: SeqMap::default(),
            frame_size: FrameMemorySize(0),
            extra_frame_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
            temp_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
        }
    }

    pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
        let (size, alignment) = type_size_and_alignment(ty);
        allocator.reserve(size, alignment)
    }

    pub fn layout_variables(&mut self, variables: &Vec<VariableRef>, return_type: &Type) {
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
                .unwrap();
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
        ctx: &Context,
    ) -> FrameMemoryRegion {
        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                let frame_address = self
                    .variable_offsets
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                *frame_address
            }

            _ => {
                let mut temp_ctx = self.temp_space_for_type(&expr.ty, "expression");

                self.gen_expression(expr, &mut temp_ctx);

                temp_ctx.target()
            }
        }
    }

    pub(crate) fn extra_frame_space_for_type(&mut self, ty: &Type) -> Context {
        let target = Self::reserve(ty, &mut self.extra_frame_allocator);
        Context::new(target)
    }

    pub fn gen_expression(&mut self, expr: &Expression, ctx: &Context) {
        match &expr.kind {
            ExpressionKind::ConstantAccess(_) => todo!(),
            ExpressionKind::VariableAccess(variable_ref) => {
                self.gen_variable_access(variable_ref, ctx);
            }
            ExpressionKind::IntrinsicFunctionAccess(_) => todo!(),
            ExpressionKind::InternalFunctionAccess(function) => {
                self.internal_function_access(function, ctx);
            }
            ExpressionKind::ExternalFunctionAccess(_) => todo!(),
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(_) => todo!(),
            ExpressionKind::PostfixChain(start, chain) => self.gen_postfix_chain(start, chain, ctx),
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::FunctionCall(_, _, _) => todo!(),
            ExpressionKind::InterpolatedString(_) => todo!(),
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.gen_variable_definition(variable, expression, ctx);
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                self.gen_variable_reassignment(variable, expression, ctx);
            }
            ExpressionKind::StructInstantiation(struct_literal) => {
                self.gen_struct_literal(struct_literal, ctx)
            }
            ExpressionKind::AnonymousStructLiteral(_) => todo!(),
            ExpressionKind::Literal(basic_literal) => self.gen_literal(basic_literal, ctx),
            ExpressionKind::Option(maybe_option) => {
                self.gen_option_expression(maybe_option.as_deref(), ctx)
            }
            ExpressionKind::Range(_, _, _) => todo!(),
            ExpressionKind::ForLoop(a, b, c) => self.gen_for_loop(a, b, c, ctx),
            ExpressionKind::WhileLoop(condition, expression) => {
                self.gen_while_loop(condition, expression, ctx);
            }
            ExpressionKind::Block(expressions) => self.gen_block(expressions, ctx),
            ExpressionKind::Match(_) => todo!(),
            ExpressionKind::Guard(_) => todo!(),
            ExpressionKind::If(conditional, true_expr, false_expr) => {
                self.gen_if(conditional, true_expr, false_expr.as_deref(), ctx);
            }
            ExpressionKind::When(_, _, _) => todo!(),
            ExpressionKind::TupleDestructuring(_, _, _) => todo!(),
            ExpressionKind::Assignment(_, _) => todo!(),
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                self.compound_assignment(target_location, operator_kind, source_expr, ctx);
            }
            ExpressionKind::IntrinsicCallMut(_, _, _) => todo!(),
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.gen_intrinsic_call_ex(intrinsic_fn, arguments, ctx);
            }
        }
    }

    fn gen_binary_operator(&mut self, binary_operator: &BinaryOperator, ctx: &Context) {
        match (&binary_operator.left.ty, &binary_operator.right.ty) {
            (Type::Int, Type::Int) => self.gen_binary_operator_i32(binary_operator, ctx),
            _ => todo!(),
        }
    }

    fn gen_binary_operator_i32(&mut self, binary_operator: &BinaryOperator, ctx: &Context) {
        let left_source = self.gen_expression_for_access(&binary_operator.left, ctx);
        let right_source = self.gen_expression_for_access(&binary_operator.right, ctx);

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
            BinaryOperatorKind::Multiply => todo!(),
            BinaryOperatorKind::Divide => todo!(),
            BinaryOperatorKind::Modulo => todo!(),
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            BinaryOperatorKind::LessThan => {
                self.state.builder.add_lt_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    "i32 lt",
                );
            }
            BinaryOperatorKind::LessEqual => todo!(),
            BinaryOperatorKind::GreaterThan => todo!(),
            BinaryOperatorKind::GreaterEqual => todo!(),
            BinaryOperatorKind::RangeExclusive => todo!(),
        }
    }

    fn gen_condition_context(
        &mut self,
        condition: &BooleanExpression,
        ctx: &Context,
    ) -> (Context, PatchPosition) {
        let mut condition_ctx = self.extra_frame_space_for_type(&Type::Bool);
        self.gen_expression(&condition.expression, &mut condition_ctx);

        let jump_on_false_condition = self
            .state
            .builder
            .add_conditional_jump_placeholder(condition_ctx.addr(), "jump boolean condition false");

        (condition_ctx, jump_on_false_condition)
    }

    fn gen_if(
        &mut self,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) {
        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition, ctx);

        // True expression just takes over our target
        self.gen_expression(true_expr, ctx);

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self
                .state
                .builder
                .add_jump_placeholder("condition is false skip");

            // If the expression was false, it should continue here
            self.state.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.gen_expression(false_expr, ctx);

            self.state.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.state.builder.patch_jump_here(jump_on_false_condition);
        }
    }

    fn gen_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.state.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition, ctx);

        // Expression is only for side effects
        let mut unit_ctx = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.gen_expression(expression, &mut unit_ctx);

        // Always jump to the condition again to see if it is true
        self.state
            .builder
            .add_jmp(ip_for_condition, "jmp to while condition");

        self.state.builder.patch_jump_here(jump_on_false_condition);
    }

    fn gen_location_argument(
        &mut self,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) {
        let region = self.gen_lvalue_address(argument);

        self.state
            .builder
            .add_mov(ctx.addr(), region.addr, region.size, comment)
    }

    fn gen_argument(
        &mut self,
        argument: &ArgumentExpressionOrLocation,
        ctx: &Context,
        comment: &str,
    ) {
        match &argument {
            ArgumentExpressionOrLocation::Expression(found_expression) => {
                self.gen_expression(found_expression, ctx);
            }
            ArgumentExpressionOrLocation::Location(location_expression) => {
                self.gen_location_argument(location_expression, ctx, comment);
            }
        }
    }

    fn gen_mut_or_immute(
        &mut self,
        mut_or_immutable_expression: &MutOrImmutableExpression,
        ctx: &mut Context,
    ) {
        match &mut_or_immutable_expression.expression_or_location {
            ArgumentExpressionOrLocation::Expression(found_expression) => {
                self.gen_expression(found_expression, ctx);
            }
            ArgumentExpressionOrLocation::Location(location_expression) => {
                self.gen_lvalue_address(location_expression);
            }
        }
    }
    fn gen_variable_assignment(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutOrImmutableExpression,
        ctx: &Context,
    ) {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name));

        let mut init_ctx =
            ctx.with_target(*target_relative_frame_pointer, "variable assignment target");

        self.gen_mut_or_immute(mut_or_immutable_expression, &mut init_ctx);
    }

    fn gen_variable_definition(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutOrImmutableExpression,
        ctx: &Context,
    ) {
        self.gen_variable_assignment(variable, mut_or_immutable_expression, ctx);
    }

    fn gen_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &Box<MutOrImmutableExpression>,
        ctx: &Context,
    ) {
        self.gen_variable_assignment(variable, mut_or_immutable_expression, ctx);
    }

    fn gen_lvalue_address(
        &mut self,
        location_expression: &SingleLocationExpression,
    ) -> FrameMemoryRegion {
        let frame_relative_base_address = self
            .variable_offsets
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap();

        *frame_relative_base_address

        /*

        // Loop over the consecutive accesses until we find the actual location
        for access in &location_expression.access_chain {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    // Calculate the offset somehow?
                }
                LocationAccessKind::IntrinsicCallMut(
                    intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    // arguments can be things like the key_value or the int index in a vector
                }
            }
        }

         */
    }

    fn copy_back_mutable_arguments(
        &mut self,
        signature: &Signature,
        maybe_self: Option<FrameMemoryRegion>,
        arguments: &Vec<ArgumentExpressionOrLocation>,
    ) {
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
                let argument_target = self.gen_lvalue_address(found_location);
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
    }
    fn gen_arguments(
        &mut self,
        signature: &Signature,
        self_region: Option<FrameMemoryRegion>,
        arguments: &Vec<ArgumentExpressionOrLocation>,
    ) {
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
    }

    fn gen_postfix_chain(
        &mut self,
        start_expression: &Expression,
        chain: &[Postfix],
        ctx: &Context,
    ) {
        if let ExpressionKind::InternalFunctionAccess(internal_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
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
                            "copy the return value to destination",
                        );
                    }
                    self.copy_back_mutable_arguments(&internal_fn.signature, None, args);

                    return;
                }
            }
        }

        let start_source = self.gen_expression_for_access(start_expression, ctx);

        for element in chain {
            match &element.kind {
                PostfixKind::StructField(_, _) => todo!(),
                PostfixKind::MemberCall(function_to_call, arguments) => {
                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            self.gen_arguments(
                                &internal_fn.signature,
                                Some(start_source),
                                arguments,
                            );
                            self.state
                                .add_call(internal_fn, &format!("frame size: {}", self.frame_size)); // will be fixed up later
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
    }

    fn gen_tuple(&mut self, expressions: &[Expression], ctx: &Context) {
        let mut scope = ScopeAllocator::new(ctx.target());

        for expr in expressions {
            let (memory_size, alignment) = type_size_and_alignment(&expr.ty);
            let start_addr = scope.allocate(memory_size, alignment);
            let element_region = FrameMemoryRegion::new(start_addr, memory_size);
            let element_ctx = Context::new(element_region);
            self.gen_expression(expr, &element_ctx);
        }
    }

    fn get_struct_field_offset(
        fields: &SeqMap<String, StructTypeField>,
        index_to_find: usize,
    ) -> MemoryOffset {
        let mut offset = 0;

        for (index, (_name, field)) in fields.iter().enumerate() {
            if index == index_to_find {
                break;
            }

            let (struct_field, struct_field_align) = type_size_and_alignment(&field.field_type);
            offset += struct_field.0;
        }

        MemoryOffset(offset)
    }

    fn gen_anonymous_struct(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        base_context: &Context,
    ) {
        for (field_index, expression) in source_order_expressions {
            let field_memory_offset = Self::get_struct_field_offset(
                &anon_struct_type.field_name_sorted_fields,
                *field_index,
            );
            let mut field_ctx = base_context.with_offset(MemorySize(field_memory_offset.0));
            self.gen_expression(expression, &mut field_ctx);
        }
    }

    fn gen_literal(&mut self, literal: &Literal, ctx: &Context) {
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
                        self.gen_tuple(expressions, &inner_ctx);
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        if let EnumVariantType::Struct(variant_struct_type) = a {
                            self.gen_anonymous_struct(
                                &variant_struct_type.anon_struct,
                                sorted_expressions,
                                &inner_ctx,
                            );
                        }
                    }
                }
            }
            Literal::TupleLiteral(_, _) => todo!(),
            Literal::StringLiteral(str) => {
                self.gen_string_literal(str, ctx);
            }
            Literal::Slice(ty, expressions) => self.gen_slice_literal(ty, expressions, ctx),
            Literal::SlicePair(_, _) => todo!(),
        }
    }

    fn gen_string_literal(&mut self, string: &str, ctx: &Context) {
        let data_ptr = self
            .state
            .constants
            .allocate(string.as_bytes(), MemoryAlignment::U8);
        let mem_size = MemorySize(string.len() as u16);

        self.gen_vec_immediate(data_ptr, mem_size, mem_size, "string", ctx);
    }

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

    fn gen_option_expression(&mut self, maybe_option: Option<&Expression>, ctx: &Context) {
        if let Some(found_value) = maybe_option {
            self.state.builder.add_ld8(ctx.addr(), 1, "option Some tag"); // 1 signals `Some`
            let mut one_offset_ctx = ctx.with_offset(MemorySize(1));
            self.gen_expression(found_value, &mut one_offset_ctx); // Fills in more of the union
        } else {
            self.state.builder.add_ld8(ctx.addr(), 0, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }
    }

    fn gen_for_loop(
        &mut self,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        body: &Box<Expression>,
        ctx: &Context,
    ) {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        match for_pattern {
            ForPattern::Single(value_variable) => {}
            ForPattern::Pair(key_variable, value_variable) => {}
        }

        let mut unit_expr = self.temp_space_for_type(&Type::Unit, "for loop body");
        self.gen_expression(body, &mut unit_expr);

        // advance iterator pointer
        // jump to check if iterator pointer has reached its end
    }

    fn gen_for_loop_for_vec(
        &mut self,
        element_type: &Type,
        vector_expr: Expression,
        ctx: &mut Context,
    ) {
        // get the vector that is referenced
        let mut vector_ctx = self.temp_space_for_type(&vector_expr.ty, "vector space");
        self.gen_expression(&vector_expr, &mut vector_ctx);

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

    fn gen_block(&mut self, expressions: &[Expression], ctx: &Context) {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let mut temp_context = self.temp_space_for_type(&Type::Unit, "block target");
                self.gen_expression(expr, &mut temp_context);
            }
            self.gen_expression(last, ctx);
        }
    }

    fn gen_variable_access(&mut self, variable: &VariableRef, ctx: &Context) {
        let frame_address = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap();
        let (size, _align) = type_size_and_alignment(&variable.resolved_type);
        self.state.builder.add_mov(
            ctx.addr(),
            frame_address.addr,
            size,
            &format!(
                "variable access '{}' ({})",
                variable.assigned_name,
                ctx.comment()
            ),
        );
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
    ) {
        let target_location = self.gen_lvalue_address(&target_location.0);

        let source_info = self.gen_expression_for_access(source, ctx);

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.gen_compound_assignment_i32(&target_location, op, &source_info);
            }
            Type::Float => todo!(),
            Type::String => todo!(),
            Type::Bool => todo!(),
            Type::Unit => todo!(),
            Type::Never => todo!(),
            Type::Tuple(_) => todo!(),
            Type::NamedStruct(_) => todo!(),
            Type::AnonymousStruct(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Function(_) => todo!(),
            Type::Iterable(_) => todo!(),
            Type::Optional(_) => todo!(),
            Type::Generic(_, _) => todo!(),
            Type::Blueprint(_) => todo!(),
            Type::Variable(_) => todo!(),
            Type::External(_) => todo!(),
            Type::MutableReference(x) => panic!("should have been checked"),
        }
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

    fn internal_function_access(
        &mut self,
        internal: &InternalFunctionDefinitionRef,
        ctx: &Context,
    ) {
        self.state.builder.add_ld_u16(
            ctx.addr(),
            internal.program_unique_id,
            &format!("function access '{}'", internal.assigned_name),
        );
    }

    fn infinite_above_frame_size(&self) -> FrameMemoryRegion {
        FrameMemoryRegion::new(FrameMemoryAddress(self.frame_size.0), MemorySize(1024))
    }

    fn gen_struct_literal(&mut self, struct_literal: &StructInstantiation, ctx: &Context) {
        let struct_type =
            Type::AnonymousStruct(struct_literal.struct_type_ref.anon_struct_type.clone());
        let (whole_struct_size, whole_struct_alignment) = type_size_and_alignment(&struct_type);
        if ctx.target_size().0 != whole_struct_size.0 {
            info!("problem");
        }
        assert_eq!(ctx.target_size().0, whole_struct_size.0);

        for (field_index, expression) in &struct_literal.source_order_expressions {
            let (field_offset, field_size, field_alignment) = struct_field_offset(
                *field_index,
                &struct_literal.struct_type_ref.anon_struct_type,
            );
            //info!(?field_offset, ?field_index, "field offset");
            let new_address = ctx.addr().advance(field_offset);
            let mut field_ctx = Context::new(FrameMemoryRegion::new(new_address, field_size));
            self.gen_expression(expression, &mut field_ctx);
        }
    }

    fn gen_slice_literal(&mut self, ty: &Type, expressions: &Vec<Expression>, ctx: &Context) {
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
            self.gen_expression(expr, &element_ctx);
        }

        let vec_len_addr = self
            .extra_frame_allocator
            .allocate(MemorySize(2), MemoryAlignment::U16);
        self.state
            .builder
            .add_ld_u16(vec_len_addr, element_count, "vec len");

        let vec_capacity_addr = self
            .extra_frame_allocator
            .allocate(MemorySize(2), MemoryAlignment::U16);
        self.state
            .builder
            .add_ld_u16(vec_capacity_addr, element_count, "vec capacity");

        let allocated_vec_address = self
            .extra_frame_allocator
            .allocate(MemorySize(PTR_SIZE), MemoryAlignment::U16);

        self.state
            .builder
            .add_alloc(allocated_vec_address, total_slice_size, "slice literal");

        self.state.builder.add_stx(
            allocated_vec_address,
            MemoryOffset(0),
            start_frame_address_to_transfer,
            total_slice_size,
            "copy from slice continuous temporary frame memory to allocated vec ptr heap area",
        );
    }

    fn gen_intrinsic_call_ex(
        &self,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &Vec<ArgumentExpressionOrLocation>,
        ctx: &Context,
    ) {
        //        info!(?intrinsic_fn, "generating intrinsic call");

        match intrinsic_fn {
            IntrinsicFunction::FloatRound => {}
            IntrinsicFunction::FloatFloor => {}
            IntrinsicFunction::FloatSqrt => {}
            IntrinsicFunction::FloatSign => {}
            IntrinsicFunction::FloatAbs => {}
            IntrinsicFunction::FloatRnd => {}
            IntrinsicFunction::FloatCos => {}
            IntrinsicFunction::FloatSin => {}
            IntrinsicFunction::FloatAcos => {}
            IntrinsicFunction::FloatAsin => {}
            IntrinsicFunction::FloatAtan2 => {}
            IntrinsicFunction::FloatMin => {}
            IntrinsicFunction::FloatMax => {}
            IntrinsicFunction::FloatClamp => {}
            IntrinsicFunction::IntAbs => {}
            IntrinsicFunction::IntRnd => {}
            IntrinsicFunction::IntMax => {}
            IntrinsicFunction::IntMin => {}
            IntrinsicFunction::IntClamp => {}
            IntrinsicFunction::IntToFloat => {}
            IntrinsicFunction::StringLen => {}
            IntrinsicFunction::VecFromSlice => {}
            IntrinsicFunction::VecPush => {}
            IntrinsicFunction::VecPop => {}
            IntrinsicFunction::VecRemoveIndex => {}
            IntrinsicFunction::VecClear => {}
            IntrinsicFunction::VecCreate => {}
            IntrinsicFunction::VecSubscript => {}
            IntrinsicFunction::VecSubscriptMut => {}
            IntrinsicFunction::VecIter => {}
            IntrinsicFunction::VecIterMut => {}
            IntrinsicFunction::VecSelfPush => {}
            IntrinsicFunction::VecSelfExtend => {}
            IntrinsicFunction::MapCreate => {}
            IntrinsicFunction::MapFromSlicePair => {}
            IntrinsicFunction::MapHas => {}
            IntrinsicFunction::MapRemove => {}
            IntrinsicFunction::MapIter => {}
            IntrinsicFunction::MapIterMut => {}
            IntrinsicFunction::MapLen => {}
            IntrinsicFunction::MapIsEmpty => {}
            IntrinsicFunction::MapSubscript => {}
            IntrinsicFunction::MapSubscriptSet => {}
            IntrinsicFunction::MapSubscriptMut => {}
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {}
            IntrinsicFunction::SparseCreate => {}
            IntrinsicFunction::SparseFromSlice => {}
            IntrinsicFunction::SparseIter => {}
            IntrinsicFunction::SparseIterMut => {}
            IntrinsicFunction::SparseSubscript => {}
            IntrinsicFunction::SparseSubscriptMut => {}
            IntrinsicFunction::SparseHas => {}
            IntrinsicFunction::SparseRemove => {}
            IntrinsicFunction::GridCreate => {}
            IntrinsicFunction::GridFromSlice => {}
            IntrinsicFunction::GridSubscript => {}
            IntrinsicFunction::GridSubscriptMut => {}
            IntrinsicFunction::Float2Magnitude => {}
            IntrinsicFunction::SparseAdd => {}
            IntrinsicFunction::VecLen => {}
            IntrinsicFunction::VecIsEmpty => {}
            IntrinsicFunction::SparseNew => {}
        }
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
