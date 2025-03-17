pub mod alloc;
pub mod alloc_util;
pub mod ctx;
mod vec;

use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::{type_size, type_size_and_alignment};
use crate::ctx::Context;
use crate::vec::{VECTOR_DATA_PTR_OFFSET, VECTOR_LENGTH_OFFSET};
use seq_map::SeqMap;
use std::ops::Deref;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, EnumLiteralData, Expression, ExpressionKind, ForPattern,
    FunctionScopeState, InternalFunctionDefinition, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, Iterable, Literal, LocationAccessKind,
    MutOrImmutableExpression, Postfix, PostfixKind, SingleLocationExpression,
    SingleMutLocationExpression, VariableRef,
};
use swamp_script_types::{AnonymousStructType, EnumVariantType, StructTypeField, Type};

use swamp_vm_instr_build::{InstructionBuilder, PTR_SIZE, PatchPosition};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemorySize, InstructionPosition, MemoryAlignment,
    MemoryOffset, MemorySize,
};
use tracing::info;

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
            function_infos: SeqMap::default(),
            function_fixups: vec![],
        }
    }

    #[must_use]
    pub fn take_instructions(self) -> Vec<BinaryInstruction> {
        self.builder.instructions
    }

    pub fn gen_function_def(
        &mut self,
        internal_fn_def: InternalFunctionDefinitionRef,
        options: &GenOptions,
    ) {
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
    variable_offsets: SeqMap<usize, FrameMemoryAddress>,
    frame_size: FrameMemorySize,
    extra_frame_allocator: ScopeAllocator,
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
        let mut current_offset = Self::reserve(return_type, &mut allocator);

        for var_ref in variables {
            let var_target = Self::reserve(&var_ref.resolved_type, &mut allocator);
            info!(?var_ref.assigned_name, ?var_target, "laying out");
            self.variable_offsets
                .insert(var_ref.unique_id_within_function, var_target.addr)
                .unwrap();
        }

        let extra_frame_size = MemorySize(80);
        let extra_target = FrameMemoryRegion::new(allocator.addr(), extra_frame_size);
        self.extra_frame_allocator = ScopeAllocator::new(extra_target);
        self.frame_size = allocator.addr().as_size().add(extra_frame_size);

        self.state.builder.add_enter(self.frame_size, "variables");

        //        self.allocator = allocator;
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn gen_expression_for_access(
        &mut self,
        expr: &Expression,
        ctx: &mut Context,
    ) -> FrameMemoryRegion {
        let size = type_size(&expr.ty);

        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                let frame_address = self
                    .variable_offsets
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                FrameMemoryRegion {
                    addr: *frame_address,
                    size,
                }
            }

            _ => {
                let mut temp_ctx = ctx.temp_space_for_type(&expr.ty, "expression");

                self.gen_expression(expr, &mut temp_ctx);

                temp_ctx.target()
            }
        }
    }

    pub(crate) fn extra_frame_space_for_type(&mut self, ty: &Type) -> Context {
        let target = Self::reserve(ty, &mut self.extra_frame_allocator);
        Context::new(target)
    }

    pub fn gen_expression(&mut self, expr: &Expression, ctx: &mut Context) {
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
            ExpressionKind::StructInstantiation(_) => todo!(),
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
            ExpressionKind::IntrinsicCallEx(_, _) => todo!(),
        }
    }

    fn gen_binary_operator(&mut self, binary_operator: &BinaryOperator, ctx: &mut Context) {
        match (&binary_operator.left.ty, &binary_operator.right.ty) {
            (Type::Int, Type::Int) => self.gen_binary_operator_i32(binary_operator, ctx),
            _ => todo!(),
        }
    }

    fn gen_binary_operator_i32(&mut self, binary_operator: &BinaryOperator, ctx: &mut Context) {
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
        ctx: &mut Context,
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
        ctx: &mut Context,
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
        ctx: &mut Context,
    ) {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.state.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition, ctx);

        // Expression is only for side effects
        let mut unit_ctx = ctx.temp_space_for_type(&Type::Unit, "while body expression");
        self.gen_expression(expression, &mut unit_ctx);

        // Always jump to the condition again to see if it is true
        self.state
            .builder
            .add_jmp(ip_for_condition, "jmp to while condition");

        self.state.builder.patch_jump_here(jump_on_false_condition);
    }

    fn gen_argument(&mut self, argument: &ArgumentExpressionOrLocation, ctx: &mut Context) {
        match &argument {
            ArgumentExpressionOrLocation::Expression(found_expression) => {
                self.gen_expression(found_expression, ctx);
            }
            ArgumentExpressionOrLocation::Location(location_expression) => {
                self.gen_location_access(location_expression, ctx);
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
                self.gen_location_access(location_expression, ctx);
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

        // TODO: the variable size could be stored in cache as well
        let variable_size = type_size(&variable.resolved_type);

        let mut init_ctx = ctx.with_target(
            *target_relative_frame_pointer,
            variable_size,
            "variable assignment target",
        );

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
        ctx: &mut Context,
    ) {
        self.gen_variable_assignment(variable, mut_or_immutable_expression, ctx);
    }

    fn gen_location_access(
        &mut self,
        location_expression: &SingleLocationExpression,
        ctx: &Context,
    ) -> FrameMemoryRegion {
        let frame_relative_base_address = self
            .variable_offsets
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap();

        FrameMemoryRegion {
            addr: *frame_relative_base_address,
            size: ctx.target_size(),
        }

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

    fn gen_arguments(&mut self, return_type: &Type, arguments: &Vec<ArgumentExpressionOrLocation>) {
        let arguments_memory_region = self.infinite_above_frame_size();
        let mut arguments_allocator = ScopeAllocator::new(arguments_memory_region);

        let _argument_addr = Self::reserve(return_type, &mut arguments_allocator);

        for argument in arguments {
            let argument_target = Self::reserve(&argument.ty(), &mut arguments_allocator);
            let mut arg_ctx = Context::new(argument_target);
            self.gen_argument(argument, &mut arg_ctx);
            // TODO: support more than one argument
        }
    }

    fn gen_postfix_chain(
        &mut self,
        start_expression: &Expression,
        chain: &[Postfix],
        ctx: &mut Context,
    ) {
        if let ExpressionKind::InternalFunctionAccess(internal_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
                    self.gen_arguments(&internal_fn.signature.return_type, args);
                    self.state
                        .add_call(internal_fn, &format!("frame size: {}", self.frame_size)); // will be fixed up later
                    let return_size = type_size(&internal_fn.signature.return_type);
                    self.state.builder.add_mov(
                        ctx.addr(),
                        self.infinite_above_frame_size().addr,
                        return_size,
                        "copy the return value to destination",
                    );
                    return;
                }
            }
        }

        let start_source = self.gen_expression_for_access(start_expression, ctx);

        for element in chain {
            match &element.kind {
                PostfixKind::StructField(_, _) => todo!(),
                PostfixKind::MemberCall(_, _) => todo!(),
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
        let mut element_ctx = ctx.with_offset(MemorySize(0));
        for expr in expressions {
            let memory_size = type_size(&expr.ty);
            self.gen_expression(expr, &mut element_ctx);
            element_ctx = element_ctx.with_offset(memory_size); // Move target pointer along
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
            offset += type_size(&field.field_type).0;
        }

        MemoryOffset(offset)
    }

    fn gen_anonymous_struct(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        base_context: &mut Context,
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

    fn gen_literal(&mut self, literal: &Literal, ctx: &mut Context) {
        match literal {
            Literal::IntLiteral(int) => {
                self.state
                    .builder
                    .add_ld_imm_i32(ctx.addr(), *int, "int literal");
            }
            Literal::FloatLiteral(fixed_point) => {
                self.state
                    .builder
                    .add_ld_imm_i32(ctx.addr(), fixed_point.inner(), "float literal");
            }
            Literal::NoneLiteral => {
                self.state
                    .builder
                    .add_ld_imm_u8(ctx.addr(), 0, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.state
                    .builder
                    .add_ld_imm_u8(ctx.addr(), u8::from(*truthy), "bool literal");
            }

            Literal::EnumVariantLiteral(a, b) => {
                self.state.builder.add_ld_imm_u8(
                    ctx.addr(),
                    a.common().container_index,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );
                match b {
                    EnumLiteralData::Nothing => {}
                    EnumLiteralData::Tuple(expressions) => {
                        let tuple_ctx = ctx.with_offset(MemorySize(1));
                        self.gen_tuple(expressions, &tuple_ctx);
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        if let EnumVariantType::Struct(variant_struct_type) = a {
                            self.gen_anonymous_struct(
                                &variant_struct_type.anon_struct,
                                sorted_expressions,
                                ctx,
                            );
                        }
                    }
                }
            }
            Literal::TupleLiteral(_, _) => {}
            Literal::StringLiteral(str) => {
                self.gen_string_literal(str, ctx);
            }
            Literal::Slice(_, _) => {}
            Literal::SlicePair(_, _) => {}
        }
    }

    fn gen_string_literal(&self, string: &str, ctx: &mut Context) {}

    fn gen_option_expression(&mut self, maybe_option: Option<&Expression>, ctx: &mut Context) {
        if let Some(found_value) = maybe_option {
            self.state
                .builder
                .add_ld_imm_u8(ctx.addr(), 1, "option Some tag"); // 1 signals `Some`
            let mut one_offset_ctx = ctx.with_offset(MemorySize(1));
            self.gen_expression(found_value, &mut one_offset_ctx); // Fills in more of the union
        } else {
            self.state
                .builder
                .add_ld_imm_u8(ctx.addr(), 0, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }
    }

    fn gen_for_loop(
        &mut self,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        body: &Box<Expression>,
        ctx: &mut Context,
    ) {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        match for_pattern {
            ForPattern::Single(value_variable) => {}
            ForPattern::Pair(key_variable, value_variable) => {}
        }

        let mut unit_expr = ctx.temp_space_for_type(&Type::Unit, "for loop body");
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
        let mut vector_ctx = ctx.temp_space_for_type(&vector_expr.ty, "vector space");
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

        let element_size = type_size(element_type);
        /*
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

    fn gen_block(&mut self, expressions: &[Expression], ctx: &mut Context) {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let mut temp_context = ctx.temp_space_for_type(&Type::Unit, "block target");
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
        let size = type_size(&variable.resolved_type);
        self.state.builder.add_mov(
            ctx.addr(),
            *frame_address,
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
        ctx: &mut Context,
    ) {
        let target_location = self.gen_location_access(&target_location.0, ctx);

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
        ctx: &mut Context,
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
}
