pub mod alloc;
pub mod alloc_util;
pub mod ctx;
mod vec;

use crate::alloc_util::type_size;
use crate::ctx::Context;
use crate::vec::{VECTOR_DATA_PTR_OFFSET, VECTOR_LENGTH_OFFSET};
use seq_map::SeqMap;
use std::ops::Deref;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    EnumLiteralData, Expression, ExpressionKind, ForPattern, FunctionScopeState, Iterable, Literal,
    LocationAccessKind, MutOrImmutableExpression, SingleLocationExpression, VariableRef,
};
use swamp_script_types::{AnonymousStructType, EnumVariantType, StructTypeField, Type};
use swamp_script_vm::instr_bldr::{
    FrameMemoryAddress, FrameMemorySize, InstructionBuilder, MemoryOffset, MemorySize,
    PatchPosition,
};
use swamp_script_vm::{BinaryInstruction, PTR_SIZE};
use tracing::info;

pub struct CodeGen {
    builder: InstructionBuilder,
    variable_offsets: SeqMap<usize, FrameMemoryAddress>,
    frame_size: FrameMemorySize,
}

impl CodeGen {
    pub fn finalize(&mut self) {
        self.builder.add_end();
    }
}

impl CodeGen {
    #[must_use]
    pub fn take_instructions(self) -> Vec<BinaryInstruction> {
        self.builder.instructions
    }
}

impl Default for CodeGen {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGen {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: InstructionBuilder::default(),
            variable_offsets: SeqMap::default(),
            frame_size: FrameMemorySize(0),
        }
    }

    pub fn layout_variables(
        &mut self,
        variables: &Vec<VariableRef>,
        return_type: &Type,
    ) -> Context {
        let mut current_offset = FrameMemoryAddress(type_size(&return_type).0);

        for var_ref in variables {
            info!(?var_ref.assigned_name, "laying out");
            self.variable_offsets
                .insert(var_ref.unique_id_within_function, current_offset)
                .unwrap();

            current_offset = current_offset.add(type_size(&var_ref.resolved_type));
        }

        self.frame_size = current_offset.as_size();

        Context::new(current_offset, MemorySize(64000))
    }

    pub fn gen_expression(&mut self, expr: &Expression, ctx: &mut Context) {
        ctx.reset_temp();

        match &expr.kind {
            ExpressionKind::ConstantAccess(_) => todo!(),
            ExpressionKind::VariableAccess(variable_ref) => {
                self.gen_variable_access(variable_ref, ctx)
            }
            ExpressionKind::IntrinsicFunctionAccess(_) => todo!(),
            ExpressionKind::InternalFunctionAccess(_) => todo!(),
            ExpressionKind::ExternalFunctionAccess(_) => todo!(),
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(_) => todo!(),
            ExpressionKind::PostfixChain(_, _) => todo!(),
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::FunctionCall(_, _, _) => todo!(),
            ExpressionKind::InterpolatedString(_) => todo!(),
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.gen_variable_definition(variable, expression, ctx);
            }
            ExpressionKind::VariableReassignment(_, _) => todo!(),
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
            ExpressionKind::CompoundAssignment(_, _, _) => todo!(),
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
        let mut left_context = ctx.temp_space_for_type(&binary_operator.left.ty);
        let mut right_context = ctx.temp_space_for_type(&binary_operator.right.ty);

        self.gen_expression(&binary_operator.left, &mut left_context);
        self.gen_expression(&binary_operator.right, &mut right_context);

        match binary_operator.kind {
            BinaryOperatorKind::Add => {
                self.builder
                    .add_add_i32(ctx.addr(), left_context.addr(), right_context.addr());
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
                self.builder
                    .add_lt_i32(ctx.addr(), left_context.addr(), right_context.addr());
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
        let mut condition_ctx = ctx.temp_space_for_type(&Type::Bool);
        self.gen_expression(&condition.expression, &mut condition_ctx);

        let jump_on_false_condition = self
            .builder
            .add_conditional_jump_placeholder(condition_ctx.addr());

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
            let skip_false_if_true = self.builder.add_jump_placeholder();

            // If the expression was false, it should continue here
            self.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.gen_expression(false_expr, ctx);

            self.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.builder.patch_jump_here(jump_on_false_condition);
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

        let ip_for_condition = self.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition, ctx);

        // Expression is only for side effects
        let mut unit_ctx = ctx.temp_space_for_type(&Type::Unit);
        self.gen_expression(expression, &mut unit_ctx);

        // Always jump to the condition again to see if it is true
        self.builder.add_jmp(ip_for_condition);

        self.builder.patch_jump_here(jump_on_false_condition);
    }

    fn gen_variable_definition(
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

        let mut init_ctx = ctx.with_target(*target_relative_frame_pointer, variable_size);

        match &mut_or_immutable_expression.expression_or_location {
            ArgumentExpressionOrLocation::Expression(found_expression) => {
                self.gen_expression(found_expression, &mut init_ctx);
            }
            ArgumentExpressionOrLocation::Location(location_expression) => {
                self.gen_location_expression(location_expression, ctx);
            }
        }
    }

    fn gen_location_expression(
        &mut self,
        location_expression: &SingleLocationExpression,
        ctx: &Context,
    ) {
        let frame_relative_base_address = self
            .variable_offsets
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap();
        self.builder
            .add_load_frame_address(ctx.addr(), *frame_relative_base_address);

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
                self.builder.add_ld_imm_i32(ctx.addr(), *int);
            }
            Literal::FloatLiteral(fixed_point) => {
                self.builder.add_ld_imm_i32(ctx.addr(), fixed_point.inner());
            }
            Literal::NoneLiteral => {
                self.builder.add_ld_imm_u8(ctx.addr(), 0);
            }
            Literal::BoolLiteral(truthy) => {
                self.builder.add_ld_imm_u8(ctx.addr(), u8::from(*truthy));
            }

            Literal::EnumVariantLiteral(a, b) => {
                self.builder
                    .add_ld_imm_u8(ctx.addr(), a.common().container_index);
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
            self.builder.add_ld_imm_u8(ctx.addr(), 1); // 1 signals `Some`
            let mut one_offset_ctx = ctx.with_offset(MemorySize(1));
            self.gen_expression(found_value, &mut one_offset_ctx); // Fills in more of the union
        } else {
            self.builder.add_ld_imm_u8(ctx.addr(), 0); // 0 signals `None`
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

        let mut unit_expr = ctx.temp_space_for_type(&Type::Unit);
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
        let mut vector_ctx = ctx.temp_space_for_type(&vector_expr.ty);
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

        // Temporary for the counter
        let counter_addr = ctx.allocate_temp(MemorySize(2)); // u16 counter
        self.builder.add_ld_imm_u16(counter_addr, 0);

        let loop_start_pos = self.builder.position();

        // vector length
        let length_addr = ctx.allocate_temp(MemorySize(2));
        self.builder.add_mov(
            length_addr,
            vector_ctx.addr().add(MemorySize(VECTOR_LENGTH_OFFSET)),
            MemorySize(2),
        );

        // Compare counter < length
        let compare_result_addr = ctx.allocate_temp(MemorySize(1)); // boolean result
        self.builder
            .add_lt_u16(compare_result_addr, counter_addr, length_addr);

        // Exit loop if counter >= length
        let exit_jump = self
            .builder
            .add_conditional_jump_placeholder(compare_result_addr);

        let data_ptr_addr = ctx.allocate_temp(MemorySize(2));
        self.builder.add_mov(
            data_ptr_addr,
            vector_ctx.addr().add(MemorySize(VECTOR_DATA_PTR_OFFSET)),
            MemorySize(PTR_SIZE),
        );

        /*
        let offset_addr = ctx.allocate_temp(2);
        self.builder.add_mul_u16(
            offset_addr,
            counter_addr,
            element_size
        );

        self.builder.add_ld_indirect(
            *value_var_addr,     // Destination: loop variable
            data_ptr_addr,       // Base: vector's data pointer
            offset_addr,         // Offset: counter * element_size
            element_size         // Size to copy
        );

        let mut body_ctx = ctx.temp_space_for_type(&Type::Unit);
        self.gen_expression(body, &mut body_ctx);

        self.builder.add_inc_u16(counter_addr);

        self.builder.add_jmp_to_position(loop_start_pos);

        let end_pos = self.builder.current_position();
        self.builder.patch_jump(exit_jump, end_pos);

         */
    }

    fn gen_block(&mut self, expressions: &[Expression], ctx: &mut Context) {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let mut temp_context = ctx.temp_space_for_type(&Type::Unit);
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
        self.builder.add_mov(ctx.addr(), *frame_address, size);
    }
}
