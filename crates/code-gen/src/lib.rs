pub mod alloc;
pub mod alloc_util;
pub mod ctx;

use crate::ctx::Context;
use swamp_script_semantic::{
    BinaryOperator, BinaryOperatorKind, BooleanExpression, Expression, ExpressionKind,
};
use swamp_script_types::Type;
use swamp_script_vm::instr_bldr::{InstructionBuilder, PatchPosition};

pub struct CodeGen {
    builder: InstructionBuilder,
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
        }
    }

    pub fn gen_expression(&mut self, expr: &Expression, ctx: &mut Context) {
        match &expr.kind {
            ExpressionKind::ConstantAccess(_) => todo!(),
            ExpressionKind::VariableAccess(_) => todo!(),
            ExpressionKind::FieldAccess(_, _) => todo!(),
            ExpressionKind::ArrayAccess(_, _, _) => todo!(),
            ExpressionKind::MapIndexAccess(_, _, _, _) => todo!(),
            ExpressionKind::StringRangeAccess(_, _) => todo!(),
            ExpressionKind::ArrayRangeAccess(_, _) => todo!(),
            ExpressionKind::IntrinsicFunctionAccess(_) => todo!(),
            ExpressionKind::InternalFunctionAccess(_) => todo!(),
            ExpressionKind::ExternalFunctionAccess(_) => todo!(),
            ExpressionKind::MapAssignment(_, _, _) => todo!(),
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(_) => todo!(),
            ExpressionKind::PostfixChain(_, _) => todo!(),
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::FunctionCall(_, _, _) => todo!(),
            ExpressionKind::InterpolatedString(_) => todo!(),
            ExpressionKind::VariableDefinition(_, _) => todo!(),
            ExpressionKind::VariableReassignment(_, _) => todo!(),
            ExpressionKind::StructInstantiation(_) => todo!(),
            ExpressionKind::AnonymousStructLiteral(_) => todo!(),
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Tuple(_) => todo!(),
            ExpressionKind::Literal(_) => todo!(),
            ExpressionKind::Option(_) => todo!(),
            ExpressionKind::Range(_, _, _) => todo!(),
            ExpressionKind::ForLoop(_, _, _) => todo!(),
            ExpressionKind::WhileLoop(condition, expression) => {
                self.gen_while_loop(condition, expression, ctx)
            }
            ExpressionKind::Return(_) => todo!(),
            ExpressionKind::Break => todo!(),
            ExpressionKind::Continue => todo!(),
            ExpressionKind::Block(_) => todo!(),
            ExpressionKind::Match(_) => todo!(),
            ExpressionKind::Guard(_) => todo!(),
            ExpressionKind::If(conditional, true_expr, false_expr) => {
                self.gen_if(conditional, true_expr, false_expr.as_deref(), ctx);
            }
            ExpressionKind::When(_, _, _) => todo!(),
            ExpressionKind::TupleDestructuring(_, _, _) => todo!(),
            ExpressionKind::Assignment(_, _) => todo!(),
            ExpressionKind::AssignmentSlice(_, _) => todo!(),
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
        let mut left_context = ctx.context_for_type(&binary_operator.left.ty);
        let mut right_context = ctx.context_for_type(&binary_operator.right.ty);

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
            BinaryOperatorKind::LessThan => todo!(),
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
        let mut condition_ctx = ctx.context_for_type(&Type::Bool);
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
        assert_eq!(ctx.target_size(), 0);

        let ip_for_condition = self.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition, ctx);

        // Expression is only for side effects
        let mut unit_ctx = ctx.context_for_type(&Type::Unit);
        self.gen_expression(expression, &mut unit_ctx);

        // Always jump to the condition again to see if it is true
        self.builder.add_jmp(ip_for_condition);

        self.builder.patch_jump_here(jump_on_false_condition);
    }
}
