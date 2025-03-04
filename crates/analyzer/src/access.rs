/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::TypeContext;
use crate::err::{Error, ErrorKind};
use swamp_script_semantic::{
    ArrayTypeRef, Expression, ExpressionKind, Function, FunctionRef, Range, RangeMode, Type,
};

impl Analyzer<'_> {
    fn convert_to_function_access(function: &FunctionRef) -> Expression {
        match &**function {
            Function::Internal(x) => Expression {
                ty: Type::Function(x.signature.clone()),
                node: function.node(),
                kind: ExpressionKind::InternalFunctionAccess(x.clone()),
            },
            Function::External(y) => Expression {
                ty: Type::Function(y.signature.clone()),
                node: function.node(),
                kind: ExpressionKind::ExternalFunctionAccess(y.clone()),
            },
        }
    }

    pub(crate) fn analyze_static_member_access(
        &mut self,
        struct_reference: &swamp_script_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let struct_type = self.get_struct_type(struct_reference)?;
        let member_name = self.get_text(member_name_node);
        let binding = struct_type.borrow();
        let member_function = binding
            .functions
            .get(&member_name.to_string())
            .ok_or_else(|| self.create_err(ErrorKind::UnknownMemberFunction, member_name_node))?;

        let expr = Self::convert_to_function_access(member_function);

        Ok(expr)
    }

    pub(crate) fn analyze_min_max_expr(
        &mut self,
        min_expr: &swamp_script_ast::Expression,
        max_expr: &swamp_script_ast::Expression,
    ) -> Result<(Expression, Expression), Error> {
        let context = TypeContext::new_argument(&Type::Int);

        let resolved_min = self.analyze_expression(min_expr, &context)?;
        let resolved_max = self.analyze_expression(max_expr, &context)?;

        Ok((resolved_min, resolved_max))
    }

    pub fn analyze_range(
        &mut self,
        min_expr: &swamp_script_ast::Expression,
        max_expr: &swamp_script_ast::Expression,
        mode: &swamp_script_ast::RangeMode,
    ) -> Result<Range, Error> {
        let (min, max) = self.analyze_min_max_expr(min_expr, max_expr)?;

        let resolved_range_mode = match mode {
            swamp_script_ast::RangeMode::Inclusive => RangeMode::Inclusive,
            swamp_script_ast::RangeMode::Exclusive => RangeMode::Exclusive,
        };
        Ok(Range {
            min,
            max,
            mode: resolved_range_mode,
        })
    }

    /// # Errors
    ///
    pub fn analyze_array_range_access(
        &mut self,
        base_expression: Expression,
        array_type_ref: &ArrayTypeRef,
        range: Range,
    ) -> Result<Expression, Error> {
        let node = &base_expression.node.clone();

        Ok(self.create_expr_resolved(
            ExpressionKind::ArrayRangeAccess(Box::from(base_expression), Box::from(range)),
            Type::Array(array_type_ref.clone()),
            &node,
        ))
    }

    /// # Errors
    ///
    pub fn analyze_string_range_access(
        &mut self,
        base_expr: Expression,
        range: Range,
    ) -> Result<Expression, Error> {
        let ty = base_expr.ty.clone();
        let node = base_expr.node.clone();
        Ok(self.create_expr_resolved(
            ExpressionKind::StringRangeAccess(Box::from(base_expr), Box::from(range)),
            ty,
            &node,
        ))
    }
}
