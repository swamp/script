/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::Resolver;

use swamp_script_semantic::{
    Expression, ExpressionKind, Function, FunctionRef, Range, RangeMode, Type,
};

impl<'a> Resolver<'a> {
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
        named_type: &swamp_script_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let some_type = self.find_named_type(named_type)?;
        let member_name = self.get_text(member_name_node);
        if let Some(member_function) = self
            .shared
            .associated_impls
            .get_member_function(&some_type, member_name)
        {
            let expr = Self::convert_to_function_access(member_function);
            Ok(expr)
        } else {
            Err(self.create_err(ErrorKind::UnknownMemberFunction, member_name_node))
        }
    }

    pub(crate) fn analyze_min_max_expr(
        &mut self,
        min_expr: &swamp_script_ast::Expression,
        max_expr: &swamp_script_ast::Expression,
    ) -> Result<(Expression, Expression), Error> {
        let resolved_min = self.analyze_expression(min_expr, Some(&Type::Int))?;
        let resolved_max = self.analyze_expression(max_expr, Some(&Type::Int))?;

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
}
