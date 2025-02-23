/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use swamp_script_modules::symtbl::FuncDef;

use swamp_script_semantic::{
    Expression, ExpressionKind, Function, FunctionRef, Range, RangeMode, Type,
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
        named_type: &swamp_script_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let some_type = self.analyze_named_type(named_type)?;
        let member_name = self.get_text(member_name_node);
        self.shared
            .state
            .associated_impls
            .get_member_function(&some_type, member_name)
            .map_or_else(
                || Err(self.create_err(ErrorKind::UnknownMemberFunction, member_name_node)),
                |member_function| {
                    let expr = Self::convert_to_function_access(member_function);
                    Ok(expr)
                },
            )
    }

    /*
    fn analyze_function_access(
        &self,
        function_ref_node: &swamp_script_ast::QualifiedIdentifier,
    ) -> Result<Expression, Error> {
        let path = self.get_module_path(&function_ref_node.module_path);
        let name = self.get_text(&function_ref_node.name).to_string();

        if let Some(intrinsic_function) = self.shared.lookup.get_intrinsic_function(&path, &name)
        {
            let return_type = *intrinsic_function.signature.return_type.clone();
            let expr = self.create_expr(
                ExpressionKind::InternalFunctionAccess(intrinsic_function.clone()),
                return_type,
                &function_ref_node.name,
            );
            return Ok(expr);
        }

        if let Some(internal_function_ref) = self.shared.lookup.get_internal_function(&path, &name)
        {
            let return_type = *internal_function_ref.signature.return_type.clone();
            let expr = self.create_expr(
                ExpressionKind::InternalFunctionAccess(internal_function_ref.clone()),
                return_type,
                &function_ref_node.name,
            );
            return Ok(expr);
        }

        if let Some(external_function_ref) = self
            .shared
            .lookup
            .get_external_function_declaration(&path, &name)
        {
            let return_type = *external_function_ref.signature.return_type.clone();
            let expr = self.create_expr(
                ExpressionKind::ExternalFunctionAccess(external_function_ref.clone()),
                return_type,
                &function_ref_node.name,
            );
            return Ok(expr);
        }

        Err(self.create_err(ErrorKind::UnknownFunction, &function_ref_node.name))
    }

     */

    pub(crate) fn analyze_static_function_access(
        &self,
        qualified_func_name: &swamp_script_ast::QualifiedIdentifier,
    ) -> Result<Expression, Error> {
        let path = self.get_module_path(qualified_func_name.module_path.as_ref());
        let function_name = self.get_text(&qualified_func_name.name.0);

        if let Some(found_table) = self.shared.get_symbol_table(&path) {
            if let Some(found_func) = found_table.get_intrinsic_function(function_name) {
                let kind = ExpressionKind::IntrinsicFunctionAccess(found_func.clone());
                return Ok(self.create_expr(
                    kind,
                    *found_func.signature.return_type.clone(),
                    &qualified_func_name.name.0,
                ));
            }

            if let Some(found_func) = found_table.get_function(function_name) {
                let (kind, return_type) = match found_func {
                    FuncDef::Internal(internal_fn) => (
                        ExpressionKind::InternalFunctionAccess(internal_fn.clone()),
                        &internal_fn.signature.return_type,
                    ),
                    FuncDef::External(external_fn) => (
                        ExpressionKind::ExternalFunctionAccess(external_fn.clone()),
                        &external_fn.signature.return_type,
                    ),
                    FuncDef::Intrinsic(_) => {
                        return Err(self
                            .create_err(ErrorKind::UnknownFunction, &qualified_func_name.name.0));
                    }
                };

                return Ok(self.create_expr(
                    kind,
                    *return_type.clone(),
                    &qualified_func_name.name.0,
                ));
            }
        }
        Err(self.create_err(ErrorKind::UnknownFunction, &qualified_func_name.name.0))
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

    /// # Errors
    ///
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
