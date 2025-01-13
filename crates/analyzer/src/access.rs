/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::{Resolver, SPARSE_TYPE_ID};
use swamp_script_ast::{Expression, Node, QualifiedTypeIdentifier, RangeMode};
use swamp_script_semantic::{
    ResolvedAccess, ResolvedArrayTypeRef, ResolvedExpression, ResolvedFunction,
    ResolvedFunctionRef, ResolvedLocalIdentifier, ResolvedMapIndexLookup, ResolvedMapTypeRef,
    ResolvedRangeMode, ResolvedStructTypeField, ResolvedStructTypeFieldRef, ResolvedType, Spanned,
};

impl<'a> Resolver<'a> {
    pub(crate) fn collect_field_chain(
        &mut self,
        expr: &Expression,
        access_chain: &mut Vec<ResolvedAccess>,
    ) -> Result<(ResolvedType, ResolvedExpression), ResolveError> {
        match expr {
            Expression::FieldOrMemberAccess(source, field) => {
                let (resolved_type, base_expr) = self.collect_field_chain(source, access_chain)?;

                let (field_type, field_index) = self.get_field_index(&resolved_type, field)?;
                access_chain.push(ResolvedAccess::FieldIndex(self.to_node(field), field_index));

                Ok((field_type, base_expr))
            }

            Expression::IndexAccess(source, index_expr) => {
                let (resolved_type, base_expr) = self.collect_field_chain(source, access_chain)?;
                let resolved_index_expr = self.resolve_expression(index_expr)?;
                match resolved_type {
                    ResolvedType::Array(array_type_ref) => {
                        access_chain.push(ResolvedAccess::ArrayIndex(resolved_index_expr));
                        Ok((array_type_ref.item_type.clone(), base_expr))
                    }
                    ResolvedType::Map(map_type_ref) => {
                        access_chain.push(ResolvedAccess::MapIndex(resolved_index_expr));
                        Ok((map_type_ref.value_type.clone(), base_expr))
                    }
                    _ => Err(ResolveError::ExpectedArray(resolved_type)),
                }
            }
            _ => {
                let resolved_expr = self.resolve_expression(expr)?;
                let resolved_type = resolved_expr.resolution();
                Ok((resolved_type, resolved_expr))
            }
        }
    }

    fn convert_to_function_access(function: &ResolvedFunctionRef) -> ResolvedExpression {
        match &**function {
            ResolvedFunction::Internal(x) => ResolvedExpression::InternalFunctionAccess(x.clone()),
            ResolvedFunction::External(y) => ResolvedExpression::ExternalFunctionAccess(y.clone()),
        }
    }

    pub(crate) fn resolve_static_member_access(
        &self,
        struct_reference: &QualifiedTypeIdentifier,
        member_name_node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        let struct_type = self.get_struct_type(struct_reference)?;
        let member_name = self.get_text(member_name_node);
        let binding = struct_type.borrow();
        let member_function = binding.functions.get(&member_name.to_string()).ok_or(
            ResolveError::UnknownMemberFunction(self.to_node(member_name_node)),
        )?;

        let expr = Self::convert_to_function_access(member_function);

        Ok(expr)
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn resolve_index_access(
        &mut self,
        expression: &Expression,
        key_expr: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expr = self.resolve_expression(expression)?;
        let resolved_type = resolved_expr.resolution();

        let expr = match resolved_type {
            ResolvedType::Array(array_type) => {
                self.resolve_array_index_access(expression, &array_type, key_expr)?
            }
            ResolvedType::Map(map_type) => {
                self.resolve_map_index_access(expression, &map_type, key_expr)?
            }
            ResolvedType::Generic(base_type, generic_type_parameters) => {
                if let ResolvedType::RustType(found_rust_type) = *base_type {
                    if found_rust_type.number == SPARSE_TYPE_ID {
                        let sparse_id = self
                            .shared
                            .lookup
                            .get_rust_type(&vec!["std".to_string()], "SparseId")
                            .expect("SparseId is missing");
                        let contained_type = &generic_type_parameters[0];
                        let resolved_key = self.resolve_expression_expecting_type(
                            key_expr,
                            &ResolvedType::RustType(sparse_id),
                            false,
                        )?;
                        return Ok(ResolvedExpression::SparseAccess(
                            Box::new(resolved_expr),
                            Box::new(resolved_key),
                            ResolvedType::Optional(Box::new(contained_type.clone())),
                        ));
                    }
                }

                return Err(ResolveError::TypeDoNotSupportIndexAccess(
                    resolved_expr.span(),
                ));
            }
            _ => {
                return Err(ResolveError::TypeDoNotSupportIndexAccess(
                    resolved_expr.span(),
                ))
            }
        };

        Ok(expr)
    }

    /// # Errors
    ///
    pub fn resolve_range_access(
        &mut self,
        expression: &Expression,
        min: &Expression,
        max: &Expression,
        mode: &RangeMode,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expr = self.resolve_expression(expression)?;
        let resolved_type = resolved_expr.resolution();

        let resolved_range_mode = match mode {
            RangeMode::Inclusive => ResolvedRangeMode::Inclusive,
            RangeMode::Exclusive => ResolvedRangeMode::Exclusive,
        };

        let expr = match resolved_type {
            ResolvedType::Array(array_type) => self.resolve_array_range_access(
                resolved_expr,
                &array_type,
                min,
                max,
                &resolved_range_mode,
            )?,
            ResolvedType::String => {
                self.resolve_string_range_access(expression, min, max, &resolved_range_mode)?
            }
            _ => {
                return Err(ResolveError::TypeDoNotSupportRangeAccess(
                    resolved_expr.span(),
                ))
            }
        };

        Ok(expr)
    }

    fn resolve_map_index_access(
        &mut self,
        base_expression: &Expression,
        map_type_ref: &ResolvedMapTypeRef,
        key_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_key_expression = self.resolve_expression(key_expression)?;
        let resolved_map_expression = self.resolve_expression(base_expression)?;

        Ok(ResolvedExpression::MapIndexAccess(ResolvedMapIndexLookup {
            map_type: ResolvedType::Map(map_type_ref.clone()),
            item_type: ResolvedType::Any,
            map_type_ref: map_type_ref.clone(),
            index_expression: Box::from(resolved_key_expression),
            map_expression: Box::from(resolved_map_expression),
        }))
    }

    pub(crate) fn resolve_field_or_member_access(
        &mut self,
        expression: &Expression,
        field_or_member_name: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let resolved_type = resolved_expression.resolution();
        let field_or_member_name_str = self.get_text(field_or_member_name).to_string();

        if let ResolvedType::Struct(struct_type_ref) = resolved_type {
            let borrow_struct = struct_type_ref.borrow();
            if let Some(found_function) = borrow_struct.functions.get(&field_or_member_name_str) {
                Ok(Self::convert_to_function_access(found_function))
            } else if let Some(found_field_ref) = borrow_struct
                .anon_struct_type
                .defined_fields
                .get(&field_or_member_name_str)
            {
                let (base_expr, access_chain, _resulting_type) =
                    self.resolve_field_access_helper(expression, field_or_member_name)?;
                let index = borrow_struct
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_or_member_name_str)
                    .expect("field name has been checked previously");
                let field = ResolvedStructTypeField {
                    struct_type_ref: struct_type_ref.clone(),
                    index,
                    resolved_type: found_field_ref.field_type.clone(),
                    field_name: ResolvedLocalIdentifier(self.to_node(field_or_member_name)),
                };
                Ok(ResolvedExpression::FieldAccess(
                    Box::from(base_expr),
                    ResolvedStructTypeFieldRef::from(field),
                    access_chain,
                ))
            } else {
                Err(ResolveError::UnknownStructField(
                    self.to_node(field_or_member_name),
                ))
            }
        } else {
            Err(ResolveError::NeedStructForFieldLookup)
        }
    }

    pub(crate) fn resolve_field_access_helper(
        &mut self,
        base_expression: &Expression,
        ast_field_name: &Node,
    ) -> Result<(ResolvedExpression, Vec<ResolvedAccess>, ResolvedType), ResolveError> {
        let mut access_chain = Vec::new();
        let (resolved_last_type, resolved_base_expression) =
            self.collect_field_chain(base_expression, &mut access_chain)?;

        // Add the last lookup that is part of the field lookup
        let (field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        access_chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        Ok((resolved_base_expression, access_chain, field_type))
    }

    pub(crate) fn resolve_array_index_access_helper(
        &mut self,
        base_expression: &Expression,
        last_index_expr: &Expression,
    ) -> Result<
        (
            ResolvedExpression,
            Vec<ResolvedAccess>,
            ResolvedArrayTypeRef,
        ),
        ResolveError,
    > {
        let mut access_chain = Vec::new();
        let (resolved_last_type, resolved_base_expression) =
            self.collect_field_chain(base_expression, &mut access_chain)?;

        let last_resolved_index = self.resolve_expression(last_index_expr)?;
        let ResolvedType::Array(resolved_array_type_ref) = resolved_last_type else {
            return Err(ResolveError::NotAnArray(resolved_base_expression.span()));
        };
        access_chain.push(ResolvedAccess::ArrayIndex(last_resolved_index));

        Ok((
            resolved_base_expression,
            access_chain,
            resolved_array_type_ref,
        ))
    }

    fn resolve_array_index_access(
        &mut self,
        base_expression: &Expression,
        array_type_ref: &ResolvedArrayTypeRef,
        array_usize_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let (base_expr, access_chain, _last_type) =
            self.resolve_array_index_access_helper(base_expression, array_usize_expression)?;

        Ok(ResolvedExpression::ArrayAccess(
            Box::from(base_expr),
            array_type_ref.clone(),
            access_chain,
        ))
    }

    pub(crate) fn resolve_min_max_expr(
        &mut self,
        min_expr: &Expression,
        max_expr: &Expression,
    ) -> Result<(ResolvedExpression, ResolvedExpression), ResolveError> {
        let resolved_min =
            self.resolve_expression_expecting_type(min_expr, &ResolvedType::Int, false)?;
        let resolved_max =
            self.resolve_expression_expecting_type(max_expr, &ResolvedType::Int, false)?;

        Ok((resolved_min, resolved_max))
    }

    /// # Errors
    ///
    pub fn resolve_array_range_access(
        &mut self,
        base_expression: ResolvedExpression,
        array_type_ref: &ResolvedArrayTypeRef,
        min_expr: &Expression,
        max_expr: &Expression,
        mode: &ResolvedRangeMode,
    ) -> Result<ResolvedExpression, ResolveError> {
        let (min_expr, max_expr) = self.resolve_min_max_expr(min_expr, max_expr)?;

        Ok(ResolvedExpression::ArrayRangeAccess(
            Box::from(base_expression),
            array_type_ref.clone(),
            Box::new(min_expr),
            Box::new(max_expr),
            mode.clone(),
        ))
    }

    /// # Errors
    ///
    pub fn resolve_string_range_access(
        &mut self,
        base_expr: &Expression,
        min_expr: &Expression,
        max_expr: &Expression,
        mode: &ResolvedRangeMode,
    ) -> Result<ResolvedExpression, ResolveError> {
        let base_expression = self.resolve_expression(base_expr)?;

        let (resolved_min_expr, resolved_max_expr) =
            self.resolve_min_max_expr(min_expr, max_expr)?;

        Ok(ResolvedExpression::StringRangeAccess(
            Box::from(base_expression),
            Box::new(resolved_min_expr),
            Box::new(resolved_max_expr),
            mode.clone(),
        ))
    }
}
