use crate::err::ResolveError;
use crate::{Resolver, SPARSE_TYPE_ID};
use swamp_script_ast::{Expression, Node};
use swamp_script_semantic::{ResolvedExpression, ResolvedType, ResolvedVariableRef};

impl<'a> Resolver<'a> {
    pub(crate) fn check_for_internal_member_call(
        &mut self,
        source: &Expression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let resolved_expr = self.resolve_expression(source)?;

        match resolved_expr.resolution() {
            ResolvedType::Array(_) => {
                if let ResolvedExpression::VariableAccess(var_ref) = resolved_expr {
                    let resolved = self.resolve_array_member_call(
                        var_ref,
                        ast_member_function_name,
                        ast_arguments,
                    )?;
                    return Ok(Some(resolved));
                }
            }
            ResolvedType::Float(_) => {
                let resolved = self.resolve_float_member_call(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }
            ResolvedType::Int(_) => {
                let resolved = self.resolve_int_member_call(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }
            ResolvedType::String(_) => {
                let resolved = self.resolve_string_member_call(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }
            _ => {
                return self.check_for_internal_member_call_extra(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                );
            }
        }
        Ok(None)
    }

    fn resolve_array_member_call(
        &mut self,
        var_ref: ResolvedVariableRef,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let member_function_name_str = self.get_text(ast_member_function_name);
        let expr = match member_function_name_str {
            "remove" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(var_ref.name.clone()));
                }

                if ast_arguments.len() != 1 {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 1));
                }
                let index_expr = self.resolve_usize_index(&ast_arguments[0])?;

                ResolvedExpression::ArrayRemoveIndex(var_ref, Box::new(index_expr))
            }

            "clear" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(var_ref.name.clone()));
                }

                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }

                ResolvedExpression::ArrayClear(var_ref)
            }
            _ => {
                return Err(ResolveError::UnknownMemberFunction(
                    self.to_node(ast_member_function_name),
                ))
            }
        };

        Ok(expr)
    }

    fn resolve_single_float_expression(
        &mut self,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        if ast_arguments.len() != 1 {
            return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 1));
        }
        let expr2 = self.resolve_expression(&ast_arguments[0])?;
        if expr2.resolution() != self.shared.types.float_type() {
            return Err(ResolveError::IncompatibleArguments(
                expr2.resolution(),
                self.shared.types.float_type(),
            ));
        }

        Ok(expr2)
    }

    fn resolve_single_int_expression(
        &mut self,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        if ast_arguments.len() != 1 {
            return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 1));
        }
        let expr2 = self.resolve_expression(&ast_arguments[0])?;
        if expr2.resolution() != self.shared.types.int_type() {
            return Err(ResolveError::IncompatibleArguments(
                expr2.resolution(),
                self.shared.types.int_type(),
            ));
        }

        Ok(expr2)
    }

    fn resolve_two_float_expressions(
        &mut self,
        ast_arguments: &[Expression],
    ) -> Result<(ResolvedExpression, ResolvedExpression), ResolveError> {
        if ast_arguments.len() != 2 {
            return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 2));
        }
        let expr2 = self.resolve_expression(&ast_arguments[0])?;
        if expr2.resolution() != self.shared.types.float_type() {
            return Err(ResolveError::IncompatibleArguments(
                expr2.resolution(),
                self.shared.types.float_type(),
            ));
        }

        let expr3 = self.resolve_expression(&ast_arguments[1])?;
        if expr3.resolution() != self.shared.types.float_type() {
            return Err(ResolveError::IncompatibleArguments(
                expr3.resolution(),
                self.shared.types.float_type(),
            ));
        }

        Ok((expr2, expr3))
    }

    fn resolve_float_member_call(
        &mut self,
        expr: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        match function_name_str {
            "round" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatRound(Box::new(expr)))
            }
            "floor" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatFloor(Box::new(expr)))
            }
            "sign" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatSign(Box::new(expr)))
            }
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatAbs(Box::new(expr)))
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatRnd(Box::new(expr)))
            }
            "cos" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatCos(Box::new(expr)))
            }
            "sin" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatSin(Box::new(expr)))
            }
            "acos" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatAcos(Box::new(expr)))
            }
            "asin" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatAsin(Box::new(expr)))
            }
            "atan2" => {
                let float_argument = self.resolve_single_float_expression(ast_arguments)?;
                Ok(ResolvedExpression::FloatAtan2(
                    Box::new(expr),
                    Box::new(float_argument),
                ))
            }
            "min" => {
                let float_argument = self.resolve_single_float_expression(ast_arguments)?;
                Ok(ResolvedExpression::FloatMin(
                    Box::new(expr),
                    Box::new(float_argument),
                ))
            }
            "max" => {
                let float_argument = self.resolve_single_float_expression(ast_arguments)?;
                Ok(ResolvedExpression::FloatMax(
                    Box::new(expr),
                    Box::new(float_argument),
                ))
            }
            "clamp" => {
                let (min, max) = self.resolve_two_float_expressions(ast_arguments)?;
                Ok(ResolvedExpression::FloatClamp(
                    Box::new(expr),
                    Box::new(min),
                    Box::new(max),
                ))
            }
            _ => Err(ResolveError::UnknownMemberFunction(
                self.to_node(ast_member_function_name),
            )),
        }
    }

    fn resolve_string_member_call(
        &mut self,
        expr: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        match function_name_str {
            "len" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::StringLen(Box::new(expr)))
            }
            _ => Err(ResolveError::UnknownMemberFunction(
                self.to_node(ast_member_function_name),
            )),
        }
    }

    fn resolve_int_member_call(
        &mut self,
        expr: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        match function_name_str {
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::IntAbs(Box::new(expr)))
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::IntRnd(Box::new(expr)))
            }

            "max" => {
                let int_argument = self.resolve_single_int_expression(ast_arguments)?;
                Ok(ResolvedExpression::IntMax(
                    Box::new(expr),
                    Box::new(int_argument),
                ))
            }

            "min" => {
                let int_argument = self.resolve_single_int_expression(ast_arguments)?;
                Ok(ResolvedExpression::IntMin(
                    Box::new(expr),
                    Box::new(int_argument),
                ))
            }

            "to_float" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::IntToFloat(Box::new(expr)))
            }
            _ => Err(ResolveError::UnknownMemberFunction(
                self.to_node(ast_member_function_name),
            )),
        }
    }

    fn check_for_internal_member_call_extra(
        &mut self,
        self_expression: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        // TODO: Early out
        if let ResolvedType::Generic(generic_type, _parameters) = self_expression.resolution() {
            if let ResolvedType::RustType(rust_type_ref) = *generic_type {
                if rust_type_ref.as_ref().number == SPARSE_TYPE_ID {
                    let function_name_str = self.get_text(ast_member_function_name);
                    // TODO: Remove hack
                    match function_name_str {
                        "add" => {
                            if ast_arguments.len() != 1 {
                                return Err(ResolveError::WrongNumberOfArguments(
                                    ast_arguments.len(),
                                    1,
                                ));
                            }
                            let value = self.resolve_expression(&ast_arguments[0])?;
                            return Ok(Some(ResolvedExpression::SparseAdd(
                                Box::new(self_expression),
                                Box::new(value),
                            )));
                        }
                        "remove" => {
                            if ast_arguments.len() != 1 {
                                return Err(ResolveError::WrongNumberOfArguments(
                                    ast_arguments.len(),
                                    1,
                                ));
                            }
                            let sparse_slot_id_expression =
                                self.resolve_expression(&ast_arguments[0])?;
                            return Ok(Some(ResolvedExpression::SparseRemove(
                                Box::new(self_expression),
                                Box::new(sparse_slot_id_expression),
                            )));
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(None)
    }
}
