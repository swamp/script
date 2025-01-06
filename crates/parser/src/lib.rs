/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use pest::error::{Error, ErrorVariant, InputLocation};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use swamp_script_ast::{
    prelude::*, CompoundOperator, CompoundOperatorKind, EnumVariantLiteral, FieldExpression,
    FieldName, FieldType, ForPattern, ForVar, IteratableExpression, LocationExpression,
    PatternElement, QualifiedIdentifier, SpanWithoutFileId, TypeForParameter,
};
use swamp_script_ast::{Function, PostfixOperator};
use tracing::info;

pub struct ParseResult<'a> {
    #[allow(dead_code)]
    script: String, // Pairs are referencing the script
    pairs: pest::iterators::Pairs<'a, Rule>,
}

pub struct GeneralError {
    pub description: String,
}

#[derive(Debug)]
pub enum SpecificError {
    General(String),
    ExpectingTypeIdentifier,
    ExpectingInnerPair,
    UnexpectedTypeRule,
    ExpectedTypeIdentifier(String),
    ExpectedLocalTypeIdentifier(String),
    UnexpectedRuleInParseScript(String),
    ExpectedControlStatement(String),
    ExpectedStatement(String),
    ExpectedIfOrElse(String),
    MissingFunctionSignature,
    MissingFunctionBody,
    ExpectedStatementBlock,
    ExpectedFunctionDefinition,
    ExpectedParameter,
    ExpectedImplItem,
    ExpectedMemberSignature,
    ExpectedBlockInWhileLoop,
    UnexpectedExpressionType(String),
    UnexpectedAccessType(String),
    UnknownAssignmentOperator(String),
    CompoundOperatorCanNotContainMut,
    InvalidAssignmentTarget,
    CompoundOperatorCanNotHaveMultipleVariables,
    ExpectedExpressionAfterPrefixOperator,
    UnknownOperator(String),
    UnexpectedPostfixOperator,
    UnexpectedUnaryOperator(String),
    InvalidMemberCall,
    UnknownMatchType,
    UnexpectedElementInPatternList,
    InvalidPrecisionValue,
    InvalidPrecisionType,
    ExpectedTypeIdentifierAfterPath,
    UnexpectedPatternListElement(String),
    MustHaveAtLeastOneArm,
    UnexpectedMatchArmRule(String),
    UnknownEnumVariant(String),
    UnknownLiteral,
    UnknownPrimary(String),
    InvalidFormatSpecifier,
    UnexpectedVariantField,
    MutOnlyForVariables,
    UnexpectedTokenInFunctionCall,
    ExpectedExpressionInInterpolation,
    UnexpectedRuleInInterpolation,
    ExpectedForPattern,
    ExpectedBlock,
    InvalidForPattern,
    UnexpectedRuleInElse(String),
    ExpectedLocationExpression,
    ExpectedImportPath,
    ExpectedIdentifier,
    ExpectedIdentifierAfterPath,
    ExpectedFieldOrRest,
}

#[derive(Debug)]
pub struct ParseError {
    pub span: SpanWithoutFileId,
    pub specific: SpecificError,
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ScriptParser;

pub const UNKNOWN_FILE_ID: u16 = 0xffff;

pub struct AstParser;

impl From<Error<Rule>> for ParseError {
    fn from(value: Error<Rule>) -> Self {
        let span = match value.location {
            InputLocation::Pos(pos) => SpanWithoutFileId {
                offset: pos as u32,
                length: 1,
            },
            InputLocation::Span((start, end)) => SpanWithoutFileId {
                offset: start as u32,
                length: (end - start) as u16,
            },
        };
        Self {
            span,
            specific: SpecificError::General(value.to_string()),
        }
    }
}

impl AstParser {
    // Helper functions for common parser operations
    fn next_pair<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<Pair<'a, Rule>, ParseError> {
        Ok(pairs.next().ok_or_else(|| {
            Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Expected more tokens".into(),
                },
                pest::Position::from_start(""),
            )
        })?)
    }

    fn expect_next<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
        expected_rule: Rule,
    ) -> Result<Pair<'a, Rule>, ParseError> {
        let pair = Self::next_pair(pairs)?;
        if pair.as_rule() != expected_rule {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("Expected {:?}, found {:?}", expected_rule, pair.as_rule()),
                },
                pair.as_span(),
            ))?;
        }
        Ok(pair)
    }

    fn expect_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::identifier)?;
        Ok(LocalIdentifier::new(self.to_node(&pair)))
    }

    fn expect_constant_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<ConstantIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::constant_identifier)?;
        Ok(ConstantIdentifier::new(self.to_node(&pair)))
    }

    fn _expect_variable_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<Variable, ParseError> {
        let identifier = self.expect_identifier_next(pairs)?;
        Ok(Variable {
            name: identifier.0,
            is_mutable: None,
        })
    }

    fn expect_field_name_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<FieldName, ParseError> {
        let pair = Self::expect_next(pairs, Rule::identifier)?;
        Ok(FieldName(self.to_node(&pair)))
    }

    fn expect_local_type_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalTypeIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::type_identifier)?;
        Ok(LocalTypeIdentifier::new(self.to_node(&pair)))
    }

    fn expect_qualified_type_identifier_next<'a>(
        &self,
        inner_pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<QualifiedTypeIdentifier, ParseError> {
        let mut first = Self::next_pair(inner_pairs)?;
        println!("first: {:?}", first.as_rule());
        match first.as_rule() {
            Rule::module_segments => {
                let module_path = self.parse_module_segments(first.clone());
                let type_id = inner_pairs.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::ExpectedTypeIdentifierAfterPath, &first)
                })?;

                let type_identifier = self.parse_local_type_identifier(&type_id)?;
                Ok(QualifiedTypeIdentifier::new(type_identifier, module_path))
            }
            Rule::type_identifier => Ok(QualifiedTypeIdentifier::new(
                LocalTypeIdentifier(self.to_node(&first)),
                Vec::new(),
            )),
            _ => Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(&first)),
                &first,
            )),
        }
    }

    fn convert_into_iterator<'a>(pair: &'a Pair<'a, Rule>) -> impl Iterator<Item = Pair<'a, Rule>> {
        pair.clone().into_inner()
    }

    fn create_error_pair(&self, kind: SpecificError, pair: &Pair<Rule>) -> ParseError {
        ParseError {
            span: self.to_span(pair.as_span()),
            specific: kind,
        }
    }

    fn next_inner_pair<'a>(&self, pair: &Pair<'a, Rule>) -> Result<Pair<'a, Rule>, ParseError> {
        let _span = pair.as_span();
        pair.clone()
            .into_inner()
            .next()
            .ok_or_else(move || self.create_error_pair(SpecificError::ExpectingInnerPair, pair))
    }

    // ---------------

    pub fn parse(rule: Rule, raw_script: &str) -> Result<ParseResult<'static>, ParseError> {
        //let script = raw_script.replace("\r", "");
        let pairs = unsafe {
            std::mem::transmute::<pest::iterators::Pairs<'_, Rule>, pest::iterators::Pairs<'_, Rule>>(
                ScriptParser::parse(rule, raw_script)?,
            )
        };
        Ok(ParseResult {
            script: raw_script.to_string(),
            pairs,
        })
    }

    pub fn parse_module(&self, raw_script: &str) -> Result<Module, ParseError> {
        let result = Self::parse(Rule::program, raw_script)?;
        let mut pairs = result.pairs;

        let program_pair = Self::next_pair(&mut pairs)?;

        let mut expressions = Vec::new();
        let mut definitions = Vec::new();
        for pair in Self::convert_into_iterator(&program_pair) {
            match pair.as_rule() {
                Rule::definition => {
                    let def = self.parse_definition(&pair)?;
                    definitions.push(def);
                }
                Rule::expression => {
                    let expr = self.parse_expression(&pair)?;
                    expressions.push(expr);
                }
                Rule::EOI => {} // End of Input - do nothing
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript(Self::pair_to_rule(&pair)),
                        &pair,
                    ));
                }
            }
        }

        let maybe_expression = match expressions.len() {
            0 => None,
            1 => Some(expressions.into_iter().next().unwrap()),
            _ => Some(Expression::Block(expressions)),
        };

        Ok(Module::new(definitions, maybe_expression))
    }

    fn parse_definition(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let inner_pair = self.next_inner_pair(pair)?;
        match inner_pair.as_rule() {
            Rule::impl_def => self.parse_impl_def(&inner_pair),
            Rule::const_def => self.parse_const_definition(&inner_pair),
            Rule::struct_def => self.parse_struct_def(&inner_pair),
            Rule::function_def => self.parse_function_def(&inner_pair),
            Rule::import_definition => self.parse_use(&inner_pair),
            Rule::doc_comment => self.parse_doc_comment(&inner_pair),
            Rule::enum_def => self.parse_enum_def(&inner_pair),
            _ => todo!(),
        }
    }

    fn parse_const_definition(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        Ok(Definition::Constant(self.parse_const_info(pair)?))
    }

    fn parse_const_info(&self, pair: &Pair<Rule>) -> Result<ConstantInfo, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let constant_identifier = self.expect_constant_identifier_next(&mut inner)?;
        let expr_pair = Self::next_pair(&mut inner)?;
        let expression = self.parse_expression(&expr_pair)?;

        Ok(ConstantInfo {
            constant_identifier,
            expression: Box::new(expression),
        })
    }

    fn parse_use(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let import_path = Self::next_pair(&mut inner)?;

        let mut segments = Vec::new();
        let mut assigned_path = Vec::new();
        for pair in import_path.into_inner() {
            segments.push(self.to_node(&pair));
            assigned_path.push(pair.as_str().to_string());
        }

        let items = if let Some(import_list) = inner.next() {
            let mut imported_items = Vec::new();
            for list_item in import_list.into_inner() {
                let item = Self::next_pair(&mut list_item.into_inner())?;

                let import_item = match item.as_rule() {
                    Rule::identifier => {
                        UseItem::Identifier(LocalIdentifier::new(self.to_node(&item)))
                    }
                    Rule::type_identifier => {
                        UseItem::Type(LocalTypeIdentifier::new(self.to_node(&item)))
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::ExpectedIdentifier, &item)
                        );
                    }
                };

                imported_items.push(import_item);
            }

            imported_items
        } else {
            Vec::new()
        };

        Ok(Definition::Use(Use {
            module_path: ModulePath(segments),
            items,
            assigned_path,
        }))
    }

    fn pair_to_rule(rule: &Pair<Rule>) -> String {
        format!("{:?}", rule.as_rule())
    }

    fn parse_block(
        &self,
        block_pair: &Pair<Rule>,
    ) -> Result<(Expression, Vec<ConstantInfo>), ParseError> {
        if block_pair.as_rule() != Rule::block {
            return Err(self.create_error_pair(SpecificError::ExpectedBlock, block_pair));
        }

        let mut expressions = Vec::new();
        let mut constants = Vec::new();

        for pair in Self::convert_into_iterator(block_pair) {
            if pair.as_rule() != Rule::expression_in_block {
                return Err(self.create_error_pair(
                    SpecificError::UnexpectedRuleInParseScript(format!(
                        "Expected expression_in_block, got: {:?}",
                        pair.as_rule()
                    )),
                    block_pair,
                ));
            }

            let inner = self.next_inner_pair(&pair)?;
            match inner.as_rule() {
                Rule::expression => {
                    let expr = self.parse_expression(&inner)?;
                    expressions.push(expr);
                }
                Rule::const_def => {
                    constants.push(self.parse_const_info(&inner)?);
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript(format!(
                            "Unexpected rule in expression_in_block: {:?}",
                            inner.as_rule()
                        )),
                        &inner,
                    ))
                }
            }
        }

        let block_expr = Expression::Block(expressions);
        Ok((block_expr, constants))
    }

    fn parse_if_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        // Parse condition
        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        // Parse `then` branch (block or inline expression)
        let then_branch = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        // Parse optional `else` branch
        let else_branch = inner
            .next()
            .map(|p| {
                match p.as_rule() {
                    Rule::if_expr => self.parse_if_expression(&p), // Recursively handle `else if`
                    _ => self.parse_expression(&p),                // Inline or block `else`
                }
            })
            .transpose()?;

        Ok(Expression::If(
            Box::new(condition),
            Box::new(then_branch),
            else_branch.map(Box::new),
        ))
    }

    fn parse_doc_comment(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        Ok(Definition::Comment(self.to_node(pair)))
    }

    fn parse_function_call_args(&self, pair: &Pair<Rule>) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();
        for argument_pair in pair.clone().into_inner() {
            let mut inner_arg = argument_pair.into_inner().peekable();
            let has_mut = inner_arg
                .peek()
                .map(|p| p.as_rule() == Rule::mut_keyword)
                .unwrap_or(false);

            if has_mut {
                let _mut_node = self.to_node(&inner_arg.next().unwrap());
            }

            let expr_pair = Self::next_pair(&mut inner_arg)?;
            let expr = self.parse_expression(&expr_pair)?;
            if has_mut {
                // TODO:
            }
            args.push(expr);
        }
        Ok(args)
    }

    fn parse_postfix_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner(); // children: primary, postfix_op, postfix_op, ...

        let primary_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &pair)
        })?;
        let mut expr = self.parse_primary(&primary_pair)?;

        for op_pair in inner {
            match op_pair.as_rule() {
                Rule::postfix_op => {
                    let mut sub_inner = op_pair.clone().into_inner();
                    let child = sub_inner.next().ok_or_else(|| {
                        self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &op_pair)
                    })?;

                    match child.as_rule() {
                        Rule::option_operator => {
                            expr = Expression::PostfixOp(
                                PostfixOperator::Unwrap(self.to_node(&child)),
                                Box::new(expr),
                            );
                        }

                        Rule::function_call => {
                            let args = self.parse_function_call_arguments(&child)?;
                            expr = Expression::FunctionCall(Box::new(expr), args);
                        }

                        Rule::member_call => {
                            let mut inner = child.into_inner();

                            let member_identifier = self.expect_identifier_next(&mut inner)?;
                            let args_pair = inner.next().unwrap();
                            let args = self.parse_function_call_arguments(&args_pair)?;
                            expr = Expression::MemberOrFieldCall(
                                Box::new(expr),
                                member_identifier.0,
                                args,
                            );
                        }

                        Rule::array_suffix => {
                            let mut arr_inner = child.clone().into_inner();
                            let index_pair = arr_inner.next().ok_or_else(|| {
                                self.create_error_pair(
                                    SpecificError::UnexpectedPostfixOperator,
                                    &child,
                                )
                            })?;
                            let index_expr = self.parse_expression(&index_pair)?;
                            expr = Expression::IndexAccess(Box::new(expr), Box::new(index_expr));
                        }
                        Rule::method_or_field_suffix => {
                            let mut inner = child.into_inner();
                            let debug_text = inner.as_str();
                            let identifier = self.expect_identifier_next(&mut inner)?;
                            info!(?debug_text, "method_or_field");
                            expr = Expression::FieldOrMemberAccess(Box::new(expr), identifier.0);
                        }
                        _ => {
                            return Err(self.create_error_pair(
                                SpecificError::UnexpectedPostfixOperator,
                                &child,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &op_pair)
                    );
                }
            }
        }

        Ok(expr)
    }

    fn parse_struct_def(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        let struct_name = self.expect_local_type_identifier_next(&mut inner)?;

        let field_defs = Self::next_pair(&mut inner)?;
        let mut fields = Vec::new();

        for field_def in Self::convert_into_iterator(&field_defs) {
            let mut field_parts = Self::convert_into_iterator(&field_def);

            let field_name = self.expect_field_name_next(&mut field_parts)?;
            let field_type = self.parse_type(Self::next_pair(&mut field_parts)?)?;

            let anonym_field = FieldType {
                field_name,
                field_type,
            };

            fields.push(anonym_field);
        }

        let struct_def = StructType::new(struct_name, fields);

        Ok(Definition::StructDef(struct_def))
    }

    fn parse_function_def(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let function_pair = self.next_inner_pair(pair)?;

        match function_pair.as_rule() {
            Rule::normal_function => {
                let mut inner = function_pair.clone().into_inner();
                let signature_pair = inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingFunctionSignature, &function_pair)
                })?;

                let signature = self.parse_function_signature(&signature_pair)?;

                let (body, constants) = self.parse_block(&inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingFunctionBody, &function_pair)
                })?)?;

                Ok(Definition::FunctionDef(Function::Internal(
                    FunctionWithBody {
                        declaration: signature,
                        body,
                        constants,
                    },
                )))
            }
            Rule::external_function => {
                let signature_pair =
                    function_pair.clone().into_inner().next().ok_or_else(|| {
                        self.create_error_pair(
                            SpecificError::MissingFunctionSignature,
                            &function_pair,
                        )
                    })?;

                let signature = self.parse_function_signature(&signature_pair)?;
                Ok(Definition::FunctionDef(Function::External(signature)))
            }
            _ => {
                Err(self
                    .create_error_pair(SpecificError::ExpectedFunctionDefinition, &function_pair))
            }
        }
    }
    fn parse_function_signature(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<FunctionDeclaration, ParseError> {
        if pair.as_rule() != Rule::function_signature {
            return Err(self.create_error_pair(SpecificError::MissingFunctionSignature, &pair));
        }

        let mut inner = pair.clone().into_inner();

        let function_name = self.expect_identifier_next(&mut inner)?;

        let next_token = inner.next();
        let (parameters, return_type) = match next_token {
            Some(token) if token.as_rule() == Rule::parameter_list => {
                let params = self.parse_parameters(&token)?;

                let ret_type = if let Some(return_type_pair) = inner.next() {
                    Some(self.parse_return_type(&return_type_pair)?)
                } else {
                    None
                };

                (params, ret_type)
            }

            Some(token) if token.as_rule() == Rule::return_type => {
                (Vec::new(), Some(self.parse_return_type(&token)?))
            }
            _ => (Vec::new(), None),
        };

        Ok(FunctionDeclaration {
            name: function_name.0,
            params: parameters,
            self_parameter: None,
            return_type,
        })
    }

    fn parse_return_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let inner_pair = self.next_inner_pair(pair)?;
        self.parse_type(inner_pair)
    }

    pub fn parse_parameters(&self, pair: &Pair<Rule>) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        for param_pair in Self::convert_into_iterator(pair) {
            match param_pair.as_rule() {
                Rule::parameter => {
                    let pairs: Vec<_> = param_pair.into_inner().collect();

                    let (maybe_mutable_pair, name_pair, type_pair) =
                        if pairs[0].as_rule() == Rule::mut_keyword {
                            (Some(&pairs[0]), &pairs[1], &pairs[2])
                        } else {
                            (None, &pairs[0], &pairs[1])
                        };

                    let param_type = self.parse_type(type_pair.clone())?;
                    let maybe_mut_node = maybe_mutable_pair.map(|v| self.to_node(v));

                    parameters.push(Parameter {
                        variable: Variable::new(self.to_node(name_pair), maybe_mut_node),
                        param_type,
                    });
                }
                Rule::self_parameter => {
                    panic!("should have been handled before parsing parameters")
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::ExpectedParameter, &param_pair)
                    );
                }
            }
        }

        Ok(parameters)
    }

    fn parse_impl_def(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);
        let type_name = self.expect_local_type_identifier_next(&mut inner)?;
        let mut functions = Vec::new();

        for item_pair in inner {
            if item_pair.as_rule() == Rule::impl_item {
                let inner_item = self.next_inner_pair(&item_pair)?;
                match inner_item.as_rule() {
                    Rule::external_member_function => {
                        let signature = self.parse_member_signature(&inner_item)?;
                        functions.push(Function::External(signature));
                    }
                    Rule::normal_member_function => {
                        let function_data = self.parse_member_data(&inner_item)?;
                        functions.push(Function::Internal(function_data));
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::ExpectedImplItem, &inner_item)
                        )
                    }
                }
            }
        }

        Ok(Definition::ImplDef(type_name.0, functions))
    }

    fn parse_member_signature(&self, pair: &Pair<Rule>) -> Result<FunctionDeclaration, ParseError> {
        if pair.as_rule() != Rule::member_signature {
            return Err(self.create_error_pair(SpecificError::ExpectedMemberSignature, &pair));
        }

        let mut inner = pair.clone().into_inner();

        let name = self.expect_identifier_next(&mut inner)?;

        let mut parameters = Vec::new();
        let mut self_parameter = None;
        let mut return_type = None;

        for next_pair in inner {
            match next_pair.as_rule() {
                Rule::self_parameter => {
                    let mut mut_keyword_node = None;
                    let mut self_node = None;

                    for pair in next_pair.into_inner() {
                        match pair.as_rule() {
                            Rule::mut_keyword => {
                                mut_keyword_node = Some(self.to_node(&pair));
                            }
                            Rule::self_identifier => {
                                self_node = Some(self.to_node(&pair));
                            }
                            _ => unreachable!("Unexpected rule in self_parameter"),
                        }
                    }

                    self_parameter = Some(SelfParameter {
                        is_mutable: mut_keyword_node,
                        self_node: self_node.expect("self node must exist"),
                    });
                }
                Rule::parameter_list => {
                    parameters = self.parse_parameters(&next_pair)?;
                }
                Rule::return_type => {
                    return_type = Some(self.parse_return_type(&next_pair)?);
                }
                _ => {}
            }
        }

        Ok(FunctionDeclaration {
            name: name.0,
            params: parameters,
            self_parameter,
            return_type,
        })
    }

    fn parse_member_data(&self, pair: &Pair<Rule>) -> Result<FunctionWithBody, ParseError> {
        if pair.as_rule() != Rule::normal_member_function {
            return Err(self.create_error_pair(SpecificError::ExpectedMemberSignature, pair));
        }

        let mut inner = Self::convert_into_iterator(pair);

        let signature_pair = Self::next_pair(&mut inner)?;
        let signature = self.parse_member_signature(&signature_pair)?;

        let block_pair = Self::next_pair(&mut inner)?;
        let (body, constants) = self.parse_block(&block_pair)?;

        Ok(FunctionWithBody {
            declaration: signature,
            body,
            constants,
        })
    }

    fn parse_for_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        let pattern_pair = Self::next_pair(&mut inner)?;
        if pattern_pair.as_rule() != Rule::for_pattern {
            return Err(self.create_error_pair(SpecificError::ExpectedForPattern, &pattern_pair));
        }

        let inner_pattern = self.next_inner_pair(&pattern_pair)?;
        let pattern = match inner_pattern.as_rule() {
            Rule::mut_identifier => {
                let mut inner_iter = inner_pattern.clone().into_inner();
                let is_mutable = inner_iter
                    .peek()
                    .map_or(false, |p| p.as_rule() == Rule::mut_keyword);

                let is_mut = if is_mutable {
                    // Capture the 'mut' keyword as a Node
                    let mut_node = self.to_node(&inner_iter.next().unwrap());
                    Some(mut_node)
                } else {
                    None
                };

                // Extract the identifier
                let identifier = if is_mutable {
                    self.expect_identifier_next(&mut inner_iter)?.0
                } else {
                    self.to_node(&inner_pattern)
                };

                ForPattern::Single(ForVar { identifier, is_mut })
            }
            Rule::for_pair => {
                let mut vars = Self::convert_into_iterator(&inner_pattern);

                // Parse first variable in the pair
                let first_var_pair = Self::next_pair(&mut vars)?;
                let mut first_inner_iter = first_var_pair.clone().into_inner();
                let first_is_mut = if first_inner_iter
                    .peek()
                    .map_or(false, |p| p.as_rule() == Rule::mut_keyword)
                {
                    Some(self.to_node(&first_inner_iter.next().unwrap()))
                } else {
                    None
                };

                let first_identifier = if first_is_mut.is_some() {
                    self.expect_identifier_next(&mut first_inner_iter)?.0
                } else {
                    self.to_node(&first_var_pair)
                };

                // Parse second variable in the pair
                let second_var_pair = Self::next_pair(&mut vars)?;
                let mut second_inner_iter = second_var_pair.clone().into_inner();
                let second_is_mut = if second_inner_iter
                    .peek()
                    .map_or(false, |p| p.as_rule() == Rule::mut_keyword)
                {
                    Some(self.to_node(&second_inner_iter.next().unwrap()))
                } else {
                    None
                };

                let second_identifier = if second_is_mut.is_some() {
                    self.expect_identifier_next(&mut second_inner_iter)?.0
                } else {
                    self.to_node(&second_var_pair)
                };

                ForPattern::Pair(
                    ForVar {
                        identifier: first_identifier,
                        is_mut: first_is_mut,
                    },
                    ForVar {
                        identifier: second_identifier,
                        is_mut: second_is_mut,
                    },
                )
            }
            _ => {
                return Err(self.create_error_pair(SpecificError::InvalidForPattern, &inner_pattern))
            }
        };

        // Parse the 'mut' keyword before the iterable expression, if present
        let next_pair = Self::next_pair(&mut inner)?;
        let iterable_expression = self.parse_expression(&next_pair)?;

        let mut_expression = IteratableExpression {
            expression: Box::new(iterable_expression),
        };

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        // Return the ForLoop statement with MutExpression
        Ok(Expression::ForLoop(
            pattern,
            mut_expression,
            Box::from(body),
        ))
    }

    fn parse_while_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        Ok(Expression::WhileLoop(Box::from(condition), Box::from(body)))
    }

    fn parse_return(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        // Return value is optional
        let expr = match inner.next() {
            Some(expr_pair) => Some(Box::new(self.parse_expression(&expr_pair)?)),
            None => None,
        };

        Ok(Expression::Return(expr))
    }

    fn parse_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        match pair.as_rule() {
            Rule::variable => Ok(Expression::VariableAccess(Variable::new(
                self.to_node(pair),
                None,
            ))),

            Rule::block => {
                // Block in expression context - expect single expression
                let inner = self.next_inner_pair(pair)?;
                self.parse_expression(&inner)
            }

            Rule::expression => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_expression(&inner)
            }

            Rule::assignment_expression => self.parse_assignment_expression(pair),

            Rule::addition => self.parse_binary_chain(pair),

            Rule::logical | Rule::comparison | Rule::multiplication => self.parse_binary_op(pair),

            Rule::prefix => self.parse_prefix_expression(pair),

            Rule::primary | Rule::literal => self.parse_primary(pair),

            Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                let op = self.parse_unary_operator(pair)?;
                let expr = self.parse_primary(&self.next_inner_pair(pair)?)?;
                Ok(Expression::UnaryOp(op, Box::new(expr)))
            }

            Rule::postfix => self.parse_postfix_expression(pair),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedExpressionType(Self::pair_to_rule(pair)),
                pair,
            )),
        }
    }

    fn parse_multi_var_assignment(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();

        let var_list_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript("missing variable_list".to_string()),
                pair,
            )
        })?;

        let variables = self.parse_variable_list(&var_list_pair)?;

        let rhs_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript("missing RHS expression".to_string()),
                pair,
            )
        })?;
        let rhs_expr = self.parse_expression(&rhs_pair)?;

        Ok(Expression::MultiVariableAssignment(
            variables,
            Box::new(rhs_expr),
        ))
    }

    fn parse_variable_list(&self, pair: &Pair<Rule>) -> Result<Vec<Variable>, ParseError> {
        let mut variables = Vec::new();
        for item_pair in pair.clone().into_inner() {
            if item_pair.as_rule() == Rule::variable_item {
                variables.push(self.parse_variable_item(item_pair)?);
            }
        }
        Ok(variables)
    }

    fn parse_variable_item(&self, pair: Pair<Rule>) -> Result<Variable, ParseError> {
        let mut inner = pair.clone().into_inner().peekable();

        let mut_node = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::mut_keyword {
                // Convert 'mut' to a Node
                let node = self.to_node(peeked);
                inner.next(); // consume the 'mut' token
                Some(node)
            } else {
                None
            }
        } else {
            None
        };

        let name_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript(
                    "Expected identifier in variable_item".into(),
                ),
                &pair,
            )
        })?;

        if name_pair.as_rule() != Rule::identifier {
            return Err(self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript(format!(
                    "Expected identifier, found {:?}",
                    name_pair.as_rule()
                )),
                &name_pair,
            ));
        }

        let var_name_node = self.to_node(&name_pair);

        Ok(Variable {
            name: var_name_node,
            is_mutable: mut_node,
        })
    }

    fn parse_assignment_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        match pair.clone().as_rule() {
            // HACK: It should be checked for an Rules::assignment_expression before calling this function
            Rule::assignment_expression => {
                let mut inner = pair.clone().into_inner();
                let child = inner.next().ok_or_else(|| {
                    self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript("Missing child".into()),
                        pair,
                    )
                })?;
                self.parse_assignment_expression(&child) // re-call on the child
            }
            Rule::multi_var_assignment => self.parse_multi_var_assignment(&pair),
            Rule::single_lhs_assignment => self.parse_assignment(&pair),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript(format!(
                    "Not an assignment expression: {:?}",
                    pair.as_rule()
                )),
                pair,
            )),
        }
    }

    fn parse_assignment(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner().peekable();

        let is_mutable = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::mut_keyword {
                let mut_node = self.to_node(peeked);
                inner.next(); // consume it
                Some(mut_node)
            } else {
                None
            }
        } else {
            None
        };

        let lhs_pair = Self::next_pair(&mut inner)?;

        let op_pair = Self::next_pair(&mut inner)?;
        let operator_node = self.to_node(&op_pair);

        let rhs_expr = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        let compound_op_kind: Option<CompoundOperatorKind> = match op_pair.as_rule() {
            Rule::assign_op => None,
            Rule::compound_assign_op => {
                let mut inner_op = op_pair.clone().into_inner();
                let child = inner_op.next().ok_or_else(|| {
                    self.create_error_pair(
                        SpecificError::UnknownOperator(
                            "No child found in compound_assign_op".to_string(),
                        ),
                        &op_pair,
                    )
                })?;
                match child.as_rule() {
                    Rule::add_assign_op => Some(CompoundOperatorKind::Add),
                    Rule::sub_assign_op => Some(CompoundOperatorKind::Sub),
                    Rule::mul_assign_op => Some(CompoundOperatorKind::Mul),
                    Rule::div_assign_op => Some(CompoundOperatorKind::Div),
                    _ => {
                        return Err(self.create_error_pair(
                            SpecificError::UnknownOperator(format!(
                                "Found unexpected operator rule: {:?}",
                                child.as_rule()
                            )),
                            &child,
                        ));
                    }
                }
            }
            _ => {
                return Err(self.create_error_pair(
                    SpecificError::UnknownAssignmentOperator(Self::pair_to_rule(&op_pair)),
                    &op_pair,
                ));
            }
        };

        // Disallow compound operator if 'mut' keyword was used
        if compound_op_kind.is_some() && is_mutable.is_some() {
            return Err(
                self.create_error_pair(SpecificError::CompoundOperatorCanNotContainMut, &op_pair)
            );
        }

        let compound_op = compound_op_kind.map(|kind| CompoundOperator {
            node: operator_node,
            kind,
        });

        match lhs_pair.as_rule() {
            Rule::variable_list => {
                let vars: Vec<_> = lhs_pair.into_inner().collect();

                if vars.len() == 1 {
                    // Single variable
                    let var = Variable::new(self.to_node(&vars[0]), is_mutable);
                    match compound_op {
                        None => Ok(Expression::VariableAssignment(var, Box::new(rhs_expr))),
                        Some(op) => Ok(Expression::VariableCompoundAssignment(
                            var.name,
                            op,
                            Box::new(rhs_expr),
                        )),
                    }
                } else {
                    // Multiple variables => disallow compound operators
                    if compound_op.is_some() {
                        return Err(self.create_error_pair(
                            SpecificError::CompoundOperatorCanNotHaveMultipleVariables,
                            &op_pair,
                        ));
                    }
                    let variables = vars
                        .into_iter()
                        .map(|v| Variable::new(self.to_node(&v), is_mutable.clone()))
                        .collect();
                    Ok(Expression::MultiVariableAssignment(
                        variables,
                        Box::new(rhs_expr),
                    ))
                }
            }

            _ => {
                let lhs_expr = self.parse_expression(&lhs_pair)?;

                match lhs_expr {
                    Expression::VariableAccess(mut var) => {
                        var.is_mutable = is_mutable;
                        match compound_op {
                            None => Ok(Expression::VariableAssignment(var, Box::new(rhs_expr))),
                            Some(op) => Ok(Expression::VariableCompoundAssignment(
                                var.name,
                                op,
                                Box::new(rhs_expr),
                            )),
                        }
                    }
                    Expression::FieldOrMemberAccess(base, field_node) => match compound_op {
                        None => Ok(Expression::FieldAssignment(
                            base,
                            field_node,
                            Box::new(rhs_expr),
                        )),
                        Some(op) => Ok(Expression::FieldCompoundAssignment(
                            base,
                            field_node,
                            op,
                            Box::new(rhs_expr),
                        )),
                    },
                    Expression::IndexAccess(base, index_expr) => match compound_op {
                        None => Ok(Expression::IndexAssignment(
                            base,
                            index_expr,
                            Box::new(rhs_expr),
                        )),
                        Some(op) => Ok(Expression::IndexCompoundAssignment(
                            base,
                            index_expr,
                            op,
                            Box::new(rhs_expr),
                        )),
                    },
                    _ => {
                        Err(self
                            .create_error_pair(SpecificError::InvalidAssignmentTarget, &lhs_pair))
                    }
                }
            }
        }
    }

    fn parse_binary_chain(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);
        let mut left = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        while let Some(op) = inner.next() {
            let right = self.parse_expression(&Self::next_pair(&mut inner)?)?;
            left = if op.as_rule() == Rule::range_op {
                Expression::ExclusiveRange(Box::new(left), Box::new(right))
            } else {
                let operator = self.parse_binary_operator(&op)?;
                Expression::BinaryOp(Box::new(left), operator, Box::new(right))
            };
        }

        Ok(left)
    }

    fn parse_prefix_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let _span = pair.as_span();
        let mut inner = Self::convert_into_iterator(pair).peekable();
        let mut expr = None;
        let mut prefix_ops = Vec::new();

        while let Some(part) = inner.next() {
            match part.as_rule() {
                Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                    let op = self.parse_unary_operator(&part)?;
                    prefix_ops.push(op);
                }
                _ => {
                    expr = Some(self.parse_expression(&part)?);
                    break;
                }
            }
        }

        let mut final_expr = expr.ok_or_else(|| {
            self.create_error_pair(SpecificError::ExpectedExpressionAfterPrefixOperator, &pair)
        })?;

        for op in prefix_ops.into_iter().rev() {
            final_expr = Expression::UnaryOp(op, Box::new(final_expr));
        }

        Ok(final_expr)
    }

    fn parse_binary_op(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);
        let mut left = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        while let Some(op) = inner.next() {
            let right = self.parse_expression(&Self::next_pair(&mut inner)?)?;
            left = match op.as_str() {
                ".." => Expression::ExclusiveRange(Box::new(left), Box::new(right)),
                _ => {
                    let operator = self.parse_binary_operator(&op)?;
                    Expression::BinaryOp(Box::new(left), operator, Box::new(right))
                }
            };
        }

        Ok(left)
    }

    fn parse_binary_operator(&self, pair: &Pair<Rule>) -> Result<BinaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::infix_op | Rule::prefix_op => self.next_inner_pair(pair)?,
            _ => pair.clone(),
        };

        let node = self.to_node(&op);

        match op.as_rule() {
            Rule::op_add => Ok(BinaryOperator::Add(node)),
            Rule::op_sub => Ok(BinaryOperator::Subtract(node)),
            Rule::op_mul => Ok(BinaryOperator::Multiply(node)),
            Rule::op_div => Ok(BinaryOperator::Divide(node)),
            Rule::op_mod => Ok(BinaryOperator::Modulo(node)),
            Rule::op_eq => Ok(BinaryOperator::Equal(node)),
            Rule::op_neq => Ok(BinaryOperator::NotEqual(node)),
            Rule::op_lt => Ok(BinaryOperator::LessThan(node)),
            Rule::op_lte => Ok(BinaryOperator::LessEqual(node)),
            Rule::op_gt => Ok(BinaryOperator::GreaterThan(node)),
            Rule::op_gte => Ok(BinaryOperator::GreaterEqual(node)),
            Rule::op_and => Ok(BinaryOperator::LogicalAnd(node)),
            Rule::op_or => Ok(BinaryOperator::LogicalOr(node)),
            _ => Err(self
                .create_error_pair(SpecificError::UnknownOperator(Self::pair_to_rule(&op)), &op)),
        }
    }

    fn parse_unary_operator(&self, pair: &Pair<Rule>) -> Result<UnaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::prefix_op => &self.next_inner_pair(pair)?,
            _ => &pair,
        };

        let node = self.to_node(op);
        match op.as_rule() {
            Rule::op_neg => Ok(UnaryOperator::Negate(node)),
            Rule::op_not => Ok(UnaryOperator::Not(node)),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedUnaryOperator(Self::pair_to_rule(&op)),
                &op,
            )),
        }
    }

    fn parse_module_segments(&self, pair: Pair<Rule>) -> Vec<Node> {
        pair.into_inner()
            .filter_map(|segment| {
                if segment.as_rule() == Rule::identifier {
                    Some(self.to_node(&segment))
                } else {
                    None
                }
            })
            .collect()
    }

    fn parse_qualified_type_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<QualifiedTypeIdentifier, ParseError> {
        let mut inner_pairs = pair.clone().into_inner();
        let mut generic_types = Vec::new();

        let first = inner_pairs.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(&pair)),
                pair,
            )
        })?;

        match first.as_rule() {
            Rule::module_segments => {
                let module_path = self.parse_module_segments(first.clone());
                let type_id = inner_pairs.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::ExpectedTypeIdentifierAfterPath, &first)
                })?;

                let type_identifier = self.parse_local_type_identifier(&type_id)?;

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next() {
                    if generic_params.as_rule() == Rule::generic_params {
                        generic_types = self.parse_generic_params(&generic_params)?;
                    }
                }

                Ok(QualifiedTypeIdentifier::new_with_generics(
                    type_identifier,
                    module_path,
                    generic_types,
                ))
            }
            Rule::type_identifier => {
                let type_identifier = LocalTypeIdentifier(self.to_node(&first));

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next() {
                    if generic_params.as_rule() == Rule::generic_params {
                        generic_types = self.parse_generic_params(&generic_params)?;
                    }
                }

                Ok(QualifiedTypeIdentifier::new_with_generics(
                    type_identifier,
                    Vec::new(),
                    generic_types,
                ))
            }
            _ => Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(&first)),
                &first,
            )),
        }
    }

    fn parse_generic_params(&self, pair: &Pair<Rule>) -> Result<Vec<Type>, ParseError> {
        let inner_pairs = pair.clone().into_inner();
        let mut generic_types = Vec::new();

        for type_pair in inner_pairs {
            if type_pair.as_rule() == Rule::type_name {
                generic_types.push(self.parse_type(type_pair)?);
            }
        }

        Ok(generic_types)
    }

    fn parse_qualified_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<QualifiedIdentifier, ParseError> {
        let mut inner_pairs = pair.clone().into_inner();

        let first = inner_pairs
            .next()
            .ok_or_else(|| self.create_error_pair(SpecificError::ExpectedIdentifier, pair))?;

        match first.as_rule() {
            Rule::module_segments => {
                let module_path = self.parse_module_segments(first.clone());
                let id = inner_pairs.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::ExpectedIdentifierAfterPath, &first)
                })?;

                let identifier = self.to_node(&id);
                Ok(QualifiedIdentifier::new(identifier, module_path))
            }
            Rule::identifier => Ok(QualifiedIdentifier::new(self.to_node(&first), Vec::new())),
            _ => Err(self.create_error_pair(SpecificError::ExpectedIdentifier, &first)),
        }
    }

    fn parse_struct_instantiation(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let type_pair = inner.next().unwrap();

        // Get struct name (required)
        let struct_name = self.parse_qualified_type_identifier(&type_pair)?;

        let mut fields = Vec::new();
        let mut has_rest = false;

        // Parsing the fields (and maybe rest_fields "..")
        if let Some(field_list) = inner.next() {
            for field_pair in field_list.into_inner() {
                match field_pair.as_rule() {
                    Rule::struct_field => {
                        let mut field_inner = field_pair.into_inner();
                        let ident = self.expect_identifier_next(&mut field_inner)?;
                        let field_name = FieldName(ident.0);
                        let field_value = self.parse_expression(&field_inner.next().unwrap())?;

                        fields.push(FieldExpression {
                            field_name,
                            expression: field_value,
                        });
                    }
                    Rule::rest_fields => {
                        has_rest = true;
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::ExpectedFieldOrRest, &field_pair)
                        )
                    }
                }
            }
        }

        Ok(Expression::StructInstantiation(
            struct_name,
            fields,
            has_rest,
        ))
    }

    fn parse_static_member_reference(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        println!("inner {}", inner.as_str());

        let type_identifier = self.parse_qualified_type_identifier(&inner.next().unwrap())?;
        let member_name = self.expect_identifier_next(&mut inner)?;

        Ok(Expression::StaticMemberFunctionReference(
            type_identifier,
            member_name.0,
        ))
    }

    fn parse_primary(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        match pair.as_rule() {
            Rule::primary => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_primary(&inner)
            }
            Rule::interpolated_string => self.parse_interpolated_string(pair),
            Rule::literal => Ok(Expression::Literal(self.parse_literal(pair)?)),
            Rule::variable => Ok(Expression::VariableAccess(Variable::new(
                self.to_node(pair),
                None,
            ))),
            Rule::static_member_reference => self.parse_static_member_reference(pair),
            Rule::constant => Ok(Expression::ConstantAccess(ConstantIdentifier(
                self.to_node(pair),
            ))),
            Rule::static_call => self.parse_static_call(pair),
            Rule::match_expr => self.parse_match_expr(pair),
            Rule::map_literal => self.parse_map_literal(pair),
            Rule::array_literal => self.parse_array_literal(pair),
            Rule::float_lit => Ok(Expression::Literal(Literal::Float(self.to_node(&pair)))),
            Rule::parenthesized => {
                let inner = self.next_inner_pair(&pair)?;
                self.parse_expression(&inner)
            }
            Rule::struct_instantiation => self.parse_struct_instantiation(pair),
            Rule::enum_literal => Ok(Expression::Literal(self.parse_enum_literal(pair)?)),

            Rule::return_expr => self.parse_return(pair),
            Rule::break_expr => Ok(Expression::Break(self.to_node(pair))),
            Rule::continue_expr => Ok(Expression::Continue(self.to_node(pair))),
            Rule::block => {
                let (expression, _constants) = self.parse_block(pair)?;
                Ok(expression)
            }
            Rule::if_expr => self.parse_if_expression(pair),
            Rule::for_loop => self.parse_for_loop(pair),
            Rule::while_loop => self.parse_while_loop(pair),

            _ => Err(self.create_error_pair(
                SpecificError::UnknownPrimary(Self::pair_to_rule(&pair)),
                pair,
            )),
        }
    }

    fn parse_interpolated_string(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut parts = Vec::new();

        for part_pair in Self::convert_into_iterator(&pair) {
            match part_pair.as_rule() {
                Rule::text => {
                    parts.push(StringPart::Literal(self.to_node(&part_pair)));
                }
                Rule::interpolation => {
                    let inner = self.next_inner_pair(&part_pair.clone())?;
                    let expr = match inner.as_rule() {
                        Rule::expression => self.parse_expression(&inner)?,
                        _ => {
                            return Err(self.create_error_pair(
                                SpecificError::ExpectedExpressionInInterpolation,
                                &inner,
                            ))
                        }
                    };

                    let format = if let Some(fmt) = Self::convert_into_iterator(&part_pair).nth(1) {
                        if fmt.as_rule() == Rule::format_specifier {
                            Some(self.parse_format_specifier(fmt)?)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    parts.push(StringPart::Interpolation(Box::new(expr), format));
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInInterpolation,
                        &part_pair,
                    ))
                }
            }
        }

        Ok(Expression::InterpolatedString(parts))
    }

    fn parse_format_specifier(&self, pair: Pair<Rule>) -> Result<FormatSpecifier, ParseError> {
        let node = self.to_node(&pair);
        match pair.as_str() {
            "x" => Ok(FormatSpecifier::LowerHex(node)),
            "X" => Ok(FormatSpecifier::UpperHex(node)),
            "b" => Ok(FormatSpecifier::Binary(node)),
            "f" => Ok(FormatSpecifier::Float(node)),
            s if s.starts_with("..") => {
                let precision: u32 = s[2..s.len() - 1].parse().map_err(|_| {
                    self.create_error_pair(SpecificError::InvalidPrecisionValue, &pair)
                })?;
                let typ = match s.chars().last().unwrap() {
                    'f' => PrecisionType::Float(node),
                    's' => PrecisionType::String(node),
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidPrecisionType, &pair)
                        )?
                    }
                };
                Ok(FormatSpecifier::Precision(
                    precision,
                    self.to_node(&pair),
                    typ,
                ))
            }
            _ => Err(self.create_error_pair(SpecificError::InvalidFormatSpecifier, &pair)),
        }
    }

    fn parse_enum_literal(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<swamp_script_ast::Literal, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        // Parse enum type name
        let enum_type = self.parse_qualified_type_identifier(&inner.next().unwrap())?;

        // Parse variant name
        let variant_pair = Self::expect_next(&mut inner, Rule::type_identifier)?;
        let variant_type_identifier = LocalTypeIdentifier::new(self.to_node(&variant_pair));

        // Parse fields if they exist
        let enum_variant_literal = if let Some(fields_pair) = inner.next() {
            match fields_pair.as_rule() {
                Rule::struct_fields_lit => {
                    let mut fields = Vec::new();
                    for field in Self::convert_into_iterator(&fields_pair) {
                        if field.as_rule() == Rule::struct_field {
                            let mut field_inner = Self::convert_into_iterator(&field);
                            let field_name = self.expect_field_name_next(&mut field_inner)?;
                            let field_expression =
                                self.parse_expression(&Self::next_pair(&mut field_inner)?)?;
                            let anonym_field = FieldExpression {
                                field_name,
                                expression: field_expression,
                            };
                            fields.push(anonym_field);
                        }
                    }
                    EnumVariantLiteral::Struct(enum_type, variant_type_identifier, fields)
                }
                Rule::tuple_fields => {
                    let mut expressions = vec![];
                    for field in Self::convert_into_iterator(&fields_pair) {
                        let field_value = self.parse_expression(&field)?;
                        expressions.push(field_value);
                    }
                    EnumVariantLiteral::Tuple(enum_type, variant_type_identifier, expressions)
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::UnexpectedVariantField, &fields_pair)
                    );
                }
            }
        } else {
            EnumVariantLiteral::Simple(enum_type, variant_type_identifier)
        };

        Ok(Literal::EnumVariant(enum_variant_literal))
    }

    fn parse_literal(&self, pair: &Pair<Rule>) -> Result<Literal, ParseError> {
        let inner = self.next_inner_pair(pair)?;
        let node = self.to_node(&inner);
        match inner.as_rule() {
            Rule::int_lit => Ok(Literal::Int(node)),
            Rule::float_lit => Ok(Literal::Float(node)),
            Rule::string_lit => {
                let _content = inner.as_str();
                // Remove quotes and handle escapes
                /*
                let processed = content[1..content.len() - 1]
                    .replace("\\\"", "\"")
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\\", "\\");

                 */
                Ok(Literal::String(node))
            }
            Rule::bool_lit => Ok(Literal::Bool(node)),
            Rule::unit_lit => Ok(Literal::Unit),
            Rule::none_lit => Ok(Literal::None(node)),
            Rule::tuple_lit => {
                let mut expressions = Vec::new();
                for expr_pair in Self::convert_into_iterator(&inner) {
                    expressions.push(self.parse_expression(&expr_pair)?);
                }
                Ok(Literal::Tuple(expressions))
            }
            _ => Err(self.create_error_pair(SpecificError::UnknownLiteral, &inner)),
        }
    }

    fn parse_array_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut elements = Vec::new();
        for element in Self::convert_into_iterator(&pair) {
            elements.push(self.parse_expression(&element)?);
        }
        Ok(Expression::Literal(Literal::Array(elements)))
    }

    fn parse_map_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut entries = Vec::new();

        for entry_pair in Self::convert_into_iterator(&pair) {
            if entry_pair.as_rule() == Rule::map_entry {
                let mut entry_inner = Self::convert_into_iterator(&entry_pair);
                let key = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                let value = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                entries.push((key, value));
            }
        }

        Ok(Expression::Literal(Literal::Map(entries)))
    }

    fn parse_function_call_arguments(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<Expression>, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let mut args = Vec::new();

        // Parse arguments
        while let Some(arg_pair) = inner.next() {
            if arg_pair.as_rule() == Rule::function_argument {
                let mut arg_inner = Self::convert_into_iterator(&arg_pair).peekable();

                // Check for mut keyword
                let has_mut = arg_inner
                    .peek()
                    .map(|p| p.as_rule() == Rule::mut_keyword)
                    .unwrap_or(false);

                if has_mut {
                    arg_inner.next(); // consume mut keyword
                }

                let expr = self.parse_expression(&Self::next_pair(&mut arg_inner)?)?;

                if has_mut {
                    match expr {
                        Expression::VariableAccess(var) => {
                            args.push(Expression::MutRef(LocationExpression::Variable(var)));
                        }
                        Expression::FieldOrMemberAccess(expr, node) => {
                            args.push(Expression::MutRef(LocationExpression::FieldAccess(
                                expr, node,
                            )));
                        }
                        _ => {
                            return Err(self
                                .create_error_pair(SpecificError::MutOnlyForVariables, &arg_pair))
                        }
                    }
                } else {
                    args.push(expr);
                }
            } else {
                return Err(
                    self.create_error_pair(SpecificError::UnexpectedTokenInFunctionCall, &arg_pair)
                );
            }
        }

        Ok(args)
    }

    fn parse_static_call(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        // Parse type name
        let type_name_pair = Self::next_pair(&mut inner)?;

        let qualified_type_identifier = self.parse_qualified_type_identifier(&type_name_pair)?;

        let function_name = self.expect_identifier_next(&mut inner)?;

        // Parse arguments
        let mut args = Vec::new();
        while let Some(arg_pair) = inner.next() {
            if arg_pair.as_rule() == Rule::function_argument {
                let mut arg_inner = Self::convert_into_iterator(&arg_pair).peekable();
                let has_mut = arg_inner
                    .peek()
                    .map(|p| p.as_rule() == Rule::mut_keyword)
                    .unwrap_or(false);

                if has_mut {
                    arg_inner.next();
                }

                let next = &Self::next_pair(&mut arg_inner)?;
                let mut expr = self.parse_expression(next)?;

                if has_mut {
                    expr = Expression::MutRef(self.convert_to_location_expression(expr, next)?);
                }
                args.push(expr);
            }
        }

        if !qualified_type_identifier.generic_params.is_empty() {
            Ok(Expression::StaticCallGeneric(
                qualified_type_identifier,
                function_name.0,
                args,
            ))
        } else {
            Ok(Expression::StaticCall(
                qualified_type_identifier,
                function_name.0,
                args,
            ))
        }
    }

    fn parse_type(&self, pair: Pair<Rule>) -> Result<Type, ParseError> {
        match pair.as_rule() {
            Rule::type_name => {
                let mut inner = pair.clone().into_inner();
                let base_type = if let Some(inner_pair) = inner.next() {
                    self.parse_type(inner_pair)?
                } else {
                    self.parse_type_from_str(&mut inner, &pair)?
                };

                let optional_marker = inner
                    .find(|p| p.as_rule() == Rule::optional_marker)
                    .map(|marker_pair| self.to_node(&marker_pair));
                if let Some(found_optional_marker) = optional_marker {
                    Ok(Type::Optional(Box::new(base_type), found_optional_marker))
                } else {
                    Ok(base_type)
                }
            }

            Rule::base_type => {
                let mut inner = pair.into_inner();
                let first = inner.next().unwrap();
                let base_type = self.parse_type(first)?;

                if let Some(generic_params) = inner.next() {
                    if generic_params.as_rule() == Rule::generic_params {
                        let mut generic_types = Vec::new();
                        for param in Self::convert_into_iterator(&generic_params) {
                            generic_types.push(self.parse_type(param)?);
                        }
                        Ok(Type::Generic(Box::new(base_type), generic_types))
                    } else {
                        Ok(base_type)
                    }
                } else {
                    Ok(base_type)
                }
            }
            Rule::function_type => {
                let mut function_inner = pair.into_inner();

                // Parse parameter types
                let param_types = if let Some(params) = function_inner
                    .next()
                    .filter(|p| p.as_rule() == Rule::function_params)
                {
                    params
                        .into_inner()
                        .map(|param| {
                            Ok(TypeForParameter {
                                ast_type: self.parse_type(param).unwrap(),
                                is_mutable: false,
                            })
                        })
                        .collect::<Result<Vec<_>, ParseError>>()?
                } else {
                    Vec::new()
                };

                // Parse return type
                let return_type = self.parse_type(function_inner.next().unwrap())?;

                Ok(Type::Function(param_types, Box::new(return_type)))
            }
            Rule::optional_type => {
                let inner = self.next_inner_pair(&pair)?;
                let base_type = self.parse_type(inner)?;
                Ok(Type::Optional(Box::new(base_type), self.to_node(&pair)))
            }
            Rule::built_in_type => {
                let mut inner = pair.clone().into_inner();
                self.parse_type_from_str(&mut inner, &pair)
            }
            Rule::qualified_type_identifier => {
                let qualified_id = self.parse_qualified_type_identifier(&pair)?;

                // Check for generic parameters
                let mut remaining_pairs = pair.into_inner();
                while let Some(next_pair) = remaining_pairs.next() {
                    if next_pair.as_rule() == Rule::generic_params {
                        let mut generic_types = Vec::new();
                        for param in Self::convert_into_iterator(&next_pair) {
                            generic_types.push(self.parse_type(param)?);
                        }
                        return Ok(Type::Generic(
                            Box::new(Type::TypeReference(qualified_id)),
                            generic_types,
                        ));
                    }
                }
                Ok(Type::TypeReference(qualified_id))
            }
            Rule::tuple_type => {
                let mut types = Vec::new();
                for type_pair in pair.into_inner() {
                    let type_value = self.parse_type(type_pair)?;
                    types.push(type_value);
                }
                Ok(Type::Tuple(types))
            }
            Rule::map_type => {
                let mut inner = pair.into_inner();
                let key_type = self.parse_type(Self::next_pair(&mut inner)?)?;
                let value_type = self.parse_type(Self::next_pair(&mut inner)?)?;
                Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
            }

            Rule::array_type => {
                let inner = self.next_inner_pair(&pair)?;
                let element_type = self.parse_type(inner)?;
                Ok(Type::Array(Box::new(element_type)))
            }

            _ => Err(self.create_error_pair(SpecificError::UnexpectedTypeRule, &pair)),
        }
    }

    fn parse_type_from_str<'a>(
        &self,
        mut iterator: &mut impl Iterator<Item = Pair<'a, Rule>>,
        pair: &Pair<Rule>,
    ) -> Result<Type, ParseError> {
        let node = self.to_node(pair);
        match pair.as_str() {
            "Int" => Ok(Type::Int(node)),
            "Float" => Ok(Type::Float(node)),
            "String" => Ok(Type::String(node)),
            "Bool" => Ok(Type::Bool(node)),
            _ => Ok(Type::TypeReference(
                self.expect_qualified_type_identifier_next(&mut iterator)?,
            )),
        }
    }

    #[allow(unused)]
    fn parse_local_type_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<LocalTypeIdentifier, ParseError> {
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(format!("{:?}", pair.as_rule())),
                pair,
            ));
        }
        Ok(LocalTypeIdentifier::new(self.to_node(&pair)))
    }

    fn parse_local_type_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalTypeIdentifier, ParseError> {
        let pair = Self::next_pair(pairs)?;
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error_pair(
                SpecificError::ExpectedLocalTypeIdentifier(Self::pair_to_rule(&pair)),
                &pair,
            ));
        }
        Ok(LocalTypeIdentifier::new(self.to_node(&pair)))
    }

    fn parse_enum_def(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);

        // Parse enum name
        let name = self.parse_local_type_identifier_next(&mut inner)?;
        let mut variants = Vec::new();

        // Parse enum variants if present
        if let Some(variants_pair) = inner.next() {
            if variants_pair.as_rule() == Rule::enum_variants {
                for variant_pair in Self::convert_into_iterator(&variants_pair) {
                    if variant_pair.as_rule() == Rule::enum_variant {
                        let variant =
                            self.parse_enum_variant(&self.next_inner_pair(&variant_pair)?)?;

                        variants.push(variant);
                    }
                }
            }
        }

        Ok(Definition::EnumDef(name.0, variants))
    }

    fn parse_enum_variant(&self, pair: &Pair<Rule>) -> Result<EnumVariantType, ParseError> {
        let enum_variant = match pair.as_rule() {
            Rule::simple_variant => EnumVariantType::Simple(self.to_node(&pair)),
            Rule::tuple_variant => {
                let mut inner = Self::convert_into_iterator(&pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let mut types = Vec::new();
                while let Some(type_pair) = inner.next() {
                    types.push(self.parse_type(type_pair)?);
                }

                EnumVariantType::Tuple(name.0, types)
            }
            Rule::struct_variant => {
                let mut inner = Self::convert_into_iterator(&pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let mut fields = Vec::new();

                while let Some(field_pair) = inner.next() {
                    let mut field_inner = Self::convert_into_iterator(&field_pair);
                    let field_name = self.expect_field_name_next(&mut field_inner)?;
                    let field_type = self.parse_type(Self::next_pair(&mut field_inner)?)?;
                    let anonym_field = FieldType {
                        field_name,
                        field_type,
                    };
                    fields.push(anonym_field);
                }

                let anon = AnonymousStructType { fields };
                EnumVariantType::Struct(name.0, anon)
            }
            _ => {
                return Err(self.create_error_pair(
                    SpecificError::UnknownEnumVariant(Self::pair_to_rule(pair)),
                    pair,
                ))
            }
        };

        Ok(enum_variant)
    }

    fn parse_match_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(&pair);
        let value = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let arms_pair = Self::next_pair(&mut inner)?;
        let mut arms = Vec::new();

        for arm_pair in Self::convert_into_iterator(&arms_pair) {
            if arm_pair.as_rule() == Rule::match_arm {
                let mut arm_inner = Self::convert_into_iterator(&arm_pair);
                let pattern = self.parse_match_pattern(&Self::next_pair(&mut arm_inner)?)?;

                // Handle both block and direct expression cases
                let expr = match Self::next_pair(&mut arm_inner)? {
                    block if block.as_rule() == Rule::block => {
                        let (expression, _constants) = self.parse_block(&block)?; // TODO: Handle constants in arm-expressions
                        expression
                    }
                    expr => self.parse_expression(&expr)?,
                };

                arms.push(MatchArm {
                    pattern,
                    expression: expr,
                });
            }
        }

        if arms.is_empty() {
            return Err(self.create_error_pair(SpecificError::MustHaveAtLeastOneArm, &pair));
        }

        Ok(Expression::Match(Box::new(value), arms))
    }

    fn parse_match_pattern(&self, pair: &Pair<Rule>) -> Result<Pattern, ParseError> {
        match pair.as_rule() {
            Rule::match_pattern => {
                // Directly pass inner to parse recursively
                self.parse_match_pattern(&self.next_inner_pair(pair)?)
            }
            Rule::pattern_list => {
                let elements = self.parse_pattern_list(pair)?;
                Ok(Pattern::PatternList(elements))
            }
            Rule::enum_pattern => {
                let mut inner = Self::convert_into_iterator(pair);
                let variant = self.expect_local_type_identifier_next(&mut inner)?;

                // Parse pattern list if present
                let elements = inner
                    .next()
                    .map(|p| self.parse_pattern_list(&p))
                    .transpose()?;

                Ok(Pattern::EnumPattern(variant.0, elements))
            }
            Rule::literal => Ok(Pattern::Literal(self.parse_literal(pair)?)),
            Rule::wildcard_pattern => Ok(Pattern::PatternList(vec![PatternElement::Wildcard(
                self.to_node(pair),
            )])),
            _ => Err(self.create_error_pair(SpecificError::UnknownMatchType, pair)),
        }
    }

    fn parse_pattern_list(&self, pair: &Pair<Rule>) -> Result<Vec<PatternElement>, ParseError> {
        let mut elements = Vec::new();
        for item in Self::convert_into_iterator(pair) {
            match item.as_rule() {
                Rule::pattern_field => {
                    if item.as_str() == "_" {
                        elements.push(PatternElement::Wildcard(self.to_node(&item)));
                    } else {
                        elements.push(PatternElement::Variable(self.to_node(&item)));
                    }
                }
                Rule::expression => {
                    elements.push(PatternElement::Expression(self.parse_expression(&item)?));
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedPatternListElement(Self::pair_to_rule(&item)),
                        &item,
                    ));
                }
            }
        }
        Ok(elements)
    }

    fn to_node(&self, pair: &Pair<Rule>) -> Node {
        let pair_span = pair.as_span();
        let span = SpanWithoutFileId {
            offset: pair_span.start() as u32,
            length: (pair_span.end() - pair_span.start()) as u16,
        };

        Node { span }
    }

    fn to_span(&self, pest_span: pest::Span) -> SpanWithoutFileId {
        SpanWithoutFileId {
            offset: pest_span.start() as u32,
            length: (pest_span.end() - pest_span.start()) as u16,
        }
    }

    fn convert_to_location_expression(
        &self,
        expr: Expression,
        next: &Pair<Rule>,
    ) -> Result<LocationExpression, ParseError> {
        let location = match expr {
            Expression::FieldOrMemberAccess(expression, node) => {
                LocationExpression::FieldAccess(expression, node)
            }
            Expression::VariableAccess(variable) => LocationExpression::Variable(variable),
            Expression::IndexAccess(a, b) => LocationExpression::IndexAccess(a, b),
            _ => {
                return Err(self.create_error_pair(SpecificError::ExpectedLocationExpression, next))
            }
        };

        Ok(location)
    }
}
