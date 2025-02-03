/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use pest::error::{Error, ErrorVariant, InputLocation};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::iter::Peekable;
use std::str::Chars;
use swamp_script_ast::{
    prelude::*, AssignmentOperatorKind, BinaryOperatorKind, CompoundOperator, CompoundOperatorKind,
    EnumVariantLiteral, ExpressionKind, FieldExpression, FieldName, FieldType, ForPattern, ForVar,
    IterableExpression, PatternElement, QualifiedIdentifier, SpanWithoutFileId, TypeForParameter,
    VariableBinding,
};
use swamp_script_ast::{Function, WhenBinding};
use swamp_script_ast::{LiteralKind, MutableOrImmutableExpression};
use swamp_script_ast::{Postfix, PostfixChain};

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
    CouldNotMoveDown,
    CouldNotMoveRight,
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
    UnknownEscapeCharacter(char),
    UnfinishedEscapeSequence,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidUtf8Sequence,
    MissingTypeName,
    UnknownTerm(String),
    UnknownExpr(String),
    UnexpectedTokenInMutableExpression,
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

    fn expect_field_label_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<FieldName, ParseError> {
        let field_label_pair = Self::expect_next(pairs, Rule::field_label)?;
        let mut inner = field_label_pair.clone().into_inner();
        let ident_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(SpecificError::ExpectedIdentifier, &field_label_pair)
        })?;

        Ok(FieldName(self.to_node(&ident_pair)))
    }

    fn parse_dot_identifier<'a>(&self, pair: &Pair<Rule>) -> Result<FieldName, ParseError> {
        assert_eq!(pair.as_rule(), Rule::dot_identifier);
        let mut inner = pair.clone().into_inner();
        let ident_pair = inner
            .next()
            .ok_or_else(|| self.create_error_pair(SpecificError::ExpectedIdentifier, &pair))?;

        Ok(FieldName(self.to_node(&ident_pair)))
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
        let first = Self::next_pair(inner_pairs)?;
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

    fn to_err(kind: SpecificError, pair: &Pair<Rule>) -> ParseError {
        ParseError {
            span: Self::span(pair.as_span()),
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

    pub fn parse(rule: Rule, raw_script: &str) -> Result<ParseResult<'static>, ParseError> {
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
            _ => Some(Expression {
                kind: ExpressionKind::Block(expressions),
                node: Node {
                    span: SpanWithoutFileId::default(),
                },
            }),
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
            Rule::import_def => self.parse_use(&inner_pair),
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
        for pair in import_path.into_inner() {
            segments.push(self.to_node(&pair));
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
        }))
    }

    fn pair_to_rule(rule: &Pair<Rule>) -> String {
        format!("{:?}", rule.as_rule())
    }

    fn parse_block(&self, block_pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        if block_pair.as_rule() != Rule::block {
            return Err(self.create_error_pair(SpecificError::ExpectedBlock, block_pair));
        }

        let mut expressions = Vec::new();

        for pair in Self::convert_into_iterator(block_pair) {
            if pair.as_rule() != Rule::expression {
                return Err(self.create_error_pair(
                    SpecificError::UnexpectedRuleInParseScript(format!(
                        "Expected expression_in_block, got: {:?}",
                        pair.as_rule()
                    )),
                    block_pair,
                ));
            }

            match pair.as_rule() {
                Rule::expression => {
                    let expr = self.parse_expression(&pair)?;
                    expressions.push(expr);
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript(format!(
                            "Unexpected rule in parse_block: {:?}",
                            pair.as_rule()
                        )),
                        &pair,
                    ))
                }
            }
        }

        let block_expr = self.create_expr(ExpressionKind::Block(expressions), block_pair);
        Ok(block_expr)
    }

    fn parse_with_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let binding_list_pair = inner.next().expect("variable list missing");
        let binding_list = self.parse_variable_binding_list(&binding_list_pair)?;

        let expr_pair = inner.next().expect("block missing");
        let expr = self.parse_expression(&expr_pair)?;

        let with_expr = self.create_expr(ExpressionKind::With(binding_list, Box::from(expr)), pair);
        Ok(with_expr)
    }

    fn parse_when_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let binding_list =
            self.parse_when_variable_binding_list(&inner.next().expect("variable list missing"))?;
        let expr = self.parse_expression(&inner.next().expect("block missing"))?;

        let next = inner.next();
        let else_expr = if let Some(found_else) = next {
            Some(Box::new(self.parse_expression(&found_else)?))
        } else {
            None
        };

        Ok(self.create_expr(
            ExpressionKind::When(binding_list, Box::from(expr), else_expr),
            pair,
        ))
    }

    fn parse_variable_binding_list(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<VariableBinding>, ParseError> {
        let inner = Self::convert_into_iterator(pair);
        let mut bindings = Vec::new();

        // Each item in inner will be a variable_binding
        for binding_pair in inner {
            if binding_pair.as_rule() == Rule::variable_binding {
                bindings.push(self.parse_variable_binding(&binding_pair)?);
            }
        }

        Ok(bindings)
    }

    fn parse_variable_binding(&self, pair: &Pair<Rule>) -> Result<VariableBinding, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let variable = self.parse_variable_item(&inner.next().expect("variable missing"))?;

        let expression = if let Some(expr_pair) = inner.next() {
            self.parse_mutable_or_immutable_expression(&expr_pair)?
        } else {
            MutableOrImmutableExpression {
                expression: self
                    .create_expr(ExpressionKind::IdentifierReference(variable.clone()), pair),
                is_mutable: None,
            }
        };

        Ok(VariableBinding {
            variable,
            expression,
        })
    }

    fn parse_when_variable_binding(&self, pair: &Pair<Rule>) -> Result<WhenBinding, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let variable = self.parse_variable_item(&inner.next().expect("variable missing"))?;

        let expression = if let Some(expr_pair) = inner.next() {
            Some(self.parse_mutable_or_immutable_expression(&expr_pair)?)
        } else {
            None
        };

        Ok(WhenBinding {
            variable,
            expression,
        })
    }

    fn parse_when_variable_binding_list(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<WhenBinding>, ParseError> {
        let inner = Self::convert_into_iterator(pair);
        let mut bindings = Vec::new();

        // Each item in inner will be a variable_binding
        for binding_pair in inner {
            if binding_pair.as_rule() == Rule::variable_when_binding {
                bindings.push(self.parse_when_variable_binding(&binding_pair)?);
            }
        }

        Ok(bindings)
    }

    fn parse_if_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let then_branch = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let else_branch = inner
            .next()
            .map(|p| {
                match p.as_rule() {
                    Rule::if_expr => self.parse_if_expression(&p), // Recursively handle `else if`
                    _ => self.parse_expression(&p),                // Inline or block `else`
                }
            })
            .transpose()?;

        Ok(self.create_expr(
            ExpressionKind::If(
                Box::new(condition),
                Box::new(then_branch),
                else_branch.map(Box::new),
            ),
            pair,
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn parse_postfix_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        assert_eq!(pair.as_rule(), Rule::postfix);
        let mut inner = pair.clone().into_inner();

        let primary_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(SpecificError::UnexpectedPostfixOperator, pair)
        })?;
        let start_expr = self.parse_term(&primary_pair)?;
        //info!(?start_expr, "start");
        let mut postfixes = Vec::new();
        if inner.len() == 0 {
            return Ok(start_expr);
        }

        for op_pair in inner.clone() {
            //info!(rule=?op_pair.as_rule(), "..continuing chain");
            match op_pair.as_rule() {
                Rule::postfix_op => {
                    let mut sub_inner = op_pair.clone().into_inner();
                    let child = sub_inner.next().ok_or_else(|| {
                        self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &op_pair)
                    })?;

                    match child.as_rule() {
                        Rule::unwrap_postfix => {
                            postfixes.push(Postfix::OptionUnwrap(self.to_node(&op_pair)));
                        }

                        Rule::none_coalesce_postfix => {
                            let mut postfix_inner = Self::convert_into_iterator(&child);
                            let expr_pair = postfix_inner.next().expect("must have following");
                            let default_expression = self.parse_expression(&expr_pair)?;
                            postfixes.push(Postfix::NoneCoalesce(default_expression));
                        }

                        Rule::function_call_postfix => {
                            let args = self.parse_function_call_postfix(&child)?;
                            let node = self.to_node(&op_pair);
                            postfixes.push(Postfix::FunctionCall(node, args));
                        }

                        Rule::member_call_postfix => {
                            let mut inner = child.into_inner();

                            let member_access = Self::next_pair(&mut inner)?;
                            assert_eq!(member_access.as_rule(), Rule::member_access_postfix);
                            let mut ma_inner = member_access.into_inner();
                            let dot_id = Self::next_pair(&mut ma_inner)?;
                            let member_identifier = self.parse_dot_identifier(&dot_id)?;

                            let args_pair = Self::next_pair(&mut inner)?;
                            let args = self.parse_function_call_arguments(&args_pair)?;

                            postfixes.push(Postfix::MemberCall(member_identifier.0, args));
                        }

                        Rule::member_access_postfix => {
                            let mut inner = child.into_inner();
                            let dot_id = Self::next_pair(&mut inner)?;
                            let identifier = self.parse_dot_identifier(&dot_id)?;
                            postfixes.push(Postfix::FieldAccess(identifier.0));
                        }

                        Rule::subscript_postfix => {
                            let mut arr_inner = child.clone().into_inner();
                            let index_pair = arr_inner.next().ok_or_else(|| {
                                self.create_error_pair(
                                    SpecificError::UnexpectedPostfixOperator,
                                    &child,
                                )
                            })?;
                            let index_expr = self.parse_expression(&index_pair)?;
                            postfixes.push(Postfix::Subscript(index_expr));
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

        Ok(self.create_expr(
            ExpressionKind::PostfixChain(PostfixChain {
                base: Box::from(start_expr),
                postfixes: postfixes,
            }),
            pair,
        ))
    }

    fn parse_struct_def(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let struct_name = self.expect_local_type_identifier_next(&mut inner)?;

        let field_definitions_pair_result = Self::next_pair(&mut inner);
        let mut fields = Vec::new();

        if let Ok(field_definitions) = field_definitions_pair_result {
            for field_def in Self::convert_into_iterator(&field_definitions) {
                let mut field_parts = Self::convert_into_iterator(&field_def);

                let field_name = self.expect_field_label_next(&mut field_parts)?;
                let field_type = self.parse_type(Self::next_pair(&mut field_parts)?)?;

                let anonymous_struct_field = FieldType {
                    field_name,
                    field_type,
                };

                fields.push(anonymous_struct_field);
            }
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

                let body = self.parse_block(&inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingFunctionBody, &function_pair)
                })?)?;

                Ok(Definition::FunctionDef(Function::Internal(
                    FunctionWithBody {
                        declaration: signature,
                        body,
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
            return Err(self.create_error_pair(SpecificError::MissingFunctionSignature, pair));
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
                    let mut iterator = Self::convert_into_iterator(&param_pair);
                    let may_mut_pair = iterator.next().unwrap();
                    let var = self.parse_maybe_mut_identifier(&may_mut_pair)?;
                    let type_pair = iterator.next().unwrap();
                    let param_type = self.parse_type(type_pair.clone())?;

                    parameters.push(Parameter {
                        variable: var,
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
        let mut inner = Self::convert_into_iterator(pair);
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
            return Err(self.create_error_pair(SpecificError::ExpectedMemberSignature, pair));
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
        let body = self.parse_block(&block_pair)?;

        Ok(FunctionWithBody {
            declaration: signature,
            body,
        })
    }

    fn parse_for_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let pattern_pair = Self::next_pair(&mut inner)?;
        if pattern_pair.as_rule() != Rule::for_pattern {
            return Err(self.create_error_pair(SpecificError::ExpectedForPattern, &pattern_pair));
        }

        let inner_pattern = self.next_inner_pair(&pattern_pair)?;
        let pattern = match inner_pattern.as_rule() {
            Rule::maybe_mut_identifier => {
                let mut inner_iter = inner_pattern.clone().into_inner();
                let is_mutable = inner_iter
                    .peek()
                    .map_or(false, |p| p.as_rule() == Rule::mut_keyword);

                let is_mut = if is_mutable {
                    let mut_node = self.to_node(&inner_iter.next().unwrap());
                    Some(mut_node)
                } else {
                    None
                };

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

        let next_pair = Self::next_pair(&mut inner)?;
        let iterable_expression = self.parse_mutable_or_immutable_expression(&next_pair)?;

        let mut_expression = IterableExpression {
            expression: Box::new(iterable_expression),
        };

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        // Return the ForLoop statement with MutExpression
        Ok(self.create_expr(
            ExpressionKind::ForLoop(pattern, mut_expression, Box::from(body)),
            pair,
        ))
    }

    fn parse_while_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        Ok(self.create_expr(
            ExpressionKind::WhileLoop(Box::from(condition), Box::from(body)),
            pair,
        ))
    }

    fn parse_return(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let expr = match inner.next() {
            Some(expr_pair) => Some(Box::new(self.parse_expression(&expr_pair)?)),
            None => None,
        };

        Ok(self.create_expr(ExpressionKind::Return(expr), pair))
    }

    fn parse_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let sub = &Self::right_alternative(pair)?;
        match sub.as_rule() {
            Rule::expression => {
                let inner = self.next_inner_pair(sub)?;

                self.parse_expression(&inner)
            }

            Rule::identifier_reference => Ok(self.create_expr(
                ExpressionKind::IdentifierReference(Variable::new(self.to_node(sub), None)),
                sub,
            )),
            Rule::block => self.parse_block(sub),

            Rule::assignment => self.parse_assignment_expression(sub),
            Rule::destructuring_assignment => self.parse_destructuring_assignment(sub),
            Rule::variable_definition => self.parse_variable_definition(sub),

            Rule::addition => self.parse_addition(sub),
            Rule::range => self.parse_range(sub),
            Rule::logical => self.parse_logical(sub),
            Rule::comparison => self.parse_comparison(sub),
            Rule::multiplication => self.parse_multiplication(sub),

            Rule::prefix => self.parse_prefix(sub),

            Rule::match_expr => self.parse_match_expr(sub),
            Rule::map_literal => self.parse_map_literal(sub),
            Rule::array_literal => self.parse_array_literal(sub),
            Rule::guard_expr => self.parse_guard_expr_list(sub),
            Rule::return_expr => self.parse_return(sub),
            Rule::break_expr => Ok(self.create_expr(ExpressionKind::Break, sub)),
            Rule::continue_expr => Ok(self.create_expr(ExpressionKind::Continue, sub)),
            Rule::with_expr => self.parse_with_expr(sub),
            Rule::when_expr => self.parse_when_expr(sub),
            Rule::if_expr => self.parse_if_expression(sub),
            Rule::for_loop => self.parse_for_loop(sub),
            Rule::while_loop => self.parse_while_loop(sub),

            //            Rule::expression | Rule::literal => self.parse_expr(pair),
            Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                let op = self.parse_unary_operator(sub)?;
                let expr = self.parse_postfix_expression(&self.next_inner_pair(sub)?)?;
                Ok(self.create_expr(ExpressionKind::UnaryOp(op, Box::new(expr)), sub))
            }

            //Rule::mut_expression => self.parse_mutable_or_immutable_expression(pair),
            Rule::postfix => self.parse_postfix_expression(sub),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedExpressionType(Self::pair_to_rule(sub)),
                sub,
            )),
        }
    }

    fn parse_variable_list(&self, pair: &Pair<Rule>) -> Result<Vec<Variable>, ParseError> {
        let mut variables = Vec::new();
        for item_pair in pair.clone().into_inner() {
            if item_pair.as_rule() == Rule::variable_item {
                variables.push(self.parse_variable_item(&item_pair)?);
            }
        }
        Ok(variables)
    }

    fn parse_maybe_mut_identifier(&self, pair: &Pair<Rule>) -> Result<Variable, ParseError> {
        assert_eq!(pair.as_rule(), Rule::maybe_mut_identifier);
        let mut inner = pair.clone().into_inner();
        let mut_node = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::mut_keyword {
                // Convert 'mut' to a Node
                let node = self.to_node(&peeked);
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
                pair,
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

        let variable = Variable {
            name: self.to_node(&name_pair),
            is_mutable: mut_node,
        };

        Ok(variable)
    }

    fn parse_variable_item(&self, pair: &Pair<Rule>) -> Result<Variable, ParseError> {
        assert_eq!(pair.as_rule(), Rule::variable_item);
        let mut inner = pair.clone().into_inner();
        self.parse_maybe_mut_identifier(&inner.next().unwrap())
    }

    fn parse_assignment_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut iterator = pair.clone().into_inner();
        let lhs_logical =
            self.parse_logical(&iterator.next().expect("parse_assignment_expression"))?;
        if let Some(assignment_op_pair) = iterator.peek().clone() {
            iterator.next();
            let assignment_op = self.parse_assignment_op(&assignment_op_pair)?;
            let rhs_expr = self.parse_expression(&iterator.next().unwrap())?;
            let kind = match assignment_op {
                AssignmentOperatorKind::Assign => {
                    ExpressionKind::Assignment(Box::new(lhs_logical), Box::from(rhs_expr))
                }
                AssignmentOperatorKind::Compound(compound) => {
                    let op = CompoundOperator {
                        node: Self::node_ex(&assignment_op_pair),
                        kind: compound,
                    };
                    ExpressionKind::CompoundAssignment(
                        Box::from(lhs_logical),
                        op,
                        Box::from(rhs_expr),
                    )
                }
            };

            Ok(self.create_expr(kind, &pair))
        } else {
            Ok(lhs_logical)
        }
    }

    fn parse_assignment_op(&self, pair: &Pair<Rule>) -> Result<AssignmentOperatorKind, ParseError> {
        assert!(pair.as_rule() == Rule::assign_op);
        let sub = Self::right_alternative(&pair)?;
        let op = match sub.as_rule() {
            Rule::compound_assign_op => {
                AssignmentOperatorKind::Compound(Self::parse_compound_assign_op(&sub)?)
            }
            Rule::normal_assign_op => AssignmentOperatorKind::Assign,
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownAssignmentOperator("strange".to_string()),
                    &sub,
                ))
            }
        };

        Ok(op)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_destructuring_assignment(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        assert_eq!(pair.as_rule(), Rule::destructuring_assignment);
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

        Ok(self.create_expr(
            ExpressionKind::DestructuringAssignment(variables, Box::new(rhs_expr)),
            &rhs_pair,
        ))
    }

    fn right_alternative<'a>(pair: &Pair<'a, Rule>) -> Result<Pair<'a, Rule>, ParseError> {
        pair.clone()
            .into_inner()
            .next()
            .ok_or_else(|| Self::to_err(SpecificError::CouldNotMoveRight, &pair))
    }

    pub fn parse_compound_assign_op(
        op_pair: &Pair<Rule>,
    ) -> Result<CompoundOperatorKind, ParseError> {
        assert_eq!(op_pair.as_rule(), Rule::compound_assign_op);

        let kind = match Self::right_alternative(&op_pair)?.as_rule() {
            Rule::add_assign_op => CompoundOperatorKind::Add,
            Rule::sub_assign_op => CompoundOperatorKind::Sub,
            Rule::mul_assign_op => CompoundOperatorKind::Mul,
            Rule::div_assign_op => CompoundOperatorKind::Div,
            Rule::modulo_assign_op => CompoundOperatorKind::Modulo,
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownOperator(format!(
                        "Found unexpected operator rule: {:?}",
                        op_pair.as_rule()
                    )),
                    &op_pair,
                ));
            }
        };

        Ok(kind)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_variable_definition(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let variable_item = Self::next_pair(&mut inner)?;
        let found_var = self.parse_variable_item(&variable_item)?;

        let type_coercion = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::type_coerce {
                let type_coerce_pair = inner.next().unwrap();
                let mut type_inner = type_coerce_pair.clone().into_inner();
                let type_name_pair = type_inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingTypeName, &type_coerce_pair)
                })?;
                Some(self.parse_type(type_name_pair)?)
            } else {
                None
            }
        } else {
            None
        };

        let rhs_expr = self.parse_mutable_or_immutable_expression(&Self::next_pair(&mut inner)?)?;

        if type_coercion.is_some() || found_var.is_mutable.is_some() {
            Ok(self.create_expr(
                ExpressionKind::VariableDefinition(found_var, type_coercion, Box::from(rhs_expr)),
                pair,
            ))
        } else {
            Ok(self.create_expr(
                ExpressionKind::VariableAssignment(found_var, Box::from(rhs_expr)),
                pair,
            ))
        }
    }
    fn parse_prefix(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        assert_eq!(pair.as_rule(), Rule::prefix);
        let _span = pair.as_span();
        let inner = Self::convert_into_iterator(pair);
        let mut expr = None;
        let mut prefix_ops = Vec::new();

        for part in inner {
            match part.as_rule() {
                Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                    let op = self.parse_unary_operator(&part)?;
                    prefix_ops.push(op);
                }
                _ => {
                    expr = Some(self.parse_postfix_expression(&part)?);
                    break;
                }
            }
        }

        let mut final_expr = expr.ok_or_else(|| {
            self.create_error_pair(SpecificError::ExpectedExpressionAfterPrefixOperator, pair)
        })?;

        for op in prefix_ops.into_iter().rev() {
            final_expr = self.create_expr(ExpressionKind::UnaryOp(op, Box::new(final_expr)), pair);
        }

        Ok(final_expr)
    }

    fn parse_binary_operator(&self, pair: &Pair<Rule>) -> Result<BinaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::prefix_op => self.next_inner_pair(pair)?,
            _ => pair.clone(),
        };

        let kind = match op.as_rule() {
            Rule::op_add => BinaryOperatorKind::Add,
            Rule::op_sub => BinaryOperatorKind::Subtract,
            Rule::op_mul => BinaryOperatorKind::Multiply,
            Rule::op_div => BinaryOperatorKind::Divide,
            Rule::op_mod => BinaryOperatorKind::Modulo,
            Rule::op_eq => BinaryOperatorKind::Equal,
            Rule::op_neq => BinaryOperatorKind::NotEqual,
            Rule::op_lt => BinaryOperatorKind::LessThan,
            Rule::op_lte => BinaryOperatorKind::LessEqual,
            Rule::op_gt => BinaryOperatorKind::GreaterThan,
            Rule::op_gte => BinaryOperatorKind::GreaterEqual,
            Rule::op_and => BinaryOperatorKind::LogicalAnd,
            Rule::op_or => BinaryOperatorKind::LogicalOr,
            _ => {
                panic!("unknown operator")
            }
        };

        Ok(BinaryOperator {
            kind,
            node: self.to_node(pair),
        })
    }

    fn parse_unary_operator(&self, pair: &Pair<Rule>) -> Result<UnaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::prefix_op => &self.next_inner_pair(pair)?,
            _ => pair,
        };

        let node = self.to_node(op);
        match op.as_rule() {
            Rule::op_neg => Ok(UnaryOperator::Negate(node)),
            Rule::op_not => Ok(UnaryOperator::Not(node)),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedUnaryOperator(Self::pair_to_rule(op)),
                op,
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
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(pair)),
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

    #[allow(unused)] // TODO: Use this again
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

    fn parse_struct_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let type_pair = inner.next().unwrap();

        let struct_name = self.parse_qualified_type_identifier(&type_pair)?;

        let mut fields = Vec::new();
        let mut has_rest = false;

        if let Some(field_list) = inner.next() {
            for field_pair in field_list.into_inner() {
                match field_pair.as_rule() {
                    Rule::struct_field => {
                        let mut field_inner = field_pair.into_inner();
                        let ident = self.expect_field_label_next(&mut field_inner)?;
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

        Ok(self.create_expr(
            ExpressionKind::StructLiteral(struct_name, fields, has_rest),
            pair,
        ))
    }

    fn parse_static_member_reference(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();

        let type_identifier = self.parse_qualified_type_identifier(&inner.next().unwrap())?;
        let member_name = self.expect_identifier_next(&mut inner)?;

        Ok(self.create_expr(
            ExpressionKind::StaticMemberFunctionReference(type_identifier, member_name.0),
            pair,
        ))
    }

    fn parse_term(&self, pair2: &Pair<Rule>) -> Result<Expression, ParseError> {
        assert_eq!(pair2.as_rule(), Rule::term);
        let sub = &Self::right_alternative(pair2)?;
        match sub.as_rule() {
            Rule::static_member_reference => self.parse_static_member_reference(sub),
            Rule::enum_literal => {
                Ok(self.create_expr(ExpressionKind::Literal(self.parse_enum_literal(sub)?), sub))
            }
            Rule::identifier_reference => Ok(self.create_expr(
                ExpressionKind::IdentifierReference(Variable::new(self.to_node(sub), None)),
                sub,
            )),
            Rule::constant_reference => Ok(self.create_expr(
                ExpressionKind::ConstantReference(ConstantIdentifier(self.to_node(sub))),
                sub,
            )),
            Rule::parenthesized => {
                let inner = self.next_inner_pair(sub)?;
                self.parse_expression(&inner)
            }
            Rule::basic_literal => {
                let (literal, node) = self.parse_basic_literal(sub)?;
                Ok(self.create_expr_span(ExpressionKind::Literal(literal), node))
            }
            Rule::struct_literal => self.parse_struct_literal(sub),

            Rule::interpolated_string => self.parse_interpolated_string(sub),
            /*
            Rule::float_lit => {
                Ok(self.create_expr(ExpressionKind::Literal(LiteralKind::Float), pair))
            }

             */
            _ => {
                Err(self
                    .create_error_pair(SpecificError::UnknownTerm(Self::pair_to_rule(sub)), sub))
            }
        }
    }

    fn parse_interpolated_string(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut parts = Vec::new();

        for part_pair in Self::convert_into_iterator(pair) {
            match part_pair.as_rule() {
                Rule::text => {
                    parts.push(StringPart::Literal(
                        self.to_node(&part_pair),
                        self.unescape_string(&part_pair, false)?,
                    ));
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
                            Some(self.parse_format_specifier(&fmt)?)
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

        Ok(self.create_expr(ExpressionKind::InterpolatedString(parts), pair))
    }

    fn parse_format_specifier(&self, pair: &Pair<Rule>) -> Result<FormatSpecifier, ParseError> {
        let node = self.to_node(pair);
        match pair.as_str() {
            "x" => Ok(FormatSpecifier::LowerHex(node)),
            "X" => Ok(FormatSpecifier::UpperHex(node)),
            "b" => Ok(FormatSpecifier::Binary(node)),
            "f" => Ok(FormatSpecifier::Float(node)),
            s if s.starts_with("..") => {
                let precision: u32 = s[2..s.len() - 1].parse().map_err(|_| {
                    self.create_error_pair(SpecificError::InvalidPrecisionValue, pair)
                })?;
                let typ = match s.chars().last().unwrap() {
                    'f' => PrecisionType::Float(node),
                    's' => PrecisionType::String(node),
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidPrecisionType, pair)
                        )?
                    }
                };
                Ok(FormatSpecifier::Precision(
                    precision,
                    self.to_node(&pair),
                    typ,
                ))
            }
            _ => Err(self.create_error_pair(SpecificError::InvalidFormatSpecifier, pair)),
        }
    }

    fn parse_enum_literal(&self, pair: &Pair<Rule>) -> Result<LiteralKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

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
                            let field_name = self.expect_field_label_next(&mut field_inner)?;
                            let field_expression =
                                self.parse_expression(&Self::next_pair(&mut field_inner)?)?;
                            let anonymous_struct_field = FieldExpression {
                                field_name,
                                expression: field_expression,
                            };
                            fields.push(anonymous_struct_field);
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

        Ok(LiteralKind::EnumVariant(enum_variant_literal))
    }

    fn unescape_unicode(
        &self,
        chars: &mut Peekable<Chars>,
        octets: &mut Vec<u8>,
        pair: &Pair<Rule>,
    ) -> Result<(), ParseError> {
        match chars.next() {
            Some('(') => {
                let mut hex_digits = String::new();

                while let Some(&c) = chars.peek() {
                    if c == ')' {
                        break;
                    }
                    if c.is_ascii_hexdigit() && hex_digits.len() < 6 {
                        hex_digits.push(c);
                        chars.next();
                    } else {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        );
                    }
                }

                match chars.next() {
                    Some(')') => {
                        if hex_digits.is_empty() {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }

                        let code = u32::from_str_radix(&hex_digits, 16).map_err(|_| {
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        })?;

                        if code > 0x0010_FFFF {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }

                        if let Some(c) = std::char::from_u32(code) {
                            let mut buf = [0; 4];
                            let encoded = c.encode_utf8(&mut buf);
                            octets.extend_from_slice(encoded.as_bytes());
                        } else {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        );
                    }
                }
            }
            _ => {
                return Err(self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair));
            }
        }
        Ok(())
    }

    fn unescape_hex(
        &self,
        chars: &mut Peekable<Chars>,
        pair: &Pair<Rule>,
    ) -> Result<u8, ParseError> {
        let mut hex_digits = String::new();
        for _ in 0..2 {
            match chars.next() {
                Some(h) if h.is_ascii_hexdigit() => {
                    hex_digits.push(h);
                }
                _ => {
                    return Err(self.create_error_pair(SpecificError::InvalidHexEscape, pair));
                }
            }
        }
        u8::from_str_radix(&hex_digits, 16)
            .map_err(|_| self.create_error_pair(SpecificError::InvalidHexEscape, pair))
    }

    fn unescape_string(&self, pair: &Pair<Rule>, is_literal: bool) -> Result<String, ParseError> {
        let mut octets = Vec::new();

        let raw = if is_literal {
            &pair.as_str()[1..pair.as_str().len() - 1]
        } else {
            pair.as_str()
        };

        let mut chars = raw.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                let Some(next_ch) = chars.next() else {
                    return Err(
                        self.create_error_pair(SpecificError::UnfinishedEscapeSequence, pair)
                    );
                };
                match next_ch {
                    'n' => {
                        octets.push(b'\n');
                    }
                    't' => {
                        octets.push(b'\t');
                    }
                    '\\' => {
                        octets.push(b'\\');
                    }
                    '"' => {
                        octets.push(b'"');
                    }
                    '\'' => {
                        octets.push(b'\'');
                    }
                    // Two hexadecimal digits that result in an `u8`
                    'x' => {
                        let code = self.unescape_hex(&mut chars, pair)?;
                        octets.push(code);
                    }
                    // Unicode character
                    'u' => {
                        self.unescape_unicode(&mut chars, &mut octets, pair)?;
                    }

                    other => {
                        return Err(self.create_error_pair(
                            SpecificError::UnknownEscapeCharacter(other),
                            pair,
                        ));
                    }
                }
            } else {
                let mut buf = [0; 4];
                let utf8_bytes = ch.encode_utf8(&mut buf);
                octets.extend_from_slice(utf8_bytes.as_bytes());
            }
        }

        let output = String::from_utf8(octets)
            .map_err(|_| self.create_error_pair(SpecificError::InvalidUtf8Sequence, pair))?;

        Ok(output)
    }

    fn parse_basic_literal(&self, pair: &Pair<Rule>) -> Result<(LiteralKind, Node), ParseError> {
        assert_eq!(pair.as_rule(), Rule::basic_literal);
        let inner = self.next_inner_pair(pair)?;
        let literal_kind = match inner.as_rule() {
            Rule::int_lit => LiteralKind::Int,
            Rule::float_lit => LiteralKind::Float,
            Rule::string_lit => {
                let processed_string = self.unescape_string(&inner, true)?;
                LiteralKind::String(processed_string)
            }
            Rule::bool_lit => LiteralKind::Bool,
            Rule::none_lit => LiteralKind::None,
            Rule::tuple_lit => {
                let mut expressions = Vec::new();
                for expr_pair in Self::convert_into_iterator(&inner) {
                    expressions.push(self.parse_expression(&expr_pair)?);
                }
                LiteralKind::Tuple(expressions)
            }
            _ => return Err(self.create_error_pair(SpecificError::UnknownLiteral, &inner)),
        };
        Ok((literal_kind, self.to_node(&inner)))
    }

    fn parse_array_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut elements = Vec::new();
        for element in Self::convert_into_iterator(pair) {
            elements.push(self.parse_expression(&element)?);
        }
        Ok(self.create_expr(ExpressionKind::Literal(LiteralKind::Array(elements)), pair))
    }

    fn parse_map_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut entries = Vec::new();

        for entry_pair in Self::convert_into_iterator(pair) {
            if entry_pair.as_rule() == Rule::map_entry {
                let mut entry_inner = Self::convert_into_iterator(&entry_pair);
                let key = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                let value = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                entries.push((key, value));
            }
        }

        Ok(self.create_expr(ExpressionKind::Literal(LiteralKind::Map(entries)), pair))
    }

    fn parse_mutable_or_immutable_expression(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<MutableOrImmutableExpression, ParseError> {
        // The mut_expression rule is defined as { lvalue | expression }.
        // Its inner pair will be one of those alternatives.
        let mut inner = pair.clone().into_inner();
        let first = Self::next_pair(&mut inner)?;
        match first.as_rule() {
            Rule::lvalue => {
                let mut lvalue_inner = first.into_inner();
                let mut_kw = Self::next_pair(&mut lvalue_inner)?;
                let postfix = Self::next_pair(&mut lvalue_inner)?;
                let expr = self.parse_postfix_expression(&postfix)?;
                Ok(MutableOrImmutableExpression {
                    is_mutable: Some(self.to_node(&mut_kw)),
                    expression: expr,
                })
            }
            Rule::expression => {
                // Otherwise, if it’s an expression, parse it normally.
                let expr = self.parse_expression(&first)?;
                Ok(MutableOrImmutableExpression {
                    is_mutable: None,
                    expression: expr,
                })
            }
            _ => {
                Err(self
                    .create_error_pair(SpecificError::UnexpectedTokenInMutableExpression, &first))
            }
        }
    }

    fn parse_function_call_postfix(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<MutableOrImmutableExpression>, ParseError> {
        assert_eq!(pair.as_rule(), Rule::function_call_postfix);
        let mut inner = pair.clone().into_inner();
        self.parse_function_call_arguments(&Self::next_pair(&mut inner)?)
    }

    fn parse_function_call_arguments(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<MutableOrImmutableExpression>, ParseError> {
        assert_eq!(pair.as_rule(), Rule::function_call_args);
        let inner = pair.clone().into_inner();
        let mut args = Vec::new();

        // Parse arguments
        for arg_pair in inner {
            if arg_pair.as_rule() == Rule::mut_expression {
                //let mut arg_inner = Self::convert_into_iterator(&arg_pair).peekable();

                let expr = self.parse_mutable_or_immutable_expression(&arg_pair)?;
                args.push(expr);
            } else {
                return Err(
                    self.create_error_pair(SpecificError::UnexpectedTokenInFunctionCall, &arg_pair)
                );
            }
        }

        Ok(args)
    }

    #[allow(clippy::too_many_lines)]
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
            Rule::built_in_type => {
                let mut inner = pair.clone().into_inner();
                self.parse_type_from_str(&mut inner, &pair)
            }
            Rule::qualified_type_identifier => {
                let qualified_id = self.parse_qualified_type_identifier(&pair)?;

                // Check for generic parameters
                let remaining_pairs = pair.into_inner();
                for next_pair in remaining_pairs {
                    if next_pair.as_rule() == Rule::generic_params {
                        let mut generic_types = Vec::new();
                        for param in Self::convert_into_iterator(&next_pair) {
                            generic_types.push(self.parse_type(param)?);
                        }
                        return Ok(Type::Generic(
                            Box::new(Type::Named(qualified_id)),
                            generic_types,
                        ));
                    }
                }
                Ok(Type::Named(qualified_id))
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
            _ => Ok(Type::Named(
                self.expect_qualified_type_identifier_next(&mut iterator)?,
            )),
        }
    }

    #[allow(unused)] // TODO: Use this again
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
        Ok(LocalTypeIdentifier::new(self.to_node(pair)))
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
        let mut inner = Self::convert_into_iterator(pair);

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
            Rule::simple_variant => EnumVariantType::Simple(self.to_node(pair)),
            Rule::tuple_variant => {
                let mut inner = Self::convert_into_iterator(pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let mut types = Vec::new();
                for type_pair in inner {
                    types.push(self.parse_type(type_pair)?);
                }

                EnumVariantType::Tuple(name.0, types)
            }
            Rule::struct_variant => {
                let mut inner = Self::convert_into_iterator(pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let mut fields = Vec::new();

                for field_pair in inner {
                    let mut field_inner = Self::convert_into_iterator(&field_pair);
                    let field_name = self.expect_field_label_next(&mut field_inner)?;
                    let field_type = self.parse_type(Self::next_pair(&mut field_inner)?)?;
                    let anonymous_enum_variant_field = FieldType {
                        field_name,
                        field_type,
                    };
                    fields.push(anonymous_enum_variant_field);
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
        let mut inner = Self::convert_into_iterator(pair);
        let value = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let arms_pair = Self::next_pair(&mut inner)?;
        let mut arms = Vec::new();

        for arm_pair in Self::convert_into_iterator(&arms_pair) {
            if arm_pair.as_rule() == Rule::match_arm {
                let mut arm_inner = Self::convert_into_iterator(&arm_pair);
                let pattern = self.parse_match_pattern(&Self::next_pair(&mut arm_inner)?)?;

                // Handle both block and direct expression cases
                let expr = match Self::next_pair(&mut arm_inner)? {
                    block if block.as_rule() == Rule::block => self.parse_block(&block)?,
                    expr => self.parse_expression(&expr)?,
                };

                arms.push(MatchArm {
                    pattern,
                    expression: expr,
                });
            }
        }

        if arms.is_empty() {
            return Err(self.create_error_pair(SpecificError::MustHaveAtLeastOneArm, pair));
        }

        Ok(self.create_expr(ExpressionKind::Match(Box::new(value), arms), pair))
    }

    fn parse_match_pattern(&self, pair: &Pair<Rule>) -> Result<Pattern, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let pattern_inside = inner.next().expect("should have inner");
        match pattern_inside.as_rule() {
            Rule::normal_pattern => {
                let (match_pattern, pattern_node) =
                    self.parse_normal_match_pattern(&pattern_inside)?;
                let inner_pairs: Vec<_> = pattern_inside.clone().into_inner().collect();
                let has_guard = inner_pairs
                    .get(1)
                    .map(|p| p.as_rule() == Rule::guard_clause)
                    .unwrap_or(false);

                let guard_clause = if has_guard {
                    Some(self.parse_guard_clause(&inner_pairs[1])?)
                } else {
                    None
                };
                Ok(Pattern::NormalPattern(
                    pattern_node,
                    match_pattern,
                    guard_clause,
                ))
            }
            Rule::wildcard_pattern => Ok(Pattern::Wildcard(self.to_node(pair))),
            _ => Err(self.create_error_pair(SpecificError::MustHaveAtLeastOneArm, pair)),
        }
    }

    fn parse_guard_clause(&self, pair: &Pair<Rule>) -> Result<GuardClause, ParseError> {
        let inner = Self::right_alternative(&pair)?;
        let clause = match inner.as_rule() {
            Rule::wildcard_pattern => GuardClause::Wildcard(Self::node_ex(&pair)),
            Rule::expression => {
                let mut iterator = inner.into_inner();
                let result = self.parse_expression(&Self::next_pair(&mut iterator)?)?;
                GuardClause::Expression(result)
            }
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownExpr("guard_clause".to_string()),
                    &pair,
                ))?
            }
        };

        Ok(clause)
    }

    fn parse_guard_expr_list(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut guard_exprs = Vec::new();

        for expr_pair in Self::convert_into_iterator(pair) {
            match expr_pair.as_rule() {
                Rule::guard_item => {
                    let mut guard_inner = Self::convert_into_iterator(&expr_pair);
                    let guard_clause = Self::next_pair(&mut guard_inner)?;
                    let condition = self.parse_guard_clause(&guard_clause)?;
                    let result = self.parse_expression(&Self::next_pair(&mut guard_inner)?)?;
                    guard_exprs.push(GuardExpr {
                        clause: condition,
                        result,
                    });
                }

                _ => {
                    panic!("Unexpected rule: {:?}", expr_pair.as_rule());
                }
            }
        }

        Ok(self.create_expr(ExpressionKind::Guard(guard_exprs), pair))
    }

    fn parse_normal_match_pattern(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(NormalPattern, Node), ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let pattern = inner.next().expect("should have inner");

        match pattern.as_rule() {
            Rule::pattern => {
                let mut pattern_inner = Self::convert_into_iterator(&pattern);
                let pattern_type = pattern_inner.next().expect("should have inner");

                match pattern_type.as_rule() {
                    Rule::enum_pattern => {
                        let mut inner = Self::convert_into_iterator(&pattern_type);
                        let variant = self.expect_local_type_identifier_next(&mut inner)?;
                        let elements = inner
                            .next()
                            .map(|p| self.parse_pattern_list(&p))
                            .transpose()?;
                        Ok((
                            NormalPattern::EnumPattern(variant.0, elements),
                            self.to_node(&pattern),
                        ))
                    }
                    Rule::pattern_list => {
                        let elements = self.parse_pattern_list(&pattern_type)?;
                        Ok((NormalPattern::PatternList(elements), self.to_node(&pattern)))
                    }
                    Rule::basic_literal => {
                        let (literal, node) = self.parse_basic_literal(&pattern_type)?;
                        Ok((NormalPattern::Literal(literal), node))
                    }
                    _ => {
                        Err(self.create_error_pair(SpecificError::UnknownMatchType, &pattern_type))
                    }
                }
            }
            _ => Err(self.create_error_pair(SpecificError::UnknownMatchType, &pattern)),
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

    fn node_ex(pair: &Pair<Rule>) -> Node {
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

    fn span(pest_span: pest::Span) -> SpanWithoutFileId {
        SpanWithoutFileId {
            offset: pest_span.start() as u32,
            length: (pest_span.end() - pest_span.start()) as u16,
        }
    }

    fn create_expr(&self, kind: ExpressionKind, pair: &Pair<Rule>) -> Expression {
        self.create_expr_span(kind, self.to_node(pair))
    }

    fn create_expr_span(&self, kind: ExpressionKind, node: Node) -> Expression {
        //info!(?kind, ?node, "create_expr()");
        Expression { kind, node }
    }

    fn parse_multiplication(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_prefix(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            // Expect the next token to be a multiplication operator, then the next operand.
            let operator = self.parse_binary_operator(&op)?; // op_mul, op_div, or op_mod
            let right = self.parse_prefix(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_addition(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_multiplication(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // op_add or op_sub
            let right = self.parse_multiplication(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_comparison(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_addition(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // e.g. op_lt, op_eq, etc.
            let right = self.parse_addition(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_range(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let left = self.parse_comparison(&inner.next().unwrap())?;
        if let Some(op) = inner.next() {
            let right = self.parse_comparison(&inner.next().unwrap())?;
            if op.as_rule() == Rule::exclusive_range_op {
                return Ok(self.create_expr(
                    ExpressionKind::ExclusiveRange(Box::new(left), Box::new(right)),
                    pair,
                ));
            }
            let operator = self.parse_binary_operator(&op)?; // inclusive_range_op or exclusive_range_op
            Ok(self.create_expr(
                ExpressionKind::BinaryOp(Box::new(left), operator, Box::new(right)),
                pair,
            ))
        } else {
            Ok(left)
        }
    }

    fn parse_logical(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_range(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // op_and or op_or
            let right = self.parse_range(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }
}
