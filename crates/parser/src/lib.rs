/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use fixed32::Fp;
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;
use seq_map::SeqMap;
use swamp_script_ast::prelude::*;
use swamp_script_ast::{Function, PostfixOperator};
use tracing::debug;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ScriptParser;

pub struct AstParser {
    #[allow(dead_code)]
    pratt_parser: PrattParser<Rule>,
}

impl AstParser {
    // Helper functions for common parser operations
    fn next_pair<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<Pair<'a, Rule>, Error<Rule>> {
        pairs.next().ok_or_else(|| {
            Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Expected more tokens".into(),
                },
                pest::Position::from_start(""),
            )
        })
    }

    fn expect_next<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
        expected_rule: Rule,
    ) -> Result<Pair<'a, Rule>, Error<Rule>> {
        let pair = Self::next_pair(pairs)?;
        if pair.as_rule() != expected_rule {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("Expected {:?}, found {:?}", expected_rule, pair.as_rule()),
                },
                pair.as_span(),
            ));
        }
        Ok(pair)
    }

    fn expect_identifier<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<String, Error<Rule>> {
        let pair = Self::expect_next(pairs, Rule::identifier)?;
        Ok(pair.as_str().to_string())
    }

    fn expect_type_identifier<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<String, Error<Rule>> {
        let pair = Self::expect_next(pairs, Rule::type_identifier)?;
        Ok(pair.as_str().to_string())
    }

    fn get_inner_pairs<'a>(pair: &'a Pair<'a, Rule>) -> impl Iterator<Item = Pair<'a, Rule>> {
        //info!("get_inner_pairs: {:?}", pair.as_rule());
        pair.clone().into_inner()
    }

    fn create_error(&self, message: &str, span: pest::Span) -> Error<Rule> {
        Error::new_from_span(
            ErrorVariant::CustomError {
                message: message.to_string(),
            },
            span,
        )
    }

    fn next_inner_pair<'a>(&self, pair: Pair<'a, Rule>) -> Result<Pair<'a, Rule>, Error<Rule>> {
        let span = pair.as_span();
        pair.into_inner().next().ok_or_else(move || {
            Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "Expected inner token".into(),
                },
                span,
            )
        })
    }

    // ---------------

    #[must_use]
    pub fn new() -> Self {
        let pratt_parser = PrattParser::new()
            .op(Op::infix(Rule::range_op, Assoc::Left))
            .op(Op::postfix(Rule::option_operator)); // TODO: this was the only way to avoid an infinite loop ( left-recursive) in the PEST grammar

        Self { pratt_parser }
    }

    pub fn parse_script(&self, raw_script: &str) -> Result<Module, pest::error::Error<Rule>> {
        let script = &raw_script.replace("\r", ""); // remove all CR, so only LF is left of CRLF

        let mut pairs = ScriptParser::parse(Rule::program, script)?;
        let program_pair = Self::next_pair(&mut pairs)?;

        let mut statements = Vec::new();
        let mut definitions = Vec::new();
        for pair in Self::get_inner_pairs(&program_pair) {
            match pair.as_rule() {
                Rule::statement => {
                    let inner = self.next_inner_pair(pair)?;
                    match inner.as_rule() {
                        Rule::definition => {
                            let def = self.parse_definition(&inner)?;
                            debug!("parsed definition {def:?}");
                            definitions.push(def);
                        }
                        Rule::control_statement => {
                            let stmt = self.parse_control_statement(inner)?;
                            debug!("parsed statement {stmt:?}");
                            statements.push(stmt);
                        }
                        _ => unreachable!(
                            "statement should only contain definition or control_statement"
                        ),
                    }
                }
                Rule::EOI => {} // Ignore end of input
                _ => {
                    return Err(self.create_error(
                        &format!("Unexpected rule in parse_script: {:?}", pair.as_rule()),
                        pair.as_span(),
                    ))
                }
            }
        }

        Ok(Module::new(definitions, statements))
    }

    fn parse_definition(&self, pair: &Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let inner_pair = self.next_inner_pair(pair.clone())?;
        match inner_pair.as_rule() {
            Rule::impl_def => self.parse_impl_def(inner_pair),
            Rule::struct_def => self.parse_struct_def(inner_pair),
            Rule::function_def => self.parse_function_def(inner_pair),
            Rule::import_stmt => self.parse_import(inner_pair),
            Rule::doc_comment => self.parse_doc_comment(inner_pair),
            Rule::enum_def => self.parse_enum_def(inner_pair),
            Rule::type_alias => self.parse_type_alias(inner_pair),
            _ => todo!(),
        }
    }

    fn parse_control_statement(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let inner = self.next_inner_pair(pair)?;
        self.parse_statement(inner)
    }

    fn parse_statement_to_control(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        match pair.as_rule() {
            Rule::statement => {
                let inner = self.next_inner_pair(pair)?;
                match inner.as_rule() {
                    Rule::control_statement => self.parse_control_statement(inner),
                    _ => Err(self.create_error(
                        &format!("Expected control statement, found: {:?}", inner.as_rule()),
                        inner.as_span(),
                    )),
                }
            }
            _ => Err(self.create_error(
                &format!("Expected statement, found: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn parse_statement(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        match pair.as_rule() {
            Rule::stmt_block => {
                let mut statements = Vec::new();
                for stmt in Self::get_inner_pairs(&pair) {
                    if stmt.as_rule() == Rule::statement {
                        statements.push(self.parse_statement_to_control(stmt)?);
                    }
                }
                Ok(Statement::Block(statements))
            }
            Rule::let_statement => self.parse_let_statement(pair),
            Rule::if_stmt => self.parse_if_statement(pair),
            Rule::for_loop => self.parse_for_loop(pair),
            Rule::while_loop => self.parse_while_loop(pair),
            Rule::return_stmt => self.parse_return(pair),
            Rule::break_stmt => Ok(Statement::Break),
            Rule::continue_stmt => Ok(Statement::Continue),
            Rule::expression_statement => {
                let expr = self.parse_expression(self.next_inner_pair(pair)?)?;
                Ok(Statement::Expression(expr))
            }
            Rule::EOI => Ok(Statement::Expression(Expression::Literal(Literal::Unit))), // Handle EOI
            _ => Err(self.create_error(
                &format!("Unexpected rule in parse_statement: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn parse_if_statement(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse condition
        let condition = self.parse_expression(Self::next_pair(&mut inner)?)?;

        // Parse then block
        let then_block = Self::next_pair(&mut inner)?;
        let mut then_statements = Vec::new();
        for stmt in Self::get_inner_pairs(&then_block) {
            if stmt.as_rule() == Rule::statement {
                then_statements.push(self.parse_statement_to_control(stmt)?);
            }
        }

        // Parse optional else block (which might be another if statement)
        let else_statements = if let Some(else_token) = inner.next() {
            // We don't need to get another token after 'else', it's part of the if_stmt rule
            match else_token.as_rule() {
                Rule::if_stmt => {
                    // Handle else if by returning the parsed if statement in a Vec
                    vec![self.parse_if_statement(else_token)?]
                }
                Rule::stmt_block => {
                    // Handle regular else block
                    let mut stmts = Vec::new();
                    for stmt in Self::get_inner_pairs(&else_token) {
                        if stmt.as_rule() == Rule::statement {
                            stmts.push(self.parse_statement_to_control(stmt)?);
                        }
                    }
                    stmts
                }
                _ => {
                    return Err(self.create_error(
                        &format!(
                            "Expected if statement or block after else, found {:?}",
                            else_token.as_rule()
                        ),
                        else_token.as_span(),
                    ))
                }
            }
        } else {
            Vec::new()
        };

        let else_value = if else_statements.is_empty() {
            None
        } else {
            Some(else_statements)
        };

        Ok(Statement::If(condition, then_statements, else_value))
    }

    fn parse_doc_comment(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let comment_text = pair.as_str();
        Ok(Definition::Comment(comment_text.to_string()))
    }

    fn parse_field_access(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        let base = Variable::new(&Self::expect_identifier(&mut inner)?, false);
        let field = LocalIdentifier::new(
            convert_from_pair(&pair),
            &Self::expect_identifier(&mut inner)?,
        );

        Ok(Expression::FieldAccess(
            Box::new(Expression::VariableAccess(base)),
            field,
        ))
    }

    fn parse_let_statement(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair).peekable();

        // Check for mut keyword first
        let is_mutable = inner
            .peek()
            .map(|p| p.as_rule() == Rule::mut_keyword)
            .unwrap_or(false);

        if is_mutable {
            inner.next(); // consume mut keyword
        }

        let pattern_pair = Self::next_pair(&mut inner)?;
        let expr_pair = Self::next_pair(&mut inner)?;

        let pattern = match pattern_pair.as_rule() {
            Rule::identifier => {
                Pattern::VariableAssignment(Variable::new(pattern_pair.as_str(), is_mutable))
            }
            Rule::struct_destructure => self.parse_struct_pattern(&pattern_pair)?,
            Rule::tuple_destructure => self.parse_tuple_pattern(&pattern_pair)?,
            _ => {
                return Err(self.create_error(
                    &format!("Unexpected pattern type: {:?}", pattern_pair.as_rule()),
                    pattern_pair.as_span(),
                ))
            }
        };

        let expr = self.parse_expression(expr_pair)?;
        Ok(Statement::Let(pattern, expr))
    }

    fn parse_struct_pattern(&self, pair: &Pair<Rule>) -> Result<Pattern, Error<Rule>> {
        let mut fields = Vec::new();
        for field in Self::get_inner_pairs(pair) {
            if field.as_rule() == Rule::identifier {
                fields.push(LocalTypeIdentifier::new(
                    convert_from_pair(&pair),
                    field.as_str(),
                ));
            }
        }
        Ok(Pattern::Struct(fields))
    }

    fn parse_tuple_pattern(&self, pair: &Pair<Rule>) -> Result<Pattern, Error<Rule>> {
        let mut fields = Vec::new();
        for field in Self::get_inner_pairs(pair) {
            if field.as_rule() == Rule::identifier {
                fields.push(LocalTypeIdentifier::new(
                    convert_from_pair(&pair),
                    field.as_str(),
                ));
            }
        }
        Ok(Pattern::Tuple(fields))
    }

    fn parse_struct_def(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse struct name
        let name = Self::expect_type_identifier(&mut inner)?;

        // Get field definitions
        let field_defs = Self::next_pair(&mut inner)?;
        let mut fields = SeqMap::new();

        // Parse each field definition
        for field_def in Self::get_inner_pairs(&field_defs) {
            let mut field_parts = Self::get_inner_pairs(&field_def);

            let field_name = Self::expect_identifier(&mut field_parts)?; // TODO: Store this
            let field_type = self.parse_type(Self::next_pair(&mut field_parts)?)?;

            fields
                .insert(
                    IdentifierName(
                        LocalIdentifier::new(convert_from_pair(&pair), &field_name).text,
                    ),
                    field_type,
                )
                .expect("duplicate field name"); // TODO: should be error
        }

        let struct_def = StructType::new(
            LocalTypeIdentifier::new(convert_from_pair(&pair), &name),
            fields,
        );

        Ok(Definition::StructDef(struct_def))
    }

    fn parse_stmt_block(&self, pair: Pair<Rule>) -> Result<Vec<Statement>, Error<Rule>> {
        if pair.as_rule() != Rule::stmt_block {
            return Err(self.create_error(
                &format!("Expected statement block, found {:?}", pair.as_rule()),
                pair.as_span(),
            ));
        }

        let mut statements = Vec::new();
        for stmt in Self::get_inner_pairs(&pair) {
            if stmt.as_rule() == Rule::statement {
                statements.push(self.parse_statement_to_control(stmt)?);
            }
        }
        Ok(statements)
    }

    fn parse_function_def(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let function_pair = self.next_inner_pair(pair)?;

        match function_pair.as_rule() {
            Rule::normal_function => {
                let mut inner = function_pair.clone().into_inner();
                let signature_pair = inner.next().ok_or_else(|| {
                    self.create_error("Missing function signature", function_pair.as_span())
                })?;

                let (name, signature) = self.parse_function_signature(signature_pair)?;

                let body = self.parse_stmt_block(inner.next().ok_or_else(|| {
                    self.create_error("Missing function body", function_pair.as_span())
                })?)?;

                Ok(Definition::FunctionDef(
                    name,
                    Function::Internal(FunctionData { signature, body }),
                ))
            }
            Rule::external_function => {
                let signature_pair =
                    function_pair.clone().into_inner().next().ok_or_else(|| {
                        self.create_error("Missing function signature", function_pair.as_span())
                    })?;

                let (name, signature) = self.parse_function_signature(signature_pair)?;
                Ok(Definition::FunctionDef(name, Function::External(signature)))
            }
            _ => Err(self.create_error(
                &format!(
                    "Expected function definition, found {:?}",
                    function_pair.as_rule()
                ),
                function_pair.as_span(),
            )),
        }
    }
    fn parse_function_signature(
        &self,
        pair: Pair<Rule>,
    ) -> Result<(LocalIdentifier, FunctionSignature), Error<Rule>> {
        if pair.as_rule() != Rule::function_signature {
            return Err(self.create_error(
                &format!("Expected function signature, found {:?}", pair.as_rule()),
                pair.as_span(),
            ));
        }

        let mut inner = pair.clone().into_inner();

        // Parse function name
        let name = LocalIdentifier::new(
            convert_from_pair(&pair),
            &AstParser::expect_identifier(&mut inner)?,
        );

        // Parse parameters
        let parameters = if let Some(param_list) = inner.next() {
            if param_list.as_rule() == Rule::parameter_list {
                self.parse_parameters(param_list)?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        // Parse return type if it exists, otherwise use Unit
        let return_type = if let Some(return_type_pair) = inner.next() {
            if return_type_pair.as_rule() == Rule::return_type {
                self.parse_return_type(return_type_pair)?
            } else {
                Type::Unit
            }
        } else {
            Type::Unit
        };

        Ok((
            name.clone(),
            FunctionSignature {
                name,
                params: parameters,
                return_type,
            },
        ))
    }

    fn parse_return_type(&self, pair: Pair<Rule>) -> Result<Type, Error<Rule>> {
        let inner_pair = self.next_inner_pair(pair)?;
        self.parse_type(inner_pair)
    }

    fn parse_parameters(&self, pair: Pair<Rule>) -> Result<Vec<Parameter>, Error<Rule>> {
        let mut parameters = Vec::new();

        for param_pair in Self::get_inner_pairs(&pair) {
            match param_pair.as_rule() {
                Rule::parameter => {
                    let pairs: Vec<_> = param_pair.into_inner().collect();

                    // Check first pair - is it mut?
                    let (is_mutable, name, type_pair) = if pairs[0].as_rule() == Rule::mut_keyword {
                        (true, &pairs[1], &pairs[2])
                    } else {
                        (false, &pairs[0], &pairs[1])
                    };

                    let param_type = self.parse_type(type_pair.clone())?;

                    parameters.push(Parameter {
                        variable: Variable::new(name.as_str(), is_mutable),
                        param_type,
                        is_mutable,
                        is_self: false,
                    });
                }
                Rule::self_parameter => {
                    // ... rest of the self_parameter handling ...
                }
                _ => {
                    return Err(self.create_error(
                        &format!("Unexpected parameter type: {:?}", param_pair.as_rule()),
                        param_pair.as_span(),
                    ))
                }
            }
        }

        Ok(parameters)
    }

    fn parse_impl_def(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let type_name = LocalTypeIdentifier::new(
            convert_from_pair(&pair),
            &Self::expect_type_identifier(&mut inner)?,
        );
        let mut functions = SeqMap::new();

        while let Some(item_pair) = inner.next() {
            if item_pair.as_rule() == Rule::impl_item {
                let inner_item = self.next_inner_pair(item_pair)?;
                match inner_item.as_rule() {
                    Rule::external_member_function => {
                        let (name, signature) = self.parse_member_signature(inner_item)?;
                        functions
                            .insert(
                                IdentifierName(name.text.clone()),
                                Function::External(signature),
                            )
                            .expect("duplicate impl function");
                    }
                    Rule::normal_member_function => {
                        let (name, function_data) = self.parse_member_data(inner_item)?;
                        functions
                            .insert(
                                IdentifierName(name.text.clone()),
                                Function::Internal(function_data),
                            )
                            .expect("duplicate impl function");
                    }
                    _ => {
                        return Err(self.create_error(
                            &format!("Unexpected impl item type: {:?}", inner_item.as_rule()),
                            inner_item.as_span(),
                        ))
                    }
                }
            }
        }

        Ok(Definition::ImplDef(type_name, functions))
    }

    fn parse_member_signature(
        &self,
        pair: Pair<Rule>,
    ) -> Result<(LocalIdentifier, FunctionSignature), Error<Rule>> {
        if pair.as_rule() != Rule::member_signature {
            return Err(self.create_error(
                &format!("Expected member signature, found {:?}", pair.as_rule()),
                pair.as_span(),
            ));
        }

        let mut inner = pair.clone().into_inner();

        // Parse function name
        let name = LocalIdentifier::new(
            convert_from_pair(&pair),
            &AstParser::expect_identifier(&mut inner)?,
        );

        // Parse self parameter and other parameters
        let mut parameters = Vec::new();

        if let Some(param_list) = inner.next() {
            match param_list.as_rule() {
                Rule::self_parameter => {
                    let pairs: Vec<_> = param_list.into_inner().collect();
                    let is_mutable = pairs
                        .get(0)
                        .map_or(false, |p| p.as_rule() == Rule::mut_keyword);

                    parameters.push(Parameter {
                        variable: Variable::new("self", is_mutable),
                        param_type: Type::Any, // self type is handled elsewhere
                        is_mutable,
                        is_self: true,
                    });

                    // Check for additional parameters
                    if let Some(more_params) = inner.next() {
                        if more_params.as_rule() == Rule::parameter_list {
                            parameters.extend(self.parse_parameters(more_params)?);
                        }
                    }
                }
                Rule::parameter_list => {
                    parameters = self.parse_parameters(param_list)?;
                }
                _ => {}
            }
        }

        // Parse return type if it exists
        let return_type = if let Some(return_type_pair) = inner.next() {
            if return_type_pair.as_rule() == Rule::return_type {
                self.parse_return_type(return_type_pair)?
            } else {
                Type::Unit
            }
        } else {
            Type::Unit
        };

        Ok((
            name.clone(),
            FunctionSignature {
                name,
                params: parameters,
                return_type,
            },
        ))
    }

    fn parse_member_data(
        &self,
        pair: Pair<Rule>,
    ) -> Result<(LocalIdentifier, FunctionData), Error<Rule>> {
        let mut inner = pair.clone().into_inner();
        let signature_pair = inner
            .next()
            .ok_or_else(|| self.create_error("Missing member signature", pair.as_span()))?;

        let (name, signature) = self.parse_member_signature(signature_pair)?;

        let body = self.parse_stmt_block(
            inner
                .next()
                .ok_or_else(|| self.create_error("Missing function body", pair.as_span()))?,
        )?;

        Ok((name, FunctionData { signature, body }))
    }

    fn parse_for_loop(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse loop variable
        let var_pair = Self::next_pair(&mut inner)?;
        let var_pattern = Pattern::VariableAssignment(Variable::new(var_pair.as_str(), false));

        // Parse iterable expression
        let iterable = self.parse_expression(Self::next_pair(&mut inner)?)?;

        // Parse block
        let block_pair = Self::next_pair(&mut inner)?;
        if block_pair.as_rule() != Rule::stmt_block {
            return Err(self.create_error("Expected block in for loop", block_pair.as_span()));
        }

        let mut body = Vec::new();
        for stmt in Self::get_inner_pairs(&block_pair) {
            if stmt.as_rule() == Rule::statement {
                body.push(self.parse_statement_to_control(stmt)?);
            }
        }

        Ok(Statement::ForLoop(var_pattern, iterable, body))
    }

    fn parse_while_loop(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse condition
        let condition = self.parse_expression(Self::next_pair(&mut inner)?)?;

        // Parse block
        let block_pair = Self::next_pair(&mut inner)?;
        if block_pair.as_rule() != Rule::stmt_block {
            return Err(self.create_error("Expected block in while loop", block_pair.as_span()));
        }

        let mut body = Vec::new();
        for stmt in Self::get_inner_pairs(&block_pair) {
            if stmt.as_rule() == Rule::statement {
                body.push(self.parse_statement_to_control(stmt)?);
            }
        }

        Ok(Statement::WhileLoop(condition, body))
    }

    fn parse_import(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let import_type = Self::next_pair(&mut inner)?;

        match import_type.as_rule() {
            Rule::import_list_from => {
                // Handle: import { sin, cos } from math
                let mut list_from = Self::get_inner_pairs(&import_type);
                let import_list = Self::next_pair(&mut list_from)?;
                let module_path = Self::next_pair(&mut list_from)?;

                // Parse specific items
                let items: Vec<LocalTypeIdentifier> = Self::get_inner_pairs(&import_list)
                    .filter(|p| p.as_rule() == Rule::identifier)
                    .map(|p| LocalTypeIdentifier::new(convert_from_pair(&pair), p.as_str()))
                    .collect();

                // Parse module path (e.g., "geometry.shapes" -> ["geometry", "shapes"])
                let path: Vec<String> = module_path
                    .as_str()
                    .split('.')
                    .map(|s| s.to_string())
                    .collect();

                Ok(Definition::Import(Import {
                    module_path: ModulePath(path),
                    items: ImportItems::Specific(items),
                }))
            }
            Rule::import_path => {
                // Handle: import math or import geometry.shapes
                let path: Vec<String> = import_type
                    .as_str()
                    .split('.')
                    .map(|s| s.to_string())
                    .collect();

                Ok(Definition::Import(Import {
                    module_path: ModulePath(path),
                    items: ImportItems::Module,
                }))
            }
            _ => Err(self.create_error("Invalid import statement", import_type.as_span())),
        }
    }

    fn parse_return(&self, pair: Pair<Rule>) -> Result<Statement, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Return value is optional
        let expr = match inner.next() {
            Some(expr_pair) => self.parse_expression(expr_pair)?,
            None => Expression::Literal(Literal::Unit),
        };

        Ok(Statement::Return(expr))
    }

    fn parse_expression(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        match pair.as_rule() {
            Rule::variable => Ok(Expression::VariableAccess(Variable::new(
                pair.as_str(),
                false,
            ))),
            Rule::block => {
                // Block in expression context - expect single expression
                let inner = self.next_inner_pair(pair)?;
                self.parse_expression(inner)
            }

            Rule::expression => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_expression(inner)
            }
            Rule::assignment => self.parse_assignment(pair),
            Rule::addition => self.parse_binary_chain(pair),
            Rule::logical | Rule::comparison | Rule::multiplication => self.parse_binary_op(pair),
            Rule::prefix => self.parse_prefix_expression(pair),
            Rule::primary | Rule::literal => self.parse_primary(pair),
            Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                let op = self.parse_unary_operator(pair.clone())?;
                let expr = self.parse_primary(self.next_inner_pair(pair)?)?;
                Ok(Expression::UnaryOp(op, Box::new(expr)))
            }
            Rule::postfix => self.parse_postfix_expression(pair),
            _ => Err(self.create_error(
                &format!("Unexpected expression type: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn parse_assignment(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let left = Self::next_pair(&mut inner)?;
        let expr = self.parse_expression(Self::next_pair(&mut inner)?)?;

        match left.as_rule() {
            Rule::array_subscript => {
                let mut subscript_inner = Self::get_inner_pairs(&left);
                let array = Variable::new(&Self::expect_identifier(&mut subscript_inner)?, false);
                let index = self.parse_expression(Self::next_pair(&mut subscript_inner)?)?;
                Ok(Expression::ArrayAssignment(
                    Box::new(Expression::VariableAccess(array)),
                    Box::new(index),
                    Box::new(expr),
                ))
            }
            Rule::identifier => {
                let var = Variable::new(left.as_str(), false);
                Ok(Expression::VariableAssignment(var, Box::new(expr)))
            }
            Rule::field_access => {
                if let Expression::FieldAccess(obj, field) = self.parse_field_access(left)? {
                    Ok(Expression::FieldAssignment(obj, field, Box::new(expr)))
                } else {
                    Err(self.create_error("Expected field access", pair.as_span()))
                }
            }
            _ => Err(self.create_error(
                &format!("Invalid assignment target: {:?}", left.as_rule()),
                left.as_span(),
            )),
        }
    }

    fn parse_binary_chain(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let mut left = self.parse_expression(Self::next_pair(&mut inner)?)?;

        while let Some(op) = inner.next() {
            let right = self.parse_expression(Self::next_pair(&mut inner)?)?;
            left = if op.as_rule() == Rule::range_op {
                Expression::ExclusiveRange(Box::new(left), Box::new(right))
            } else {
                let operator = self.parse_binary_operator(op)?;
                Expression::BinaryOp(Box::new(left), operator, Box::new(right))
            };
        }

        Ok(left)
    }

    fn parse_prefix_expression(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let span = pair.as_span();
        let mut inner = Self::get_inner_pairs(&pair).peekable();
        let mut expr = None;
        let mut prefix_ops = Vec::new();

        while let Some(part) = inner.next() {
            match part.as_rule() {
                Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                    let op = self.parse_unary_operator(part)?;
                    prefix_ops.push(op);
                }
                _ => {
                    expr = Some(self.parse_expression(part)?);
                    break;
                }
            }
        }

        let mut final_expr = expr
            .ok_or_else(|| self.create_error("Expected expression after prefix operators", span))?;

        for op in prefix_ops.into_iter().rev() {
            final_expr = Expression::UnaryOp(op, Box::new(final_expr));
        }

        Ok(final_expr)
    }

    fn parse_postfix_expression(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let mut expr = self.parse_primary(Self::next_pair(&mut inner)?)?;

        while let Some(op) = inner.next() {
            match op.as_rule() {
                Rule::postfix_op => {
                    let inner_op = self.next_inner_pair(op)?;
                    match inner_op.as_rule() {
                        Rule::option_operator => {
                            expr = Expression::PostfixOp(PostfixOperator::Unwrap, Box::new(expr));
                        }
                        _ => {
                            return Err(self.create_error(
                                &format!("Unexpected operator type: {:?}", inner_op.as_rule()),
                                inner_op.as_span(),
                            ))
                        }
                    }
                }
                _ => {
                    return Err(self.create_error(
                        &format!("Unexpected postfix operator: {:?}", op.as_rule()),
                        op.as_span(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn parse_binary_op(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let mut left = self.parse_expression(Self::next_pair(&mut inner)?)?;

        while let Some(op) = inner.next() {
            let right = self.parse_expression(Self::next_pair(&mut inner)?)?;
            left = match op.as_str() {
                ".." => Expression::ExclusiveRange(Box::new(left), Box::new(right)),
                _ => {
                    let operator = self.parse_binary_operator(op)?;
                    Expression::BinaryOp(Box::new(left), operator, Box::new(right))
                }
            };
        }

        Ok(left)
    }

    fn parse_binary_operator(&self, pair: Pair<Rule>) -> Result<BinaryOperator, Error<Rule>> {
        let op = match pair.as_rule() {
            Rule::infix_op | Rule::prefix_op => {
                let inner = self.next_inner_pair(pair.clone())?;
                inner
            }
            _ => pair,
        };

        match op.as_rule() {
            Rule::op_add => Ok(BinaryOperator::Add),
            Rule::op_sub => Ok(BinaryOperator::Subtract),
            Rule::op_mul => Ok(BinaryOperator::Multiply),
            Rule::op_div => Ok(BinaryOperator::Divide),
            Rule::op_mod => Ok(BinaryOperator::Modulo),
            Rule::op_eq => Ok(BinaryOperator::Equal),
            Rule::op_neq => Ok(BinaryOperator::NotEqual),
            Rule::op_lt => Ok(BinaryOperator::LessThan),
            Rule::op_lte => Ok(BinaryOperator::LessEqual),
            Rule::op_gt => Ok(BinaryOperator::GreaterThan),
            Rule::op_gte => Ok(BinaryOperator::GreaterEqual),
            Rule::op_and => Ok(BinaryOperator::LogicalAnd),
            Rule::op_or => Ok(BinaryOperator::LogicalOr),
            _ => Err(self.create_error(
                &format!("Unknown binary operator: {:?}", op.as_rule()),
                op.as_span(),
            )),
        }
    }

    fn parse_unary_operator(&self, pair: Pair<Rule>) -> Result<UnaryOperator, Error<Rule>> {
        let op = match pair.as_rule() {
            Rule::prefix_op => self.next_inner_pair(pair)?,
            _ => pair,
        };

        match op.as_rule() {
            Rule::op_neg => Ok(UnaryOperator::Negate),
            Rule::op_not => Ok(UnaryOperator::Not),
            _ => Err(self.create_error(
                &format!("Unknown unary operator: {:?}", op.as_rule()),
                op.as_span(),
            )),
        }
    }

    fn parse_member_chain(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse the base expression
        let base_pair = Self::next_pair(&mut inner)?;
        let base = match base_pair.as_rule() {
            Rule::array_literal => self.parse_array_literal(base_pair)?,
            Rule::variable => Expression::VariableAccess(Variable::new(base_pair.as_str(), false)),
            Rule::function_call => self.parse_function_call(base_pair)?,
            Rule::struct_instantiation => self.parse_struct_instantiation(base_pair)?,
            Rule::literal => Expression::Literal(self.parse_literal(base_pair)?),
            Rule::parenthesized => self.parse_expression(self.next_inner_pair(base_pair)?)?,
            _ => {
                return Err(self.create_error(
                    &format!("Unexpected base in member chain: {:?}", base_pair.as_rule()),
                    base_pair.as_span(),
                ))
            }
        };

        let mut expr = base;

        // Handle the chain of member accesses and possible assignment
        while let Some(chain_part) = inner.next() {
            match chain_part.as_rule() {
                Rule::member_access_or_call => {
                    let access_part = self.next_inner_pair(chain_part)?;
                    match access_part.as_rule() {
                        Rule::member_call => {
                            let mut call_inner = Self::get_inner_pairs(&access_part);
                            let member_name = call_inner.next().unwrap().as_str().to_string();
                            let mut args = Vec::new();

                            if let Some(arg_list) = call_inner.next() {
                                for arg in Self::get_inner_pairs(&arg_list) {
                                    args.push(self.parse_expression(arg)?);
                                }
                            }

                            expr = Expression::MemberCall(
                                Box::new(expr),
                                LocalIdentifier::new(convert_from_pair(&pair), &member_name),
                                args,
                            );
                        }
                        Rule::identifier => {
                            let field_name = access_part.as_str().to_string();
                            expr = Expression::FieldAccess(
                                Box::new(expr),
                                LocalIdentifier::new(convert_from_pair(&pair), &field_name),
                            );
                        }
                        _ => {
                            return Err(self.create_error(
                                &format!(
                                    "Unexpected member access type: {:?}",
                                    access_part.as_rule()
                                ),
                                access_part.as_span(),
                            ))
                        }
                    }
                }

                Rule::array_index => {
                    let index_expr = self.parse_expression(self.next_inner_pair(chain_part)?)?;
                    expr = Expression::ArrayAccess(Box::new(expr), Box::new(index_expr));
                }

                Rule::postfix_op => {
                    // Handle postfix operators
                    let inner_op = self.next_inner_pair(chain_part)?;
                    match inner_op.as_rule() {
                        Rule::option_operator => {
                            expr = Expression::PostfixOp(PostfixOperator::Unwrap, Box::new(expr));
                        }
                        _ => {
                            return Err(self.create_error(
                                &format!("Unexpected operator type: {:?}", inner_op.as_rule()),
                                inner_op.as_span(),
                            ))
                        }
                    }
                }

                Rule::expression => {
                    let value = self.parse_expression(chain_part.clone())?;
                    if let Expression::FieldAccess(obj, field) = expr {
                        expr = Expression::FieldAssignment(obj, field, Box::new(value));
                    } else {
                        return Err(self.create_error(
                            "Expected field access before assignment",
                            chain_part.as_span(),
                        ));
                    }
                }
                _ => {
                    return Err(self.create_error(
                        &format!("Unexpected chain part: {:?}", chain_part.as_rule()),
                        chain_part.as_span(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn parse_module_path(&self, pair: Pair<Rule>) -> Vec<String> {
        pair.into_inner()
            .filter_map(|segment| {
                if segment.as_rule() == Rule::module_segment {
                    segment
                        .into_inner()
                        .next()
                        .map(|id| id.as_str().to_string())
                } else {
                    None
                }
            })
            .collect()
    }

    fn parse_qualified_type_identifier(
        &self,
        pair: Pair<Rule>,
    ) -> Result<QualifiedTypeIdentifier, Error<Rule>> {
        let mut inner_pairs = pair.clone().into_inner();

        let first = inner_pairs
            .next()
            .ok_or_else(|| self.create_error("Expected identifier", pair.as_span()))?;

        match first.as_rule() {
            Rule::module_path => {
                let module_path = self.parse_module_path(first);
                let type_id = inner_pairs.next().ok_or_else(|| {
                    self.create_error("Expected type identifier after module path", pair.as_span())
                })?;
                Ok(QualifiedTypeIdentifier::new(
                    LocalTypeIdentifier::new(convert_from_pair(&type_id), type_id.as_str()),
                    module_path,
                ))
            }
            Rule::type_identifier => Ok(QualifiedTypeIdentifier::new(
                LocalTypeIdentifier::new(convert_from_pair(&first), first.as_str()),
                Vec::new(),
            )),
            _ => Err(self.create_error("Expected type identifier", pair.as_span())),
        }
    }

    fn parse_struct_instantiation(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Get struct name (required)
        let struct_name = self.parse_qualified_type_identifier(inner.next().unwrap())?;
        let mut fields = SeqMap::new(); // Use sequence map since hashmaps are literally random in how they are inserted

        // Parse fields if they exist
        if let Some(fields_pair) = inner.next() {
            for field in Self::get_inner_pairs(&fields_pair) {
                if field.as_rule() == Rule::struct_field {
                    let mut field_inner = Self::get_inner_pairs(&field);
                    let field_name = LocalIdentifier::new(
                        convert_from_pair(&pair),
                        &Self::expect_identifier(&mut field_inner)?,
                    ); // TODO: Save the field name
                    let field_value = self.parse_expression(Self::next_pair(&mut field_inner)?)?;
                    fields
                        .insert(IdentifierName(field_name.text), field_value)
                        .expect("should not be duplicates"); // TODO: Should return an error in result instead
                }
            }
        }

        Ok(Expression::StructInstantiation(struct_name, fields))
    }

    fn parse_primary(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        match pair.as_rule() {
            Rule::primary => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_primary(inner)
            }
            Rule::interpolated_string => self.parse_interpolated_string(pair),
            Rule::array_subscript => self.parse_array_access(pair),
            Rule::literal => Ok(Expression::Literal(self.parse_literal(pair)?)),
            Rule::variable => Ok(Expression::VariableAccess(Variable::new(
                pair.as_str(),
                false,
            ))),
            Rule::field_access => self.parse_field_access(pair),
            Rule::function_call => self.parse_function_call(pair),
            Rule::static_call => self.parse_static_call(pair),
            Rule::match_expr => self.parse_match_expr(pair),
            Rule::map_literal => self.parse_map_literal(pair),
            Rule::array_literal => self.parse_array_literal(pair),
            Rule::member_chain => self.parse_member_chain(pair),
            Rule::float_lit => {
                let float: f32 = pair
                    .as_str()
                    .parse()
                    .map_err(|_| self.create_error("Invalid float literal", pair.as_span()))?;
                let fp: Fp = float.try_into().expect("should work to convert float");
                Ok(Expression::Literal(Literal::Float(fp)))
            }
            Rule::parenthesized => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_expression(inner)
            }
            Rule::if_expr => self.parse_if_expr(pair),
            Rule::struct_instantiation => self.parse_struct_instantiation(pair),
            Rule::enum_literal => Ok(Expression::Literal(self.parse_enum_literal(pair)?)),
            _ => Err(self.create_error(
                &format!("Unexpected primary expression: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn parse_interpolated_string(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut parts = Vec::new();
        let mut current_literal = String::new();

        for part in Self::get_inner_pairs(&pair) {
            match part.as_rule() {
                Rule::text => {
                    let text = part.as_str();
                    let text = text.replace("{{", "{").replace("}}", "}");
                    current_literal.push_str(&text);
                }
                Rule::interpolation => {
                    if !current_literal.is_empty() {
                        parts.push(StringPart::Literal(current_literal.clone()));
                        current_literal.clear();
                    }

                    let inner = self.next_inner_pair(part.clone())?;
                    let expr = match inner.as_rule() {
                        Rule::expression => self.parse_expression(inner)?,
                        _ => {
                            return Err(self.create_error(
                                &format!(
                                    "Expected expression in interpolation, found: {:?}",
                                    inner.as_rule()
                                ),
                                inner.as_span(),
                            ))
                        }
                    };

                    let format = if let Some(fmt) = Self::get_inner_pairs(&part).nth(1) {
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
                    return Err(self.create_error(
                        &format!("Unexpected rule in string: {:?}", part.as_rule()),
                        part.as_span(),
                    ))
                }
            }
        }

        // Add any remaining literal text
        if !current_literal.is_empty() {
            parts.push(StringPart::Literal(current_literal));
        }

        Ok(Expression::InterpolatedString(parts))
    }

    fn parse_format_specifier(&self, pair: Pair<Rule>) -> Result<FormatSpecifier, Error<Rule>> {
        match pair.as_str() {
            "?" => Ok(FormatSpecifier::Debug),
            "x" => Ok(FormatSpecifier::LowerHex),
            "X" => Ok(FormatSpecifier::UpperHex),
            "b" => Ok(FormatSpecifier::Binary),
            "f" => Ok(FormatSpecifier::Float),
            s if s.starts_with("..") => {
                let precision: u32 = s[2..s.len() - 1]
                    .parse()
                    .map_err(|_| self.create_error("Invalid precision value", pair.as_span()))?;
                let typ = match s.chars().last().unwrap() {
                    'f' => PrecisionType::Float,
                    's' => PrecisionType::String,
                    _ => return Err(self.create_error("Invalid precision type", pair.as_span())),
                };
                Ok(FormatSpecifier::Precision(precision, typ))
            }
            _ => Err(self.create_error("Invalid format specifier", pair.as_span())),
        }
    }

    fn parse_array_access(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let var = Variable::new(&Self::expect_identifier(&mut inner)?, false);
        let index = self.parse_expression(Self::next_pair(&mut inner)?)?;
        Ok(Expression::ArrayAccess(
            Box::new(Expression::VariableAccess(var)),
            Box::new(index),
        ))
    }

    fn parse_enum_literal(
        &self,
        pair: Pair<Rule>,
    ) -> Result<swamp_script_ast::Literal, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse enum type name
        let enum_type = self.parse_qualified_type_identifier(inner.next().unwrap())?;

        // Parse variant name
        let variant_pair = Self::expect_next(&mut inner, Rule::type_identifier)?;
        let variant = LocalTypeIdentifier::new(convert_from_pair(&pair), variant_pair.as_str());

        // Parse fields if they exist
        let outer_data = if let Some(fields_pair) = inner.next() {
            match fields_pair.as_rule() {
                Rule::struct_fields_lit => {
                    let mut fields = SeqMap::new();
                    for field in Self::get_inner_pairs(&fields_pair) {
                        if field.as_rule() == Rule::struct_field {
                            let mut field_inner = Self::get_inner_pairs(&field);
                            let field_name = LocalTypeIdentifier::new(
                                convert_from_pair(&pair),
                                &Self::expect_identifier(&mut field_inner)?,
                            );
                            let field_value =
                                self.parse_expression(Self::next_pair(&mut field_inner)?)?;
                            fields
                                .insert(field_name, field_value)
                                .expect("should not be duplicates"); // TODO: Error handling
                        }
                    }
                    EnumLiteralData::Struct(fields)
                }
                Rule::tuple_fields => {
                    let mut expressions = vec![];
                    for field in Self::get_inner_pairs(&fields_pair) {
                        let field_value = self.parse_expression(field)?;
                        expressions.push(field_value);
                    }
                    EnumLiteralData::Tuple(expressions)
                }
                _ => {
                    return Err(self.create_error(
                        &format!(
                            "Unexpected enum variant fields: {:?}",
                            fields_pair.as_rule()
                        ),
                        fields_pair.as_span(),
                    ))
                }
            }
        } else {
            EnumLiteralData::Nothing
        };

        Ok(Literal::EnumVariant(enum_type, variant, outer_data))
    }

    fn parse_if_expr(&self, pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
        let mut inner = pair.into_inner();

        // Parse condition
        let condition = self.parse_expression(inner.next().unwrap())?;

        // Parse then block
        let then_block = inner.next().unwrap();
        let then_expr = self.parse_expression(then_block.into_inner().next().unwrap())?;

        // Parse else block if it exists
        let else_expr = if let Some(else_block) = inner.next() {
            self.parse_expression(else_block.into_inner().next().unwrap())?
        } else {
            Expression::Literal(Literal::Bool(false)) // Default else expression
        };

        Ok(Expression::IfElse(
            Box::new(condition),
            Box::new(then_expr),
            Box::new(else_expr),
        ))
    }

    fn parse_literal(&self, pair: Pair<Rule>) -> Result<Literal, Error<Rule>> {
        let inner = self.next_inner_pair(pair)?;

        match inner.as_rule() {
            Rule::int_lit => Ok(Literal::Int(inner.as_str().parse().map_err(|_| {
                self.create_error("Invalid integer literal", inner.as_span())
            })?)),
            Rule::float_lit => {
                let float: f32 = inner
                    .as_str()
                    .parse()
                    .map_err(|_| self.create_error("Invalid float literal", inner.as_span()))?;

                Ok(Literal::Float(Fp::from(float)))
            }
            Rule::string_lit => {
                let content = inner.as_str();
                // Remove quotes and handle escapes
                let processed = content[1..content.len() - 1]
                    .replace("\\\"", "\"")
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\\", "\\");
                Ok(Literal::String(StringConst(processed)))
            }
            Rule::bool_lit => Ok(Literal::Bool(inner.as_str().parse().map_err(|_| {
                self.create_error("Invalid boolean literal", inner.as_span())
            })?)),
            Rule::unit_lit => Ok(Literal::Unit),
            Rule::none_lit => Ok(Literal::None),
            Rule::tuple_lit => {
                let mut expressions = Vec::new();
                for expr_pair in Self::get_inner_pairs(&inner) {
                    expressions.push(self.parse_expression(expr_pair)?);
                }
                Ok(Literal::Tuple(expressions))
            }
            _ => Err(self.create_error(
                &format!("Unknown literal type: {:?}", inner.as_rule()),
                inner.as_span(),
            )),
        }
    }

    fn parse_array_literal(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut elements = Vec::new();
        for element in Self::get_inner_pairs(&pair) {
            elements.push(self.parse_expression(element)?);
        }
        Ok(Expression::Literal(Literal::Array(elements)))
    }

    // TODO: Not supported yet
    fn parse_map_literal(&self, _pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
        Ok(Expression::Literal(Literal::Map(SeqMap::new())))
    }

    fn parse_function_call(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse function name
        let func_name = Self::expect_identifier(&mut inner)?;
        let mut args = Vec::new();

        // Parse arguments
        while let Some(arg_pair) = inner.next() {
            if arg_pair.as_rule() == Rule::function_argument {
                let mut arg_inner = Self::get_inner_pairs(&arg_pair).peekable();

                // Check for mut keyword
                let has_mut = arg_inner
                    .peek()
                    .map(|p| p.as_rule() == Rule::mut_keyword)
                    .unwrap_or(false);

                if has_mut {
                    arg_inner.next(); // consume mut keyword
                }

                let expr = self.parse_expression(Self::next_pair(&mut arg_inner)?)?;

                if has_mut {
                    match expr {
                        Expression::VariableAccess(var) => {
                            args.push(Expression::MutRef(MutVariableRef(var)));
                        }
                        _ => {
                            return Err(self.create_error(
                                "Can only use mut with variables",
                                arg_pair.as_span(),
                            ))
                        }
                    }
                } else {
                    args.push(expr);
                }
            } else {
                return Err(self.create_error(
                    &format!(
                        "Unexpected token in function call: {:?}",
                        arg_pair.as_rule()
                    ),
                    arg_pair.as_span(),
                ));
            }
        }

        Ok(Expression::FunctionCall(
            Box::new(Expression::VariableAccess(Variable::new(&func_name, false))),
            args,
        ))
    }

    fn parse_static_call(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse type name and function name
        let type_name_pair = Self::next_pair(&mut inner)?;
        let func_name_pair = Self::next_pair(&mut inner)?;

        if type_name_pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error(
                &format!(
                    "Expected type identifier, found {:?}",
                    type_name_pair.as_rule()
                ),
                type_name_pair.as_span(),
            ));
        }

        if func_name_pair.as_rule() != Rule::identifier {
            return Err(self.create_error(
                &format!("Expected identifier, found {:?}", func_name_pair.as_rule()),
                func_name_pair.as_span(),
            ));
        }

        let mut args = Vec::new();
        // Parse arguments - same logic as function_call
        while let Some(arg_pair) = inner.next() {
            if arg_pair.as_rule() == Rule::function_argument {
                let mut arg_inner = Self::get_inner_pairs(&arg_pair).peekable();

                // Check for mut keyword
                let has_mut = arg_inner
                    .peek()
                    .map(|p| p.as_rule() == Rule::mut_keyword)
                    .unwrap_or(false);

                if has_mut {
                    arg_inner.next(); // consume mut keyword
                }

                let expr = self.parse_expression(Self::next_pair(&mut arg_inner)?)?;
                args.push(expr);
            }
        }

        Ok(Expression::StaticCall(
            LocalTypeIdentifier::new(convert_from_pair(&type_name_pair), type_name_pair.as_str()),
            LocalIdentifier::new(convert_from_pair(&func_name_pair), func_name_pair.as_str()),
            args,
        ))
    }

    /*
    fn parse_qualified_identifier(
        &self,
        pair: Pair<Rule>,
    ) -> Result<QualifiedTypeIdentifier, Error<Rule>> {
        let mut inner_pairs = pair.clone().into_inner();

        // Get module path if it exists
        let module_path = inner_pairs
            .next()
            .filter(|p| p.as_rule() == Rule::module_path)
            .map(|p| self.parse_module_path(p))
            .unwrap_or_default();

        // Get the type identifier
        if let Some(type_pair) = inner_pairs.next() {
            let type_identifier =
                LocalTypeIdentifier::new(convert_from_pair(&type_pair), type_pair.as_str());
            return Ok(QualifiedTypeIdentifier::new(type_identifier, module_path));
        }

        Err(self.create_error("Expected type identifier", pair.as_span()))
    }

     */

    fn parse_type(&self, pair: Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
        match pair.as_rule() {
            Rule::type_name => {
                let mut inner = pair.clone().into_inner();
                if let Some(inner_pair) = inner.next() {
                    self.parse_type(inner_pair)
                } else {
                    self.parse_type_from_str(&pair)
                }
            }
            Rule::built_in_type => self.parse_type_from_str(&pair),
            Rule::qualified_type_identifier => {
                let qualified_identifier = self.parse_qualified_type_identifier(pair)?;
                Ok(Type::TypeReference(qualified_identifier))
            }
            Rule::tuple_type => {
                let mut types = Vec::new();
                for type_pair in pair.into_inner() {
                    let type_value = self.parse_type(type_pair)?;
                    types.push(type_value);
                }
                Ok(Type::Tuple(types))
            }
            _ => Err(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: format!("Unexpected type rule: {:?}", pair.as_rule()),
                },
                pair.as_span(),
            )),
        }
    }

    fn parse_type_from_str(&self, pair: &Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
        match pair.as_str() {
            "Int" => Ok(Type::Int),
            "Float" => Ok(Type::Float),
            "String" => Ok(Type::String),
            "Bool" => Ok(Type::Bool),
            _ => Ok(Type::Struct(QualifiedTypeIdentifier::new(
                LocalTypeIdentifier::new(convert_from_pair(&pair), pair.as_str()),
                [].to_vec(),
            ))),
        }
    }

    #[allow(unused)]
    fn parse_local_type_identifier(
        &self,
        pair: Pair<Rule>,
    ) -> Result<LocalTypeIdentifier, Error<Rule>> {
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error(
                &format!("Expected type_identifier, found {:?}", pair.as_rule()),
                pair.as_span(),
            ));
        }
        Ok(LocalTypeIdentifier::new(
            convert_from_pair(&pair),
            pair.as_str(),
        ))
    }

    fn parse_local_type_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalTypeIdentifier, Error<Rule>> {
        let pair = Self::next_pair(pairs)?;
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error(
                &format!("Expected type_identifier, found {:?}", pair.as_rule()),
                pair.as_span(),
            ));
        }
        Ok(LocalTypeIdentifier::new(
            convert_from_pair(&pair),
            pair.as_str(),
        ))
    }

    fn parse_type_alias(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse the alias name
        let name_pair = Self::next_pair(&mut inner)?;
        if name_pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error(
                &format!("Expected type identifier, found {:?}", name_pair.as_rule()),
                name_pair.as_span(),
            ));
        }
        let name = LocalTypeIdentifier::new(convert_from_pair(&name_pair), name_pair.as_str());

        // Parse the target type
        let target_type = self.parse_type(Self::next_pair(&mut inner)?)?;

        Ok(Definition::TypeAlias(name, target_type))
    }

    fn parse_enum_def(&self, pair: Pair<Rule>) -> Result<Definition, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);

        // Parse enum name
        let name = self.parse_local_type_identifier_next(&mut inner)?;
        let mut variants = SeqMap::new();

        // Parse enum variants if present
        if let Some(variants_pair) = inner.next() {
            if variants_pair.as_rule() == Rule::enum_variants {
                for variant_pair in Self::get_inner_pairs(&variants_pair) {
                    if variant_pair.as_rule() == Rule::enum_variant {
                        let (ident, variant) =
                            self.parse_enum_variant(self.next_inner_pair(variant_pair)?)?;
                        variants
                            .insert(ident, variant)
                            .expect("duplicate enum variant"); // TODO: Should return as Err
                    }
                }
            }
        }

        Ok(Definition::EnumDef(name, variants))
    }

    fn parse_enum_variant(
        &self,
        pair: Pair<Rule>,
    ) -> Result<(LocalTypeIdentifier, EnumVariant), Error<Rule>> {
        match pair.as_rule() {
            Rule::simple_variant => {
                let name = self.next_inner_pair(pair.clone())?;
                Ok((
                    LocalTypeIdentifier::new(convert_from_pair(&pair), name.as_str()),
                    EnumVariant::Simple,
                ))
            }
            Rule::tuple_variant => {
                let mut inner = Self::get_inner_pairs(&pair);
                let name = LocalTypeIdentifier::new(
                    convert_from_pair(&pair),
                    &Self::expect_type_identifier(&mut inner)?,
                );

                let mut types = Vec::new();
                while let Some(type_pair) = inner.next() {
                    types.push(self.parse_type(type_pair)?);
                }

                Ok((name, EnumVariant::Tuple(types)))
            }
            Rule::struct_variant => {
                let mut inner = Self::get_inner_pairs(&pair);
                let name = LocalTypeIdentifier::new(
                    convert_from_pair(&pair),
                    &Self::expect_type_identifier(&mut inner)?,
                );

                let mut fields = SeqMap::new();
                while let Some(field_pair) = inner.next() {
                    let mut field_inner = Self::get_inner_pairs(&field_pair);
                    let field_name = LocalIdentifier::new(
                        convert_from_pair(&pair),
                        &Self::expect_identifier(&mut field_inner)?,
                    );
                    let field_type = self.parse_type(Self::next_pair(&mut field_inner)?)?;
                    fields
                        .insert(IdentifierName(field_name.text), field_type)
                        .expect("duplicate field field"); // TODO: Handle as Err()
                }

                let anon = AnonymousStruct::new(fields);
                Ok((name, EnumVariant::Struct(anon)))
            }
            _ => Err(self.create_error(
                &format!("Unknown enum variant type: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn parse_match_expr(&self, pair: Pair<Rule>) -> Result<Expression, Error<Rule>> {
        let mut inner = Self::get_inner_pairs(&pair);
        let value = self.parse_expression(Self::next_pair(&mut inner)?)?;
        let arms_pair = Self::next_pair(&mut inner)?;
        let mut arms = Vec::new();

        for arm_pair in Self::get_inner_pairs(&arms_pair) {
            if arm_pair.as_rule() == Rule::match_arm {
                let mut arm_inner = Self::get_inner_pairs(&arm_pair);
                let pattern = self.parse_match_pattern(Self::next_pair(&mut arm_inner)?)?;

                // Handle both block and direct expression cases
                let expr = match Self::next_pair(&mut arm_inner)? {
                    block if block.as_rule() == Rule::match_block => {
                        let mut statements = Vec::new();

                        for stmt in Self::get_inner_pairs(&block) {
                            match stmt.as_rule() {
                                Rule::statement => {
                                    statements.push(self.parse_statement_to_control(stmt)?);
                                }
                                Rule::expression => {
                                    //last_expr = Some(self.parse_expression(stmt)?);
                                }
                                _ => {
                                    return Err(self.create_error(
                                        &format!(
                                            "Unexpected rule in match block: {:?}",
                                            stmt.as_rule()
                                        ),
                                        stmt.as_span(),
                                    ))
                                }
                            }
                        }

                        Expression::Block(statements)
                    }
                    expr => self.parse_expression(expr)?,
                };

                arms.push(MatchArm {
                    pattern,
                    expression: expr,
                });
            }
        }

        if arms.is_empty() {
            return Err(self.create_error(
                "Match expression must have at least one arm",
                pair.as_span(),
            ));
        }

        Ok(Expression::Match(Box::new(value), arms))
    }
    fn parse_match_pattern(&self, pair: Pair<Rule>) -> Result<Pattern, Error<Rule>> {
        match pair.as_rule() {
            Rule::match_pattern => {
                let inner = self.next_inner_pair(pair)?;
                self.parse_match_pattern(inner)
            }
            Rule::enum_pattern => {
                let mut inner = Self::get_inner_pairs(&pair);
                let variant = LocalTypeIdentifier::new(
                    convert_from_pair(&pair),
                    &Self::expect_type_identifier(&mut inner)?,
                );

                // Check for optional tuple or struct pattern
                if let Some(pattern) = inner.next() {
                    match pattern.as_rule() {
                        Rule::enum_pattern_tuple => {
                            let mut fields = Vec::new();
                            for field in Self::get_inner_pairs(&pattern) {
                                if field.as_rule() == Rule::pattern_field {
                                    fields.push(LocalIdentifier::new(
                                        convert_from_pair(&pair),
                                        field.as_str(),
                                    ));
                                }
                            }
                            Ok(Pattern::EnumTuple(variant, fields))
                        }
                        Rule::enum_pattern_struct => {
                            let mut fields = Vec::new();
                            for field in Self::get_inner_pairs(&pattern) {
                                if field.as_rule() == Rule::struct_pattern_field {
                                    fields.push(LocalIdentifier::new(
                                        convert_from_pair(&pair),
                                        field.as_str(),
                                    ));
                                }
                            }
                            Ok(Pattern::EnumStruct(variant, fields))
                        }
                        _ => Err(self.create_error(
                            &format!("Unexpected enum pattern type: {:?}", pattern.as_rule()),
                            pattern.as_span(),
                        )),
                    }
                } else {
                    // Simple variant with no fields
                    Ok(Pattern::EnumSimple(variant))
                }
            }
            Rule::literal => {
                let lit = self.parse_literal(pair)?;
                Ok(Pattern::Literal(lit))
            }
            Rule::tuple_pattern => {
                let mut fields = Vec::new();
                for field in Self::get_inner_pairs(&pair) {
                    if field.as_rule() == Rule::pattern_field {
                        fields.push(LocalTypeIdentifier::new(
                            convert_from_pair(&pair),
                            field.as_str(),
                        ));
                    }
                }
                Ok(Pattern::Tuple(fields))
            }
            Rule::struct_pattern => {
                let mut fields = Vec::new();
                for field in Self::get_inner_pairs(&pair) {
                    if field.as_rule() == Rule::pattern_field {
                        fields.push(LocalTypeIdentifier::new(
                            convert_from_pair(&pair),
                            field.as_str(),
                        ));
                    }
                }
                Ok(Pattern::Struct(fields))
            }
            Rule::wildcard_pattern => Ok(Pattern::Wildcard),
            Rule::identifier_pattern => Ok(Pattern::VariableAssignment(Variable::new(
                pair.as_str(),
                false,
            ))),
            _ => Err(self.create_error(
                &format!("Unknown match pattern type: {:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }
}

fn convert_from_pair(pair: &Pair<Rule>) -> Node {
    let pair_span = pair.as_span();
    let span = Span {
        start: Position {
            offset: pair_span.start(),
            line: pair_span.start_pos().line_col().0,
            column: pair_span.start_pos().line_col().1,
        },
        end: Position {
            offset: pair_span.end(),
            line: pair_span.end_pos().line_col().0,
            column: pair_span.end_pos().line_col().1,
        },
    };

    Node { span }
}
