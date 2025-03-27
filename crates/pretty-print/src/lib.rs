use seq_map::SeqMap;
use std::fmt::{Display, Formatter};
use swamp_script_modules::modules::{ModuleRef, Modules};
use swamp_script_modules::symtbl::{FuncDef, Symbol, SymbolTable, TypeGenerator};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, AssociatedImpls, MutOrImmutableExpression, Postfix, PostfixKind,
    SingleLocationExpression, SingleLocationExpressionKind, SingleMutLocationExpression,
};

use swamp_script_types::*;

use swamp_script_source_map_lookup::SourceMapLookup;
use yansi::{Color, Paint};

pub struct SourceMapDisplay<'a> {
    pub source_map: &'a dyn SourceMapLookup,
}

impl SourceMapDisplay<'_> {
    pub fn set_color(on: bool) {
        if on {
            yansi::enable();
        } else {
            yansi::disable();
        }
    }
    const fn get_color_from_symbol(symbol: &Symbol) -> Color {
        match symbol {
            Symbol::Type(_) => Color::BrightYellow,
            Symbol::Module(_) => Color::BrightGreen,
            Symbol::PackageVersion(_) => Color::BrightMagenta,
            Symbol::Constant(_) => Color::BrightCyan,
            Symbol::FunctionDefinition(_) => Color::Cyan,
            Symbol::Alias(_) => Color::Red,
            Symbol::Blueprint(_) => Color::Green,
            Symbol::TypeGenerator(_) => Color::BrightGreen,
        }
    }

    pub(crate) fn show_impls(
        &self,
        f: &mut Formatter,
        impls: &AssociatedImpls,
        tabs: usize,
    ) -> std::fmt::Result {
        writeln!(f)?;
        Self::new_line_and_tab(f, tabs)?;
        for (type_number, associated_impl) in &impls.functions {
            writeln!(f, "{type_number}: ")?;

            for (name, associated_fn) in &associated_impl.functions {
                Self::new_line_and_tab(f, tabs + 1)?;
                write!(f, "{}:", name.blue())?;

                self.show_function(f, associated_fn, tabs + 1)?;
                writeln!(f)?;
            }
        }

        Ok(())
    }

    /// # Errors
    ///
    pub fn pretty_print_symbol(
        &self,
        f: &mut Formatter<'_>,
        name: &str,
        symbol: &Symbol,
        tabs: usize,
    ) -> std::fmt::Result {
        let tab_str = "..".repeat(tabs);

        let color = Self::get_color_from_symbol(symbol);
        write!(f, "{tab_str}{}: ", name.paint(color))?;

        match symbol {
            Symbol::Type(ty) => self.show_type(f, ty, tabs + 1),
            Symbol::Module(module_ref) => {
                write!(f, "{module_ref:?}")
            }
            Symbol::Constant(constant_ref) => self.show_constant(f, constant_ref, tabs),
            Symbol::FunctionDefinition(func_def) => match func_def {
                FuncDef::Internal(internal_fn) => self.show_internal_function(f, internal_fn, tabs),
                FuncDef::Intrinsic(intrinsic_fn) => {
                    self.show_intrinsic_function(f, intrinsic_fn, tabs)
                }
                FuncDef::External(external_fn) => {
                    self.show_external_function_declaration(f, external_fn, tabs)
                }
            },
            Symbol::Alias(alias_type_ref) => self.show_alias(f, alias_type_ref),
            Symbol::PackageVersion(version) => {
                write!(f, "version {version}")
            }
            Symbol::Blueprint(blueprint_ref) => self.show_blueprint(f, blueprint_ref, tabs),
            Symbol::TypeGenerator(generator) => self.show_type_generator(f, generator, tabs),
        }
    }

    /// # Errors
    ///
    pub fn pretty_print_symbol_table(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &SymbolTable,
        tabs: usize,
    ) -> std::fmt::Result {
        writeln!(f)?;
        for (name, symbol) in symbol_table.symbols() {
            self.pretty_print_symbol(f, name, symbol, tabs)?;
            writeln!(f)?;
        }
        Ok(())
    }

    fn show_blueprint(
        &self,
        f: &mut Formatter,
        blueprint: &ParameterizedTypeBlueprint,
        tabs: usize,
    ) -> std::fmt::Result {
        self.show_type_variables(f, &blueprint.type_variables, tabs)?;
        write!(f, " ")?;
        self.show_blueprint_kind(f, &blueprint.kind, tabs)
    }

    fn show_type_generator(
        &self,
        f: &mut Formatter,
        generator: &TypeGenerator,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{:?}", generator)
    }
}

pub struct SymbolTableDisplay<'a> {
    pub symbol_table: &'a SymbolTable,
    pub source_map_display: &'a SourceMapDisplay<'a>,
}

impl Display for SymbolTableDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.source_map_display
            .pretty_print_symbol_table(f, self.symbol_table, 0)
    }
}

pub struct ExpressionDisplay<'a> {
    pub expression: &'a Expression,
    pub source_map_display: &'a SourceMapDisplay<'a>,
}

impl Display for ExpressionDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.source_map_display
            .show_expression(f, self.expression, 0)
    }
}

pub struct ImplsDisplay<'a> {
    pub all_impls: &'a AssociatedImpls,
    pub source_map: &'a SourceMapDisplay<'a>,
}

impl Display for ImplsDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.source_map.show_impls(f, self.all_impls, 0)
    }
}

pub struct ModulesDisplay<'a> {
    pub resolved_modules: &'a Modules,
    pub source_map: &'a dyn SourceMapLookup,
}

impl Display for ModulesDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mods = self.resolved_modules;

        for (name, _module) in mods.modules() {
            writeln!(f, "{}", "===================".green())?;
            writeln!(f, "{:?}: ", name.green())?;

            /*
            let mod_borrow = module;
            let ns = &mod_borrow.namespace;
            self.show_constants(f, ns.constants())?;
            self.show_aliases(f, ns.aliases())?;
            self.show_namespace_links(f, ns.namespaces())?;

            self.show_structs(f, ns.structs())?;
            self.show_enums(f, ns.enums())?;
            self.show_internal_functions(f, ns.internal_functions())?;
            self.show_external_function_declarations(f, ns.external_function_declarations())?;

             */
        }

        Ok(())
    }
}

impl SourceMapDisplay<'_> {
    /// # Errors
    ///
    pub fn show_structs(
        &self,
        f: &mut Formatter<'_>,
        structs: &SeqMap<String, NamedStructType>,
        tabs: usize,
    ) -> std::fmt::Result {
        for (_struct_name, struct_type) in structs {
            writeln!(f, "{}: ", struct_type.assigned_name.yellow())?;
            self.show_struct(f, struct_type, tabs)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_struct(
        &self,
        f: &mut Formatter<'_>,
        struct_type: &NamedStructType,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{} ", struct_type.assigned_name.bright_magenta())?;

        self.show_anon_struct(f, &struct_type.anon_struct_type, tabs)?;

        Ok(())
    }

    pub fn show_anon_struct(
        &self,
        f: &mut Formatter<'_>,
        anon_struct_type: &AnonymousStructType,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{{")?;
        for (field_name, field) in &anon_struct_type.field_name_sorted_fields {
            Self::new_line_and_tab(f, tabs + 1)?;

            write!(f, "{}: ", field_name.cyan())?;
            self.show_type(f, &field.field_type, tabs + 1)?;
            write!(f, "{}", ", ".white())?;
        }
        Self::new_line_and_tab(f, tabs)?;

        write!(f, "}}")
    }

    /// # Errors
    ///
    pub fn show_constants(
        &self,
        f: &mut Formatter<'_>,
        constants: &SeqMap<String, ConstantRef>,
        tabs: usize,
    ) -> std::fmt::Result {
        for (_constant_name, constant) in constants {
            //            writeln!(f, "  {}:  ", constant.assigned_name.yellow())?;
            self.show_constant(f, constant, tabs)?;
            writeln!(f)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_constant(
        &self,
        f: &mut Formatter<'_>,
        constant: &ConstantRef,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {} = ",
            constant.assigned_name.blue(),
            constant.resolved_type.bright_yellow()
        )?;

        self.show_expression(f, &constant.expr, tabs)?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_aliases(
        &self,
        f: &mut Formatter<'_>,
        aliases: &SeqMap<String, AliasType>,
    ) -> std::fmt::Result {
        for (_constant_name, alias) in aliases {
            //            writeln!(f, "  {}:  ", constant.assigned_name.yellow())?;
            self.show_alias(f, alias)?;
            writeln!(f)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_alias(&self, f: &mut Formatter<'_>, alias: &AliasType) -> std::fmt::Result {
        write!(f, "{} ==> ", alias.assigned_name.blue(),)?;

        self.show_type_short(f, &alias.referenced_type, 0)?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_namespace_links(
        &self,
        f: &mut Formatter<'_>,
        aliases: &SeqMap<String, ModuleRef>,
    ) -> std::fmt::Result {
        for (link_name, namespace) in aliases {
            write!(f, "  {} ===>  ", link_name.yellow())?;
            self.show_module_basic_info(f, namespace)?;
            writeln!(f)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_module_path(&self, f: &mut Formatter<'_>, path: &[String]) -> std::fmt::Result {
        for (index, item) in path.iter().enumerate() {
            if index > 0 {
                write!(f, "{}", "::".bright_white())?;
            }

            write!(f, "{}", item.blue())?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_module_basic_info(
        &self,
        f: &mut Formatter<'_>,
        ns: &ModuleRef,
    ) -> std::fmt::Result {
        self.show_module_path(f, &ns.symbol_table.module_path())
    }

    fn show_mut_or_not_expression(
        &self,
        f: &mut Formatter,
        mut_expr: &MutOrImmutableExpression,
        tabs: usize,
    ) -> std::fmt::Result {
        if mut_expr.is_mutable.is_some() {
            write!(f, "{}", "mut".red())?;
        }

        self.show_argument(f, &mut_expr.expression_or_location, tabs)
    }

    #[allow(clippy::too_many_lines)]
    fn show_expression(
        &self,
        f: &mut Formatter,
        expr: &Expression,
        tabs: usize,
    ) -> std::fmt::Result {
        match &expr.kind {
            ExpressionKind::ConstantAccess(a) => {
                write!(f, "{}", a.assigned_name.magenta())
            }
            ExpressionKind::VariableAccess(var) => self.show_variable(f, var),

            ExpressionKind::InternalFunctionAccess(internal_function) => {
                write!(
                    f,
                    "[fn:{}]",
                    //self.source_map.get_text(&internal_function.name.0)
                    internal_function.assigned_name
                )
            }
            ExpressionKind::ExternalFunctionAccess(_) => {
                write!(f, "ExternalFunctionAccess()")
            }
            ExpressionKind::BinaryOp(_) => {
                write!(f, "BinaryOp()")
            }
            ExpressionKind::UnaryOp(_) => {
                write!(f, "UnaryOp()")
            }
            ExpressionKind::PostfixChain(base_expr, postfixes) => {
                self.show_expression(f, base_expr, tabs)?;
                for postfix in postfixes {
                    write!(f, " . ")?;
                    self.show_postfix(f, postfix, tabs)?;
                }
                Ok(())
            }
            ExpressionKind::CoerceOptionToBool(_) => {
                write!(f, "CoerceOptionToBool()")
            }
            ExpressionKind::InterpolatedString(_) => {
                write!(f, "InterpolatedString()")
            }
            ExpressionKind::VariableDefinition(a, b) => {
                write!(
                    f,
                    "let {} = ",
                    self.source_map.get_text(&a.name).bright_blue()
                )?;
                self.show_mut_or_not_expression(f, b, tabs)
            }
            ExpressionKind::VariableReassignment(_, _) => {
                write!(f, "VariableReassignment()")
            }
            ExpressionKind::StructInstantiation(struct_literal) => {
                self.show_struct_literal(f, struct_literal, tabs)
            }

            ExpressionKind::Literal(basic_literal) => {
                self.show_basic_literal(f, basic_literal, tabs)
            }
            ExpressionKind::Option(_) => {
                write!(f, "Option()")
            }
            ExpressionKind::ForLoop(_, _, _) => {
                write!(f, "ForLoop()")
            }
            ExpressionKind::WhileLoop(_, _) => {
                write!(f, "WhileLoop()")
            }
            ExpressionKind::Block(expressions) => {
                for expression in expressions {
                    Self::new_line_and_tab(f, tabs + 1)?;
                    self.show_expression(f, expression, tabs + 1)?;
                }
                Ok(())
            }
            ExpressionKind::Match(_) => {
                write!(f, "Match()")
            }
            ExpressionKind::Guard(_) => {
                write!(f, "Guard()")
            }
            ExpressionKind::If(_, _, _) => {
                write!(f, "If()")
            }
            ExpressionKind::When(_, _, _) => {
                write!(f, "When()")
            }
            ExpressionKind::TupleDestructuring(_, _, _) => {
                write!(f, "TupleDestructuring()")
            }
            ExpressionKind::Assignment(mut_location, expression) => {
                self.show_mut_location(f, &mut_location, tabs)?;
                write!(f, "=")?;
                self.show_expression(f, expression, tabs)
            }
            ExpressionKind::CompoundAssignment(_, _, _) => {
                write!(f, "CompoundAssignment()")
            }
            ExpressionKind::IntrinsicFunctionAccess(intrinsic_func_def) => {
                write!(f, "intrinsic_access {intrinsic_func_def:?}")
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_func_def, args) => {
                write!(f, "intrinsic_call{intrinsic_func_def:?}")?;
                self.show_arguments(f, args, tabs + 1)
            }
            _ => todo!(),
        }
    }

    fn show_variable(&self, f: &mut Formatter, var: &VariableRef) -> std::fmt::Result {
        if var.name.span.file_id == 0 {
            return Ok(());
        }
        write!(f, "{}", self.source_map.get_text(&var.name))
    }

    fn show_basic_literal(
        &self,
        f: &mut Formatter,
        basic_literal: &Literal,
        tabs: usize,
    ) -> std::fmt::Result {
        match basic_literal {
            Literal::FloatLiteral(fp) => {
                write!(f, "{}f", fp.bright_magenta())
            }
            Literal::NoneLiteral => {
                write!(f, "{}", "none".green())
            }
            Literal::IntLiteral(i) => {
                write!(f, "{}i", i.bright_cyan())
            }
            Literal::StringLiteral(s) => {
                write!(f, "'{}'", s.bright_red())
            }
            Literal::BoolLiteral(b) => {
                write!(f, "{}", b.bright_white())
            }
            Literal::EnumVariantLiteral(enum_type, variant, data) => {
                write!(f, "{:?}::{:?}", enum_type.blue(), data.green())
            }
            Literal::TupleLiteral(_tuple_type, expressions) => {
                write!(f, "(")?;
                self.show_expressions(f, expressions, tabs + 1)?;
                write!(f, ")")
            }
            Literal::Slice(_slice_type, expressions) => {
                write!(f, "[")?;
                self.show_expressions(f, expressions, tabs + 1)?;
                write!(f, "]")
            }
            Literal::SlicePair(_slice_pair_type, pairs) => {
                write!(f, "[|")?;
                for (key, value) in pairs {
                    self.show_expression(f, key, tabs + 1)?;
                    write!(f, "{}", ":".bright_blue())?;
                    self.show_expression(f, value, tabs + 1)?;
                }
                write!(f, "|]")
            }
        }
    }

    fn show_expressions(
        &self,
        f: &mut Formatter,
        expressions: &[Expression],
        tabs: usize,
    ) -> std::fmt::Result {
        for (i, expr) in expressions.iter().enumerate() {
            if i > 0 {
                writeln!(f, ", ")?;
            }
            self.show_expression(f, expr, tabs + 1)?;
        }

        if !expressions.is_empty() {
            writeln!(f)?;
        }

        Ok(())
    }

    fn show_postfix(&self, f: &mut Formatter, postfix: &Postfix, tabs: usize) -> std::fmt::Result {
        match &postfix.kind {
            PostfixKind::StructField(struct_type, field) => {
                let name = struct_type
                    .field_name_sorted_fields
                    .keys()
                    .collect::<Vec<_>>()[*field]
                    .clone();
                write!(f, ".{}", name.bright_blue())
            }
            PostfixKind::MemberCall(_function_ref, b) => write!(f, "membercall {b:?}"),
            PostfixKind::FunctionCall(arguments) => {
                write!(f, "[call:")?;
                self.show_arguments(f, arguments, tabs + 1)?;
                write!(f, "]")
            }
            PostfixKind::OptionUnwrap => todo!(),
            PostfixKind::NoneCoalesce(_) => todo!(),
            /*
            PostfixKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                write!(f, "[intrinsic_call_ex: {intrinsic_fn:?}")?;
                self.show_arguments(f, arguments, tabs + 1)?;
                write!(f, "]")
            }


             */
            PostfixKind::IntrinsicCall(intrinsic_fn, arguments) => {
                write!(f, "[intrinsic_call: {intrinsic_fn:?}")?;
                self.show_expressions(f, arguments, tabs + 1)?;
                write!(f, "]")
            }
        }
    }

    fn show_blueprint_kind(
        &self,
        f: &mut Formatter,
        kind: &ParameterizedTypeKind,
        tabs: usize,
    ) -> std::fmt::Result {
        match kind {
            ParameterizedTypeKind::Struct(struct_ref) => self.show_struct(f, &struct_ref, tabs),
            ParameterizedTypeKind::Enum(enum_ref) => self.show_enum_type_name(f, &enum_ref),
        }
    }

    fn show_generic(
        &self,
        f: &mut Formatter,
        parameterized_type: &ParameterizedTypeBlueprint,
        types: &[Type],
        tabs: usize,
    ) -> std::fmt::Result {
        self.show_blueprint_kind(f, &parameterized_type.kind, tabs)?;
        write!(f, "{}", "<".bright_white())?;

        self.show_types(f, types, tabs)?;

        write!(f, "{}", ">".bright_white())?;
        Ok(())
    }

    fn show_parameterized_like(
        &self,
        f: &mut Formatter,
        name: &str,
        types: &[Type],
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{}<", name.bright_magenta())?;

        self.show_types(f, &types, tabs)?;

        write!(f, ">")?;
        Ok(())
    }

    fn show_types(&self, f: &mut Formatter, types: &[Type], tabs: usize) -> std::fmt::Result {
        for (index, ty) in types.iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            self.show_type(f, ty, tabs)?;
        }

        Ok(())
    }

    fn show_type_short(
        &self,
        f: &mut Formatter,
        resolved_type: &Type,
        tabs: usize,
    ) -> std::fmt::Result {
        match resolved_type {
            Type::Int => write!(f, "{}", "Int".bright_blue()),
            Type::Float => write!(f, "{}", "Float".bright_blue()),
            Type::String => write!(f, "{}", "String".bright_blue()),
            Type::Bool => write!(f, "{}", "Bool".bright_blue()),
            Type::Unit => write!(f, "{}", "()".bright_blue()),

            Type::Tuple(tuple_type) => {
                write!(f, "(")?;
                for (index, item_type) in tuple_type.iter().enumerate() {
                    if index > 0 {
                        write!(f, "{}", ", ".bright_black())?;
                    }
                    self.show_type_short(f, item_type, tabs + 1)?;
                }
                write!(f, ")")
            }

            Type::NamedStruct(struct_ref) => write!(f, "{}", struct_ref.assigned_name.blue()),
            Type::AnonymousStruct(struct_ref) => self.show_anon_struct(f, struct_ref, tabs),

            Type::Enum(enum_type) => write!(f, "{}", enum_type.assigned_name.bright_cyan()),
            Type::Function(signature) => write!(f, "function {signature}"),
            Type::Optional(base_type) => {
                self.show_type_short(f, base_type, tabs);
                write!(f, "{}", "?".yellow())
            }
            Type::MutableReference(base_type) => {
                write!(f, "{}", "mut ref".red());
                self.show_type_short(f, base_type, tabs)
            }
            Type::External(external_type) => write!(f, "External {}", external_type.type_name),
            Type::Generic(blueprint, concrete_types) => {
                self.show_generic(f, blueprint, concrete_types, tabs)
            }
            Type::Blueprint(blueprint) => self.show_blueprint(f, blueprint, tabs),
            Type::Slice(value) => todo!(),
            Type::SlicePair(key, value) => todo!(),
            Type::Variable(var) => self.show_type_variable(f, var, tabs),
            Type::Never => write!(f, "!"),
        }
    }

    fn show_type(&self, f: &mut Formatter, resolved_type: &Type, tabs: usize) -> std::fmt::Result {
        match resolved_type {
            Type::Int => write!(f, "{}", "Int".bright_blue()),
            Type::Float => write!(f, "{}", "Float".bright_blue()),
            Type::String => write!(f, "{}", "String".bright_blue()),
            Type::Bool => write!(f, "{}", "Bool".bright_blue()),
            Type::Unit => write!(f, "{}", "()".bright_blue()),

            Type::Tuple(tuple_type) => {
                write!(f, "(")?;
                for (index, item_type) in tuple_type.iter().enumerate() {
                    if index > 0 {
                        write!(f, "{}", ", ".bright_black())?;
                    }
                    self.show_type(f, item_type, tabs + 1)?;
                }
                write!(f, ")")
            }

            Type::NamedStruct(struct_ref) => self.show_struct(f, struct_ref, tabs),
            Type::AnonymousStruct(struct_ref) => self.show_anon_struct(f, struct_ref, tabs),

            Type::Enum(enum_type) => write!(f, "{}", enum_type.assigned_name),
            Type::Function(signature) => write!(f, "function {signature}"),
            Type::Optional(base_type) => write!(f, "{}?", base_type.yellow()),
            Type::MutableReference(base_type) => {
                write!(f, "{}", "mut ref".red());
                self.show_type_short(f, base_type, tabs)
            }
            Type::External(external_type) => write!(f, "External {}", external_type.type_name),
            Type::Generic(blueprint, concrete_types) => {
                self.show_generic(f, blueprint, concrete_types, tabs)
            }
            Type::Blueprint(blueprint) => self.show_blueprint(f, blueprint, tabs),
            Type::Slice(value) => todo!(),
            Type::SlicePair(key, value) => todo!(),
            Type::Variable(var) => self.show_type_variable(f, var, tabs),
            Type::Never => write!(f, "!"),
        }
    }

    pub fn show_external_function_declaration(
        &self,
        f: &mut Formatter,
        external_function: &ExternalFunctionDefinition,
        tabs: usize,
    ) -> std::fmt::Result {
        self.show_signature(f, &external_function.signature, tabs)
    }

    pub fn show_external_function_declarations(
        &self,
        f: &mut Formatter,
        external_functions: &SeqMap<String, ExternalFunctionDefinitionRef>,
        tabs: usize,
    ) -> std::fmt::Result {
        for (_name, func) in external_functions {
            write!(f, "{}", func.assigned_name)?;
            self.show_external_function_declaration(f, func, tabs)?;
            writeln!(f)?;
        }
        Ok(())
    }
    pub fn show_parameter(
        &self,
        f: &mut Formatter,
        parameter_type: &TypeForParameter,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}:",
            if parameter_type.is_mutable {
                "mut ".red()
            } else {
                "".white()
            },
            parameter_type.name.bright_green(),
        )?;

        self.show_type_short(f, &parameter_type.resolved_type, tabs)
    }

    pub fn show_parameters_and_type(
        &self,
        f: &mut Formatter,
        parameter_types: &[TypeForParameter],
        tabs: usize,
    ) -> std::fmt::Result {
        for (index, parameter_type) in parameter_types.iter().enumerate() {
            if index > 0 {
                write!(f, "{}", ", ".white())?;
            }
            self.show_parameter(f, parameter_type, tabs)?;
        }
        Ok(())
    }

    pub fn show_signature(
        &self,
        f: &mut Formatter,
        function_type_signature: &Signature,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", "(".bright_green())?;

        self.show_parameters_and_type(f, &function_type_signature.parameters, tabs)?;

        write!(f, "{}", ")".bright_green())?;

        write!(f, " {} ", "->".bright_green())?;

        self.show_type_short(f, &function_type_signature.return_type, tabs)
    }

    pub fn new_line_and_tab(f: &mut Formatter, tabs: usize) -> std::fmt::Result {
        let tab_str = "..".repeat(tabs);
        writeln!(f)?;
        write!(f, "{}", tab_str)
    }

    pub fn show_internal_function(
        &self,
        f: &mut Formatter,
        internal_func: &InternalFunctionDefinition,
        tabs: usize,
    ) -> std::fmt::Result {
        self.show_signature(f, &internal_func.signature, tabs)?;
        Self::new_line_and_tab(f, tabs)?;
        self.show_expression(f, &internal_func.body, tabs)
    }

    fn show_intrinsic_function(
        &self,
        f: &mut Formatter,
        intrinsic_fn: &IntrinsicFunctionDefinitionRef,
        tabs: usize,
    ) -> std::fmt::Result {
        self.show_signature(f, &intrinsic_fn.signature, tabs)?;
        write!(f, " ==> {}", intrinsic_fn.intrinsic.blue())
    }

    pub fn show_internal_functions(
        &self,
        f: &mut Formatter,
        internal_functions: &SeqMap<String, InternalFunctionDefinitionRef>,
        tabs: usize,
    ) -> std::fmt::Result {
        for (_name, func) in internal_functions {
            write!(f, "{}", self.source_map.get_text(&func.name.0).bright_red())?;
            Self::new_line_and_tab(f, tabs + 1)?;
            self.show_internal_function(f, func, tabs + 1)?;
        }
        Ok(())
    }

    fn show_function(&self, f: &mut Formatter, func: &Function, tabs: usize) -> std::fmt::Result {
        match func {
            Function::Internal(internal) => self.show_internal_function(f, internal, tabs),
            Function::External(external) => {
                self.show_external_function_declaration(f, external, tabs)
            }
        }
    }

    pub fn show_enum_type_name(&self, f: &mut Formatter, enum_type: &EnumType) -> std::fmt::Result {
        write!(f, "{}", &enum_type.assigned_name.bright_red())
    }
    pub fn show_enums(
        &self,
        f: &mut Formatter,
        enums: &SeqMap<String, EnumType>,
    ) -> std::fmt::Result {
        for (_name, enum_type) in enums {
            self.show_enum_type_name(f, enum_type)?;
        }
        Ok(())
    }

    fn show_arguments(
        &self,
        f: &mut Formatter,
        arguments: &Vec<ArgumentExpressionOrLocation>,
        tabs: usize,
    ) -> std::fmt::Result {
        for arg in arguments {
            self.show_argument(f, arg, tabs)?;
        }

        Ok(())
    }

    fn show_argument(
        &self,
        f: &mut Formatter,
        arg: &ArgumentExpressionOrLocation,
        tabs: usize,
    ) -> std::fmt::Result {
        match arg {
            ArgumentExpressionOrLocation::Expression(expression) => {
                self.show_expression(f, expression, tabs)
            }
            ArgumentExpressionOrLocation::Location(location) => {
                self.show_location(f, location, tabs)
            }
        }
    }

    fn show_mut_location(
        &self,
        f: &mut Formatter,
        location: &SingleMutLocationExpression,
        tabs: usize,
    ) -> std::fmt::Result {
        /*
                pub kind: SingleLocationExpressionKind,
        pub node: Node,
        pub ty: Type,

        pub starting_variable: VariableRef,
        pub access_chain: Vec<LocationAccess>,
             */

        write!(f, "{} ", "mut".red());

        self.show_location(f, &location.0, tabs)
    }

    fn show_location(
        &self,
        f: &mut Formatter,
        location: &SingleLocationExpression,
        tabs: usize,
    ) -> std::fmt::Result {
        /*
                pub kind: SingleLocationExpressionKind,
        pub node: Node,
        pub ty: Type,

        pub starting_variable: VariableRef,
        pub access_chain: Vec<LocationAccess>,
             */

        self.show_variable(f, &location.starting_variable)?;
        //  self.show_type(f, &location.ty, tabs);
        match location.kind {
            SingleLocationExpressionKind::MutStructFieldRef(..) => write!(f, "mut_struct_field"),
            SingleLocationExpressionKind::MutVariableRef => write!(f, "mut_var"),
        }
    }

    fn show_struct_literal(
        &self,
        f: &mut Formatter,
        struct_instantiation: &StructInstantiation,
        tabs: usize,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} {{",
            struct_instantiation.struct_type_ref.assigned_name.green()
        )?;
        for (index, expression) in &struct_instantiation.source_order_expressions {
            let borrow = struct_instantiation.struct_type_ref.clone();
            let (name, _struct_field) = borrow
                .anon_struct_type
                .field_name_sorted_fields
                .iter()
                .collect::<Vec<_>>()[*index];
            Self::new_line_and_tab(f, tabs + 1)?;
            write!(f, "{}: ", name.yellow())?;
            self.show_expression(f, expression, tabs)?;
            write!(f, "{}", ", ".white())?;
        }

        Self::new_line_and_tab(f, tabs)?;
        write!(f, "}}")?;

        Ok(())
    }

    fn show_type_variable(
        &self,
        f: &mut Formatter,
        type_variable: &str,
        _tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", type_variable.red())
    }

    fn show_type_variables(
        &self,
        f: &mut Formatter,
        type_variables: &[String],
        tabs: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", "<".white())?;
        for (index, variable) in type_variables.iter().enumerate() {
            if index > 0 {
                write!(f, "{}", ", ".white())?;
            }
            self.show_type_variable(f, variable, tabs)?;
        }
        write!(f, "{}", ">".white())
    }
}
