use seq_map::SeqMap;
use std::fmt::{Display, Formatter};
use swamp_script_core::prelude::SourceMapLookup;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{ResolvedPostfix, ResolvedPostfixKind};
use yansi::Paint;

pub struct ResolvedModulesDisplay<'a> {
    pub resolved_modules: &'a ResolvedModules,
    pub source_map: &'a dyn SourceMapLookup,
}

impl Display for ResolvedModulesDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let sm = self.source_map;
        let mods = self.resolved_modules;

        for (name, module) in &mods.modules {
            writeln!(f, "{}", "===================".green())?;
            writeln!(f, "{:?}: ", name.green())?;

            let mod_borrow = module.borrow();
            let ns = &mod_borrow.namespace.borrow();
            self.show_constants(f, ns.constants())?;
            self.show_aliases(f, ns.aliases())?;
            self.show_namespace_links(f, ns.namespaces())?;

            self.show_structs(f, ns.structs())?;
            self.show_enums(f, ns.enums())?;
            self.show_internal_functions(f, ns.internal_functions())?;
            self.show_external_function_declarations(f, ns.external_function_declarations())?;
        }

        Ok(())
    }
}

impl ResolvedModulesDisplay<'_> {
    /// # Errors
    ///
    pub fn show_structs(
        &self,
        f: &mut Formatter<'_>,
        structs: &SeqMap<String, ResolvedStructTypeRef>,
    ) -> std::fmt::Result {
        for (_struct_name, struct_type) in structs {
            writeln!(f, "  {}:  ", struct_type.borrow().assigned_name.yellow())?;
            self.show_struct(f, &struct_type.borrow())?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_struct(
        &self,
        f: &mut Formatter<'_>,
        struct_type: &ResolvedStructType,
    ) -> std::fmt::Result {
        for (field_name, field) in &struct_type.anon_struct_type.defined_fields {
            writeln!(
                f,
                "      {}:  {}",
                field_name.cyan(),
                field.field_type.magenta(),
            )?;
        }

        if !struct_type.functions.is_empty() {
            writeln!(f, "    {}", "functions".bright_red())?;
            for (func_name, func) in &struct_type.functions {
                write!(f, "      {}:  ", func_name.cyan())?;
                self.show_function(f, func)?;
                writeln!(f)?;
            }
        }

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_constants(
        &self,
        f: &mut Formatter<'_>,
        constants: &SeqMap<String, ResolvedConstantRef>,
    ) -> std::fmt::Result {
        for (_constant_name, constant) in constants {
            //            writeln!(f, "  {}:  ", constant.assigned_name.yellow())?;
            self.show_constant(f, constant)?;
            writeln!(f)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_constant(
        &self,
        f: &mut Formatter<'_>,
        constant: &ResolvedConstantRef,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {} = ",
            constant.assigned_name.blue(),
            constant.resolved_type.bright_yellow()
        )?;

        self.show_expression(f, &constant.expr)?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_aliases(
        &self,
        f: &mut Formatter<'_>,
        aliases: &SeqMap<String, ResolvedAliasTypeRef>,
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
    pub fn show_alias(
        &self,
        f: &mut Formatter<'_>,
        alias: &ResolvedAliasTypeRef,
    ) -> std::fmt::Result {
        write!(f, "{} ==> ", alias.assigned_name.blue(),)?;

        self.show_type(f, &alias.referenced_type)?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_namespace_links(
        &self,
        f: &mut Formatter<'_>,
        aliases: &SeqMap<String, ResolvedModuleNamespaceRef>,
    ) -> std::fmt::Result {
        for (link_name, namespace) in aliases {
            write!(f, "  {} ===>  ", link_name.yellow())?;
            self.show_namespace_basic_info(f, namespace)?;
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
    pub fn show_namespace_basic_info(
        &self,
        f: &mut Formatter<'_>,
        ns: &ResolvedModuleNamespaceRef,
    ) -> std::fmt::Result {
        self.show_module_path(f, &ns.borrow().path)
    }

    #[allow(clippy::too_many_lines)]
    fn show_expression(&self, f: &mut Formatter, expr: &ResolvedExpression) -> std::fmt::Result {
        let result = match &expr.kind {
            ResolvedExpressionKind::ConstantAccess(a) => {
                write!(f, "{}", a.assigned_name.magenta())
            }
            ResolvedExpressionKind::VariableAccess(var) => self.show_variable(f, var),
            ResolvedExpressionKind::FieldAccess(_, _) => {
                write!(f, "FieldAccess()")
            }
            ResolvedExpressionKind::ArrayAccess(_, _, _) => {
                write!(f, "ArrayAccess()")
            }
            ResolvedExpressionKind::MapIndexAccess(_, _, _) => {
                write!(f, "MapIndexAccess()")
            }
            ResolvedExpressionKind::StringRangeAccess(_, _) => {
                write!(f, "StringRangeAccess()")
            }
            ResolvedExpressionKind::ArrayRangeAccess(_, _) => {
                write!(f, "ArrayRangeAccess()")
            }
            ResolvedExpressionKind::InternalFunctionAccess(_) => {
                write!(f, "InternalFunctionAccess()")
            }
            ResolvedExpressionKind::ExternalFunctionAccess(_) => {
                write!(f, "ExternalFunctionAccess()")
            }
            ResolvedExpressionKind::MapAssignment(_, _, _) => {
                write!(f, "MapAssignment()")
            }
            ResolvedExpressionKind::BinaryOp(_) => {
                write!(f, "BinaryOp()")
            }
            ResolvedExpressionKind::UnaryOp(_) => {
                write!(f, "UnaryOp()")
            }
            ResolvedExpressionKind::PostfixChain(base_expr, postfixes) => {
                self.show_expression(f, base_expr)?;
                for postfix in postfixes {
                    self.show_postfix(f, postfix)?;
                }
                Ok(())
            }
            ResolvedExpressionKind::CoerceOptionToBool(_) => {
                write!(f, "CoerceOptionToBool()")
            }
            ResolvedExpressionKind::FunctionCall(_, _, _) => {
                write!(f, "FunctionCall()")
            }
            ResolvedExpressionKind::MemberCall(_) => {
                write!(f, "MemberCall()")
            }
            ResolvedExpressionKind::InterpolatedString(_) => {
                write!(f, "InterpolatedString()")
            }
            ResolvedExpressionKind::VariableDefinition(_, _) => {
                write!(f, "VariableDefinition()")
            }
            ResolvedExpressionKind::VariableReassignment(_, _) => {
                write!(f, "VariableReassignment()")
            }
            ResolvedExpressionKind::StructInstantiation(_) => {
                write!(f, "StructInstantiation()")
            }
            ResolvedExpressionKind::Array(_) => {
                write!(f, "Array()")
            }
            ResolvedExpressionKind::Tuple(_) => {
                write!(f, "Tuple()")
            }
            ResolvedExpressionKind::Literal(basic_literal) => {
                self.show_basic_literal(f, basic_literal)
            }
            ResolvedExpressionKind::Option(_) => {
                write!(f, "Option()")
            }
            ResolvedExpressionKind::Range(_, _, _) => {
                write!(f, "Range()")
            }
            ResolvedExpressionKind::ForLoop(_, _, _) => {
                write!(f, "ForLoop()")
            }
            ResolvedExpressionKind::WhileLoop(_, _) => {
                write!(f, "WhileLoop()")
            }
            ResolvedExpressionKind::Return(_) => {
                write!(f, "Return()")
            }
            ResolvedExpressionKind::Break => {
                write!(f, "Break")
            }
            ResolvedExpressionKind::Continue => {
                write!(f, "Continue")
            }
            ResolvedExpressionKind::Block(_) => {
                write!(f, "Block()")
            }
            ResolvedExpressionKind::Match(_) => {
                write!(f, "Match()")
            }
            ResolvedExpressionKind::Guard(_) => {
                write!(f, "Guard()")
            }
            ResolvedExpressionKind::If(_, _, _) => {
                write!(f, "If()")
            }
            ResolvedExpressionKind::When(_, _, _) => {
                write!(f, "When()")
            }
            ResolvedExpressionKind::TupleDestructuring(_, _, _) => {
                write!(f, "TupleDestructuring()")
            }
            ResolvedExpressionKind::Assignment(_, _) => {
                write!(f, "Assignment()")
            }
            ResolvedExpressionKind::AssignmentSlice(_, _) => {
                write!(f, "AssignmentSlice()")
            }
            ResolvedExpressionKind::CompoundAssignment(_, _, _) => {
                write!(f, "CompoundAssignment()")
            }
            ResolvedExpressionKind::ArrayExtend(_, _) => {
                write!(f, "ArrayExtend()")
            }
            ResolvedExpressionKind::ArrayPush(_, _) => {
                write!(f, "ArrayPush()")
            }
            ResolvedExpressionKind::SparseNew(_, _) => {
                write!(f, "SparseNew()")
            }
        };

        result
    }

    fn show_variable(&self, f: &mut Formatter, var: &ResolvedVariableRef) -> std::fmt::Result {
        write!(f, "{}", self.source_map.get_text(&var.name))
    }

    fn show_basic_literal(
        &self,
        f: &mut Formatter,
        basic_literal: &ResolvedLiteral,
    ) -> std::fmt::Result {
        match basic_literal {
            ResolvedLiteral::FloatLiteral(fp) => {
                write!(f, "{}", fp.bright_magenta())
            }
            ResolvedLiteral::NoneLiteral => {
                write!(f, "{}", "none".green())
            }
            ResolvedLiteral::IntLiteral(i) => {
                write!(f, "{}", i.bright_cyan())
            }
            ResolvedLiteral::StringLiteral(s) => {
                write!(f, "{}", s.bright_red())
            }
            ResolvedLiteral::BoolLiteral(b) => {
                write!(f, "{}", b.bright_white())
            }
            ResolvedLiteral::EnumVariantLiteral(variant, data) => {
                write!(f, "{:?}::{:?}", variant.blue(), data.green())
            }
            ResolvedLiteral::TupleLiteral(a, expressions) => {
                write!(f, "(")?;
                self.show_expressions(f, expressions)?;
                write!(f, ")")
            }
            ResolvedLiteral::Array(a, expressions) => {
                write!(f, "[")?;
                self.show_expressions(f, expressions)?;
                write!(f, "]")
            }
            ResolvedLiteral::Map(a, tuple_expressions) => {
                for (key, value) in tuple_expressions {
                    self.show_expression(f, key)?;
                    write!(f, "{}", ":".bright_blue())?;
                    self.show_expression(f, value)?;
                }
                Ok(())
            }
        }
    }

    fn show_expressions(
        &self,
        f: &mut Formatter,
        expressions: &[ResolvedExpression],
    ) -> std::fmt::Result {
        for (i, expr) in expressions.iter().enumerate() {
            if i > 0 {
                writeln!(f, ", ")?;
            }
            self.show_expression(f, expr)?;
        }

        if !expressions.is_empty() {
            writeln!(f, "")?;
        }

        Ok(())
    }

    fn show_postfix(&self, f: &mut Formatter, postfix: &ResolvedPostfix) -> std::fmt::Result {
        match postfix.kind {
            ResolvedPostfixKind::StructField(_, _) => todo!(),
            ResolvedPostfixKind::ArrayIndex(_, _) => todo!(),
            ResolvedPostfixKind::ArrayRangeIndex(_, _) => todo!(),
            ResolvedPostfixKind::StringIndex(_) => todo!(),
            ResolvedPostfixKind::StringRangeIndex(_) => todo!(),
            ResolvedPostfixKind::MapIndex(_, _) => todo!(),
            ResolvedPostfixKind::RustTypeIndexRef(_, _) => todo!(),
            ResolvedPostfixKind::MemberCall(_, _) => todo!(),
            ResolvedPostfixKind::FunctionCall(_) => todo!(),
            ResolvedPostfixKind::OptionUnwrap => todo!(),
            ResolvedPostfixKind::NoneCoalesce(_) => todo!(),
            ResolvedPostfixKind::SparseAdd(_) => todo!(),
            ResolvedPostfixKind::SparseRemove(_) => todo!(),
            ResolvedPostfixKind::SparseAccess(_) => todo!(),
            ResolvedPostfixKind::ArrayRemoveIndex(_) => todo!(),
            ResolvedPostfixKind::ArrayClear => todo!(),
            ResolvedPostfixKind::MapRemove(_, _) => todo!(),
            ResolvedPostfixKind::MapHas(_) => todo!(),
            ResolvedPostfixKind::IntAbs => todo!(),
            ResolvedPostfixKind::IntRnd => todo!(),
            ResolvedPostfixKind::IntToFloat => {
                write!(f, ".to_float()")
            }
            ResolvedPostfixKind::IntClamp(_, _) => todo!(),
            ResolvedPostfixKind::IntMin(_) => todo!(),
            ResolvedPostfixKind::IntMax(_) => todo!(),
            ResolvedPostfixKind::FloatRound => {
                write!(f, ".round()")
            }
            ResolvedPostfixKind::FloatFloor => todo!(),
            ResolvedPostfixKind::FloatSign => todo!(),
            ResolvedPostfixKind::FloatAbs => todo!(),
            ResolvedPostfixKind::FloatRnd => todo!(),
            ResolvedPostfixKind::FloatCos => todo!(),
            ResolvedPostfixKind::FloatSin => todo!(),
            ResolvedPostfixKind::FloatAcos => todo!(),
            ResolvedPostfixKind::FloatAsin => todo!(),
            ResolvedPostfixKind::FloatAtan2(_) => todo!(),
            ResolvedPostfixKind::FloatSqrt => todo!(),
            ResolvedPostfixKind::FloatClamp(_, _) => todo!(),
            ResolvedPostfixKind::FloatMin(_) => todo!(),
            ResolvedPostfixKind::FloatMax(_) => todo!(),
            ResolvedPostfixKind::StringLen => todo!(),
            ResolvedPostfixKind::Tuple2FloatMagnitude => todo!(),
        }
    }

    fn show_type(&self, f: &mut Formatter, resolved_type: &ResolvedType) -> std::fmt::Result {
        match resolved_type {
            ResolvedType::Int => write!(f, "{}", "Int".bright_blue()),
            ResolvedType::Float => write!(f, "{}", "Float".bright_blue()),
            ResolvedType::String => write!(f, "{}", "String".bright_blue()),
            ResolvedType::Bool => write!(f, "{}", "Bool".bright_blue()),
            ResolvedType::Unit => write!(f, "{}", "()".bright_blue()),
            ResolvedType::Array(array_type) => {
                write!(f, "[")?;
                self.show_type(f, &array_type.item_type)?;
                write!(f, "]")
            }

            ResolvedType::Tuple(tuple_type) => {
                write!(f, "(")?;
                for (index, item_type) in tuple_type.0.iter().enumerate() {
                    if index > 0 {
                        write!(f, "{}", ", ".bright_black())?;
                    }
                    self.show_type(f, item_type)?;
                }
                write!(f, ")")
            }

            ResolvedType::Struct(struct_ref) => write!(f, "{}", struct_ref.borrow().assigned_name),
            ResolvedType::Map(map_ref) => {
                write!(f, "[{}:{}]", map_ref.key_type, map_ref.value_type)
            }
            ResolvedType::Enum(enum_type) => write!(f, "{}", enum_type.borrow().assigned_name),
            //ResolvedType::EnumVariant(variant) => write!(
            //  f,
            //"{}::{}",
            //variant.owner.assigned_name, variant.assigned_name
            //),
            ResolvedType::Function(signature) => write!(f, "function {signature}"),
            ResolvedType::Iterator(generating_type) => write!(f, "Iterator<{generating_type:?}>"),
            ResolvedType::Optional(base_type) => write!(f, "{}?", base_type.yellow()),
            ResolvedType::RustType(rust_type) => write!(f, "RustType {}", rust_type.type_name),
            ResolvedType::Range => write!(f, "Range"),
        }
    }

    pub(crate) fn show_external_function_declaration(
        &self,
        f: &mut Formatter,
        external_function: &ResolvedExternalFunctionDefinition,
    ) -> std::fmt::Result {
        self.show_signature(f, &external_function.signature)
    }

    pub(crate) fn show_external_function_declarations(
        &self,
        f: &mut Formatter,
        external_functions: &SeqMap<String, ResolvedExternalFunctionDefinitionRef>,
    ) -> std::fmt::Result {
        for (_name, func) in external_functions {
            write!(f, "{}", func.assigned_name)?;
            self.show_external_function_declaration(f, func)?;
            writeln!(f)?;
        }
        Ok(())
    }
    pub fn show_type_parameter(
        &self,
        f: &mut Formatter,
        parameter_type: &ResolvedTypeForParameter,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}: {:?}",
            if parameter_type.is_mutable {
                "mut ".red()
            } else {
                "".white()
            },
            parameter_type.name.bright_green(),
            parameter_type.resolved_type.bright_cyan(),
        )
    }

    pub fn show_type_parameters(
        &self,
        f: &mut Formatter,
        parameter_types: &[ResolvedTypeForParameter],
    ) -> std::fmt::Result {
        for (index, parameter_type) in parameter_types.iter().enumerate() {
            if index > 0 {
                write!(f, "{}", ", ".white())?;
            }
            self.show_type_parameter(f, parameter_type)?;
        }
        Ok(())
    }

    pub fn show_signature(
        &self,
        f: &mut Formatter,
        function_type_signature: &FunctionTypeSignature,
    ) -> std::fmt::Result {
        write!(f, "{}", "(".bright_green())?;

        self.show_type_parameters(f, &function_type_signature.parameters)?;

        write!(f, "{}", ")".bright_green())?;

        write!(f, " {} ", "->".bright_green())?;

        self.show_type(f, &function_type_signature.return_type)
    }

    pub fn show_internal_function(
        &self,
        f: &mut Formatter,
        internal_func: &ResolvedInternalFunctionDefinition,
    ) -> std::fmt::Result {
        self.show_signature(f, &internal_func.signature)?;
        writeln!(f)?;
        self.show_expression(f, &internal_func.body)
    }

    pub(crate) fn show_internal_functions(
        &self,
        f: &mut Formatter,
        internal_functions: &SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    ) -> std::fmt::Result {
        for (_name, func) in internal_functions {
            write!(f, "{}", self.source_map.get_text(&func.name.0).bright_red())?;
            self.show_internal_function(f, func)?;
        }
        Ok(())
    }

    fn show_function(&self, f: &mut Formatter, func: &ResolvedFunction) -> std::fmt::Result {
        match func {
            ResolvedFunction::Internal(internal) => self.show_internal_function(f, internal),
            ResolvedFunction::External(external) => {
                self.show_external_function_declaration(f, external)
            }
        }
    }

    pub(crate) fn show_enums(
        &self,
        f: &mut Formatter,
        enums: &SeqMap<String, ResolvedEnumTypeRef>,
    ) -> std::fmt::Result {
        for (_name, enum_type) in enums {
            write!(f, "{}", &enum_type.borrow().assigned_name.bright_red())?;
        }
        Ok(())
    }
}
