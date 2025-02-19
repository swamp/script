use seq_map::SeqMap;
use std::fmt::{Display, Formatter};
use swamp_script_core_extra::prelude::SourceMapLookup;
use swamp_script_modules::modules::{ModuleRef, Modules};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{Postfix, PostfixKind};
use yansi::Paint;

pub struct ModulesDisplay<'a> {
    pub resolved_modules: &'a Modules,
    pub source_map: &'a dyn SourceMapLookup,
}

impl Display for ModulesDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mods = self.resolved_modules;

        for (name, _module) in &mods.modules {
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

impl ModulesDisplay<'_> {
    /// # Errors
    ///
    pub fn show_structs(
        &self,
        f: &mut Formatter<'_>,
        structs: &SeqMap<String, StructTypeRef>,
    ) -> std::fmt::Result {
        for (_struct_name, struct_type) in structs {
            writeln!(f, "  {}:  ", struct_type.assigned_name.yellow())?;
            self.show_struct(f, &struct_type)?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn show_struct(&self, f: &mut Formatter<'_>, struct_type: &StructType) -> std::fmt::Result {
        for (field_name, field) in &struct_type.anon_struct_type.defined_fields {
            writeln!(
                f,
                "      {}:  {}",
                field_name.cyan(),
                field.field_type.magenta(),
            )?;
        }

        Ok(())
    }

    /// # Errors
    ///
    pub fn show_constants(
        &self,
        f: &mut Formatter<'_>,
        constants: &SeqMap<String, ConstantRef>,
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
    pub fn show_constant(&self, f: &mut Formatter<'_>, constant: &ConstantRef) -> std::fmt::Result {
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
        aliases: &SeqMap<String, AliasTypeRef>,
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
    pub fn show_alias(&self, f: &mut Formatter<'_>, alias: &AliasTypeRef) -> std::fmt::Result {
        write!(f, "{} ==> ", alias.assigned_name.blue(),)?;

        self.show_type(f, &alias.referenced_type)?;

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
        self.show_module_path(f, &ns.namespace.path)
    }

    #[allow(clippy::too_many_lines)]
    fn show_expression(&self, f: &mut Formatter, expr: &Expression) -> std::fmt::Result {
        let result = match &expr.kind {
            ExpressionKind::ConstantAccess(a) => {
                write!(f, "{}", a.assigned_name.magenta())
            }
            ExpressionKind::VariableAccess(var) => self.show_variable(f, var),

            ExpressionKind::InternalFunctionAccess(_) => {
                write!(f, "InternalFunctionAccess()")
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
                self.show_expression(f, base_expr)?;
                for postfix in postfixes {
                    self.show_postfix(f, postfix)?;
                }
                Ok(())
            }
            ExpressionKind::CoerceOptionToBool(_) => {
                write!(f, "CoerceOptionToBool()")
            }
            ExpressionKind::InterpolatedString(_) => {
                write!(f, "InterpolatedString()")
            }
            ExpressionKind::VariableDefinition(_, _) => {
                write!(f, "VariableDefinition()")
            }
            ExpressionKind::VariableReassignment(_, _) => {
                write!(f, "VariableReassignment()")
            }
            ExpressionKind::StructInstantiation(_) => {
                write!(f, "StructInstantiation()")
            }

            ExpressionKind::Literal(basic_literal) => self.show_basic_literal(f, basic_literal),
            ExpressionKind::Option(_) => {
                write!(f, "Option()")
            }
            ExpressionKind::Range(_, _, _) => {
                write!(f, "Range()")
            }
            ExpressionKind::ForLoop(_, _, _) => {
                write!(f, "ForLoop()")
            }
            ExpressionKind::WhileLoop(_, _) => {
                write!(f, "WhileLoop()")
            }
            ExpressionKind::Return(_) => {
                write!(f, "Return()")
            }
            ExpressionKind::Break => {
                write!(f, "Break")
            }
            ExpressionKind::Continue => {
                write!(f, "Continue")
            }
            ExpressionKind::Block(expressions) => {
                for expression in expressions {
                    self.show_expression(f, expression)?;
                    writeln!(f)?;
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
            ExpressionKind::Assignment(_, _) => {
                write!(f, "Assignment()")
            }
            ExpressionKind::AssignmentSlice(_, _) => {
                write!(f, "AssignmentSlice()")
            }
            ExpressionKind::CompoundAssignment(_, _, _) => {
                write!(f, "CompoundAssignment()")
            }
            ExpressionKind::ArrayExtend(_, _) => {
                write!(f, "ArrayExtend()")
            }
            ExpressionKind::ArrayPush(_, _) => {
                write!(f, "ArrayPush()")
            }
            ExpressionKind::RustValueInstantiation(_, _) => {
                write!(f, "SparseNew()")
            }
            ExpressionKind::IntrinsicCall(intrinsic_func, arguments) => {
                write!(f, "intrinsic {intrinsic_func:?} {arguments:?}")
            }
            ExpressionKind::IntrinsicFunctionAccess(intrinsic_func_def) => {
                write!(f, "intrinsic_access {intrinsic_func_def:?}")
            }
        };

        result
    }

    fn show_variable(&self, f: &mut Formatter, var: &VariableRef) -> std::fmt::Result {
        if var.name.span.file_id == 0 {
            return Ok(());
        }
        write!(f, "{}", self.source_map.get_text(&var.name))
    }

    fn show_basic_literal(&self, f: &mut Formatter, basic_literal: &Literal) -> std::fmt::Result {
        match basic_literal {
            Literal::FloatLiteral(fp) => {
                write!(f, "{}", fp.bright_magenta())
            }
            Literal::NoneLiteral => {
                write!(f, "{}", "none".green())
            }
            Literal::IntLiteral(i) => {
                write!(f, "{}", i.bright_cyan())
            }
            Literal::StringLiteral(s) => {
                write!(f, "{}", s.bright_red())
            }
            Literal::BoolLiteral(b) => {
                write!(f, "{}", b.bright_white())
            }
            Literal::EnumVariantLiteral(variant, data) => {
                write!(f, "{:?}::{:?}", variant.blue(), data.green())
            }
            Literal::TupleLiteral(_tuple_type, expressions) => {
                write!(f, "(")?;
                self.show_expressions(f, expressions)?;
                write!(f, ")")
            }
            Literal::Array(_array_type, expressions) => {
                write!(f, "[")?;
                self.show_expressions(f, expressions)?;
                write!(f, "]")
            }
            Literal::Map(_map_type, tuple_expressions) => {
                for (key, value) in tuple_expressions {
                    self.show_expression(f, key)?;
                    write!(f, "{}", ":".bright_blue())?;
                    self.show_expression(f, value)?;
                }
                Ok(())
            }
        }
    }

    fn show_expressions(&self, f: &mut Formatter, expressions: &[Expression]) -> std::fmt::Result {
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

    fn show_postfix(&self, f: &mut Formatter, postfix: &Postfix) -> std::fmt::Result {
        match &postfix.kind {
            PostfixKind::StructField(struct_type, field) => {
                let name = struct_type
                    .anon_struct_type
                    .defined_fields
                    .keys()
                    .collect::<Vec<_>>()[*field]
                    .clone();
                write!(f, ".{}", name.bright_blue())
            }
            PostfixKind::ArrayIndex(_, _) => todo!(),
            PostfixKind::ArrayRangeIndex(_, _) => todo!(),
            PostfixKind::StringIndex(_) => todo!(),
            PostfixKind::StringRangeIndex(_) => todo!(),
            PostfixKind::MapIndex(_, _) => todo!(),
            PostfixKind::RustTypeIndexRef(_, _) => todo!(),
            PostfixKind::MemberCall(_function_ref, b) => write!(f, "membercall {b:?}"),
            PostfixKind::FunctionCall(call) => write!(f, "call: {call:?}"),
            PostfixKind::OptionUnwrap => todo!(),
            PostfixKind::NoneCoalesce(_) => todo!(),
        }
    }

    fn show_type(&self, f: &mut Formatter, resolved_type: &Type) -> std::fmt::Result {
        match resolved_type {
            Type::Int => write!(f, "{}", "Int".bright_blue()),
            Type::Float => write!(f, "{}", "Float".bright_blue()),
            Type::String => write!(f, "{}", "String".bright_blue()),
            Type::Bool => write!(f, "{}", "Bool".bright_blue()),
            Type::Unit => write!(f, "{}", "()".bright_blue()),
            Type::Array(array_type) => {
                write!(f, "[")?;
                self.show_type(f, &array_type.item_type)?;
                write!(f, "]")
            }

            Type::Tuple(tuple_type) => {
                write!(f, "(")?;
                for (index, item_type) in tuple_type.0.iter().enumerate() {
                    if index > 0 {
                        write!(f, "{}", ", ".bright_black())?;
                    }
                    self.show_type(f, item_type)?;
                }
                write!(f, ")")
            }

            Type::Struct(struct_ref) => write!(f, "{}", struct_ref.assigned_name),
            Type::Map(map_ref) => {
                write!(f, "[{}:{}]", map_ref.key_type, map_ref.value_type)
            }
            Type::Enum(enum_type) => write!(f, "{}", enum_type.borrow().assigned_name),
            //Type::EnumVariant(variant) => write!(
            //  f,
            //"{}::{}",
            //variant.owner.assigned_name, variant.assigned_name
            //),
            Type::Function(signature) => write!(f, "function {signature}"),
            Type::Iterator(generating_type) => write!(f, "Iterator<{generating_type:?}>"),
            Type::Optional(base_type) => write!(f, "{}?", base_type.yellow()),
            Type::External(external_type) => write!(f, "External {}", external_type.type_name),
            Type::Range => write!(f, "Range"),
        }
    }

    pub(crate) fn show_external_function_declaration(
        &self,
        f: &mut Formatter,
        external_function: &ExternalFunctionDefinition,
    ) -> std::fmt::Result {
        self.show_signature(f, &external_function.signature)
    }

    pub(crate) fn show_external_function_declarations(
        &self,
        f: &mut Formatter,
        external_functions: &SeqMap<String, ExternalFunctionDefinitionRef>,
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
        parameter_type: &TypeForParameter,
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
        parameter_types: &[TypeForParameter],
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
        function_type_signature: &Signature,
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
        internal_func: &InternalFunctionDefinition,
    ) -> std::fmt::Result {
        self.show_signature(f, &internal_func.signature)?;
        writeln!(f)?;
        self.show_expression(f, &internal_func.body)
    }

    pub(crate) fn show_internal_functions(
        &self,
        f: &mut Formatter,
        internal_functions: &SeqMap<String, InternalFunctionDefinitionRef>,
    ) -> std::fmt::Result {
        for (_name, func) in internal_functions {
            write!(f, "{}", self.source_map.get_text(&func.name.0).bright_red())?;
            self.show_internal_function(f, func)?;
        }
        Ok(())
    }

    fn show_function(&self, f: &mut Formatter, func: &Function) -> std::fmt::Result {
        match func {
            Function::Internal(internal) => self.show_internal_function(f, internal),
            Function::External(external) => self.show_external_function_declaration(f, external),
        }
    }

    pub(crate) fn show_enums(
        &self,
        f: &mut Formatter,
        enums: &SeqMap<String, EnumTypeRef>,
    ) -> std::fmt::Result {
        for (_name, enum_type) in enums {
            write!(f, "{}", &enum_type.borrow().assigned_name.bright_red())?;
        }
        Ok(())
    }
}
