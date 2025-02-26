use crate::Analyzer;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::{
    AnonymousStructType, Function, FunctionRef, IntrinsicFunction, Signature, StructType,
    TupleType, TupleTypeRef,
};
use swamp_script_semantic::{
    ExpressionKind, InternalFunctionDefinition, InternalFunctionDefinitionRef, LocalIdentifier,
    Node, TypeForParameter,
};
use swamp_script_semantic::{StructTypeRef, Type};

impl<'a> Analyzer<'a> {
    pub fn generate_empty_struct(&mut self, name: &str, type_parameters: &[Type]) -> StructTypeRef {
        let struct_type = StructType {
            name: Default::default(),
            assigned_name: format!("{name}<{type_parameters:?}>"),
            anon_struct_type: AnonymousStructType {
                defined_fields: Default::default(),
            },
            number: self.shared.state.allocate_number(),
        };

        Rc::new(struct_type)
    }

    pub fn gen_function(
        &self,
        core_symbol_table: &SymbolTable,
        name_prefix: &str,
        name: &str,
        parameters: &[Type],
        return_type: Type,
        func: IntrinsicFunction,
    ) -> Function {
        let params = parameters
            .iter()
            .map(|ty| TypeForParameter {
                name: name.to_string(),
                resolved_type: ty.clone(),
                is_mutable: false,
                node: None,
            })
            .collect();

        let generated_signature = Signature {
            parameters: params,
            return_type: Box::new(return_type),
        };

        let generated_function_type = Type::Function(generated_signature.clone());

        let intrinsic_name = func.to_string();

        let intrinsic_fn_def = core_symbol_table
            .get_intrinsic_function(&intrinsic_name)
            .unwrap();

        let body_expr = self.create_expr(
            ExpressionKind::IntrinsicFunctionAccess(intrinsic_fn_def.clone()),
            generated_function_type,
            &swamp_script_ast::Node::default(),
        );

        let generated_internal_func_def = InternalFunctionDefinition {
            body: body_expr,
            name: LocalIdentifier(Node::default()),
            assigned_name: format!("{}::{}", name_prefix, name),
            signature: generated_signature,
        };

        Function::Internal(InternalFunctionDefinitionRef::from(
            generated_internal_func_def,
        ))
    }

    pub fn generate_sparse_struct(
        &mut self,
        core_symbol_table: &SymbolTable,
        key_type: &Type,
        value_type: &Type,
    ) -> StructTypeRef {
        let struct_ref = self.generate_empty_struct("Sparse", &[value_type.clone()]);
        let gen_name = &struct_ref.assigned_name;
        let sparse_type = Type::Struct(struct_ref.clone());

        let mut functions = SeqMap::new();

        let new_func = self.gen_function(
            core_symbol_table,
            &gen_name,
            "new",
            &[],
            sparse_type.clone(),
            IntrinsicFunction::SparseCreate,
        );
        functions.insert("new", new_func).unwrap();

        let new_func = self.gen_function(
            core_symbol_table,
            &gen_name,
            "has",
            &[sparse_type.clone(), key_type.clone()],
            Type::Bool,
            IntrinsicFunction::SparseHas,
        );
        functions.insert("has", new_func).unwrap();

        let new_func = self.gen_function(
            core_symbol_table,
            &gen_name,
            "iter",
            &[sparse_type.clone(), key_type.clone()],
            Type::Tuple(TupleTypeRef::from(TupleType(vec![
                key_type.clone(),
                value_type.clone(),
            ]))),
            IntrinsicFunction::SparseIter,
        );
        functions.insert("iter", new_func).unwrap();

        let new_func = self.gen_function(
            core_symbol_table,
            &gen_name,
            "add",
            &[sparse_type.clone(), value_type.clone()],
            key_type.clone(),
            IntrinsicFunction::SparseAdd,
        );
        functions.insert("add", new_func).unwrap();

        let subscript_fn = self.gen_function(
            core_symbol_table,
            &gen_name,
            "subscript",
            &[sparse_type.clone(), key_type.clone()],
            Type::Optional(Box::from(value_type.clone())),
            IntrinsicFunction::SparseSubscript,
        );
        functions.insert("subscript", subscript_fn).unwrap();

        // -----
        self.shared.state.associated_impls.prepare(&sparse_type);

        for (name, func) in functions {
            self.shared
                .state
                .associated_impls
                .add_member_function(&sparse_type, name, FunctionRef::from(func))
                .unwrap()
        }

        struct_ref
    }
}
