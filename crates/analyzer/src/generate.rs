use crate::Analyzer;
use seq_map::SeqMap;
use std::rc::Rc;
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
    pub fn instantiated_name(name: &str, type_parameters: &[Type]) -> String {
        let parameters = type_parameters
            .iter()
            .map(|ty| ty.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("{name}<{parameters}>")
    }

    pub fn generate_empty_struct(&mut self, name: &str, type_parameters: &[Type]) -> StructTypeRef {
        let struct_type = StructType {
            name: Default::default(),
            assigned_name: Self::instantiated_name(name, type_parameters),
            anon_struct_type: AnonymousStructType {
                defined_fields: Default::default(),
            },
            number: self.shared.state.allocate_number(),
        };

        Rc::new(struct_type)
    }

    fn gen_function(
        &self,
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
            .collect::<Vec<_>>();

        self.gen_function_mut(name_prefix, name, &params, return_type, func)
    }

    pub fn gen_function_mut(
        &self,
        name_prefix: &str,
        name: &str,
        params: &[TypeForParameter],
        return_type: Type,
        func: IntrinsicFunction,
    ) -> Function {
        let generated_signature = Signature {
            parameters: Vec::from(params),
            return_type: Box::new(return_type),
        };

        let generated_function_type = Type::Function(generated_signature.clone());

        let intrinsic_name = func.to_string();

        let intrinsic_fn_def = self
            .shared
            .core_symbol_table
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

    pub fn generate_sparse_struct(&mut self, key_type: &Type, value_type: &Type) -> StructTypeRef {
        let struct_ref = self.generate_empty_struct("Sparse", &[value_type.clone()]);
        let gen_name = &struct_ref.assigned_name;
        let sparse_type = Type::Struct(struct_ref.clone());

        let mut functions = SeqMap::new();

        let new_func = self.gen_function(
            &gen_name,
            "new",
            &[],
            sparse_type.clone(),
            IntrinsicFunction::SparseCreate,
        );
        functions.insert("new", new_func).unwrap();

        let new_func = self.gen_function(
            &gen_name,
            "has",
            &[sparse_type.clone(), key_type.clone()],
            Type::Bool,
            IntrinsicFunction::SparseHas,
        );
        functions.insert("has", new_func).unwrap();

        let new_func = self.gen_function(
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
            &gen_name,
            "add",
            &[sparse_type.clone(), value_type.clone()],
            key_type.clone(),
            IntrinsicFunction::SparseAdd,
        );
        functions.insert("add", new_func).unwrap();

        let subscript_fn = self.gen_function(
            &gen_name,
            "subscript",
            &[sparse_type.clone(), key_type.clone()],
            Type::Optional(Box::from(value_type.clone())),
            IntrinsicFunction::SparseSubscript,
        );
        functions.insert("subscript", subscript_fn).unwrap();

        self.add_functions(&sparse_type, functions);

        struct_ref
    }

    fn add_functions(&mut self, ty: &Type, functions: SeqMap<&str, Function>) {
        self.shared.state.associated_impls.prepare(&ty);

        for (name, func) in functions {
            self.shared
                .state
                .associated_impls
                .add_member_function(&ty, name, FunctionRef::from(func))
                .unwrap()
        }
    }

    pub fn generate_vec_struct(&mut self, value_type: &Type) -> StructTypeRef {
        let struct_ref = self.generate_empty_struct("Vec", &[value_type.clone()]);
        let gen_name = &struct_ref.assigned_name;
        let vec_type = Type::Struct(struct_ref.clone());

        let mut functions = SeqMap::new();

        let from_slice_fn = self.gen_function(
            &gen_name,
            "from_slice",
            &[Type::Slice(Box::from(value_type.clone()))],
            vec_type.clone(),
            IntrinsicFunction::VecFromSlice,
        );
        functions.insert("from_slice", from_slice_fn).unwrap();

        let new_fn = self.gen_function(
            &gen_name,
            "new",
            &[],
            vec_type.clone(),
            IntrinsicFunction::VecCreate,
        );
        functions.insert("new", new_fn).unwrap();

        self.add_functions(&vec_type, functions);

        struct_ref
    }

    pub fn generate_map_struct(&mut self, key_type: &Type, value_type: &Type) -> StructTypeRef {
        let struct_ref = self.generate_empty_struct("Map", &[key_type.clone(), value_type.clone()]);
        let gen_name = &struct_ref.assigned_name;
        let map_type = Type::Struct(struct_ref.clone());

        let mut functions = SeqMap::new();

        let from_slice_pair_fn = self.gen_function(
            &gen_name,
            "from_slice_pair",
            &[],
            map_type.clone(),
            IntrinsicFunction::MapFromSlicePair,
        );
        functions
            .insert("from_slice_pair", from_slice_pair_fn)
            .unwrap();

        let new_fn = self.gen_function(
            &gen_name,
            "new",
            &[],
            map_type.clone(),
            IntrinsicFunction::MapCreate,
        );
        functions.insert("new", new_fn).unwrap();

        let subscript_mut = self.gen_function_mut(
            &gen_name,
            "subscript_mut",
            &[
                TypeForParameter {
                    name: "self".to_string(),
                    resolved_type: map_type.clone(),
                    is_mutable: true,
                    node: None,
                },
                TypeForParameter {
                    name: "key".to_string(),
                    resolved_type: key_type.clone(),
                    is_mutable: false,
                    node: None,
                },
                TypeForParameter {
                    name: "out_value".to_string(),
                    resolved_type: value_type.clone(),
                    is_mutable: true,
                    node: None,
                },
            ],
            Type::Unit,
            IntrinsicFunction::MapSubscriptMut,
        );
        functions.insert("subscript_mut", subscript_mut).unwrap();

        self.add_functions(&map_type, functions);

        struct_ref
    }
}
