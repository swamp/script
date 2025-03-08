use crate::all_types_are_concrete;
use crate::prelude::Error;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::{SemanticError, TypeIdGenerator};
use swamp_script_types::{
    AnonymousStructType, NamedStructType, ParameterizedTypeBlueprint, ParameterizedTypeKind,
    Signature, StructTypeField, Type, TypeForParameter,
};
use tracing::info;

#[derive(Debug)]
pub struct TypeVariableScope {
    pub type_variables: SeqMap<String, Type>,
}

impl TypeVariableScope {}

impl TypeVariableScope {
    pub fn new(scope: SeqMap<String, Type>) -> Self {
        Self {
            type_variables: scope,
        }
    }

    pub(crate) fn variables(&self) -> Vec<Type> {
        self.type_variables.values().map(|ty| ty.clone()).collect()
    }
}
pub struct Instantiator;

impl Instantiator {
    pub fn create_type_parameter_scope(
        parameters: &[Type],
        concrete: &[Type],
    ) -> Result<TypeVariableScope, Error> {
        if parameters.len() != concrete.len() {
            panic!("wrong parameter count")
        }

        let mut scope = SeqMap::new();
        for (param, concrete) in parameters.iter().zip(concrete) {
            if let Type::Variable(type_variable_name) = param {
                scope
                    .insert(type_variable_name.clone(), concrete.clone())
                    .unwrap();
            };
        }

        Ok(TypeVariableScope::new(scope))
    }

    pub fn create_type_parameter_scope_from_variables(
        variables: &[String],
        concrete_types: &[Type],
    ) -> Result<TypeVariableScope, Error> {
        if variables.len() != concrete_types.len() {
            panic!("wrong parameter count")
        }

        assert!(all_types_are_concrete(concrete_types));

        let mut scope = SeqMap::new();
        for (param, concrete) in variables.iter().zip(concrete_types) {
            scope.insert(param.clone(), concrete.clone()).unwrap();
        }

        Ok(TypeVariableScope::new(scope))
    }

    pub(crate) fn instantiate_blueprint(
        blueprint: &ParameterizedTypeBlueprint,
        scope: &TypeVariableScope,
        type_id_generator: &mut TypeIdGenerator,
    ) -> Result<(bool, Type), Error> {
        match &blueprint.kind {
            ParameterizedTypeKind::Struct(struct_ref) => {
                Self::instantiate_struct(struct_ref, scope, type_id_generator)
            }
            ParameterizedTypeKind::Enum(_) => todo!(),
        }
    }

    pub fn instantiate_signature(
        signature: Signature,
        self_type: &Type,
        scope: &TypeVariableScope,
        type_id_generator: &mut TypeIdGenerator,
    ) -> Result<(bool, Signature), Error> {
        let mut was_replaced = false;
        let mut instantiated_type_for_parameters = Vec::new();
        for type_for_parameter in &signature.parameters {
            let (type_was_replaced, resolved) =
                if let Type::Blueprint(_blueprint) = &type_for_parameter.resolved_type {
                    // HACK: Assume blueprints are self
                    (false, self_type.clone())
                } else {
                    Self::instantiate_type_if_needed(
                        &type_for_parameter.resolved_type,
                        scope,
                        type_id_generator,
                    )?
                };

            if type_was_replaced {
                was_replaced = true;
            }
            instantiated_type_for_parameters.push(TypeForParameter {
                name: type_for_parameter.name.clone(),
                resolved_type: resolved,
                is_mutable: type_for_parameter.is_mutable,
                node: type_for_parameter.node.clone(),
            });
        }

        let (return_type_was_replaced, instantiated_return_type) =
            Self::instantiate_type_if_needed(&signature.return_type, scope, type_id_generator)?;
        if return_type_was_replaced {
            was_replaced = true;
        }

        let new_signature = Signature {
            parameters: instantiated_type_for_parameters,
            return_type: Box::new(instantiated_return_type),
        };

        info!(?new_signature, "instantiated signature");

        Ok((was_replaced, new_signature))
    }

    fn instantiate_type_if_needed(
        ty: &Type,
        type_variables: &TypeVariableScope,
        type_id_generator: &mut TypeIdGenerator,
    ) -> Result<(bool, Type), Error> {
        let (replaced, result_type) = match ty {
            Type::Generic(parameterized_type, arguments) => {
                let new_scope = Self::create_type_parameter_scope_from_variables(
                    &parameterized_type.type_variables,
                    arguments,
                )?;
                Self::instantiate_blueprint(parameterized_type, &new_scope, type_id_generator)?
            }

            Type::Variable(type_variable) => {
                let found_type = type_variables
                    .type_variables
                    .get(&type_variable)
                    .ok_or(SemanticError::UnknownTypeVariable)?;
                (true, found_type.clone())
            }

            _ => (false, ty.clone()),
        };

        Ok((replaced, result_type))
    }

    fn parameterized_name(name: &str, parameters: &[Type]) -> String {
        let type_strings: Vec<String> = parameters
            .iter()
            .map(std::string::ToString::to_string)
            .collect();

        format!("{}<{}>", name, type_strings.join(","))
    }

    fn instantiate_struct(
        struct_type: &NamedStructType,
        type_variables: &TypeVariableScope,
        type_id_generator: &mut TypeIdGenerator,
    ) -> Result<(bool, Type), Error> {
        let mut was_any_replaced = false;
        let mut new_fields = SeqMap::new();
        for (name, field) in &struct_type.anon_struct_type.field_name_sorted_fields {
            let (was_replaced, new_type) = Self::instantiate_type_if_needed(
                &field.field_type,
                type_variables,
                type_id_generator,
            )?;
            was_any_replaced |= was_replaced;
            let new_field = StructTypeField {
                identifier: field.identifier.clone(),
                field_type: new_type,
            };
            new_fields.insert(name.clone(), new_field).unwrap();
        }

        let new_assigned_name =
            Self::parameterized_name(&struct_type.assigned_name, &type_variables.variables());

        let new_struct = NamedStructType {
            name: struct_type.name.clone(),
            assigned_name: new_assigned_name,
            anon_struct_type: AnonymousStructType {
                field_name_sorted_fields: new_fields,
            },
            type_id: type_id_generator.allocate(),
        };

        let new_struct_ref = Rc::new(RefCell::new(new_struct));

        Ok((was_any_replaced, Type::NamedStruct(new_struct_ref)))
    }
}
