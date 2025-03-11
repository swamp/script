use crate::prelude::Error;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::SemanticError;
use swamp_script_semantic::prelude::InstantiationCache;
use swamp_script_types::{
    AnonymousStructType, NamedStructType, ParameterizedTypeBlueprint, ParameterizedTypeKind,
    Signature, StructTypeField, Type, TypeForParameter, all_types_are_concrete,
    all_types_are_concrete_or_unit,
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

        assert!(all_types_are_concrete_or_unit(concrete_types));

        let mut scope = SeqMap::new();
        for (param, concrete) in variables.iter().zip(concrete_types) {
            scope.insert(param.clone(), concrete.clone()).unwrap();
        }

        Ok(TypeVariableScope::new(scope))
    }

    pub(crate) fn instantiate_blueprint(
        blueprint: &ParameterizedTypeBlueprint,
        scope: &TypeVariableScope,
    ) -> Result<(bool, Type), Error> {
        match &blueprint.kind {
            ParameterizedTypeKind::Struct(struct_ref) => {
                Self::instantiate_struct(struct_ref, scope)
            }
            ParameterizedTypeKind::Enum(_) => todo!(),
        }
    }

    pub fn instantiate_signature(
        signature: Signature,
        self_type: &Type,
        scope: &TypeVariableScope,
    ) -> Result<(bool, Signature), Error> {
        let mut was_replaced = false;
        let mut instantiated_type_for_parameters = Vec::new();
        for type_for_parameter in &signature.parameters {
            let (type_was_replaced, resolved) = Self::instantiate_type_if_needed_with_self(
                self_type,
                &type_for_parameter.resolved_type,
                scope,
            )?;

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
            Self::instantiate_type_if_needed_with_self(self_type, &signature.return_type, scope)?;
        if return_type_was_replaced {
            was_replaced = true;
        }

        let new_signature = Signature {
            parameters: instantiated_type_for_parameters,
            return_type: Box::new(instantiated_return_type),
        };

        Ok((was_replaced, new_signature))
    }

    fn instantiate_types_if_needed(
        types: &[Type],
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Vec<Type>), Error> {
        let mut converted = Vec::new();

        for ty in types {
            let (_was_converted, instantiated_type) =
                Self::instantiate_type_if_needed(ty, type_variables)?;

            converted.push(instantiated_type);
        }

        Ok((true, converted))
    }

    fn instantiate_type_if_needed_with_self(
        self_type: &Type,
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), Error> {
        match ty {
            Type::Blueprint(blueprint) => Ok((false, self_type.clone())), // HACK: assume self if there is a blueprint here
            _ => Self::instantiate_type_if_needed(ty, type_variables),
        }
    }

    fn instantiate_type_if_needed(
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), Error> {
        let (replaced, result_type) = match ty {
            Type::Generic(parameterized_type, arguments) => {
                let (_was_replaced, new_arguments) =
                    Self::instantiate_types_if_needed(arguments, type_variables)?;

                if all_types_are_concrete(&new_arguments) {
                    let new_scope = Self::create_type_parameter_scope_from_variables(
                        &parameterized_type.type_variables,
                        &new_arguments,
                    )?;
                    Self::instantiate_blueprint(parameterized_type, &new_scope)?
                } else {
                    panic!("can we leave generics")
                }
            }

            Type::Variable(type_variable) => {
                let found_type = type_variables
                    .type_variables
                    .get(type_variable)
                    .ok_or(SemanticError::UnknownTypeVariable)?;
                info!(?found_type, "found type");
                assert!(found_type.is_concrete());
                (true, found_type.clone())
            }

            Type::Blueprint(blueprint) => {
                //error!(?blueprint, "not allowed with blueprints here for types");
                panic!("not allowed with blueprints here for types")
            }

            Type::Tuple(types) => {
                let (was_replaced, new_types) =
                    Self::instantiate_types_if_needed(types, &type_variables)?;
                (was_replaced, Type::Tuple(new_types))
            }

            Type::Optional(inner_type) => {
                let (was_replaced, new_type) =
                    Self::instantiate_type_if_needed(inner_type, &type_variables)?;
                (was_replaced, Type::Optional(Box::new(new_type)))
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
    ) -> Result<(bool, Type), Error> {
        let mut was_any_replaced = false;
        let mut new_fields = SeqMap::new();
        for (name, field) in &struct_type.anon_struct_type.field_name_sorted_fields {
            let (was_replaced, new_type) =
                Self::instantiate_type_if_needed(&field.field_type, type_variables)?;
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
            module_path: struct_type.module_path.clone(),
        };

        Ok((was_any_replaced, Type::NamedStruct(new_struct)))
    }
}
