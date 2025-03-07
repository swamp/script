use crate::err::ErrorKind;
use crate::prelude::Error;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::SemanticError;
use swamp_script_types::{
    AnonymousStructType, NamedStructType, ParameterizedTypeBlueprint, ParameterizedTypeKind,
    StructTypeField, Type,
};

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
        concrete: &[Type],
    ) -> Result<TypeVariableScope, Error> {
        if variables.len() != concrete.len() {
            panic!("wrong parameter count")
        }

        let mut scope = SeqMap::new();
        for (param, concrete) in variables.iter().zip(concrete) {
            scope.insert(param.clone(), concrete.clone()).unwrap();
        }

        Ok(TypeVariableScope::new(scope))
    }

    pub(crate) fn instantiate_blueprint(
        blueprint: &ParameterizedTypeBlueprint,
        concrete_types: &[Type],
    ) -> Result<(bool, Type), Error> {
        let scope = Self::create_type_parameter_scope_from_variables(
            &blueprint.type_variables,
            concrete_types,
        )?;

        match &blueprint.kind {
            ParameterizedTypeKind::Struct(struct_ref) => {
                Self::instantiate_struct(struct_ref, &scope)
            }
            ParameterizedTypeKind::Enum(_) => todo!(),
        }
    }

    pub fn instantiate(
        blue_print: &ParameterizedTypeBlueprint,
        arguments: &[Type],
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), Error> {
        let mut resolved_params = Vec::new();
        for (i, param) in arguments.iter().enumerate() {
            let (_was_replaced, resolved) =
                Self::instantiate_type_if_needed(param, type_variables)?;
            resolved_params.push(resolved);
        }

        Self::instantiate_blueprint(blue_print, &resolved_params)
    }

    fn instantiate_type_if_needed(
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), Error> {
        let (replaced, result_type) = match ty {
            Type::Generic(parameterized_type, arguments) => {
                Self::instantiate(parameterized_type, arguments, type_variables)?
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
            type_id: 0, // TODO: generate unique number
        };

        let new_struct_ref = Rc::new(RefCell::new(new_struct));

        Ok((was_any_replaced, Type::NamedStruct(new_struct_ref)))
    }
}
