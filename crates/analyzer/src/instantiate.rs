use crate::lookup::TypeVariableScope;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_semantic::{
    AnonymousStructType, ParameterizedType, ParameterizedTypeKind, SemanticError, StructType,
    StructTypeField, StructTypeRef, Type,
};

pub struct Instantiator;

impl Instantiator {
    pub fn create_type_parameter_scope(
        parameters: &[Type],
        concrete: &[Type],
    ) -> Result<TypeVariableScope, SemanticError> {
        if parameters.len() != concrete.len() {
            return Err(SemanticError::WrongParameterCount);
        }

        let mut scope = SeqMap::new();
        for (param, concrete) in parameters.iter().zip(concrete) {
            match param {
                Type::Variable(type_variable) => {
                    scope
                        .insert(type_variable.name.clone(), concrete.clone())
                        .unwrap();
                }
                _ => {}
            };
        }

        Ok(TypeVariableScope::new(scope))
    }

    pub fn instantiate(
        parameterized_type: &ParameterizedType,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), SemanticError> {
        match &parameterized_type.base {
            ParameterizedTypeKind::Struct(struct_ref) => {
                Self::instantiate_struct(struct_ref, type_variables)
            }
            ParameterizedTypeKind::Enum(_) => todo!(),
            ParameterizedTypeKind::Parameterized(inner) => Self::instantiate(inner, type_variables),
        }
    }

    fn instantiate_type_if_needed(
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), SemanticError> {
        let (replaced, result_type) = match ty {
            Type::Parameterized(parameterized_type) => {
                Self::instantiate(parameterized_type, type_variables)?
            }

            Type::Variable(type_variable) => {
                let found_type = type_variables
                    .type_variables
                    .get(&type_variable.name)
                    .ok_or_else(|| SemanticError::UnknownTypeVariable)?;
                (true, found_type.clone())
            }

            _ => (false, ty.clone()),
        };

        Ok((replaced, result_type))
    }

    fn parameterized_name(name: &str, parameters: &[Type]) -> String {
        let type_strings: Vec<String> = parameters.iter().map(|t| t.to_string()).collect();

        format!("{}<{}>", name, type_strings.join(","))
    }

    fn instantiate_struct(
        struct_type: &StructTypeRef,
        type_variables: &TypeVariableScope,
    ) -> Result<(bool, Type), SemanticError> {
        let mut was_any_replaced = false;
        let mut new_fields = SeqMap::new();
        for (name, field) in &struct_type.anon_struct_type.defined_fields {
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

        let new_struct = StructType {
            name: struct_type.name.clone(),
            assigned_name: new_assigned_name,
            anon_struct_type: AnonymousStructType {
                defined_fields: new_fields,
            },
            number: 0, // TODO: generate unique number
        };

        let new_struct_ref = Rc::new(new_struct);

        Ok((was_any_replaced, Type::Struct(new_struct_ref)))
    }
}
