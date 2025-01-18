/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::value::Value;
use crate::value::ValueRef;
use swamp_script_semantic::ResolvedStructTypeRef;
use tracing::info;

pub fn overwrite_value(target: &ValueRef, source: Value) {
    if let Value::Struct(ref mut target_struct_type_ref, ref mut target_fields) =
        *target.borrow_mut()
    {
        if let Value::Struct(source_struct_type_ref, source_fields) = source {
            overwrite_struct(
                target_struct_type_ref.clone(),
                target_fields,
                source_struct_type_ref,
                source_fields,
            );
        }
    }
}

/// # Panics
///
pub fn overwrite_struct(
    target_struct_type_ref: ResolvedStructTypeRef,
    target_values: &mut Vec<ValueRef>,
    source_struct: ResolvedStructTypeRef,
    source_values: Vec<ValueRef>,
) {
    let borrowed_source_struct_type = source_struct.borrow();
    let source_anon_type = &borrowed_source_struct_type.anon_struct_type;

    for ((field_name, target_field_type), target_field_value) in target_struct_type_ref
        .borrow()
        .anon_struct_type
        .defined_fields
        .iter()
        .zip(target_values)
    {
        if let Some(source_field_type) = source_anon_type.defined_fields.get(field_name) {
            if source_field_type
                .field_type
                .same_type(&target_field_type.field_type)
            {
                let index = source_anon_type
                    .defined_fields
                    .get_index(field_name)
                    .expect("should work, we checked get()");
                info!(?target_field_type, ?field_name, "overwriting!");
                *target_field_value.borrow_mut() = source_values[index].borrow().clone();
            } else {
                info!(
                    ?target_field_type,
                    ?source_field_type,
                    ?field_name,
                    "skipping, wrong type"
                );
            }
        }
    }
}
