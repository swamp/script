/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

/*
pub fn new_hidden_rust_type<T: RustType + 'static>(
    name: &str,
    rust_description: RustTypeRef,
    value: T,
    namespace: &mut ModuleNamespace,
) -> Result<(Self, StructTypeRef), SemanticError> {
    let struct_type = namespace.util_insert_struct_type(name, &[("hidden", Type::Any)])?;
    let struct_value = Self::new_hidden_rust_struct(
        struct_type.clone(),
        rust_description,
        value,
        Type::Struct(struct_type.clone()),
    );
    Ok((struct_value, struct_type))
}


 */
pub mod sparse;
