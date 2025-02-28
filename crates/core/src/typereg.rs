/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use std::cell::RefCell;
use swamp_script_semantic::prelude::*;

#[derive(Debug)]
pub struct TypeRegistry {
    // Container type constructors
    #[allow(unused)]
    array_types: RefCell<SeqMap<TypeNumber, ArrayTypeRef>>,
    #[allow(unused)]
    struct_types: RefCell<SeqMap<String, StructTypeRef>>,

    // Type numbering
    next_type_number: RefCell<TypeNumber>,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    #[must_use]
    pub fn new() -> Self {
        Self {
            array_types: RefCell::new(SeqMap::new()),
            struct_types: RefCell::new(SeqMap::new()),
            next_type_number: RefCell::new(0),
        }
    }

    pub fn allocate_type_number(&self) -> TypeNumber {
        let mut number = self.next_type_number.borrow_mut();
        *number += 1;
        *number
    }

    // Primitive type getters
    pub const fn get_float_type(&self) -> Type {
        Type::Float
    }

    pub const fn get_int_type(&self) -> Type {
        Type::Int
    }

    pub const fn get_string_type(&self) -> Type {
        Type::String
    }

    pub const fn get_bool_type(&self) -> Type {
        Type::Bool
    }

    pub const fn get_unit_type(&self) -> Type {
        Type::Unit
    }

    // Container type constructors
    /*
        pub fn get_array_type(&self, item_type: Type) -> Type {
            let mut array_types = self.array_types.borrow_mut();
            if let Some(existing) = array_types.get(&item_type) {
                return Type::Array(existing.clone());
            }

            let array_type = Rc::new(ArrayType {
                item_type: item_type.clone(),
            });
            array_types.insert(item_type, array_type.clone());
            Type::Array(array_type)
        }
    */

    /*
    pub fn register_struct_type(
        &self,
        name: String,
        module_path: ModulePath,
        fields: SeqMap<IdentifierName, Type>,
    ) -> Type {
        let mut struct_types = self.struct_types.borrow_mut();
        if let Some(existing) = struct_types.get(&name) {
            // TODO: should include module_path
            return Type::Struct(existing.clone());
        }

        let number = self.allocate_type_number();
        let struct_type = Rc::new(RefCell::new(StructType {
            number,
            module_path,
            fields,
            name: LocalTypeIdentifier {
                text: name.clone(),
                node: Default::default(), // TODO: proper node info
            },
            ast_struct: Default::default(), // TODO: proper AST info
            functions: SeqMap::default(),
        }));

        struct_types
            .insert(name, struct_type.clone())
            .expect("could not insert name");
        Type::Struct(struct_type)
    }

    // Helper for deriving macros
    pub fn register_derived_struct(
        &self,
        name: &str,
        fields: Vec<(&str, Type)>,
    ) -> Type {
        let fields = fields
            .into_iter()
            .map(|(name, ty)| (IdentifierName(name.to_string()), ty))
            .collect();

        self.register_struct_type(
            name.to_string(),
            ModulePath(vec![]), // TODO: proper module path
            fields,
        )
    }

     */
}
