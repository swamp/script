use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::prelude::*;

#[derive(Debug)]
pub struct TypeRegistry {
    // Primitive types
    float_type: ResolvedFloatTypeRef,
    int_type: ResolvedIntTypeRef,
    string_type: ResolvedStringTypeRef,
    bool_type: ResolvedBoolTypeRef,
    unit_type: ResolvedUnitTypeRef,

    // Container type constructors
    #[allow(unused)]
    array_types: RefCell<SeqMap<TypeNumber, ResolvedArrayTypeRef>>,
    struct_types: RefCell<SeqMap<String, ResolvedStructTypeRef>>,

    // Type numbering
    next_type_number: RefCell<TypeNumber>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let registry = Self {
            float_type: Rc::new(ResolvedFloatType {}),
            int_type: Rc::new(ResolvedIntType {}),
            string_type: Rc::new(ResolvedStringType {}),
            bool_type: Rc::new(ResolvedBoolType {}),
            unit_type: Rc::new(ResolvedUnitType {}),
            array_types: RefCell::new(SeqMap::new()),
            struct_types: RefCell::new(SeqMap::new()),
            next_type_number: RefCell::new(0),
        };

        registry
    }

    fn allocate_type_number(&self) -> TypeNumber {
        let mut number = self.next_type_number.borrow_mut();
        *number += 1;
        *number
    }

    // Primitive type getters
    pub fn get_float_type(&self) -> ResolvedType {
        ResolvedType::Float(self.float_type.clone())
    }

    pub fn get_int_type(&self) -> ResolvedType {
        ResolvedType::Int(self.int_type.clone())
    }

    pub fn get_string_type(&self) -> ResolvedType {
        ResolvedType::String(self.string_type.clone())
    }

    pub fn get_bool_type(&self) -> ResolvedType {
        ResolvedType::Bool(self.bool_type.clone())
    }

    pub fn get_unit_type(&self) -> ResolvedType {
        ResolvedType::Unit(self.unit_type.clone())
    }

    // Container type constructors
    /*
        pub fn get_array_type(&self, item_type: ResolvedType) -> ResolvedType {
            let mut array_types = self.array_types.borrow_mut();
            if let Some(existing) = array_types.get(&item_type) {
                return ResolvedType::Array(existing.clone());
            }

            let array_type = Rc::new(ResolvedArrayType {
                item_type: item_type.clone(),
            });
            array_types.insert(item_type, array_type.clone());
            ResolvedType::Array(array_type)
        }
    */
    pub fn register_struct_type(
        &self,
        name: String,
        module_path: ModulePath,
        fields: SeqMap<IdentifierName, ResolvedType>,
    ) -> ResolvedType {
        let mut struct_types = self.struct_types.borrow_mut();
        if let Some(existing) = struct_types.get(&name) {
            // TODO: should include module_path
            return ResolvedType::Struct(existing.clone());
        }

        let number = self.allocate_type_number();
        let struct_type = Rc::new(RefCell::new(ResolvedStructType {
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

        struct_types.insert(name, struct_type.clone()).expect("could not insert name");
        ResolvedType::Struct(struct_type)
    }

    // Helper for deriving macros
    pub fn register_derived_struct(
        &self,
        name: &str,
        fields: Vec<(&str, ResolvedType)>,
    ) -> ResolvedType {
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
}
