use crate::Signature;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Hash, Clone)]
pub enum IntrinsicFunction {
    FloatRound,
    FloatFloor,
    FloatSqrt,
    FloatSign,
    FloatAbs,
    FloatRnd,
    FloatCos,
    FloatSin,
    FloatAcos,
    FloatAsin,
    FloatAtan2,
    FloatMin,
    FloatMax,
    FloatClamp,

    // Int
    IntAbs,
    IntRnd,
    IntMax,
    IntMin,
    IntClamp,
    IntToFloat,

    // String
    StringLen,

    // Vec
    VecFromSlice,
    VecPush,
    VecPop,
    VecRemoveIndex,
    VecClear,
    VecCreate,
    VecSubscript,
    VecSubscriptMut,
    VecIter,
    VecIterMut,

    // Map
    MapCreate,
    MapFromSlicePair,
    MapHas,
    MapRemove,
    MapIter,
    MapIterMut,
    MapSubscript,
    MapSubscriptMut,

    // Sparse
    SparseCreate,
    SparseFromSlice,
    SparseIter,
    SparseIterMut,
    SparseSubscript,
    SparseSubscriptMut,
    SparseHas,
    SparseRemove,

    Float2Magnitude,
    SparseAdd,
    VecLen,
    VecIsEmpty,
}

pub type IntrinsicFunctionDefinitionRef = Rc<IntrinsicFunctionDefinition>;
#[derive(Clone, Debug)]
pub struct IntrinsicFunctionDefinition {
    pub name: String,
    pub signature: Signature,
    pub intrinsic: IntrinsicFunction,
}

impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            // Float
            Self::FloatRound => "float_round",
            Self::FloatFloor => "float_floor",
            Self::FloatSqrt => "float_sqrt",
            Self::FloatSign => "float_sign",
            Self::FloatAbs => "float_abs",
            Self::FloatRnd => "float_rnd",
            Self::FloatCos => "float_cos",
            Self::FloatSin => "float_sin",
            Self::FloatAcos => "float_acos",
            Self::FloatAsin => "float_asin",
            Self::FloatAtan2 => "float_atan2",
            Self::FloatMin => "float_min",
            Self::FloatMax => "float_max",
            Self::FloatClamp => "float_clamp",

            // Int
            Self::IntAbs => "int_abs",
            Self::IntRnd => "int_rnd",
            Self::IntMax => "int_max",
            Self::IntMin => "int_min",
            Self::IntToFloat => "int_to_float",
            Self::IntClamp => "int_clamp",

            // String
            Self::StringLen => "string_len",

            // Vec
            Self::VecFromSlice => "vec_from_slice",
            Self::VecPush => "vec_push",
            Self::VecPop => "vec_pop",
            Self::VecRemoveIndex => "vec_remove",
            Self::VecClear => "vec_clear",
            Self::VecCreate => "vec_create",
            Self::VecSubscriptMut => "vec_subscript_mut",
            Self::VecSubscript => "vec_subscript",
            Self::VecIter => "vec_iter",
            Self::VecIterMut => "vec_iter_mut",
            Self::VecLen => "vec_len",
            Self::VecIsEmpty => "vec_is_empty",

            // Map
            Self::MapCreate => "map_create",
            Self::MapFromSlicePair => "map_from_slice_pair",
            Self::MapHas => "map_has",
            Self::MapRemove => "map_remove",
            Self::MapSubscriptMut => "map_subscript_mut",
            Self::MapSubscript => "map_subscript",
            Self::MapIter => "map_iter",
            Self::MapIterMut => "map_iter_mut",

            // Sparse
            Self::SparseCreate => "sparse_create",
            Self::SparseFromSlice => "sparse_from_slice",
            Self::SparseAdd => "sparse_add",
            Self::SparseHas => "sparse_has",
            Self::SparseRemove => "sparse_remove",
            Self::SparseSubscriptMut => "sparse_subscript_mut",
            Self::SparseSubscript => "sparse_subscript",
            Self::SparseIter => "sparse_iter",
            Self::SparseIterMut => "sparse_iter_mut",

            // Other
            Self::Float2Magnitude => "float2_magnitude",
        };

        write!(f, "{name}")
    }
}
