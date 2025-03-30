/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_types::prelude::*;

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
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecSubscript,
    VecSubscriptMut,
    VecSubscriptRange,
    VecIter,
    VecIterMut,
    VecFor,
    VecWhile,
    VecFindMap,
    VecAny,
    VecAll,
    VecMap,
    VecFilter,
    VecFilterMap,
    VecFind,
    VecFold,
    VecSwap,
    VecInsert,
    VecFirst,
    VecGet,
    VecLast,

    VecSelfPush,
    VecSelfExtend,

    // Map
    MapCreate,
    MapFromSlicePair,
    MapHas,
    MapRemove,
    MapIter,
    MapIterMut,
    MapLen,
    MapIsEmpty,
    MapSubscript,
    MapSubscriptSet,
    MapSubscriptMut,
    MapSubscriptMutCreateIfNeeded,

    // Map2
    Map2Create,
    Map2Insert,
    Map2Remove,
    Map2Get,
    Map2GetColumn,
    Map2GetRow,
    Map2Has,

    // Sparse
    SparseCreate,
    SparseFromSlice,
    SparseIter,
    SparseIterMut,
    SparseSubscript,
    SparseSubscriptMut,
    SparseHas,
    SparseRemove,

    // Grid
    GridCreate,
    GridFromSlice,
    GridGetColumn,
    //GridIter,
    //GridIterMut,
    GridSet,
    GridGet,
    //GridSubscriptMut,
    Float2Magnitude,
    SparseAdd,
    VecLen,
    VecIsEmpty,
    SparseNew,
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
            Self::VecRemoveIndex => "vec_remove_index",
            Self::VecRemoveIndexGetValue => "vec_remove_index_get_value",
            Self::VecClear => "vec_clear",
            Self::VecGet => "vec_get",
            Self::VecCreate => "vec_create",
            Self::VecSubscriptMut => "vec_subscript_mut",
            Self::VecSubscript => "vec_subscript",
            Self::VecSubscriptRange => "vec_subscript_range",
            Self::VecIter => "vec_iter",
            Self::VecIterMut => "vec_iter_mut",
            Self::VecFor => "vec_for",
            Self::VecWhile => "vec_while",
            Self::VecFindMap => "vec_find_map",
            Self::VecLen => "vec_len",
            Self::VecIsEmpty => "vec_is_empty",
            Self::VecAny => "vec_any",
            Self::VecAll => "vec_all",
            Self::VecMap => "vec_map",
            Self::VecFilter => "vec_filter",
            Self::VecFilterMap => "vec_filter_map",
            Self::VecFind => "vec_find",
            Self::VecFold => "vec_fold",
            Self::VecSwap => "vec_swap",
            Self::VecInsert => "vec_insert",
            Self::VecFirst => "vec_first",
            Self::VecLast => "vec_last",

            Self::VecSelfPush => "vec_self_push",
            Self::VecSelfExtend => "vec_self_extend",

            // Map
            Self::MapCreate => "map_create",
            Self::MapFromSlicePair => "map_from_slice_pair",
            Self::MapHas => "map_has",
            Self::MapRemove => "map_remove",
            Self::MapSubscriptMut => "map_subscript_mut",
            Self::MapSubscriptMutCreateIfNeeded => "map_subscript_mut_create_if_needed",
            Self::MapSubscript => "map_subscript",
            Self::MapSubscriptSet => "map_subscript_set",
            Self::MapIter => "map_iter",
            Self::MapIterMut => "map_iter_mut",
            Self::MapLen => "map_len",
            Self::MapIsEmpty => "map_is_empty",

            // Map2
            Self::Map2Create => "map2_create",
            Self::Map2Insert => "map2_insert",
            Self::Map2Remove => "map2_remove",
            Self::Map2Has => "map2_has",
            Self::Map2GetColumn => "map2_get_column",
            Self::Map2GetRow => "map2_get_row",
            Self::Map2Get => "map2_get",

            // Sparse
            Self::SparseNew => "sparse_new",
            Self::SparseCreate => "sparse_create",
            Self::SparseFromSlice => "sparse_from_slice",
            Self::SparseAdd => "sparse_add",
            Self::SparseHas => "sparse_has",
            Self::SparseRemove => "sparse_remove",
            Self::SparseSubscriptMut => "sparse_subscript_mut",
            Self::SparseSubscript => "sparse_subscript",
            Self::SparseIter => "sparse_iter",
            Self::SparseIterMut => "sparse_iter_mut",

            // Grid
            Self::GridCreate => "grid_new",
            Self::GridSet => "grid_set",
            Self::GridGet => "grid_get",
            Self::GridFromSlice => "grid_from_slice",
            Self::GridGetColumn => "grid_get_column",

            // Other
            Self::Float2Magnitude => "float2_magnitude",
        };

        write!(f, "{name}")
    }
}
