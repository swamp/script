/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::alloc::FrameMemoryRegion;
use crate::ctx::Context;
use crate::{Error, FunctionCodeGen};
use swamp_semantic::{LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression};

impl FunctionCodeGen<'_> {
    pub(crate) fn gen_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FrameMemoryRegion, Error> {
        self.gen_for_access_or_location_ex(&mut_or_immutable_expression)
    }

    pub(crate) fn gen_mut_or_immute(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_lvalue_address(location_expression)?;
            }
        }

        Ok(())
    }

    pub(crate) fn gen_argument(
        &mut self,
        argument: &MutRefOrImmutableExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        match &argument {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_location_argument(location_expression, ctx, comment)?;
            }
        }
        Ok(())
    }

    pub(crate) fn gen_for_access_or_location_ex(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FrameMemoryRegion, Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression_for_access(found_expression)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_lvalue_address(location_expression)
            }
        }
    }

    pub(crate) fn gen_lvalue_address(
        &mut self,
        location_expression: &SingleLocationExpression,
    ) -> Result<FrameMemoryRegion, Error> {
        let mut frame_relative_base_address = *self
            .variable_offsets
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap();

        // Loop over the consecutive accesses until we find the actual location
        for access in &location_expression.access_chain {
            match &access.kind {
                LocationAccessKind::FieldIndex(anonymous_struct_type, field_index) => {
                    let (memory_offset, memory_size, _max_alignment) =
                        Self::get_struct_field_offset(
                            &anonymous_struct_type.field_name_sorted_fields,
                            *field_index,
                        );
                    frame_relative_base_address = FrameMemoryRegion::new(
                        frame_relative_base_address.addr.advance(memory_offset),
                        memory_size,
                    );
                }
                LocationAccessKind::IntrinsicCallMut(
                    _intrinsic_function,
                    _arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    // arguments can be things like the key_value or the int index in a vector
                    todo!()
                }
            }
        }

        Ok(frame_relative_base_address)
    }
}
