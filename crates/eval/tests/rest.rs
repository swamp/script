/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check;

#[test_log::test]
fn struct_fill_rest() {
    check(
        r"
    struct SpriteParams {
        rotate: Int,
        i: Float,
    }

    impl SpriteParams {
        fn default() -> SpriteParams {
            SpriteParams {
                rotate: -1,
                i: 20.0,
            }
        }
    }

    a = SpriteParams {
        i: 3.3,
        ..
    }

    print(a)
    ",
        "
    SpriteParams { rotate: -1, i: 3.30 }
    ",
    )
}
