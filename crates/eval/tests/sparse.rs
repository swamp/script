/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

mod util;

use crate::util::{check, eval_string};

#[test_log::test]
fn sparse_map_create() {
    eval_string(
        "

    sparse = Sparse<Int>::new()
    ",
        "Sparse<Int(ResolvedIntType)> len:0",
    );
}

#[test_log::test]
fn sparse_map_add() {
    eval_string(
        "

    mut sparse = Sparse<Int>::new()
    print(sparse)
    id = sparse.add(2)
    print(id)
    id
    ",
        "id:0:0",
    );
}

#[test_log::test]
fn sparse_map_remove() {
    eval_string(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)

    print(sparse)
    print(sparse_id)

    sparse.remove(sparse_id)
    ",
        "Sparse<Int(ResolvedIntType)> len:0",
    );
}

#[test_log::test]
fn sparse_map_iterate() {
    check(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)
    sparse.add(3)

    print(sparse)
    print(sparse_id)

    for x in sparse {
        print(x)
    }
    ",
        "\
    Sparse<Int(ResolvedIntType)> len:2
    id:0:0
    2
    3
    ",
    );
}

#[test_log::test]
fn sparse_map_struct_iterate() {
    check(
        "

    struct Projectile {
        x: Float,
        y: Float,
    }

    mut sparse = Sparse<Projectile>::new()
    sparse_id = sparse.add(Projectile {x: 1.0, y: 2.0})
    sparse.add(Projectile {x: 1.0, y: 3.0})

    print(sparse)
    print(sparse_id)

    for mut x in sparse {
        print(x)
    }
    ",
        "\
    Sparse<Projectile { x: Float, y: Float }> len:2
    id:0:0
    Projectile { x: 1.00, y: 2.00 }
    Projectile { x: 1.00, y: 3.00 }
    ",
    );
}

#[test_log::test]
fn sparse_map_struct_empty_iterator() {
    check(
        "

    struct Projectile {
        x: Float,
        y: Float,
    }

    mut sparse = Sparse<Projectile>::new()
    print('before')
    for x in sparse {
        print(x)
    }
    ",
        "\
        before
    ",
    );
}

#[test_log::test]
fn sparse_map_iterate_pairs() {
    check(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)
    sparse.add(3)

    print(sparse)
    print(sparse_id)

    for x, y in sparse {
        print(x)
        print(y)
    }
    ",
        "\
    Sparse<Int(ResolvedIntType)> len:2
    id:0:0
    id:0:0
    2
    id:1:0
    3
    ",
    );
}

#[test_log::test]
fn sparse_map_in_field() {
    check(
        "
    struct Hello {
        sparse: std::Sparse<Int>,
    }

    h = Hello {
        sparse: Sparse<Int>::new()
    }
    ",
        "\
    ",
    );
}
