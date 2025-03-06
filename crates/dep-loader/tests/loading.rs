/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_script_dep_loader::mount_name_from_path;
#[test]
fn do_something() {
    mount_name_from_path(&["tjoho".into()]);
}
