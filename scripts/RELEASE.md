# Release

## Release example

- update version

```bash
cargo set-version "0.0.0"
cargo deps-order --workspace-only --exec "cargo readme-update"
```

- git commit and tag

- publish

```bash
cargo deps-order --workspace-only --exec "cargo publish" --wait 5
```

## Check line counts

```console
tokei crates/ --files --sort code --type rust
```
export MTL_DEBUG_LAYER=1
export MTL_DEVICE_WRAPPER_TYPE=1