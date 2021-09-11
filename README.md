# import_map

[![](https://img.shields.io/crates/v/import_map.svg)](https://crates.io/crates/import_map)
[![Discord Chat](https://img.shields.io/discord/684898665143206084?logo=discord&style=social)](https://discord.gg/deno)

A Rust implementation of
[WICG Import Maps specification](https://github.com/WICG/import-maps).

This crates is used in [`Deno`](https://github.com/denoland/deno) project.

The implementation is tested against WPT test suite.

## Developing

Make sure to have latest stable version of Rust installed (1.55.0).

```shell
// check version
$ rustc --version
rustc 1.55.0 (c8dfcfe04 2021-09-06)

// checkout git submodules
$ git submodule update --init --recursive

// build all targets
$ cargo build --all-targets

// test it
$ cargo test
```

## Opening a pull request

1. Format code: `rustfmt ./src/lib.rs`

2. Check that `clippy` doesn't produce warnings:
   `cargo clippy --all-targets --all-features --release -- -D clippy::all`
