# rust-ffi

A project to generate bindings to a rust foreign function interface for other languages.

This is only a proof of concept and not designed to be easy to use.

Currently, only generating a `C/C++` header for a rust foreign function interface is supported.

For more information see this introductory [blog post](http://dreamingofbits.com/post/future-directions-for-cbindgen-rust-ffi/).

## Overview

This tool is split into several parts.

1. `metadata-ffi`: A schema for describing a rust foreign function interface that can be communicated between tools
1. `emit-ffi`: A rust compiler driver that will ouput a `ffi.json` for a crate
1. `c-ast`: A `C/C++` abstract syntax tree designed for code generation
1. `c-ffi`: A library to convert a `ffi.json` into a `C/C++` AST and output it as a header
1. `cargo-ffi`: A cargo subcommand to use `emit-ffi` for a crate, then translate the `ffi.json` into a `C/C++` header

## Usage

```bash
cd ~/path/to/project/
cargo +nightly ffi
```

The header will be output in the current working directory, named after the crate.

No command line options or configuration files are currently supported.

## Example

See [`example/`](example/) for a test crate with generated headers already made.

## Building

The `emit-ffi` driver links to the `rustc` compiler internals and requires a nightly compiler.

The latest compiler confirmed to work is:
```
rustc 1.34.0-nightly (4b1e39b7b 2019-02-05)
```

You can install nightly `rustc` using `rustup`:

```bash
rustup toolchain add nightly
```

To build this workspace using `rustup`:

```bash
cargo +nightly build
```

Additionally, `emit-ffi` requires libraries from the sysroot to be in the parent directory:

```bash
cp -R ~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib target/
set -gx DYLD_LIBRARY_PATH '~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib'
```

Be sure to re-run that command after deleting the target directory, such as after `cargo clean`.

In order to use `cargo ffi`, `cargo-ffi` must be in the current `$PATH`:

```bash
export PATH='path/to/rust-ffi/target/debug/':$PATH
```

### Example

```bash
cd ~/Projects/rust-ffi/
cargo +nightly build

cp -R ~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib target/
export PATH='$PWD/target/debug/':$PATH

cd example/easy
cargo +nightly ffi
```

## Warning

Do not use `cargo-ffi` in the `rust-ffi` directory or you will need to rebuild.

`cargo-ffi` will perform a clean before generating a header to ensure the shim driver will be used, this will remove the `emit-ffi` binary before it can be used.

## Future work

This is currently just a proof of concept.

The significant work items remaining are:

1. `C/C++` output for `enum`'s containing fields
1. Generics
1. Statics
1. Zero sized types
1. `repr(transparent)`
1. Opaque type propagation
1. `fn` ABI specifications
1. Documentation comments
1. Configuration/extensibility
1. Tests
