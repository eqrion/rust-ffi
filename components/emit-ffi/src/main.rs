#![feature(box_syntax)]
#![feature(rustc_private)]

use std::env;
use std::process;

extern crate serde_json;

extern crate getopts;
extern crate rustc;
extern crate rustc_codegen_utils;
extern crate rustc_driver;
extern crate rustc_metadata;
extern crate rustc_target;
extern crate syntax;

extern crate metadata_ffi as ffi;

mod emit;

fn main() {
    rustc_driver::init_rustc_env_logger();
    let result = rustc_driver::run(|| {
        let args: Vec<_> = env::args().collect();
        rustc_driver::run_compiler(&args, box emit::Driver::new(), None, None)
    });
    process::exit(result as i32);
}
