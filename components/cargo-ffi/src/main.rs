extern crate clap;
extern crate serde_json;

extern crate c_ffi;

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use clap::{App, Arg, SubCommand};

fn main() {
    let _ = App::new("cargo-ffi")
                      .subcommand(
                        SubCommand::with_name("ffi")
                          .version("0.1.0")
                          .author("Ryan Hunt <rhunt@eqrion.net>")
                          .about("Generate FFI bindings for rust crates")
                          .arg(Arg::with_name("crate")
                               .long("crate")
                               .value_name("NAME")
                               .help("Sets the name of the crate in the current project to generate bindings for")
                               .takes_value(true))
                      )
                      .get_matches();

    let mut binary_dir = env::current_exe().expect("could not find binary directory");
    binary_dir.set_file_name("emit-ffi");

    env::set_var("RUSTC", binary_dir.as_os_str());

    let cargo = env::var_os("CARGO").unwrap_or("cargo".into());

    let _ = process::Command::new(&cargo)
        .args(&["clean"])
        .status()
        .expect("command failed to run");

    let _ = process::Command::new(&cargo)
        .args(&["check"])
        .status()
        .expect("command failed to run");

    for file in
        fs::read_dir(env::current_dir().unwrap()).expect("couldn't read the current directory")
    {
        let file = file.unwrap().path();
        if file.extension().and_then(|x| x.to_str()) == Some("json".into()) {
            run(file.file_stem().and_then(|x| x.to_str()).unwrap(), &file);
        }
    }
}

fn run(crate_name: &str, path: &Path) {
    println!("run {:?} {:?}", crate_name, path);
    let json = fs::read_to_string(path).unwrap();
    let parsed = serde_json::from_str(&json).expect("couldn't parse ffi.json");
    let out_path = PathBuf::from(path).with_extension("h");
    let out_file = fs::File::create(out_path).expect("couldn't open file for bindings");
    let config = c_ffi::Config::default();
    c_ffi::generate(parsed, config, out_file)
}
