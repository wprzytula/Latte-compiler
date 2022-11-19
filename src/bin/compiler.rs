// use latte::backend::compiler;
// use latte::frontend::{parse, Prog};
use std::error::Error;
use std::fs::{canonicalize, File};
use std::io::{BufRead, BufReader, Write, self};

use std::{mem, env};
use std::path::Path;
use std::process::{Command, ExitStatus};

const RUNTIME_PATH: &str = "lib/runtime.ll";

pub fn execute(exe: &str, args: &[&str]) -> io::Result<ExitStatus> {
    println!("{}", args.join(", "));
    Command::new(exe).args(args).spawn()?.wait()
}

fn main() -> Result<(), Box<dyn Error>> {
    return Ok(());
    // let tmp_path = env::var(key);
    let path = std::env::args().nth(1).expect("Filename arg missing");
    let canonical_path = canonicalize(path)?;
    let progname = canonical_path
        .file_stem()
        .expect("Path does not lead to a valid file")
        .to_str()
        .unwrap();
    let parent_path = canonical_path
        .parent()
        .map(|path| path.as_os_str().to_str())
        .unwrap_or(Some("./"))
        .unwrap();

    let latte_file = File::open(&canonical_path)?;
    let latte_reader = BufReader::new(latte_file);

    let lines = latte_reader.lines().map(|res| res.expect("IO error"));

    // let ast = Prog(parse(lines).collect());

    // let nasm_code = compiler::compile(ast);
    let s_filename = format!("{}.s", progname);
    let s_filepath = Path::new(parent_path).join(s_filename);
    let o_filename = format!("{}.o", progname);
    let o_filepath = Path::new(parent_path).join(o_filename);
    let bin_filename = progname.to_string();
    let bin_filepath = Path::new(parent_path).join(bin_filename);
    let mut s_file = File::create(&s_filepath)?;

    // for chunk in nasm_code {
    //     write!(s_file, "{}", chunk)?;
    // }
    mem::drop(s_file); // also flushes

    execute("nasm", &["-o", o_filepath.to_str().unwrap(), s_filepath.to_str().unwrap()])?;

    execute(
        "ld",
        &[
            "-s",
            "-o",
            bin_filepath.to_str().unwrap(),
            // RUNTIME_PATH,
            o_filepath.to_str().unwrap(),
        ],
    )?;

    Ok(())
}
