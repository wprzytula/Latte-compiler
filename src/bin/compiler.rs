use std::fmt::{Debug, Display};
use std::fs::{canonicalize, File};

use std::path::Path;
use std::process::{Command, ExitStatus};
use std::rc::Rc;
use std::{io, mem};

use latte::frontend::{
    parser::{build_parser, latteparser::ProgramContextAll},
    semantic_analysis::{ast::Program, ConversionError, TypeCheckError},
};
use thiserror::Error;

const RUNTIME_PATH: &str = "lib/runtime.o";

pub fn execute(exe: &str, args: &[&str]) -> io::Result<ExitStatus> {
    Command::new(exe).args(args).spawn()?.wait()
}

#[derive(Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("during parsing, described above")]
    Parse,

    #[error("Type check: {0}")]
    Conversion(ConversionError),

    #[error("Type check: {0}")]
    TypeCheck(TypeCheckError),
}
impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

fn main() -> Result<(), Error> {
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

    let (mut parser, was_error) = build_parser(&canonical_path);
    let antlr_ast: Rc<ProgramContextAll<'_>> = match parser.program() {
        Ok(program) => program,
        Err(_) => return Err(Error::Parse),
    };

    if was_error.get() {
        return Err(Error::Parse);
    }
    let ast = Program::try_from(antlr_ast);
    let program = match ast {
        Ok(program) => match program.type_check() {
            Ok(()) => {
                eprintln!("OK");
                program
            }
            Err(e) => {
                eprintln!("ERROR");
                return Err(Error::TypeCheck(e));
            }
        },
        Err(err) => {
            eprintln!("ERROR");
            return Err(Error::Conversion(err));
        }
    };

    let cfg = program.ir();

    let asm_filename = format!("{}.asm", progname);
    let asm_filepath = Path::new(parent_path).join(asm_filename);
    let mut asm_file = File::create(&asm_filepath)?;

    cfg.emit_assembly(&mut asm_file)?;
    mem::drop(asm_file); // also flushes

    let o_filename = format!("{}.o", progname);
    let o_filepath = Path::new(parent_path).join(o_filename);
    let bin_filename = progname.to_string();
    let bin_filepath = Path::new(parent_path).join(bin_filename);

    execute(
        "nasm",
        &[
            "-f",
            "elf64",
            "-o",
            o_filepath.to_str().unwrap(),
            asm_filepath.to_str().unwrap(),
        ],
    )?;

    execute(
        "gcc",
        &[
            "-o",
            bin_filepath.to_str().unwrap(),
            RUNTIME_PATH,
            o_filepath.to_str().unwrap(),
        ],
    )?;

    Ok(())
}
