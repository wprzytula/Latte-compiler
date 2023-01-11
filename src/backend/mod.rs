#[allow(dead_code)]
pub mod asmgen;
pub mod ir;

use std::fmt::{Debug, Display};
use std::fs::{canonicalize, File};

use std::path::Path;
use std::process::{Command, ExitStatus};
use std::rc::Rc;
use std::{io, mem};

use crate::frontend::{
    parser::{build_parser, latteparser::ProgramContextAll},
    semantic_analysis::{ast::Program, ConversionError, TypeCheckError},
};
use thiserror::Error;

const RUNTIME_PATH: &str = "lib/runtime.o";

pub fn execute(exe: &str, args: &[&str]) -> io::Result<ExitStatus> {
    Command::new(exe).args(args).spawn()?.wait()
}

#[derive(Error)]
pub enum CompilerError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("during parsing, described above")]
    Parse,

    #[error("Type check: {0}")]
    Conversion(ConversionError),

    #[error("Type check: {0}")]
    TypeCheck(TypeCheckError),
}
impl Debug for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

pub fn compile(path: impl AsRef<Path>) -> Result<(), CompilerError> {
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
        Err(_) => return Err(CompilerError::Parse),
    };

    if was_error.get() {
        return Err(CompilerError::Parse);
    }
    let ast = Program::try_from(antlr_ast);
    let program = match ast {
        Ok(mut program) => match program.type_check() {
            Ok(()) => {
                eprintln!("OK");
                program.topo_sort_classes();
                program
            }
            Err(e) => {
                eprintln!("ERROR");
                return Err(CompilerError::TypeCheck(e));
            }
        },
        Err(err) => {
            eprintln!("ERROR");
            return Err(CompilerError::Conversion(err));
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
    )?
    .success()
    .then_some(())
    .unwrap_or_else(|| panic!("NASM error!"));

    execute(
        "gcc",
        &[
            // "-fsanitize=address",
            "-o",
            bin_filepath.to_str().unwrap(),
            RUNTIME_PATH,
            o_filepath.to_str().unwrap(),
        ],
    )?
    .success()
    .then_some(())
    .unwrap_or_else(|| panic!("GCC error!"));

    Ok(())
}
