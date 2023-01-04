use std::fmt::Debug;
use std::io;
use std::{fmt::Display, rc::Rc};

use latte::frontend::{
    parser::{build_parser, latteparser::ProgramContextAll},
    semantic_analysis::{ast::Program, ConversionError, TypeCheckError},
};
use thiserror::Error;

#[derive(Error)]
enum Error {
    #[error("during parsing, described above")]
    Parse,

    #[error("Type check: {0}")]
    Conversion(ConversionError),

    #[error("Type check: {0}")]
    TypeCheck(TypeCheckError),

    #[error("IO error: {0}")]
    IO(#[from] io::Error),
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

fn main() -> Result<(), Error> {
    let filename = std::env::args().nth(1).expect("Filename arg missing");

    let (mut parser, was_error) = build_parser(&filename);
    let antlr_ast: Rc<ProgramContextAll<'_>> = match parser.program() {
        Ok(program) => program,
        Err(_) => return Err(Error::Parse),
    };

    if was_error.get() {
        return Err(Error::Parse);
    } else {
        let ast = Program::try_from(antlr_ast);
        match ast {
            Ok(ref ast) => match ast.type_check() {
                Ok(()) => eprintln!("OK"),
                Err(e) => {
                    eprintln!("ERROR");
                    return Err(Error::TypeCheck(e));
                }
            },
            Err(err) => {
                eprintln!("ERROR");
                return Err(Error::Conversion(err));
            }
        }
        let cfg = ast.unwrap().ir();

        let mut stdout = io::stdout();
        cfg.emit_assembly(&mut stdout)?;
    }

    Ok(())
}
