use std::rc::Rc;

use latte::frontend::{
    parser::{build_parser, latteparser::ProgramContextAll},
    semantic_analysis::Program,
};

#[derive(Debug)]
struct ParseError;

fn main() -> Result<(), ParseError> {
    let filename = std::env::args().nth(1).expect("Filename arg missing");

    let (mut parser, was_error, _) = build_parser(&filename);
    let antlr_ast: Rc<ProgramContextAll<'_>> = parser.program().unwrap();

    if was_error.get() {
        return Err(ParseError);
    } else {
        eprintln!("Parsed successfully!");
        let ast = Program::from(antlr_ast);
        eprintln!("Converted successfully!\n{:#?}", &ast);
    }

    Ok(())
}
