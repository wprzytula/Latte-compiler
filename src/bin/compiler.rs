use latte::backend::{compile, CompilerError};

fn main() -> Result<(), CompilerError> {
    let path = std::env::args().nth(1).expect("Filename arg missing");
    compile(path)
}
