use latte::backend::{compile, CompilerError};

fn main() -> Result<(), CompilerError> {
    let path = std::env::args().nth(1).expect("Filename arg missing");
    let optimisations = std::env::args().nth(2).is_none();
    env_logger::builder().format_timestamp(None).init();
    compile(path, optimisations)
}
