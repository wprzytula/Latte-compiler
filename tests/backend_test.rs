use std::{
    fs::{read_dir, File},
    path::Path,
    process::{Command, Stdio},
};

use latte::{
    backend::compile,
    frontend::{
        parser::build_parser,
        semantic_analysis::{ast::Program, TypeCheckError},
    },
};

#[derive(Debug)]
struct ParseError;

fn parse_file<P: AsRef<Path>>(filename: P) -> Result<Program, ParseError> {
    let (mut parser, was_error) = build_parser(&filename);
    let ast = if let Ok(program) = parser.program() {
        program
    } else {
        return Err(ParseError);
    };

    if was_error.get() {
        return Err(ParseError);
    }
    match Program::try_from(ast) {
        Ok(prog) => Ok(prog),
        Err(_) => Err(ParseError),
    }
}

#[test]
fn lattests_compile_core() {
    let test_path = Path::new("/home/xps15/Studia/Sem7/MRJP/Laby/Latte/lattests");

    let good_paths = [
        "lattests/good",
        "mrjp-tests/good/basic",
        "wp/good",
        "pp/good",
    ];

    let good = good_paths
        .iter()
        .map(|&path| {
            let case_path = test_path.join(path);
            read_dir(&case_path)
                .unwrap()
                .map(move |entry| (case_path.clone(), entry))
        })
        .flatten();

    for (path, file) in good {
        let file = file.unwrap();
        let os_name = file.file_name();
        let name = os_name.to_str().unwrap();
        if file.file_type().unwrap().is_file() && name.ends_with(".lat") {
            println!("\nCOMPILING {:?}", &file);
            let full_path = path.join(name);

            let prog = parse_file(path.join(name))
                .unwrap_or_else(|err| panic!("Parse failed: {}\n{:#?}", full_path.display(), err));

            prog.type_check().unwrap_or_else(|err| {
                panic!("Type check failed: {}\n{:#?}", full_path.display(), err)
            });

            compile(full_path.clone()).unwrap_or_else(|err| {
                panic!("Compilation failed: {}\n{:#?}", full_path.display(), err)
            });

            let output_name = name.replace(".lat", ".output");
            if let Some((bin_path, full_output_path)) =
                name.rsplit_once(".lat").and_then(|(bin_name, _)| {
                    let bin_path = path.join(bin_name);
                    let full_output_path = path.join(output_name);
                    (full_output_path.exists() && bin_path.exists())
                        .then_some((bin_path, full_output_path))
                })
            {
                println!(
                    "Running file {} and comparing output with provided.",
                    bin_path.display()
                );

                let bin_exec = Command::new(bin_path)
                    .stderr(Stdio::inherit())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("Bin spawn error")
                    .wait()
                    .expect("Bin wait error");

                let diff = Command::new("diff")
                    .arg("-")
                    .arg(full_output_path)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::inherit())
                    .stderr(Stdio::inherit())
                    .spawn()
                    .expect("Diff spawn error")
                    .wait()
                    .expect("Diff wait error");

                if !diff.success() {
                    panic!("Output differs for test: {}", full_path.display());
                }
            }
        }
    }
}