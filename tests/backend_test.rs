use std::{
    fs::{read_dir, File},
    io::{Read, Write},
    path::Path,
    process::{Command, Stdio},
};

use latte::{
    backend::compile,
    frontend::{parser::build_parser, semantic_analysis::ast::Program},
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
    lattests_compile(&[
        "lattests/good",
        "mrjp-tests/good/basic",
        "pp/good/core",
        "wp/good/core",
        "margdoc/good",
    ]);
}

#[test]
fn lattests_compile_structs() {
    lattests_compile(&["lattests/extensions/struct"]);
}

#[test]
fn lattests_compile_classes() {
    lattests_compile(&["lattests/extensions/objects1"]);
}

// #[test]
fn lattests_compile_arrays() {
    lattests_compile(&["lattests/extensions/arrays1"]);
}

#[test]
fn lattests_compile_virtual_methods() {
    lattests_compile(&["lattests/extensions/objects2"]);
}

fn lattests_compile(good_paths: &[&str]) {
    let test_path = Path::new("/home/xps15/Studia/Sem7/MRJP/Laby/Latte/lattests");
    let _ = env_logger::builder().format_timestamp(None).try_init();
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

            compile(full_path.clone(), false).unwrap_or_else(|err| {
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

                let input_name = name.replace(".lat", ".input");
                let input = {
                    let full_input_path = path.join(input_name);
                    full_input_path.exists().then(|| {
                        let mut buf = String::new();
                        File::open(full_input_path)
                            .unwrap()
                            .read_to_string(&mut buf)
                            .unwrap();
                        buf
                    })
                };

                let mut diff = Command::new("diff")
                    .arg("-")
                    .arg(full_output_path)
                    .stdin(Stdio::piped())
                    .spawn()
                    .expect("Diff spawn error");

                let mut bin_exec = Command::new(bin_path)
                    .stdin(Stdio::piped())
                    .stderr(Stdio::inherit())
                    .stdout(diff.stdin.take().unwrap())
                    .spawn()
                    .expect("Bin spawn error");

                if let Some(input) = input {
                    write!(bin_exec.stdin.as_mut().unwrap(), "{}", input).unwrap();
                }

                bin_exec.wait().expect("Bin wait error");

                if !diff.wait().map(|res| res.success()).unwrap_or(false) {
                    panic!("Output differs for test: {}", full_path.display());
                }
            }
        }
    }
}
