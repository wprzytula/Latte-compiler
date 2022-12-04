use std::{fs::read_dir, iter::repeat, path::Path};

use latte::frontend::{
    parser::build_parser,
    semantic_analysis::{Program, TypeCheckError},
};

#[derive(Debug)]
struct ParseError;

fn parse_file<P: AsRef<Path>>(filename: P) -> Result<(), ParseError> {
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
        Ok(_) => Ok(()),
        Err(_) => Err(ParseError),
    }
}

#[test]
fn lattests_parse() {
    let test_path = Path::new("/home/xps15/Studia/Sem7/MRJP/Laby/Latte/lattests");

    let bad_paths = [
        "lattests/bad/parse",
        "wp/bad/parse",
        "mrjp-tests/bad/parse",
        "margdoc/bad/parse",
    ];
    let good_paths = [
        "lattests/bad/typecheck",
        "lattests/good",
        "mrjp-tests/good/basic",
        "mrjp-tests/bad/semantic",
        "mrjp-tests/bad/runtime",
        "mrjp-tests/bad/infinite_loop",
        "margdoc/good/extensions",
        "margdoc/bad/semantic",
        "margdoc/bad/casting",
        "lattests/extensions/arrays1",
        "lattests/extensions/objects1",
        "lattests/extensions/objects2",
        "lattests/extensions/struct",
        "wp/good",
    ];

    let bad = bad_paths
        .iter()
        .map(|&path| {
            let case_path = test_path.join(path);
            read_dir(&case_path)
                .unwrap()
                .map(move |entry| (case_path.clone(), entry))
        })
        .flatten();
    let good = good_paths
        .iter()
        .map(|&path| {
            let case_path = test_path.join(path);
            read_dir(&case_path)
                .unwrap()
                .map(move |entry| (case_path.clone(), entry))
        })
        .flatten();

    let chained = repeat(false).zip(bad).chain(repeat(true).zip(good));
    for (good, (path, file)) in chained {
        let file = file.unwrap();
        println!("{:?}", &file);
        let os_name = file.file_name();
        let name = os_name.to_str().unwrap();
        if file.file_type().unwrap().is_file() && name.ends_with(".lat") {
            let res = parse_file(path.join(name));
            if good {
                res.unwrap();
            } else {
                res.unwrap_err();
            }
        }
    }
}

fn typecheck_file<P: AsRef<Path>>(filename: P) -> Result<(), TypeCheckError> {
    let (mut parser, was_error) = build_parser(&filename);
    let ast = parser.program().expect("Error while parsing!");

    if was_error.get() {
        panic!("Error while parsing!")
    }
    let program = Program::try_from(ast).unwrap();
    program.type_check()
}

#[test]
fn lattests_typecheck() {
    let test_path = Path::new("/home/xps15/Studia/Sem7/MRJP/Laby/Latte/lattests");

    let bad_paths = [
        "lattests/bad/typecheck",
        "wp/bad/typecheck",
        "mrjp-tests/bad/semantic",
        "margdoc/bad/semantic",
        "margdoc/bad/casting",
        "pp/bad",
    ];
    let good_paths = [
        "lattests/good",
        "mrjp-tests/good/basic",
        "lattests/extensions/arrays1",
        "lattests/extensions/objects1",
        "lattests/extensions/objects2",
        "lattests/extensions/struct",
        "wp/good",
        "margdoc/good/extensions",
        "pp/good",
    ];

    let bad = bad_paths
        .iter()
        .map(|&path| {
            let case_path = test_path.join(path);
            read_dir(&case_path)
                .unwrap()
                .map(move |entry| (case_path.clone(), entry))
        })
        .flatten();
    let good = good_paths
        .iter()
        .map(|&path| {
            let case_path = test_path.join(path);
            read_dir(&case_path)
                .unwrap()
                .map(move |entry| (case_path.clone(), entry))
        })
        .flatten();

    let chained = repeat(false).zip(bad).chain(repeat(true).zip(good));
    for (good, (path, file)) in chained {
        let file = file.unwrap();
        println!("{:?}", &file);
        let os_name = file.file_name();
        let name = os_name.to_str().unwrap();
        if file.file_type().unwrap().is_file() && name.ends_with(".lat") {
            let full_path = path.join(name);
            let res = typecheck_file(path.join(name));
            if good {
                res.unwrap_or_else(|err| {
                    panic!(
                        "Test failed but should succeed: {}\n{:#?}",
                        full_path.display(),
                        err
                    )
                });
            } else {
                res.err().unwrap_or_else(|| {
                    panic!("Test succeeded but should fail: {}", full_path.display())
                });
            }
        }
    }
}
