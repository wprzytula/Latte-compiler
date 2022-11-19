use std::{fs::read_dir, path::Path, iter::repeat};

use latte::frontend::parser::build_parser;

#[derive(Debug)]
struct ParseError;

fn parse_file<P: AsRef<Path>>(filename: P) -> Result<(), ParseError> {
    let (mut parser, was_error, _) = build_parser(&filename);
    let ast = parser.program().unwrap();

    if was_error.get() {
        return Err(ParseError);
    }
    Ok(())
}

#[test]
fn lattests_parse() {
    let test_path = Path::new("/home/xps15/Studia/Sem7/MRJP/Laby/Latte/lattests/lattests");

    let bad_paths = ["bad/parse"];
    let good_paths = ["good", "extensions/arrays1", "extensions/objects1", "extensions/objects2", "extensions/struct"];

    let bad = bad_paths.iter().map(|&path| {
        let case_path = test_path.join(path);
        read_dir(&case_path).unwrap().map(move |entry| (case_path.clone(), entry))
    }).flatten();
    let good = good_paths.iter().map(|&path| {
        let case_path = test_path.join(path);
        read_dir(&case_path).unwrap().map(move |entry| (case_path.clone(), entry))
    }).flatten();

    let chained = repeat(false).zip(bad).chain(repeat(true).zip(good));
    for (good, (path, file, )) in chained {
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