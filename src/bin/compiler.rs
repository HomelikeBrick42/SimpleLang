use simple_lang::{parser::parse_file, validate_syntax::validate_item};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap_or_else(|error| {
        eprintln!("Failed to read '{filepath}': {error:?}");
        std::process::exit(1)
    });

    let syntax_tree = parse_file(filepath.into(), &source).unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1)
    });
    drop(source);

    let asts = syntax_tree
        .into_iter()
        .map(|item| validate_item(&item))
        .collect::<Result<Box<[_]>, _>>()
        .unwrap_or_else(|error| {
            eprintln!("{error}");
            std::process::exit(1)
        });

    println!("{asts:#?}");
}
