use simple_lang::{
    parsing::parse_file,
    typing::{TypingResult, type_items},
    validating::validate_items,
};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap_or_else(print_error_and_exit);

    let syntax_items = parse_file(filepath.into(), &source).unwrap_or_else(print_error_and_exit);
    drop(source);

    let validated_items = validate_items(&syntax_items).unwrap_or_else(print_error_and_exit);
    drop(syntax_items);

    let TypingResult {
        types,
        functions,
        global_names,
        errors,
    } = type_items(&validated_items);
    drop(validated_items);

    println!("{types:#?}");
    println!("{functions:#?}");
    println!("{global_names:#?}");
    for error in errors {
        println!("{error}");
    }
}

fn print_error_and_exit<T: std::fmt::Display, U>(error: T) -> U {
    eprintln!("{error}");
    std::process::exit(1)
}
