use simple_lang::{
    ids::{Id, IdMap},
    inferred_tree as it,
    inferring::{InferError, InferErrorKind, InferResult, infer_items},
    parsing::parse_file,
    validating::validate_items,
};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap_or_else(print_error_and_exit);

    let syntax_items = parse_file(filepath.into(), &source).unwrap_or_else(print_error_and_exit);
    drop(source);

    let validated_items = validate_items(&syntax_items).unwrap_or_else(print_error_and_exit);
    drop(syntax_items);

    let InferResult {
        types,
        functions,
        function_bodies,
        global_names,
        errors,
    } = infer_items(&validated_items);
    drop(validated_items);
    if !errors.is_empty() {
        print_infer_errors(&errors, &types, &functions);
        std::process::exit(1)
    }

    println!("{types:#?}");
    println!("{functions:#?}");
    println!("{function_bodies:#?}");
    println!("{global_names:#?}");
}

fn print_error_and_exit<T: std::fmt::Display, U>(error: T) -> U {
    eprintln!("{error}");
    std::process::exit(1)
}

fn print_infer_errors(
    errors: &[InferError],
    types: &IdMap<it::Type>,
    functions: &IdMap<it::Function>,
) {
    for error in errors {
        eprint!("{}: ", error.location);
        match error.kind {
            InferErrorKind::NameAlreadyDeclared {
                name,
                defined_location,
            } => eprintln!("'{name}' was already declared at '{defined_location}'"),
            InferErrorKind::UnknownName { name } => eprintln!("Unknown name '{name}'"),
            InferErrorKind::UnknownLabel { label } => eprintln!("Unknown label '{label}"),
            InferErrorKind::ExpectedValue { declared_location } => {
                eprintln!("Expected a value but got the thing declared at {declared_location}")
            }
            InferErrorKind::ExpectedType { declared_location } => {
                eprintln!("Expected a type but got the thing declared at {declared_location}")
            }
            InferErrorKind::ExpectedTypeButGotType { expected, got } => {
                eprintln!(
                    "Expected type {}, but got type {}",
                    PrettyPrintType {
                        typ: expected,
                        types,
                        functions,
                    },
                    PrettyPrintType {
                        typ: got,
                        types,
                        functions,
                    }
                );
                eprintln!("{}: Expected type declared here", types[expected].location);
                eprintln!("{}: Got type declared here", types[got].location);
            }
            InferErrorKind::MemberAlreadyInitialised {
                original_location,
                name,
            } => {
                eprintln!("The '{name}' member was already initialised at {original_location}")
            }
            InferErrorKind::MemberAlreadyDeconstructed {
                original_location,
                name,
            } => eprintln!("The '{name}' member was already deconstructed at {original_location}"),
            InferErrorKind::CyclicDependency { resolving_location } => {
                eprintln!("Found a cyclic dependency that was started at {resolving_location}")
            }
            InferErrorKind::UnableToInferType { typ } => eprintln!(
                "Unable to infer type, only got as far as {}",
                PrettyPrintType {
                    typ,
                    types,
                    functions,
                }
            ),
        }
    }
}

struct PrettyPrintType<'a> {
    typ: Id<it::Type>,
    types: &'a IdMap<it::Type>,
    functions: &'a IdMap<it::Function>,
}

impl std::fmt::Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn print_type(
            typ: Id<it::Type>,
            types: &IdMap<it::Type>,
            functions: &IdMap<it::Function>,
            mut f: impl std::fmt::Write,
        ) -> std::fmt::Result {
            match types[typ].kind {
                it::TypeKind::Resolved(typ) => print_type(typ, types, functions, f),
                it::TypeKind::Infer(ref infer) => match *infer {
                    it::Infer::Anything => write!(f, "_"),
                    it::Infer::FunctionLike {
                        ref parameters,
                        return_type,
                    } => {
                        write!(f, "fn(")?;
                        for (i, &parameter) in parameters.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(
                                f,
                                "{}",
                                PrettyPrintType {
                                    typ: parameter,
                                    types,
                                    functions
                                },
                            )?;
                        }
                        write!(
                            f,
                            ") -> {}",
                            PrettyPrintType {
                                typ: return_type,
                                types,
                                functions
                            },
                        )
                    }
                    it::Infer::StructLike { ref members } => {
                        write!(f, "_ {{ ")?;
                        for (i, (&name, &member)) in members.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(
                                f,
                                "{name}: {}",
                                PrettyPrintType {
                                    typ: member,
                                    types,
                                    functions
                                },
                            )?;
                        }
                        write!(f, " }}")
                    }
                    it::Infer::NumberLike => write!(f, "{{{{number}}}}"),
                },
                it::TypeKind::Struct { name, .. } => write!(f, "{name}"),
                it::TypeKind::Enum { name, .. } => write!(f, "{name}"),
                it::TypeKind::FunctionItem(function) => {
                    write!(f, "fn(")?;
                    for (i, &parameter) in functions[function].parameter_types.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(
                            f,
                            "{}",
                            PrettyPrintType {
                                typ: parameter,
                                types,
                                functions
                            },
                        )?;
                    }
                    write!(
                        f,
                        ") -> {} {{{}}}",
                        PrettyPrintType {
                            typ: functions[function].return_type,
                            types,
                            functions
                        },
                        functions[function].name
                    )
                }
                it::TypeKind::I32 => write!(f, "I32"),
                it::TypeKind::Runtime => write!(f, "Runtime"),
            }
        }

        print_type(self.typ, self.types, self.functions, f)
    }
}
