use simple_lang::{
    ids::{Id, IdMap},
    inferred_tree as it,
    inferring::{InferError, InferErrorKind, infer_items},
    parsing::parse_file,
    type_check::{TypeCheckError, TypeCheckErrorKind, type_check},
    typed_tree as tt,
    validating::validate_items,
};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap_or_else(print_error_and_exit);

    let syntax_items = parse_file(filepath.into(), &source).unwrap_or_else(print_error_and_exit);
    drop(source);

    let validated_items = validate_items(&syntax_items).unwrap_or_else(print_error_and_exit);
    drop(syntax_items);

    let infer_result = infer_items(&validated_items);
    drop(validated_items);
    if !infer_result.errors.is_empty() {
        print_infer_errors(
            &infer_result.errors,
            &infer_result.types,
            &infer_result.functions,
        );
    }

    let type_check_result = type_check(&infer_result);
    drop(infer_result);
    if !type_check_result.errors.is_empty() {
        print_type_check_errors(
            &type_check_result.errors,
            &type_check_result.types,
            &type_check_result.functions,
        );
    }

    println!("{:#?}", type_check_result.types);
    println!("{:#?}", type_check_result.functions);
    println!("{:#?}", type_check_result.function_bodies);
}

fn print_error_and_exit<T: std::fmt::Display, U>(error: T) -> U {
    eprintln!("{error}");
    std::process::exit(1)
}

fn print_infer_errors(
    errors: &[InferError],
    types: &IdMap<it::Type>,
    functions: &IdMap<it::Function>,
) -> ! {
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
                    PrettyPrintInferType {
                        typ: expected,
                        types,
                        functions,
                    },
                    PrettyPrintInferType {
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
                PrettyPrintInferType {
                    typ,
                    types,
                    functions,
                }
            ),
            InferErrorKind::TypeInferenceCannotBeUsedHere => {
                eprintln!("Type inference cannot be used here")
            }
        }
    }
    std::process::exit(1)
}

struct PrettyPrintInferType<'a> {
    typ: Id<it::Type>,
    types: &'a IdMap<it::Type>,
    functions: &'a IdMap<it::Function>,
}

impl std::fmt::Display for PrettyPrintInferType<'_> {
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
                                PrettyPrintInferType {
                                    typ: parameter,
                                    types,
                                    functions
                                },
                            )?;
                        }
                        write!(
                            f,
                            ") -> {}",
                            PrettyPrintInferType {
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
                                PrettyPrintInferType {
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
                it::TypeKind::Opaque { name } => write!(f, "{name} {{{{opaque type}}}}"),
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
                            PrettyPrintInferType {
                                typ: parameter,
                                types,
                                functions
                            },
                        )?;
                    }
                    write!(
                        f,
                        ") -> {} {{{}}}",
                        PrettyPrintInferType {
                            typ: functions[function].return_type,
                            types,
                            functions
                        },
                        functions[function].name
                    )
                }
                it::TypeKind::U8 => write!(f, "U8"),
                it::TypeKind::U16 => write!(f, "U16"),
                it::TypeKind::U32 => write!(f, "U32"),
                it::TypeKind::U64 => write!(f, "U64"),
                it::TypeKind::I8 => write!(f, "I8"),
                it::TypeKind::I16 => write!(f, "I16"),
                it::TypeKind::I32 => write!(f, "I32"),
                it::TypeKind::I64 => write!(f, "I64"),
                it::TypeKind::ISize => write!(f, "ISize"),
                it::TypeKind::USize => write!(f, "USize"),
                it::TypeKind::Runtime => write!(f, "Runtime"),
            }
        }

        print_type(self.typ, self.types, self.functions, f)
    }
}

fn print_type_check_errors(
    errors: &[TypeCheckError],
    types: &IdMap<tt::Type>,
    functions: &IdMap<tt::Function>,
) -> ! {
    for error in errors {
        eprint!("{}: ", error.location);
        match error.kind {
            TypeCheckErrorKind::ExpectedStructOrEnumButGot { typ } => {
                eprintln!(
                    "Expected struct or enum type but got type {}",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Got type declared here", types[typ].location);
            }
            TypeCheckErrorKind::ExpectedStructButGot { typ } => {
                eprintln!(
                    "Expected struct type but got type {}",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Got type declared here", types[typ].location);
            }
            TypeCheckErrorKind::UnknownMemberOnType { member_name, typ } => {
                eprintln!(
                    "Unknown member '{member_name}' on type {}",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::MemberWasLeftUninitialised { member_name, typ } => {
                eprintln!(
                    "Member '{member_name}' on type {} was left uninitialised",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::OnlyOneEnumVariantCanBeInitialised { typ } => {
                eprintln!("Only one enum variant can be initialised");
                eprintln!("{}: Enum type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::MemberWasNotDeconstructed { member_name, typ } => {
                eprintln!(
                    "Member '{member_name}' on type {} was not deconstructed",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::OnlyOneEnumVariantCanBeDeconstructed { typ } => {
                eprintln!("Only one enum variant can be deconstructed");
                eprintln!("{}: Enum type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::IntegerOutOfRangeForType { value, typ } => {
                eprintln!(
                    "Integer value {value} is out of range for type {}",
                    PrettyPrintType {
                        typ,
                        types,
                        functions
                    }
                );
                eprintln!("{}: Integer type was declared here", types[typ].location);
            }
            TypeCheckErrorKind::PlaceMustBeReadable => eprintln!("Place must be readable"),
            TypeCheckErrorKind::PatternMustBeWritable => eprintln!("Pattern must be writable"),
            TypeCheckErrorKind::PatternMustBeConstant => eprintln!("Pattern must be constant"),
        }
    }
    std::process::exit(1)
}

struct PrettyPrintType<'a> {
    typ: Id<tt::Type>,
    types: &'a IdMap<tt::Type>,
    functions: &'a IdMap<tt::Function>,
}

impl std::fmt::Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn print_type(
            typ: Id<tt::Type>,
            types: &IdMap<tt::Type>,
            functions: &IdMap<tt::Function>,
            mut f: impl std::fmt::Write,
        ) -> std::fmt::Result {
            match types[typ].kind {
                tt::TypeKind::Opaque { name } => write!(f, "{name} {{{{opaque type}}}}"),
                tt::TypeKind::Struct { name, .. } => write!(f, "{name}"),
                tt::TypeKind::Enum { name, .. } => write!(f, "{name}"),
                tt::TypeKind::FunctionItem(function) => {
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
                tt::TypeKind::U8 => write!(f, "U8"),
                tt::TypeKind::U16 => write!(f, "U16"),
                tt::TypeKind::U32 => write!(f, "U32"),
                tt::TypeKind::U64 => write!(f, "U64"),
                tt::TypeKind::I8 => write!(f, "I8"),
                tt::TypeKind::I16 => write!(f, "I16"),
                tt::TypeKind::I32 => write!(f, "I32"),
                tt::TypeKind::I64 => write!(f, "I64"),
                tt::TypeKind::ISize => write!(f, "ISize"),
                tt::TypeKind::USize => write!(f, "USize"),
                tt::TypeKind::Runtime => write!(f, "Runtime"),
            }
        }

        print_type(self.typ, self.types, self.functions, f)
    }
}
