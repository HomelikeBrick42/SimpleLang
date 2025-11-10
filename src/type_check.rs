use crate::{
    ids::{Id, IdMap, IdSecondaryMap},
    inferred_tree as it,
    inferring::InferResult,
    lexing::SourceLocation,
    typed_tree as tt,
};

#[derive(Debug, Clone)]
pub struct TypeCheckError {
    pub location: SourceLocation,
    pub kind: TypeCheckErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeCheckErrorKind {}

pub struct TypeCheckResult {
    pub types: IdMap<tt::Type>,
    pub functions: IdMap<tt::Function>,
    pub function_bodies: IdSecondaryMap<tt::Function, tt::FunctionBody>,
    pub errors: Vec<TypeCheckError>,
}

pub fn type_check(infer_result: &InferResult) -> TypeCheckResult {
    let mut checker = TypeChecker {
        infer_types: &infer_result.types,
        infer_functions: &infer_result.functions,

        types: IdMap::new(),
        functions: IdMap::new(),

        type_translations: IdSecondaryMap::new(),
        function_translations: IdSecondaryMap::new(),
    };

    let errors = vec![];

    for (typ, _) in infer_result.types.iter() {
        checker.typ(typ);
    }

    for (function, _) in infer_result.functions.iter() {
        checker.function(function);
    }

    TypeCheckResult {
        types: checker.types,
        functions: checker.functions,
        function_bodies: IdSecondaryMap::new(),
        errors,
    }
}

struct TypeChecker<'a> {
    infer_types: &'a IdMap<it::Type>,
    infer_functions: &'a IdMap<it::Function>,

    types: IdMap<tt::Type>,
    functions: IdMap<tt::Function>,

    type_translations: IdSecondaryMap<it::Type, Id<tt::Type>>,
    function_translations: IdSecondaryMap<it::Function, Id<tt::Function>>,
}

impl TypeChecker<'_> {
    fn typ(&mut self, typ: Id<it::Type>) -> Id<tt::Type> {
        if let Some(&id) = self.type_translations.get(typ) {
            return id;
        }

        let infer_type = &self.infer_types[typ];
        let id = match infer_type.kind {
            it::TypeKind::Resolved(typ) => self.typ(typ),

            it::TypeKind::Infer(_) => self.types.insert(tt::Type {
                location: infer_type.location,
                kind: tt::TypeKind::Opaque {
                    name: "{{error}}".into(),
                },
            }),

            it::TypeKind::Opaque { name } => self.types.insert(tt::Type {
                location: infer_type.location,
                kind: tt::TypeKind::Opaque { name },
            }),

            it::TypeKind::Struct { name, ref members } => {
                let id = self.types.insert(tt::Type {
                    location: infer_type.location,
                    kind: tt::TypeKind::Opaque { name },
                });
                self.type_translations.insert(typ, id);

                let members = members
                    .iter()
                    .map(
                        |&it::Member {
                             location,
                             name,
                             typ,
                         }| {
                            tt::Member {
                                location,
                                name,
                                typ: self.typ(typ),
                            }
                        },
                    )
                    .collect();

                self.types[id].kind = tt::TypeKind::Struct { name, members };

                id
            }

            it::TypeKind::Enum { name, ref members } => {
                let id = self.types.insert(tt::Type {
                    location: infer_type.location,
                    kind: tt::TypeKind::Opaque { name },
                });
                self.type_translations.insert(typ, id);

                let members = members
                    .iter()
                    .map(
                        |&it::Member {
                             location,
                             name,
                             typ,
                         }| {
                            tt::Member {
                                location,
                                name,
                                typ: self.typ(typ),
                            }
                        },
                    )
                    .collect();

                self.types[id].kind = tt::TypeKind::Enum { name, members };

                id
            }

            it::TypeKind::FunctionItem(function) => {
                let id = self.types.insert(tt::Type {
                    location: infer_type.location,
                    kind: tt::TypeKind::Opaque {
                        name: self.infer_functions[function].name,
                    },
                });
                self.type_translations.insert(typ, id);

                let function = self.function(function);

                self.types[id].kind = tt::TypeKind::FunctionItem(function);

                id
            }

            it::TypeKind::I32 => self.types.insert(tt::Type {
                location: infer_type.location,
                kind: tt::TypeKind::I32,
            }),
            it::TypeKind::Runtime => self.types.insert(tt::Type {
                location: infer_type.location,
                kind: tt::TypeKind::Runtime,
            }),
        };
        self.type_translations.insert(typ, id);
        id
    }

    fn function(&mut self, function: Id<it::Function>) -> Id<tt::Function> {
        if let Some(&function) = self.function_translations.get(function) {
            return function;
        }

        let infer_function = &self.infer_functions[function];

        let function = tt::Function {
            location: infer_function.location,
            name: infer_function.name,
            parameter_types: infer_function
                .parameter_types
                .iter()
                .map(|&parameter_type| self.typ(parameter_type))
                .collect(),
            return_type: self.typ(infer_function.return_type),
        };

        self.functions.insert(function)
    }
}
