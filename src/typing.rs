use crate::{
    ast,
    ids::{Id, IdMap},
    interning::InternedStr,
    lexing::SourceLocation,
    typed_tree as tt,
};
use derive_more::Display;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::{collections::hash_map::Entry, marker::PhantomData};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct TypingError {
    pub location: SourceLocation,
    pub kind: TypingErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum TypingErrorKind {
    #[display("The name '{name}' has already been defined at {defined_location}")]
    NameAlreadyDeclared {
        name: InternedStr,
        defined_location: SourceLocation,
    },
    #[display("Unknown name '{name}'")]
    UnknownName { name: InternedStr },
    #[display("Expected a value but got thing declared at {declared_location}")]
    ExpectedValue { declared_location: SourceLocation },
    #[display("Expected a {expected:?} but got {got:?}")]
    ExpectedTypeButGotType {
        expected: Id<tt::Type>,
        got: Id<tt::Type>,
    },
    // TODO: make this better
    #[display("Expected a function type but got {got:?}")]
    ExpectedFunctionTypeButGot { got: Id<tt::Type> },
    #[display("Function has {expected} number of parameters but got {got} arguments")]
    ExpectedUnexpectedArgumentsCount { expected: usize, got: usize },
    #[display("The '{name}' member was not initialised")]
    MemberLeftUninitialised { name: InternedStr },
    #[display("The '{name}' member has already been initialised at {original_location}")]
    MemberAlreadyInitialised {
        original_location: SourceLocation,
        name: InternedStr,
    },
    #[display("Expected a struct type but got {got:?}")]
    ExpectedStructTypeButGot { got: Id<tt::Type> },
    #[display("Expected a type but got thing declared at {declared_location}")]
    ExpectedType { declared_location: SourceLocation },
}

#[derive(Debug)]
pub struct TypingResult {
    pub types: IdMap<tt::Type>,
    pub functions: IdMap<tt::Function>,
    pub global_names: FxHashMap<InternedStr, GlobalBinding>,
    pub errors: Vec<TypingError>,
}

pub fn type_items(items: &[ast::Item]) -> TypingResult {
    let mut typer = Typer {
        types: IdMap::new(),
        functions: IdMap::new(),
        unit_type: None,
        i32_type: None,
        runtime_type: None,

        _ast: PhantomData,
    };
    let mut global_names = FxHashMap::default();
    let mut errors = vec![];

    for item in items {
        match typer.item(item, &mut global_names) {
            Ok(()) => {}
            Err(error) => errors.push(error),
        }
    }

    TypingResult {
        types: typer.types,
        functions: typer.functions,
        global_names: global_names
            .into_iter()
            .map(|(name, binding)| {
                (
                    name,
                    GlobalBinding {
                        location: binding.location,
                        kind: match binding.kind {
                            BindingKind::Type(id) => GlobalBindingKind::Type(id),
                            BindingKind::Function(id) => GlobalBindingKind::Function(id),

                            BindingKind::Variable(id) => {
                                unreachable!("variable bindings should not be in the global scope, but {id:?} was")
                            },
                        },
                    },
                )
            })
            .collect(),
        errors,
    }
}

#[derive(Debug, Clone)]
pub struct GlobalBinding {
    pub location: SourceLocation,
    pub kind: GlobalBindingKind,
}

#[derive(Debug, Clone)]
pub enum GlobalBindingKind {
    Type(Id<tt::Type>),
    Function(Id<tt::Function>),
}

#[derive(Debug, Clone)]
struct Binding {
    pub location: SourceLocation,
    pub kind: BindingKind,
}

#[derive(Debug, Clone)]
enum BindingKind {
    Type(Id<tt::Type>),
    Function(Id<tt::Function>),
    Variable(Id<tt::Variable>),
}

#[derive(Debug, Clone)]
struct ResolvedBinding {
    pub location: SourceLocation,
    pub kind: ResolvedBindingKind,
}

#[derive(Debug, Clone)]
enum ResolvedBindingKind {
    Type(Id<tt::Type>),
    Function(Id<tt::Function>),
    Variable(Id<tt::Variable>),
}

struct Typer<'ast> {
    types: IdMap<tt::Type>,
    functions: IdMap<tt::Function>,
    unit_type: Option<Id<tt::Type>>,
    i32_type: Option<Id<tt::Type>>,
    runtime_type: Option<Id<tt::Type>>,

    _ast: PhantomData<&'ast ()>,
}

impl<'ast> Typer<'ast> {
    fn item(
        &mut self,
        item: &'ast ast::Item,
        bindings: &mut FxHashMap<InternedStr, Binding>,
    ) -> Result<(), TypingError> {
        match item.kind {
            ast::ItemKind::Struct {
                ref builtin,
                name,
                ref members,
            } => {
                let members = {
                    let mut member_names = FxHashMap::default();
                    members
                        .iter()
                        .map(
                            |&ast::StructMember {
                                 location,
                                 name,
                                 ref typ,
                             }| {
                                if let Some(defined_location) = member_names.insert(name, location)
                                {
                                    return Err(TypingError {
                                        location,
                                        kind: TypingErrorKind::NameAlreadyDeclared {
                                            name,
                                            defined_location,
                                        },
                                    });
                                }

                                Ok(tt::StructMember {
                                    location,
                                    name,
                                    typ: self.typ(typ, bindings)?,
                                })
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?
                };

                let id = self.types.insert(tt::Type {
                    location: item.location,
                    kind: tt::TypeKind::Struct { members },
                });

                if let Some(builtin) = builtin {
                    match *builtin {
                        ast::BuiltinStruct::Unit => {
                            assert!(
                                self.unit_type.is_none(),
                                "#builtin Unit type cannot be declared twice"
                            );
                            self.unit_type = Some(id);
                        }
                    }
                }

                match bindings.entry(name) {
                    Entry::Vacant(entry) => {
                        entry.insert(Binding {
                            location: item.location,
                            kind: BindingKind::Type(id),
                        });
                        Ok(())
                    }

                    Entry::Occupied(entry) => Err(TypingError {
                        location: item.location,
                        kind: TypingErrorKind::NameAlreadyDeclared {
                            name,
                            defined_location: entry.get().location,
                        },
                    }),
                }
            }

            ast::ItemKind::Type { name, ref typ } => {
                let typ = self.typ(typ, bindings)?;
                match bindings.entry(name) {
                    Entry::Vacant(entry) => {
                        entry.insert(Binding {
                            location: item.location,
                            kind: BindingKind::Type(typ),
                        });
                        Ok(())
                    }

                    Entry::Occupied(entry) => Err(TypingError {
                        location: item.location,
                        kind: TypingErrorKind::NameAlreadyDeclared {
                            name,
                            defined_location: entry.get().location,
                        },
                    }),
                }
            }

            ast::ItemKind::Function {
                name,
                ref parameters,
                ref return_type,
                ref body,
            } => {
                let id = {
                    let mut local_bindings = bindings
                        .iter()
                        .filter_map(|(&name, binding)| {
                            Some((
                                name,
                                match binding.kind {
                                    BindingKind::Type(id) => Binding {
                                        kind: BindingKind::Type(id),
                                        ..*binding
                                    },

                                    BindingKind::Function(id) => Binding {
                                        kind: BindingKind::Function(id),
                                        ..*binding
                                    },

                                    BindingKind::Variable(_) => return None,
                                },
                            ))
                        })
                        .collect::<FxHashMap<InternedStr, Binding>>();

                    let mut parameter_variables = Vec::with_capacity(parameters.len());

                    let mut variables = IdMap::new();
                    let parameter_types = parameters
                        .iter()
                        .map(
                            |&ast::FunctionParameter {
                                 location,
                                 name,
                                 ref typ,
                             }| {
                                let typ = self.typ(typ, bindings)?;
                                let variable = variables.insert(tt::Variable {
                                    location,
                                    name: Some(name),
                                    typ,
                                });

                                parameter_variables.push(variable);

                                match local_bindings.entry(name) {
                                    Entry::Vacant(entry) => {
                                        entry.insert(Binding {
                                            location: item.location,
                                            kind: BindingKind::Variable(variable),
                                        });
                                        Ok(typ)
                                    }

                                    Entry::Occupied(entry) => Err(TypingError {
                                        location: item.location,
                                        kind: TypingErrorKind::NameAlreadyDeclared {
                                            name,
                                            defined_location: entry.get().location,
                                        },
                                    }),
                                }
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?;

                    let return_type = self.typ(return_type, bindings)?;

                    let body = match body {
                        ast::FunctionBody::Expression(expression) => tt::FunctionBody::Body {
                            expression: Box::new(self.expression(
                                expression,
                                &mut local_bindings,
                                &mut variables,
                            )?),
                            variables,
                            parameters: parameter_variables.into_boxed_slice(),
                        },

                        ast::FunctionBody::Builtin(builtin_function) => {
                            tt::FunctionBody::Builtin(match *builtin_function {
                                ast::BuiltinFunction::PrintI32 => tt::BuiltinFunction::PrintI32,
                            })
                        }
                    };

                    self.functions.insert_with(|id| tt::Function {
                        location: item.location,
                        parameter_types,
                        return_type,
                        typ: self.types.insert(tt::Type {
                            location: item.location,
                            kind: tt::TypeKind::FunctionItem(id),
                        }),
                        body,
                    })
                };

                match bindings.entry(name) {
                    Entry::Vacant(entry) => {
                        entry.insert(Binding {
                            location: item.location,
                            kind: BindingKind::Function(id),
                        });
                        Ok(())
                    }

                    Entry::Occupied(entry) => Err(TypingError {
                        location: item.location,
                        kind: TypingErrorKind::NameAlreadyDeclared {
                            name,
                            defined_location: entry.get().location,
                        },
                    }),
                }
            }
        }
    }

    fn statement(
        &mut self,
        statement: &'ast ast::Statement,
        bindings: &mut FxHashMap<InternedStr, Binding>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<Option<tt::Statement>, TypingError> {
        Ok(match statement.kind {
            ast::StatementKind::Item(ref item) => {
                self.item(item, bindings)?;
                None
            }

            ast::StatementKind::Expression(ref expression) => Some(tt::Statement::Expression(
                Box::new(self.expression(expression, bindings, variables)?),
            )),
        })
    }

    fn expression(
        &mut self,
        expression: &'ast ast::Expression,
        bindings: &mut FxHashMap<InternedStr, Binding>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<tt::Expression, TypingError> {
        Ok(match expression.kind {
            ast::ExpressionKind::Path(ref path) => match self.path(path, bindings)? {
                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Function(id),
                } => tt::Expression {
                    location: expression.location,
                    typ: self.functions[id].typ,
                    kind: tt::ExpressionKind::Function(id),
                },

                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Variable(id),
                } => tt::Expression {
                    location: expression.location,
                    typ: variables[id].typ,
                    kind: tt::ExpressionKind::Variable(id),
                },

                ResolvedBinding {
                    location: declared_location,
                    kind: _,
                } => {
                    return Err(TypingError {
                        location: path.location,
                        kind: TypingErrorKind::ExpectedValue { declared_location },
                    });
                }
            },

            ast::ExpressionKind::Integer(value) => tt::Expression {
                location: expression.location,
                typ: self
                    .i32_type
                    .expect("the I32 type should have already been defined"),
                kind: tt::ExpressionKind::Integer(value),
            },

            ast::ExpressionKind::Block {
                ref statements,
                ref last_expression,
            } => {
                let mut bindings = bindings.clone();
                let statements = statements
                    .iter()
                    .filter_map(|statement| {
                        self.statement(statement, &mut bindings, variables)
                            .transpose()
                    })
                    .collect::<Result<Box<[_]>, _>>()?;
                let last_expression =
                    Box::new(self.expression(last_expression, &mut bindings, variables)?);
                tt::Expression {
                    location: expression.location,
                    typ: last_expression.typ,
                    kind: tt::ExpressionKind::Block {
                        statements,
                        last_expression,
                    },
                }
            }

            ast::ExpressionKind::Call {
                ref operand,
                ref arguments,
            } => {
                let operand = Box::new(self.expression(operand, bindings, variables)?);

                let tt::TypeKind::FunctionItem(function) = self.types[operand.typ].kind else {
                    return Err(TypingError {
                        location: operand.location,
                        kind: TypingErrorKind::ExpectedFunctionTypeButGot { got: operand.typ },
                    });
                };
                let function = &self.functions[function];
                let parameter_types = function.parameter_types.clone();
                let return_type = function.return_type;

                if parameter_types.len() != arguments.len() {
                    return Err(TypingError {
                        location: expression.location,
                        kind: TypingErrorKind::ExpectedUnexpectedArgumentsCount {
                            expected: parameter_types.len(),
                            got: arguments.len(),
                        },
                    });
                }

                let arguments = arguments
                    .iter()
                    .zip(parameter_types)
                    .map(|(expression, expected_type)| {
                        let expression = self.expression(expression, bindings, variables)?;
                        self.expect_types(expression.location, expected_type, expression.typ)?;
                        Ok(expression)
                    })
                    .collect::<Result<Box<[_]>, _>>()?;

                tt::Expression {
                    location: expression.location,
                    typ: return_type,
                    kind: tt::ExpressionKind::Call { operand, arguments },
                }
            }

            ast::ExpressionKind::Constructor {
                ref typ,
                ref arguments,
            } => {
                let typ_location = typ.location;
                let typ = self.typ(typ, bindings)?;
                let tt::TypeKind::Struct { ref members } = self.types[typ].kind else {
                    return Err(TypingError {
                        location: typ_location,
                        kind: TypingErrorKind::ExpectedStructTypeButGot { got: typ },
                    });
                };

                let mut initialised_members =
                    FxHashMap::<InternedStr, SourceLocation>::with_capacity_and_hasher(
                        members.len(),
                        FxBuildHasher,
                    );
                let mut members_to_initialise = members
                    .iter()
                    .enumerate()
                    .map(
                        |(
                            index,
                            &tt::StructMember {
                                location,
                                name,
                                typ,
                            },
                        )| (name, (location, index, typ)),
                    )
                    .collect::<FxHashMap<_, _>>();

                let arguments = arguments
                    .iter()
                    .map(
                        |&ast::ConstructorArgument {
                             location,
                             name,
                             ref value,
                         }| {
                            let value = self.expression(value, bindings, variables)?;

                            if let Some(&original_location) = initialised_members.get(&name) {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::MemberAlreadyInitialised {
                                        original_location,
                                        name,
                                    },
                                });
                            }
                            initialised_members.insert(name, location);

                            let Some((_, member_index, typ)) = members_to_initialise.remove(&name)
                            else {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::UnknownName { name },
                                });
                            };

                            self.expect_types(value.location, typ, value.typ)?;

                            Ok(tt::StructConstructorArgument {
                                member_index,
                                value,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                if let Some((name, (location, _, _))) = members_to_initialise.into_iter().next() {
                    return Err(TypingError {
                        location,
                        kind: TypingErrorKind::MemberLeftUninitialised { name },
                    });
                }

                tt::Expression {
                    location: expression.location,
                    typ,
                    kind: tt::ExpressionKind::StructConstructor { arguments },
                }
            }
        })
    }

    fn typ(
        &mut self,
        typ: &'ast ast::Type,
        bindings: &mut FxHashMap<InternedStr, Binding>,
    ) -> Result<Id<tt::Type>, TypingError> {
        Ok(match typ.kind {
            ast::TypeKind::Infer => {
                panic!("{}: type inference is not implemented yet", typ.location)
            }

            ast::TypeKind::Path(ref path) => match self.path(path, bindings)? {
                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Type(id),
                } => id,

                ResolvedBinding {
                    location: declared_location,
                    kind: _,
                } => {
                    return Err(TypingError {
                        location: path.location,
                        kind: TypingErrorKind::ExpectedType { declared_location },
                    });
                }
            },

            ast::TypeKind::Builtin(ref builtin_type) => match builtin_type {
                ast::BuiltinType::Unit => self
                    .unit_type
                    .expect("Unit type should have already been declared with #builtin"),

                ast::BuiltinType::I32 => *self.i32_type.get_or_insert_with(|| {
                    self.types.insert(tt::Type {
                        location: typ.location,
                        kind: tt::TypeKind::I32,
                    })
                }),

                ast::BuiltinType::Runtime => *self.runtime_type.get_or_insert_with(|| {
                    self.types.insert(tt::Type {
                        location: typ.location,
                        kind: tt::TypeKind::Runtime,
                    })
                }),
            },
        })
    }

    fn path(
        &mut self,
        path: &'ast ast::Path,
        bindings: &mut FxHashMap<InternedStr, Binding>,
    ) -> Result<ResolvedBinding, TypingError> {
        match bindings.get(&path.name) {
            Some(binding) => Ok(ResolvedBinding {
                location: path.location,
                kind: match binding.kind {
                    BindingKind::Type(id) => ResolvedBindingKind::Type(id),
                    BindingKind::Function(id) => ResolvedBindingKind::Function(id),
                    BindingKind::Variable(id) => ResolvedBindingKind::Variable(id),
                },
            }),

            None => Err(TypingError {
                location: path.location,
                kind: TypingErrorKind::UnknownName { name: path.name },
            }),
        }
    }

    fn expect_types(
        &mut self,
        location: SourceLocation,
        expected: Id<tt::Type>,
        got: Id<tt::Type>,
    ) -> Result<(), TypingError> {
        if expected == got {
            Ok(())
        } else {
            Err(TypingError {
                location,
                kind: TypingErrorKind::ExpectedTypeButGotType { expected, got },
            })
        }
    }
}
