use std::collections::VecDeque;

use crate::{
    ast,
    ids::{Id, IdMap, IdSecondaryMap},
    interning::InternedStr,
    lexing::SourceLocation,
    typed_tree as tt,
};
use derive_more::Display;
use rustc_hash::{FxBuildHasher, FxHashMap};
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
    #[display("The '{name}' member has already been deconstructed at {original_location}")]
    MemberAlreadyDeconstructed {
        original_location: SourceLocation,
        name: InternedStr,
    },
    #[display("Expected a struct type but got {got:?}")]
    ExpectedStructTypeButGot { got: Id<tt::Type> },
    #[display("Expected a type but got thing declared at {declared_location}")]
    ExpectedType { declared_location: SourceLocation },
    #[display("Found a cyclic dependency that was started at {resolving_location}")]
    CyclicDependency { resolving_location: SourceLocation },
}

#[derive(Debug)]
pub struct TypingResult {
    pub types: IdMap<tt::Type>,
    pub functions: IdMap<tt::Function>,
    pub function_bodies: IdSecondaryMap<tt::Function, tt::FunctionBody>,
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

        bindings: IdMap::new(),
        unresolved_function_bodies: VecDeque::new(),
    };
    let mut global_names = FxHashMap::default();
    let mut errors = vec![];

    if let Err(error) = typer.handle_items(items.iter(), &mut global_names) {
        errors.push(error);
    }

    let mut function_bodies = IdSecondaryMap::new();

    while let Some(UnresolvedFunctionBody {
        id,
        body,
        names,
        mut variables,
        parameters,
    }) = typer.unresolved_function_bodies.pop_front()
    {
        function_bodies.insert(
            id,
            match body {
                ast::FunctionBody::Expression(expression) => tt::FunctionBody::Body {
                    expression: Box::new(
                        match typer
                            .expression(expression, &names, &mut variables)
                            .and_then(|expression| {
                                typer.expect_types(
                                    expression.location,
                                    typer.functions[id].return_type,
                                    expression.typ,
                                )?;
                                Ok(expression)
                            }) {
                            Ok(expression) => expression,
                            Err(error) => {
                                errors.push(error);
                                continue;
                            }
                        },
                    ),
                    variables,
                    parameters,
                },

                ast::FunctionBody::Builtin(builtin_function) => {
                    tt::FunctionBody::Builtin(match builtin_function {
                        ast::BuiltinFunction::PrintI32 => tt::BuiltinFunction::PrintI32,
                    })
                }
            },
        );
    }

    TypingResult {
        types: typer.types,
        functions: typer.functions,
        function_bodies,
        global_names: if errors.is_empty() {
            global_names
            .into_iter()
            .map(|(name, binding)| {
                (
                    name,
                    GlobalBinding {
                        location: typer.bindings[binding].location,
                        kind: match typer.bindings[binding].kind {
                            BindingKind::UnresolvedItem { item, names: _ } => {
                                unreachable!("all bindings should have been resolved, but {item:?} wasnt")
                            }

                            BindingKind::ResolvingItem { resolving_location } => {
                                unreachable!("global binding was started resolving at {resolving_location} but never finished")
                            }

                            BindingKind::Resolved(ref resolved_binding) => match resolved_binding.kind {
                                ResolvedBindingKind::Type(id) => GlobalBindingKind::Type(id),
                                ResolvedBindingKind::Function(id) => GlobalBindingKind::Function(id),

                                ResolvedBindingKind::Variable(id) => {
                                    unreachable!("variable bindings should not be in the global scope, but {id:?} was")
                                },
                            },
                        },
                    },
                )
            })
            .collect()
        } else {
            FxHashMap::default()
        },
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
struct Binding<'ast> {
    pub location: SourceLocation,
    pub kind: BindingKind<'ast>,
}

#[derive(Debug, Clone)]
enum BindingKind<'ast> {
    UnresolvedItem {
        item: &'ast ast::Item,
        names: Names<'ast>,
    },
    ResolvingItem {
        resolving_location: SourceLocation,
    },
    Resolved(ResolvedBinding),
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

struct UnresolvedFunctionBody<'ast> {
    id: Id<tt::Function>,
    body: &'ast ast::FunctionBody,
    names: Names<'ast>,
    variables: IdMap<tt::Variable>,
    parameters: Box<[Id<tt::Variable>]>,
}

struct Typer<'ast> {
    types: IdMap<tt::Type>,
    functions: IdMap<tt::Function>,
    unit_type: Option<Id<tt::Type>>,
    i32_type: Option<Id<tt::Type>>,
    runtime_type: Option<Id<tt::Type>>,

    bindings: IdMap<Binding<'ast>>,
    unresolved_function_bodies: VecDeque<UnresolvedFunctionBody<'ast>>,
}

type Names<'ast> = FxHashMap<InternedStr, Id<Binding<'ast>>>;

impl<'ast> Typer<'ast> {
    fn handle_items(
        &mut self,
        items: impl Iterator<Item = &'ast ast::Item>,
        names: &mut Names<'ast>,
    ) -> Result<(), TypingError> {
        let mut inserted_items = vec![];
        for item in items {
            let name = match item.kind {
                ast::ItemKind::Struct { name, .. } => name,
                ast::ItemKind::Type { name, .. } => name,
                ast::ItemKind::Function { name, .. } => name,
            };

            let id = self.bindings.insert(Binding {
                location: item.location,
                kind: BindingKind::UnresolvedItem {
                    item,
                    names: FxHashMap::default(),
                },
            });
            names.insert(name, id);
            inserted_items.push((id, item));
        }

        for &(id, _) in &inserted_items {
            match &mut self.bindings[id].kind {
                BindingKind::UnresolvedItem {
                    item: _,
                    names: binding_names,
                } => *binding_names = names.clone(),

                _ => unreachable!("they were just set to unresolved item"),
            }
        }

        for (id, item) in inserted_items {
            if let BindingKind::UnresolvedItem {
                item: binding_item,
                ref mut names,
            } = self.bindings[id].kind
            {
                assert!(core::ptr::eq(item, binding_item));

                let mut names = std::mem::take(names);
                self.bindings[id].kind = BindingKind::ResolvingItem {
                    resolving_location: item.location,
                };

                let resolved_binding = self.item(item, &mut names)?;
                assert!(matches!(
                    self.bindings[id].kind,
                    BindingKind::ResolvingItem { .. }
                ));
                self.bindings[id].kind = BindingKind::Resolved(resolved_binding);
            }
        }

        Ok(())
    }

    fn item(
        &mut self,
        item: &'ast ast::Item,
        names: &mut Names<'ast>,
    ) -> Result<ResolvedBinding, TypingError> {
        Ok(match item.kind {
            ast::ItemKind::Struct {
                ref builtin,
                name: _,
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
                                    typ: self.typ(typ, names)?,
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

                ResolvedBinding {
                    location: item.location,
                    kind: ResolvedBindingKind::Type(id),
                }
            }

            ast::ItemKind::Type { name: _, ref typ } => {
                let id = self.typ(typ, names)?;
                ResolvedBinding {
                    location: item.location,
                    kind: ResolvedBindingKind::Type(id),
                }
            }

            ast::ItemKind::Function {
                name: _,
                ref parameters,
                ref return_type,
                ref body,
            } => {
                let mut local_names =
                    names
                        .clone()
                        .into_iter()
                        .filter(|&(_, binding)| match self.bindings[binding].kind {
                            BindingKind::UnresolvedItem { .. }
                            | BindingKind::ResolvingItem { .. } => true,

                            BindingKind::Resolved(ref resolved_binding) => {
                                match resolved_binding.kind {
                                    ResolvedBindingKind::Type(_)
                                    | ResolvedBindingKind::Function(_) => true,

                                    ResolvedBindingKind::Variable(_) => false,
                                }
                            }
                        })
                        .collect::<Names<'ast>>();

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
                            let typ = self.typ(typ, names)?;
                            let variable = variables.insert(tt::Variable {
                                location,
                                name: Some(name),
                                typ,
                            });

                            parameter_variables.push(variable);

                            local_names.insert(
                                name,
                                self.bindings.insert(Binding {
                                    location,
                                    kind: BindingKind::Resolved(ResolvedBinding {
                                        location,
                                        kind: ResolvedBindingKind::Variable(variable),
                                    }),
                                }),
                            );

                            Ok(typ)
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                let return_type = self.typ(return_type, names)?;

                let id = self.functions.insert_with(|id| tt::Function {
                    location: item.location,
                    parameter_types,
                    return_type,
                    typ: self.types.insert(tt::Type {
                        location: item.location,
                        kind: tt::TypeKind::FunctionItem(id),
                    }),
                });

                self.unresolved_function_bodies
                    .push_back(UnresolvedFunctionBody {
                        id,
                        body,
                        names: local_names,
                        variables,
                        parameters: parameter_variables.into_boxed_slice(),
                    });

                ResolvedBinding {
                    location: item.location,
                    kind: ResolvedBindingKind::Function(id),
                }
            }
        })
    }

    fn statements(
        &mut self,
        statements: impl Iterator<Item = &'ast ast::Statement> + Clone,
        names: &mut Names<'ast>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<Box<[tt::Statement]>, TypingError> {
        self.handle_items(
            statements
                .clone()
                .filter_map(|statement| match statement.kind {
                    ast::StatementKind::Item(ref item) => Some(&**item),
                    _ => None,
                }),
            names,
        )?;

        statements
            .map(|statement| {
                Ok(match statement.kind {
                    ast::StatementKind::Item(_) => None,

                    ast::StatementKind::Expression(ref expression) => {
                        Some(tt::Statement::Expression(Box::new(
                            self.expression(expression, names, variables)?,
                        )))
                    }

                    ast::StatementKind::Assignment {
                        ref pattern,
                        ref value,
                    } => {
                        let pattern = Box::new(self.pattern(pattern, names, variables)?);
                        let value = Box::new(self.expression(value, names, variables)?);
                        self.expect_types(statement.location, pattern.typ, value.typ)?;
                        Some(tt::Statement::Assignment { pattern, value })
                    }
                })
            })
            .filter_map(|result| result.transpose())
            .collect()
    }

    fn expression(
        &mut self,
        expression: &'ast ast::Expression,
        names: &Names<'ast>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<tt::Expression, TypingError> {
        Ok(match expression.kind {
            ast::ExpressionKind::Place(ref place) => {
                let (place, typ) = self.place(place, expression.location, names, variables)?;
                tt::Expression {
                    location: expression.location,
                    typ,
                    kind: tt::ExpressionKind::Place(place),
                }
            }

            ast::ExpressionKind::Constant(ref constant) => {
                let (constant, typ) =
                    self.constant(constant, expression.location, names, variables)?;
                tt::Expression {
                    location: expression.location,
                    typ,
                    kind: tt::ExpressionKind::Constant(constant),
                }
            }

            ast::ExpressionKind::Block {
                ref statements,
                ref last_expression,
            } => {
                let mut names = names.clone();
                let statements = self.statements(statements.iter(), &mut names, variables)?;
                let last_expression =
                    Box::new(self.expression(last_expression, &names, variables)?);
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
                let operand = Box::new(self.expression(operand, names, variables)?);

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
                        let expression = self.expression(expression, names, variables)?;
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
                let typ = self.typ(typ, names)?;
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
                            let value = self.expression(value, names, variables)?;

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

            ast::ExpressionKind::Match {
                ref scruitnee,
                ref arms,
            } => {
                let scruitnee = Box::new(self.expression(scruitnee, names, variables)?);

                let mut arms_type = None;
                let arms = arms
                    .iter()
                    .map(
                        |&ast::MatchArm {
                             location,
                             ref pattern,
                             ref value,
                         }| {
                            let mut names = names.clone();

                            let pattern = self.pattern(pattern, &mut names, variables)?;
                            self.expect_types(pattern.location, scruitnee.typ, pattern.typ)?;

                            let value = self.expression(value, &names, variables)?;
                            if let Some(arms_type) = arms_type {
                                self.expect_types(value.location, arms_type, value.typ)?;
                            } else {
                                arms_type = Some(value.typ);
                            }

                            Ok(tt::MatchArm {
                                location,
                                pattern,
                                value,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;
                let typ = arms_type.expect("type inference is not implemented yet");

                tt::Expression {
                    location: expression.location,
                    typ,
                    kind: tt::ExpressionKind::Match { scruitnee, arms },
                }
            }
        })
    }

    fn place(
        &mut self,
        place: &'ast ast::Place,
        location: SourceLocation,
        names: &Names<'ast>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<(tt::Place, Id<tt::Type>), TypingError> {
        Ok(match *place {
            ast::Place::Path(ref path) => match self.path(path, names)? {
                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Function(id),
                } => (tt::Place::Function(id), self.functions[id].typ),

                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Variable(id),
                } => (tt::Place::Variable(id), variables[id].typ),

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

            ast::Place::MemberAccess {
                ref operand,
                member_name,
            } => {
                let operand = Box::new(self.expression(operand, names, variables)?);
                let tt::TypeKind::Struct { ref members } = self.types[operand.typ].kind else {
                    return Err(TypingError {
                        location: operand.location,
                        kind: TypingErrorKind::ExpectedStructTypeButGot { got: operand.typ },
                    });
                };

                let member_index = members
                    .iter()
                    .position(|member| member.name == member_name)
                    .ok_or(TypingError {
                        location,
                        kind: TypingErrorKind::UnknownName { name: member_name },
                    })?;

                (
                    tt::Place::StructMemberAccess {
                        operand,
                        member_index,
                    },
                    members[member_index].typ,
                )
            }
        })
    }

    fn constant(
        &mut self,
        constant: &ast::Constant,
        #[expect(unused)] location: SourceLocation,
        #[expect(unused)] names: &Names<'ast>,
        #[expect(unused)] variables: &mut IdMap<tt::Variable>,
    ) -> Result<(tt::Constant, Id<tt::Type>), TypingError> {
        Ok(match *constant {
            ast::Constant::Integer(value) => (
                tt::Constant::Integer(value),
                self.i32_type
                    .expect("the I32 type should have already been defined"),
            ),
        })
    }

    fn typ(
        &mut self,
        typ: &'ast ast::Type,
        names: &Names<'ast>,
    ) -> Result<Id<tt::Type>, TypingError> {
        Ok(match typ.kind {
            ast::TypeKind::Infer => {
                panic!("{}: type inference is not implemented yet", typ.location)
            }

            ast::TypeKind::Path(ref path) => match self.path(path, names)? {
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

    fn pattern(
        &mut self,
        pattern: &'ast ast::Pattern,
        names: &mut Names<'ast>,
        variables: &mut IdMap<tt::Variable>,
    ) -> Result<tt::Pattern, TypingError> {
        Ok(match pattern.kind {
            ast::PatternKind::Discard => panic!(
                "{}: type inference is not implemented yet",
                pattern.location
            ),

            ast::PatternKind::Place(ref place) => {
                let (place, typ) = self.place(place, pattern.location, names, variables)?;
                tt::Pattern {
                    location: pattern.location,
                    typ,
                    kind: tt::PatternKind::Place(place),
                }
            }

            ast::PatternKind::Constant(ref constant) => {
                let (constant, typ) =
                    self.constant(constant, pattern.location, names, variables)?;
                tt::Pattern {
                    location: pattern.location,
                    typ,
                    kind: tt::PatternKind::Constant(constant),
                }
            }

            ast::PatternKind::Deconstructor {
                ref typ,
                ref arguments,
            } => {
                let typ_location = typ.location;
                let typ = self.typ(typ, names)?;
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
                let mut members_to_deconstruct = members
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
                        |&ast::DeconstructorArgument {
                             location,
                             name,
                             ref pattern,
                         }| {
                            let pattern = self.pattern(pattern, names, variables)?;

                            if let Some(&original_location) = initialised_members.get(&name) {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::MemberAlreadyDeconstructed {
                                        original_location,
                                        name,
                                    },
                                });
                            }
                            initialised_members.insert(name, location);

                            let Some((_, member_index, typ)) = members_to_deconstruct.remove(&name)
                            else {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::UnknownName { name },
                                });
                            };

                            self.expect_types(pattern.location, typ, pattern.typ)?;

                            Ok(tt::StructDeconstructorArgument {
                                member_index,
                                pattern,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                if let Some((name, (location, _, _))) = members_to_deconstruct.into_iter().next() {
                    return Err(TypingError {
                        location,
                        kind: TypingErrorKind::MemberLeftUninitialised { name },
                    });
                }

                tt::Pattern {
                    location: pattern.location,
                    typ,
                    kind: tt::PatternKind::StructDeconstructor { arguments },
                }
            }

            ast::PatternKind::Let { name, ref typ } => {
                let typ = self.typ(typ, names)?;
                let variable = variables.insert(tt::Variable {
                    location: pattern.location,
                    name: Some(name),
                    typ,
                });
                names.insert(
                    name,
                    self.bindings.insert(Binding {
                        location: pattern.location,
                        kind: BindingKind::Resolved(ResolvedBinding {
                            location: pattern.location,
                            kind: ResolvedBindingKind::Variable(variable),
                        }),
                    }),
                );
                tt::Pattern {
                    location: pattern.location,
                    typ,
                    kind: tt::PatternKind::Let { variable },
                }
            }
        })
    }

    fn path(
        &mut self,
        path: &'ast ast::Path,
        names: &Names<'ast>,
    ) -> Result<ResolvedBinding, TypingError> {
        match names.get(&path.name) {
            Some(&id) => match self.bindings[id].kind {
                BindingKind::UnresolvedItem {
                    item,
                    ref mut names,
                } => {
                    let mut names = std::mem::take(names);

                    self.bindings[id].kind = BindingKind::ResolvingItem {
                        resolving_location: path.location,
                    };

                    let resolved_binding = self.item(item, &mut names)?;
                    assert!(matches!(
                        self.bindings[id].kind,
                        BindingKind::ResolvingItem { .. }
                    ));
                    self.bindings[id].kind = BindingKind::Resolved(resolved_binding.clone());

                    Ok(resolved_binding)
                }

                BindingKind::ResolvingItem { resolving_location } => Err(TypingError {
                    location: path.location,
                    kind: TypingErrorKind::CyclicDependency { resolving_location },
                }),

                BindingKind::Resolved(ref resolved_binding) => Ok(resolved_binding.clone()),
            },

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
