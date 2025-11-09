use crate::{
    ast,
    ids::{Id, IdMap, IdSecondaryMap},
    inferred_tree as it,
    interning::InternedStr,
    lexing::SourceLocation,
};
use derive_more::Display;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::collections::VecDeque;
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
    #[display("Unknown label '{label}")]
    UnknownLabel { label: InternedStr },
    #[display("Expected a value but got thing declared at {declared_location}")]
    ExpectedValue { declared_location: SourceLocation },
    #[display("Expected a type but got thing declared at {declared_location}")]
    ExpectedType { declared_location: SourceLocation },
    #[display("Expected a {expected:?} but got {got:?}")]
    ExpectedTypeButGotType {
        expected: Id<it::Type>,
        got: Id<it::Type>,
    },
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
    #[display("Found a cyclic dependency that was started at {resolving_location}")]
    CyclicDependency { resolving_location: SourceLocation },
    #[display("Unable to infer type, only got as far as {typ:?}")]
    UnableToInferType { typ: Id<it::Type> },
}

#[derive(Debug)]
pub struct TypingResult {
    pub types: IdMap<it::Type>,
    pub functions: IdMap<it::Function>,
    pub function_bodies: IdSecondaryMap<it::Function, it::FunctionBody>,
    pub global_names: FxHashMap<InternedStr, GlobalBinding>,
    pub errors: Vec<TypingError>,
}

pub fn type_items(items: &[ast::Item]) -> TypingResult {
    let mut typer = Typer {
        types: IdMap::new(),
        functions: IdMap::new(),
        unit_type: None,
        never_type: None,
        i32_type: None,
        runtime_type: None,
        bool_type: None,

        bindings: IdMap::new(),
        label_types: IdSecondaryMap::new(),
        unresolved_function_bodies: VecDeque::new(),
    };
    let mut global_names = Default::default();
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
                ast::FunctionBody::Expression(expression) => it::FunctionBody::Body {
                    expression: Box::new(
                        match typer
                            .expression(expression, &names, &mut variables)
                            .and_then(|expression| {
                                typer.expect_types_equal(
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
                    it::FunctionBody::Builtin(match builtin_function {
                        ast::BuiltinFunction::PrintI32 => it::BuiltinFunction::PrintI32,
                    })
                }
            },
        );
    }

    for (id, typ) in typer.types.iter() {
        if matches!(typ.kind, it::TypeKind::Infer(_)) {
            errors.push(TypingError {
                location: typ.location,
                kind: TypingErrorKind::UnableToInferType { typ: id },
            });
        }
    }

    TypingResult {
        types: typer.types,
        functions: typer.functions,
        function_bodies,
        global_names: if errors.is_empty() {
            global_names
            .variables_and_types
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
    Type(Id<it::Type>),
    Function(Id<it::Function>),
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
    Type(Id<it::Type>),
    Function(Id<it::Function>),
    Variable(Id<it::Variable>),
}

struct UnresolvedFunctionBody<'ast> {
    id: Id<it::Function>,
    body: &'ast ast::FunctionBody,
    names: Names<'ast>,
    variables: IdMap<it::Variable>,
    parameters: Box<[Id<it::Variable>]>,
}

struct Typer<'ast> {
    types: IdMap<it::Type>,
    functions: IdMap<it::Function>,
    unit_type: Option<Id<it::Type>>,
    never_type: Option<Id<it::Type>>,
    i32_type: Option<Id<it::Type>>,
    runtime_type: Option<Id<it::Type>>,
    bool_type: Option<Id<it::Type>>,

    bindings: IdMap<Binding<'ast>>,
    label_types: IdSecondaryMap<it::Label, Id<it::Type>>,
    unresolved_function_bodies: VecDeque<UnresolvedFunctionBody<'ast>>,
}

#[derive(Default, Debug, Clone)]
struct Names<'ast> {
    variables_and_types: FxHashMap<InternedStr, Id<Binding<'ast>>>,
    labels: FxHashMap<InternedStr, Id<it::Label>>,
    label_translations: IdSecondaryMap<ast::Label, Id<it::Label>>,
}

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
                ast::ItemKind::Enum { name, .. } => name,
                ast::ItemKind::Type { name, .. } => name,
                ast::ItemKind::Function { name, .. } => name,
            };

            let id = self.bindings.insert(Binding {
                location: item.location,
                kind: BindingKind::UnresolvedItem {
                    item,
                    names: Default::default(),
                },
            });
            names.variables_and_types.insert(name, id);
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
                            |&ast::Member {
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

                                Ok(it::Member {
                                    location,
                                    name,
                                    typ: self.typ(typ, names)?,
                                })
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?
                };

                let id = self.types.insert(it::Type {
                    location: item.location,
                    kind: it::TypeKind::Struct { members },
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

            ast::ItemKind::Enum {
                ref builtin,
                name: _,
                ref members,
            } => {
                let members = {
                    let mut member_names = FxHashMap::default();
                    members
                        .iter()
                        .map(
                            |&ast::Member {
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

                                Ok(it::Member {
                                    location,
                                    name,
                                    typ: self.typ(typ, names)?,
                                })
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?
                };

                let id = self.types.insert(it::Type {
                    location: item.location,
                    kind: it::TypeKind::Enum { members },
                });

                if let Some(builtin) = builtin {
                    match *builtin {
                        ast::BuiltinEnum::Never => {
                            assert!(
                                self.never_type.is_none(),
                                "#builtin Never type cannot be declared twice"
                            );
                            self.never_type = Some(id);
                        }

                        ast::BuiltinEnum::Bool => {
                            assert!(
                                self.bool_type.is_none(),
                                "#builtin Bool type cannot be declared twice"
                            );
                            self.bool_type = Some(id);
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
                let mut local_names = Names {
                    variables_and_types: names
                        .variables_and_types
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
                        .collect(),
                    labels: Default::default(),
                    label_translations: Default::default(),
                };

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
                            let variable = variables.insert(it::Variable {
                                location,
                                name: Some(name),
                                typ,
                            });

                            parameter_variables.push(variable);

                            local_names.variables_and_types.insert(
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

                let id = self.functions.insert_with(|id| it::Function {
                    location: item.location,
                    parameter_types,
                    return_type,
                    typ: self.types.insert(it::Type {
                        location: item.location,
                        kind: it::TypeKind::FunctionItem(id),
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
        variables: &mut IdMap<it::Variable>,
    ) -> Result<Box<[it::Statement]>, TypingError> {
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
                        Some(it::Statement::Expression(Box::new(
                            self.expression(expression, names, variables)?,
                        )))
                    }

                    ast::StatementKind::Assignment {
                        ref pattern,
                        ref value,
                    } => {
                        let pattern = Box::new(self.pattern(pattern, names, variables)?);
                        let value = Box::new(self.expression(value, names, variables)?);
                        self.expect_types_equal(statement.location, pattern.typ, value.typ)?;
                        Some(it::Statement::Assignment { pattern, value })
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
        variables: &mut IdMap<it::Variable>,
    ) -> Result<it::Expression, TypingError> {
        Ok(match expression.kind {
            ast::ExpressionKind::Place(ref place) => {
                let (place, typ) = self.place(place, expression.location, names, variables)?;
                it::Expression {
                    location: expression.location,
                    typ,
                    kind: it::ExpressionKind::Place(place),
                }
            }

            ast::ExpressionKind::Constant(ref constant) => {
                let (constant, typ) =
                    self.constant(constant, expression.location, names, variables)?;
                it::Expression {
                    location: expression.location,
                    typ,
                    kind: it::ExpressionKind::Constant(constant),
                }
            }

            ast::ExpressionKind::Block {
                label,
                label_name,
                ref statements,
                ref last_expression,
            } => {
                let mut names = names.clone();

                let label_id = Id::new();
                let typ = self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                });
                self.label_types.insert(label_id, typ);
                names.label_translations.insert(label, label_id);
                if let Some(label) = label_name {
                    names.labels.insert(label, label_id);
                }

                let statements = self.statements(statements.iter(), &mut names, variables)?;

                let last_expression =
                    Box::new(self.expression(last_expression, &names, variables)?);
                self.expect_types_equal(last_expression.location, typ, last_expression.typ)?;

                it::Expression {
                    location: expression.location,
                    typ,
                    kind: it::ExpressionKind::Block {
                        label: label_id,
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
                let arguments = arguments
                    .iter()
                    .map(|expression| self.expression(expression, names, variables))
                    .collect::<Result<Box<[_]>, _>>()?;

                let return_type = self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                });
                let function_type = self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::FunctionLike {
                        parameters: arguments.iter().map(|argument| argument.typ).collect(),
                        return_type,
                    }),
                });

                self.expect_types_equal(operand.location, function_type, operand.typ)?;

                it::Expression {
                    location: expression.location,
                    typ: return_type,
                    kind: it::ExpressionKind::Call { operand, arguments },
                }
            }

            ast::ExpressionKind::Constructor {
                ref typ,
                ref arguments,
            } => {
                let typ = self.typ(typ, names)?;

                let mut initialised_members =
                    FxHashMap::<InternedStr, SourceLocation>::with_capacity_and_hasher(
                        arguments.len(),
                        FxBuildHasher,
                    );
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

                            Ok(it::ConstructorArgument { name, value })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                let required_struct_type = self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::StructLike {
                        members: arguments
                            .iter()
                            .map(|member| (member.name, member.value.typ))
                            .collect(),
                    }),
                });
                self.expect_types_equal(expression.location, typ, required_struct_type)?;

                it::Expression {
                    location: expression.location,
                    typ,
                    kind: it::ExpressionKind::Constructor { arguments },
                }
            }

            ast::ExpressionKind::Match {
                ref scruitnee,
                ref arms,
            } => {
                let scruitnee = Box::new(self.expression(scruitnee, names, variables)?);

                let arms_type = self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                });

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
                            self.expect_types_equal(pattern.location, scruitnee.typ, pattern.typ)?;

                            let value = self.expression(value, &names, variables)?;
                            self.expect_types_equal(value.location, arms_type, value.typ)?;

                            Ok(it::MatchArm {
                                location,
                                pattern,
                                value,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                it::Expression {
                    location: expression.location,
                    typ: arms_type,
                    kind: it::ExpressionKind::Match { scruitnee, arms },
                }
            }

            ast::ExpressionKind::Break {
                ref label,
                ref value,
            } => {
                let label = match *label {
                    ast::Label::Id(id) => names.label_translations[id],

                    ast::Label::Name(label) => match names.labels.get(&label) {
                        Some(&id) => id,
                        None => {
                            return Err(TypingError {
                                location: expression.location,
                                kind: TypingErrorKind::UnknownLabel { label },
                            });
                        }
                    },
                };
                let label_type = self.label_types[label];

                let value = Box::new(self.expression(value, names, variables)?);
                self.expect_types_equal(value.location, label_type, value.typ)?;

                it::Expression {
                    location: expression.location,
                    typ: self.types.insert(it::Type {
                        location: expression.location,
                        kind: it::TypeKind::Infer(it::Infer::Anything),
                    }),
                    kind: it::ExpressionKind::Break { label, value },
                }
            }

            ast::ExpressionKind::Continue { ref label } => it::Expression {
                location: expression.location,
                typ: self.types.insert(it::Type {
                    location: expression.location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                }),
                kind: it::ExpressionKind::Continue {
                    label: match *label {
                        ast::Label::Id(id) => names.label_translations[id],

                        ast::Label::Name(label) => match names.labels.get(&label) {
                            Some(&id) => id,
                            None => {
                                return Err(TypingError {
                                    location: expression.location,
                                    kind: TypingErrorKind::UnknownLabel { label },
                                });
                            }
                        },
                    },
                },
            },
        })
    }

    fn place(
        &mut self,
        place: &'ast ast::Place,
        location: SourceLocation,
        names: &Names<'ast>,
        variables: &mut IdMap<it::Variable>,
    ) -> Result<(it::Place, Id<it::Type>), TypingError> {
        Ok(match *place {
            ast::Place::Path(ref path) => match self.path(path, names)? {
                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Function(id),
                } => (it::Place::Function(id), self.functions[id].typ),

                ResolvedBinding {
                    location: _,
                    kind: ResolvedBindingKind::Variable(id),
                } => (it::Place::Variable(id), variables[id].typ),

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

                let member_type = self.types.insert(it::Type {
                    location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                });
                let struct_type = self.types.insert(it::Type {
                    location,
                    kind: it::TypeKind::Infer(it::Infer::StructLike {
                        members: [(member_name, member_type)].into_iter().collect(),
                    }),
                });

                self.expect_types_equal(operand.location, struct_type, operand.typ)?;

                (
                    it::Place::MemberAccess {
                        operand,
                        member_name,
                    },
                    member_type,
                )
            }
        })
    }

    fn constant(
        &mut self,
        constant: &ast::Constant,
        location: SourceLocation,
        #[expect(unused)] names: &Names<'ast>,
        #[expect(unused)] variables: &mut IdMap<it::Variable>,
    ) -> Result<(it::Constant, Id<it::Type>), TypingError> {
        Ok(match *constant {
            ast::Constant::Integer(value) => (
                it::Constant::Integer(value),
                self.types.insert(it::Type {
                    location,
                    kind: it::TypeKind::Infer(it::Infer::NumberLike),
                }),
            ),
        })
    }

    fn typ(
        &mut self,
        typ: &'ast ast::Type,
        names: &Names<'ast>,
    ) -> Result<Id<it::Type>, TypingError> {
        Ok(match typ.kind {
            ast::TypeKind::Infer => self.types.insert(it::Type {
                location: typ.location,
                kind: it::TypeKind::Infer(it::Infer::Anything),
            }),

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

                ast::BuiltinType::Never => self
                    .never_type
                    .expect("Never type should have already been declared with #builtin"),

                ast::BuiltinType::I32 => *self.i32_type.get_or_insert_with(|| {
                    self.types.insert(it::Type {
                        location: typ.location,
                        kind: it::TypeKind::I32,
                    })
                }),

                ast::BuiltinType::Runtime => *self.runtime_type.get_or_insert_with(|| {
                    self.types.insert(it::Type {
                        location: typ.location,
                        kind: it::TypeKind::Runtime,
                    })
                }),

                ast::BuiltinType::Bool => self
                    .bool_type
                    .expect("Bool type should have already been declared with #builtin"),
            },
        })
    }

    fn pattern(
        &mut self,
        pattern: &'ast ast::Pattern,
        names: &mut Names<'ast>,
        variables: &mut IdMap<it::Variable>,
    ) -> Result<it::Pattern, TypingError> {
        Ok(match pattern.kind {
            ast::PatternKind::Discard => it::Pattern {
                location: pattern.location,
                typ: self.types.insert(it::Type {
                    location: pattern.location,
                    kind: it::TypeKind::Infer(it::Infer::Anything),
                }),
                kind: it::PatternKind::Discard,
            },

            ast::PatternKind::Place(ref place) => {
                let (place, typ) = self.place(place, pattern.location, names, variables)?;
                it::Pattern {
                    location: pattern.location,
                    typ,
                    kind: it::PatternKind::Place(place),
                }
            }

            ast::PatternKind::Constant(ref constant) => {
                let (constant, typ) =
                    self.constant(constant, pattern.location, names, variables)?;
                it::Pattern {
                    location: pattern.location,
                    typ,
                    kind: it::PatternKind::Constant(constant),
                }
            }

            ast::PatternKind::Deconstructor {
                ref typ,
                ref arguments,
            } => {
                let typ = self.typ(typ, names)?;

                let mut initialised_members =
                    FxHashMap::<InternedStr, SourceLocation>::with_capacity_and_hasher(
                        arguments.len(),
                        FxBuildHasher,
                    );
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

                            Ok(it::DeconstructorArgument { name, pattern })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?;

                let required_struct_type = self.types.insert(it::Type {
                    location: pattern.location,
                    kind: it::TypeKind::Infer(it::Infer::StructLike {
                        members: arguments
                            .iter()
                            .map(|member| (member.name, member.pattern.typ))
                            .collect(),
                    }),
                });
                self.expect_types_equal(pattern.location, typ, required_struct_type)?;

                it::Pattern {
                    location: pattern.location,
                    typ,
                    kind: it::PatternKind::Deconstructor { arguments },
                }
            }

            ast::PatternKind::Let { name, ref typ } => {
                let typ = self.typ(typ, names)?;
                let variable = variables.insert(it::Variable {
                    location: pattern.location,
                    name: Some(name),
                    typ,
                });
                names.variables_and_types.insert(
                    name,
                    self.bindings.insert(Binding {
                        location: pattern.location,
                        kind: BindingKind::Resolved(ResolvedBinding {
                            location: pattern.location,
                            kind: ResolvedBindingKind::Variable(variable),
                        }),
                    }),
                );
                it::Pattern {
                    location: pattern.location,
                    typ,
                    kind: it::PatternKind::Let { variable },
                }
            }
        })
    }

    fn path(
        &mut self,
        path: &'ast ast::Path,
        names: &Names<'ast>,
    ) -> Result<ResolvedBinding, TypingError> {
        match names.variables_and_types.get(&path.name) {
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

    fn expect_types_equal(
        &mut self,
        location: SourceLocation,
        expected: Id<it::Type>,
        got: Id<it::Type>,
    ) -> Result<(), TypingError> {
        if expected == got {
            Ok(())
        } else {
            match (&self.types[expected].kind, &self.types[got].kind) {
                (&it::TypeKind::Resolved(expected), _) => {
                    self.expect_types_equal(location, expected, got)
                }
                (_, &it::TypeKind::Resolved(got)) => {
                    self.expect_types_equal(location, expected, got)
                }

                (&it::TypeKind::Infer(it::Infer::Anything), _) => {
                    self.types[expected].kind = it::TypeKind::Resolved(got);
                    Ok(())
                }
                (_, &it::TypeKind::Infer(it::Infer::Anything)) => {
                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }

                (
                    &it::TypeKind::Infer(it::Infer::NumberLike),
                    &it::TypeKind::Infer(it::Infer::NumberLike),
                ) => {
                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (&it::TypeKind::I32, &it::TypeKind::Infer(it::Infer::NumberLike)) => {
                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (&it::TypeKind::Infer(it::Infer::NumberLike), &it::TypeKind::I32) => {
                    self.types[expected].kind = it::TypeKind::Resolved(got);
                    Ok(())
                }

                (
                    &it::TypeKind::Infer(it::Infer::FunctionLike {
                        parameters: ref expected_parameters,
                        return_type: expected_return_type,
                    }),
                    &it::TypeKind::Infer(it::Infer::FunctionLike {
                        parameters: ref got_parameters,
                        return_type: got_return_type,
                    }),
                ) if expected_parameters.len() == got_parameters.len() => {
                    let expected_parameters = expected_parameters.clone();
                    let got_parameters = got_parameters.clone();

                    for (expected, got) in expected_parameters.into_iter().zip(got_parameters) {
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.expect_types_equal(location, expected_return_type, got_return_type)?;

                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (
                    &it::TypeKind::FunctionItem(function),
                    &it::TypeKind::Infer(it::Infer::FunctionLike {
                        ref parameters,
                        return_type,
                    }),
                ) if self.functions[function].parameter_types.len() == parameters.len() => {
                    let function_parameters = self.functions[function].parameter_types.clone();
                    let parameters = parameters.clone();

                    for (expected, got) in function_parameters.into_iter().zip(parameters) {
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.expect_types_equal(
                        location,
                        self.functions[function].return_type,
                        return_type,
                    )?;

                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (
                    &it::TypeKind::Infer(it::Infer::FunctionLike {
                        ref parameters,
                        return_type,
                    }),
                    &it::TypeKind::FunctionItem(function),
                ) if self.functions[function].parameter_types.len() == parameters.len() => {
                    let function_parameters = self.functions[function].parameter_types.clone();
                    let parameters = parameters.clone();

                    for (expected, got) in function_parameters.into_iter().zip(parameters) {
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.expect_types_equal(
                        location,
                        return_type,
                        self.functions[function].return_type,
                    )?;

                    self.types[expected].kind = it::TypeKind::Resolved(got);
                    Ok(())
                }

                (
                    &it::TypeKind::Infer(it::Infer::StructLike {
                        members: ref expected_members,
                    }),
                    &it::TypeKind::Infer(it::Infer::StructLike {
                        members: ref got_members,
                    }),
                ) => {
                    let expected_members = expected_members.clone();
                    let got_members = got_members.clone();

                    let mut members = FxHashMap::default();
                    for (&name, &expected) in expected_members.iter() {
                        members.insert(name, expected);
                    }
                    for (&name, &got) in got_members.iter() {
                        if let Some(expected) = members.insert(name, expected) {
                            self.expect_types_equal(location, expected, got)?;
                        }
                    }

                    self.types[expected].kind =
                        it::TypeKind::Infer(it::Infer::StructLike { members });
                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }

                (
                    it::TypeKind::Struct {
                        members: struct_members,
                    },
                    it::TypeKind::Infer(it::Infer::StructLike {
                        members: got_members,
                    }),
                ) if got_members.iter().all(|(&got_name, _)| {
                    struct_members.iter().any(|member| member.name == got_name)
                }) =>
                {
                    let struct_members = struct_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for (name, got) in got_members.clone() {
                        let expected = struct_members[&name];
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (
                    it::TypeKind::Infer(it::Infer::StructLike {
                        members: got_members,
                    }),
                    it::TypeKind::Struct {
                        members: struct_members,
                    },
                ) if got_members.iter().all(|(&got_name, _)| {
                    struct_members.iter().any(|member| member.name == got_name)
                }) =>
                {
                    let struct_members = struct_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for (name, got) in got_members.clone() {
                        let expected = struct_members[&name];
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.types[expected].kind = it::TypeKind::Resolved(got);
                    Ok(())
                }

                (
                    it::TypeKind::Enum {
                        members: enum_members,
                    },
                    it::TypeKind::Infer(it::Infer::StructLike {
                        members: got_members,
                    }),
                ) if got_members.iter().all(|(&got_name, _)| {
                    enum_members.iter().any(|member| member.name == got_name)
                }) =>
                {
                    let enum_members = enum_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for (name, got) in got_members.clone() {
                        let expected = enum_members[&name];
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.types[got].kind = it::TypeKind::Resolved(expected);
                    Ok(())
                }
                (
                    it::TypeKind::Infer(it::Infer::StructLike {
                        members: got_members,
                    }),
                    it::TypeKind::Enum {
                        members: enum_members,
                    },
                ) if got_members.iter().all(|(&got_name, _)| {
                    enum_members.iter().any(|member| member.name == got_name)
                }) =>
                {
                    let enum_members = enum_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for (name, got) in got_members.clone() {
                        let expected = enum_members[&name];
                        self.expect_types_equal(location, expected, got)?;
                    }

                    self.types[expected].kind = it::TypeKind::Resolved(got);
                    Ok(())
                }

                _ => Err(TypingError {
                    location,
                    kind: TypingErrorKind::ExpectedTypeButGotType { expected, got },
                }),
            }
        }
    }
}
