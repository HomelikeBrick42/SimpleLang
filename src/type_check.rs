use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use crate::{
    ids::{Id, IdMap, IdSecondaryMap},
    inferred_tree as it,
    inferring::InferResult,
    interning::InternedStr,
    lexing::SourceLocation,
    typed_tree as tt,
};

#[derive(Debug, Clone)]
pub struct TypeCheckError {
    pub location: SourceLocation,
    pub kind: TypeCheckErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeCheckErrorKind {
    ExpectedStructOrEnumButGot {
        typ: Id<tt::Type>,
    },
    UnknownMemberOnType {
        member_name: InternedStr,
        typ: Id<tt::Type>,
    },
    MemberWasLeftUninitialised {
        member_name: InternedStr,
        typ: Id<tt::Type>,
    },
    OnlyOneEnumVariantCanBeInitialised {
        typ: Id<tt::Type>,
    },
    MemberWasNotDeconstructed {
        member_name: InternedStr,
        typ: Id<tt::Type>,
    },
    OnlyOneEnumVariantCanBeDeconstructed {
        typ: Id<tt::Type>,
    },
    IntegerOutOfRangeForType {
        value: u128,
        typ: Id<tt::Type>,
    },
    PlaceMustBeReadable,
    PatternMustBeWritable,
    PatternMustBeConstant,
    TypeHasInfiniteSize {
        typ: Id<tt::Type>,
    },
    TypeDoesNotHaveALayout {
        typ: Id<tt::Type>,
    },
}

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
        label_translations: IdSecondaryMap::new(),
        variable_translations: IdSecondaryMap::new(),
    };

    let mut errors = vec![];

    for (typ, _) in infer_result.types.iter() {
        checker.typ(typ);
    }

    for typ in checker.types.iter().map(|(id, _)| id).collect::<Vec<_>>() {
        match get_type_layout(typ, &checker.types, &mut IdSecondaryMap::new()) {
            Ok(layout) => checker.types[typ].layout = layout,
            Err(error) => {
                errors.push(error);
            }
        }
    }

    for (function, _) in infer_result.functions.iter() {
        checker.function(function);
    }

    let mut function_bodies = IdSecondaryMap::new();
    'function_loop: for (function, function_body) in infer_result.function_bodies.iter() {
        let function = checker.function_translations[function];
        function_bodies.insert(
            function,
            match function_body {
                it::FunctionBody::Builtin(builtin_function) => {
                    tt::FunctionBody::Builtin(match builtin_function {
                        it::BuiltinFunction::PrintI32 => {
                            let [first_parameter, second_parameter] =
                                *checker.functions[function].parameter_types
                            else {
                                panic!("#builtin(\"PrintI32\") should have 2 parameters")
                            };

                            let tt::TypeKind::I32 = checker.types[first_parameter].kind else {
                                panic!(
                                    "The first parameter to #builtin(\"PrintI32\") should be I32"
                                );
                            };
                            let tt::TypeKind::Runtime = checker.types[second_parameter].kind else {
                                panic!(
                                    "The second parameter to #builtin(\"PrintI32\") should be Runtime"
                                );
                            };

                            tt::BuiltinFunction::PrintI32
                        }
                    })
                }

                it::FunctionBody::Body {
                    variables: infer_variables,
                    parameters: infer_parameters,
                    expression,
                } => {
                    let mut variables = IdMap::new();
                    for (
                        infer_variable,
                        &it::Variable {
                            location,
                            name,
                            typ,
                        },
                    ) in infer_variables.iter()
                    {
                        let typ = checker.typ(typ);
                        let variable = variables.insert(tt::Variable {
                            location,
                            name,
                            layout: match checker.types[typ].layout {
                                Some(layout) => layout,
                                None => {
                                    errors.push(TypeCheckError { location, kind: TypeCheckErrorKind::TypeDoesNotHaveALayout { typ } });
                                    continue 'function_loop;
                                },
                            },
                            typ,
                        });
                        checker
                            .variable_translations
                            .insert(infer_variable, variable);
                    }

                    let parameters = infer_parameters
                        .iter()
                        .map(|&parameter| checker.variable_translations[parameter])
                        .collect();

                    let expression = match checker.expression(expression) {
                        Ok(expression) => Box::new(expression),
                        Err(error) => {
                            errors.push(error);
                            continue 'function_loop;
                        }
                    };

                    tt::FunctionBody::Body {
                        variables,
                        parameters,
                        expression,
                    }
                }
            },
        );
    }

    TypeCheckResult {
        types: checker.types,
        functions: checker.functions,
        function_bodies,
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
    label_translations: IdSecondaryMap<it::Label, Id<tt::Label>>,
    variable_translations: IdSecondaryMap<it::Variable, Id<tt::Variable>>,
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
                layout: None,
                kind: tt::TypeKind::Opaque {
                    name: "{{error}}".into(),
                },
            }),

            it::TypeKind::Opaque { name } => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::Opaque { name },
            }),

            it::TypeKind::Struct { name, ref members } => {
                let id = self.types.insert(tt::Type {
                    location: infer_type.location,
                    layout: None,
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
                    layout: None,
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
                    layout: None,
                    kind: tt::TypeKind::Opaque {
                        name: self.infer_functions[function].name,
                    },
                });
                self.type_translations.insert(typ, id);

                let function = self.function(function);

                self.types[id].kind = tt::TypeKind::FunctionItem(function);

                id
            }

            it::TypeKind::U8 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::U8,
            }),
            it::TypeKind::U16 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::U16,
            }),
            it::TypeKind::U32 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::U32,
            }),
            it::TypeKind::U64 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::U64,
            }),
            it::TypeKind::I8 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::I8,
            }),
            it::TypeKind::I16 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::I16,
            }),
            it::TypeKind::I32 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::I32,
            }),
            it::TypeKind::I64 => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::I64,
            }),
            it::TypeKind::ISize => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::ISize,
            }),
            it::TypeKind::USize => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::USize,
            }),

            it::TypeKind::Runtime => self.types.insert(tt::Type {
                location: infer_type.location,
                layout: None,
                kind: tt::TypeKind::Runtime,
            }),
        };
        self.type_translations.insert(typ, id);
        id
    }

    fn function(&mut self, infer_function_id: Id<it::Function>) -> Id<tt::Function> {
        if let Some(&function) = self.function_translations.get(infer_function_id) {
            return function;
        }

        let infer_function = &self.infer_functions[infer_function_id];

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

        let function = self.functions.insert(function);
        self.function_translations
            .insert(infer_function_id, function);
        function
    }

    fn expression(
        &mut self,
        expression: &it::Expression,
    ) -> Result<tt::Expression, TypeCheckError> {
        let typ = self.typ(expression.typ);
        Ok(tt::Expression {
            location: expression.location,
            typ,
            kind: match expression.kind {
                it::ExpressionKind::Place(ref place) => {
                    let place = self.place(expression.location, typ, place)?;
                    check_place_readable(expression.location, &place)?;
                    tt::ExpressionKind::Place(place)
                }

                it::ExpressionKind::Constant(ref constant) => tt::ExpressionKind::Constant(
                    self.constant(expression.location, typ, constant)?,
                ),

                it::ExpressionKind::Block {
                    label: infer_label,
                    ref statements,
                    ref last_expression,
                } => tt::ExpressionKind::Block {
                    label: {
                        let label = Id::new();
                        self.label_translations.insert(infer_label, label);
                        label
                    },
                    statements: statements
                        .iter()
                        .map(|statement| self.statement(statement))
                        .collect::<Result<Box<[_]>, _>>()?,
                    last_expression: Box::new(self.expression(last_expression)?),
                },

                it::ExpressionKind::Call {
                    ref operand,
                    ref arguments,
                } => tt::ExpressionKind::Call {
                    operand: Box::new(self.expression(operand)?),
                    arguments: arguments
                        .iter()
                        .map(|argument| self.expression(argument))
                        .collect::<Result<Box<[_]>, _>>()?,
                },

                it::ExpressionKind::Constructor { ref arguments } => match self.types[typ].kind {
                    tt::TypeKind::Struct {
                        name: _,
                        ref members,
                    } => {
                        let mut members_to_initialise = members
                            .iter()
                            .enumerate()
                            .map(|(index, member)| (member.name, index))
                            .collect::<FxHashMap<_, _>>();

                        let arguments = arguments
                            .iter()
                            .map(
                                |&it::ConstructorArgument {
                                     location,
                                     name,
                                     ref value,
                                 }| {
                                    Ok(tt::StructConstructorArgument {
                                        location,
                                        member_index: members_to_initialise.remove(&name).ok_or(
                                            TypeCheckError {
                                                location,
                                                kind: TypeCheckErrorKind::UnknownMemberOnType {
                                                    member_name: name,
                                                    typ,
                                                },
                                            },
                                        )?,
                                        value: self.expression(value)?,
                                    })
                                },
                            )
                            .collect::<Result<Box<[_]>, _>>()?;

                        if let Some((name, _)) = members_to_initialise.into_iter().next() {
                            return Err(TypeCheckError {
                                location: expression.location,
                                kind: TypeCheckErrorKind::MemberWasLeftUninitialised {
                                    member_name: name,
                                    typ,
                                },
                            });
                        }

                        tt::ExpressionKind::StructConstructor { arguments }
                    }

                    tt::TypeKind::Enum {
                        name: _,
                        ref members,
                    } => tt::ExpressionKind::EnumConstructor {
                        argument: match **arguments {
                            [
                                it::ConstructorArgument {
                                    location,
                                    name,
                                    ref value,
                                },
                            ] => Box::new(tt::EnumConstructorArgument {
                                location,
                                variant_index: members
                                    .iter()
                                    .position(|member| member.name == name)
                                    .ok_or(TypeCheckError {
                                        location,
                                        kind: TypeCheckErrorKind::UnknownMemberOnType {
                                            member_name: name,
                                            typ,
                                        },
                                    })?,
                                value: self.expression(value)?,
                            }),

                            _ => {
                                return Err(TypeCheckError {
                                    location: expression.location,
                                    kind: TypeCheckErrorKind::OnlyOneEnumVariantCanBeInitialised {
                                        typ,
                                    },
                                });
                            }
                        },
                    },

                    tt::TypeKind::Opaque { .. }
                    | tt::TypeKind::FunctionItem(_)
                    | tt::TypeKind::U8
                    | tt::TypeKind::U16
                    | tt::TypeKind::U32
                    | tt::TypeKind::U64
                    | tt::TypeKind::I8
                    | tt::TypeKind::I16
                    | tt::TypeKind::I32
                    | tt::TypeKind::I64
                    | tt::TypeKind::ISize
                    | tt::TypeKind::USize
                    | tt::TypeKind::Runtime => {
                        return Err(TypeCheckError {
                            location: expression.location,
                            kind: TypeCheckErrorKind::ExpectedStructOrEnumButGot { typ },
                        });
                    }
                },

                it::ExpressionKind::Match {
                    ref scruitnee,
                    ref arms,
                } => {
                    let scruitnee = Box::new(self.expression(scruitnee)?);
                    let arms = arms
                        .iter()
                        .map(
                            |&it::MatchArm {
                                 location,
                                 ref pattern,
                                 ref value,
                             }| {
                                Ok(tt::MatchArm {
                                    location,
                                    pattern: {
                                        let pattern = self.pattern(pattern)?;
                                        check_pattern_constant(&pattern)?;
                                        pattern
                                    },
                                    value: self.expression(value)?,
                                })
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?;

                    // TODO: check match arm exhaustiveness

                    tt::ExpressionKind::Match { scruitnee, arms }
                }

                it::ExpressionKind::Break { label, ref value } => tt::ExpressionKind::Break {
                    label: self.label_translations[label],
                    value: Box::new(self.expression(value)?),
                },

                it::ExpressionKind::Continue { label } => tt::ExpressionKind::Continue {
                    label: self.label_translations[label],
                },
            },
        })
    }

    fn place(
        &mut self,
        location: SourceLocation,
        #[expect(unused)] typ: Id<tt::Type>,
        place: &it::Place,
    ) -> Result<tt::Place, TypeCheckError> {
        Ok(match *place {
            it::Place::Variable(id) => tt::Place::Variable(self.variable_translations[id]),

            it::Place::Function(id) => tt::Place::Function(self.function(id)),

            it::Place::MemberAccess {
                ref operand,
                member_name,
            } => {
                let operand = Box::new(self.expression(operand)?);
                let typ = operand.typ;

                match self.types[typ].kind {
                    tt::TypeKind::Struct {
                        name: _,
                        ref members,
                    } => {
                        let member_index = members
                            .iter()
                            .position(|member| member.name == member_name)
                            .ok_or(TypeCheckError {
                                location,
                                kind: TypeCheckErrorKind::UnknownMemberOnType { member_name, typ },
                            })?;
                        tt::Place::StructMemberAccess {
                            operand,
                            member_index,
                        }
                    }

                    tt::TypeKind::Enum {
                        name: _,
                        ref members,
                    } => {
                        let variant_index = members
                            .iter()
                            .position(|member| member.name == member_name)
                            .ok_or(TypeCheckError {
                                location,
                                kind: TypeCheckErrorKind::UnknownMemberOnType { member_name, typ },
                            })?;
                        tt::Place::EnumMemberAccess {
                            operand,
                            variant_index,
                        }
                    }

                    tt::TypeKind::Opaque { .. }
                    | tt::TypeKind::FunctionItem(_)
                    | tt::TypeKind::U8
                    | tt::TypeKind::U16
                    | tt::TypeKind::U32
                    | tt::TypeKind::U64
                    | tt::TypeKind::I8
                    | tt::TypeKind::I16
                    | tt::TypeKind::I32
                    | tt::TypeKind::I64
                    | tt::TypeKind::ISize
                    | tt::TypeKind::USize
                    | tt::TypeKind::Runtime => {
                        return Err(TypeCheckError {
                            location,
                            kind: TypeCheckErrorKind::ExpectedStructOrEnumButGot { typ },
                        });
                    }
                }
            }
        })
    }

    fn constant(
        &mut self,
        location: SourceLocation,
        typ: Id<tt::Type>,
        constant: &it::Constant,
    ) -> Result<tt::Constant, TypeCheckError> {
        macro_rules! integer {
            ($value:ident, $integer:ident, $variant:ident $(, $($signed:tt)*)?) => {{
                if $value $($($signed)*)? < $integer::MIN as _ || $value $($($signed)*)? > $integer::MAX as _ {
                    return Err(TypeCheckError {
                        location,
                        kind: TypeCheckErrorKind::IntegerOutOfRangeForType { value: $value, typ },
                    });
                }
                tt::Constant::$variant($integer::try_from($value).unwrap())
            }};
        }

        Ok(match *constant {
            it::Constant::Integer(value) => match self.types[typ].kind {
                tt::TypeKind::U8 => integer!(value, u8, U8),
                tt::TypeKind::U16 => integer!(value, u16, U16),
                tt::TypeKind::U32 => integer!(value, u32, U32),
                tt::TypeKind::U64 => integer!(value, u64, U64),
                tt::TypeKind::I8 => integer!(value, i8, I8, .cast_signed()),
                tt::TypeKind::I16 => integer!(value, i16, I16, .cast_signed()),
                tt::TypeKind::I32 => integer!(value, i32, I32, .cast_signed()),
                tt::TypeKind::I64 => integer!(value, i64, I64, .cast_signed()),
                tt::TypeKind::ISize => integer!(value, isize, ISize, .cast_signed()),
                tt::TypeKind::USize => integer!(value, usize, USize),

                tt::TypeKind::Opaque { .. }
                | tt::TypeKind::Struct { .. }
                | tt::TypeKind::Enum { .. }
                | tt::TypeKind::FunctionItem(_)
                | tt::TypeKind::Runtime => {
                    unreachable!("Integer type was somehow {:?}", self.types[typ])
                }
            },
        })
    }

    fn statement(&mut self, statement: &it::Statement) -> Result<tt::Statement, TypeCheckError> {
        Ok(tt::Statement {
            location: statement.location,
            kind: match statement.kind {
                it::StatementKind::Expression(ref expression) => {
                    tt::StatementKind::Expression(Box::new(self.expression(expression)?))
                }

                it::StatementKind::Assignment {
                    ref pattern,
                    ref value,
                } => {
                    let pattern = Box::new(self.pattern(pattern)?);
                    let value = Box::new(self.expression(value)?);

                    check_pattern_writable(&pattern)?;

                    tt::StatementKind::Assignment { pattern, value }
                }
            },
        })
    }

    fn pattern(&mut self, pattern: &it::Pattern) -> Result<tt::Pattern, TypeCheckError> {
        let typ = self.typ(pattern.typ);
        Ok(tt::Pattern {
            location: pattern.location,
            typ,
            kind: match pattern.kind {
                it::PatternKind::Discard => tt::PatternKind::Discard,

                it::PatternKind::Place(ref place) => {
                    tt::PatternKind::Place(self.place(pattern.location, typ, place)?)
                }

                it::PatternKind::Constant(ref constant) => {
                    tt::PatternKind::Constant(self.constant(pattern.location, typ, constant)?)
                }

                it::PatternKind::Deconstructor { ref arguments } => match self.types[typ].kind {
                    tt::TypeKind::Struct {
                        name: _,
                        ref members,
                    } => {
                        let mut members_to_initialise = members
                            .iter()
                            .enumerate()
                            .map(|(index, member)| (member.name, index))
                            .collect::<FxHashMap<_, _>>();

                        let arguments = arguments
                            .iter()
                            .map(
                                |&it::DeconstructorArgument {
                                     location,
                                     name,
                                     ref pattern,
                                 }| {
                                    Ok(tt::StructDeconstructorArgument {
                                        location,
                                        member_index: members_to_initialise.remove(&name).ok_or(
                                            TypeCheckError {
                                                location,
                                                kind: TypeCheckErrorKind::UnknownMemberOnType {
                                                    member_name: name,
                                                    typ,
                                                },
                                            },
                                        )?,
                                        pattern: self.pattern(pattern)?,
                                    })
                                },
                            )
                            .collect::<Result<Box<[_]>, _>>()?;

                        if let Some((name, _)) = members_to_initialise.into_iter().next() {
                            return Err(TypeCheckError {
                                location: pattern.location,
                                kind: TypeCheckErrorKind::MemberWasNotDeconstructed {
                                    member_name: name,
                                    typ,
                                },
                            });
                        }

                        tt::PatternKind::StructDeconstructor { arguments }
                    }

                    tt::TypeKind::Enum {
                        name: _,
                        ref members,
                    } => tt::PatternKind::EnumDeconstructor {
                        argument: match **arguments {
                            [
                                it::DeconstructorArgument {
                                    location,
                                    name,
                                    ref pattern,
                                },
                            ] => Box::new(tt::EnumDeconstructorArgument {
                                location,
                                variant_index: members
                                    .iter()
                                    .position(|member| member.name == name)
                                    .ok_or(TypeCheckError {
                                        location,
                                        kind: TypeCheckErrorKind::UnknownMemberOnType {
                                            member_name: name,
                                            typ,
                                        },
                                    })?,
                                pattern: self.pattern(pattern)?,
                            }),

                            _ => {
                                return Err(TypeCheckError {
                                    location: pattern.location,
                                    kind:
                                        TypeCheckErrorKind::OnlyOneEnumVariantCanBeDeconstructed {
                                            typ,
                                        },
                                });
                            }
                        },
                    },

                    tt::TypeKind::Opaque { .. }
                    | tt::TypeKind::FunctionItem(_)
                    | tt::TypeKind::U8
                    | tt::TypeKind::U16
                    | tt::TypeKind::U32
                    | tt::TypeKind::U64
                    | tt::TypeKind::I8
                    | tt::TypeKind::I16
                    | tt::TypeKind::I32
                    | tt::TypeKind::I64
                    | tt::TypeKind::ISize
                    | tt::TypeKind::USize
                    | tt::TypeKind::Runtime => {
                        return Err(TypeCheckError {
                            location: pattern.location,
                            kind: TypeCheckErrorKind::ExpectedStructOrEnumButGot { typ },
                        });
                    }
                },

                it::PatternKind::Let { variable } => tt::PatternKind::Let {
                    variable: self.variable_translations[variable],
                },
            },
        })
    }
}

fn check_place_readable(location: SourceLocation, place: &tt::Place) -> Result<(), TypeCheckError> {
    let readable = match place {
        tt::Place::Function(_) => true,
        tt::Place::Variable(_) => true,
        tt::Place::StructMemberAccess { .. } => true,
        tt::Place::EnumMemberAccess { .. } => false,
    };
    if readable {
        Ok(())
    } else {
        Err(TypeCheckError {
            location,
            kind: TypeCheckErrorKind::PlaceMustBeReadable,
        })
    }
}

fn check_pattern_writable(pattern: &tt::Pattern) -> Result<(), TypeCheckError> {
    let writable = match pattern.kind {
        tt::PatternKind::Discard => true,
        tt::PatternKind::Place(ref place) => match *place {
            tt::Place::Function(_) => false,
            tt::Place::Variable(_) => true,
            tt::Place::StructMemberAccess { .. } => true,
            tt::Place::EnumMemberAccess { .. } => false,
        },
        tt::PatternKind::Constant(_) => false,
        tt::PatternKind::StructDeconstructor { ref arguments } => {
            return arguments
                .iter()
                .try_for_each(|argument| check_pattern_writable(&argument.pattern));
        }
        tt::PatternKind::EnumDeconstructor { ref argument } => {
            return check_pattern_writable(&argument.pattern);
        }
        tt::PatternKind::Let { .. } => true,
    };
    if writable {
        Ok(())
    } else {
        Err(TypeCheckError {
            location: pattern.location,
            kind: TypeCheckErrorKind::PatternMustBeWritable,
        })
    }
}

fn check_pattern_constant(pattern: &tt::Pattern) -> Result<(), TypeCheckError> {
    let constant = match pattern.kind {
        tt::PatternKind::Discard => true,
        tt::PatternKind::Place(ref place) => match *place {
            tt::Place::Function(_) => true,
            tt::Place::Variable(_) => false,
            tt::Place::StructMemberAccess {
                operand: _,
                member_index: _,
            } => false,
            tt::Place::EnumMemberAccess {
                operand: _,
                variant_index: _,
            } => false,
        },
        tt::PatternKind::Constant(_) => true,
        tt::PatternKind::StructDeconstructor { ref arguments } => {
            return arguments
                .iter()
                .try_for_each(|argument| check_pattern_constant(&argument.pattern));
        }
        tt::PatternKind::EnumDeconstructor { ref argument } => {
            return check_pattern_constant(&argument.pattern);
        }
        tt::PatternKind::Let { .. } => true,
    };
    if constant {
        Ok(())
    } else {
        Err(TypeCheckError {
            location: pattern.location,
            kind: TypeCheckErrorKind::PatternMustBeConstant,
        })
    }
}

fn get_type_layout(
    typ: Id<tt::Type>,
    types: &IdMap<tt::Type>,
    seen_types: &mut IdSecondaryMap<tt::Type, ()>,
) -> Result<Option<tt::Layout>, TypeCheckError> {
    let location = types[typ].location;
    if seen_types.insert(typ, ()).is_none() {
        Ok(Some(match types[typ].kind {
            tt::TypeKind::Opaque { name: _ } => {
                return Ok(None);
            }
            tt::TypeKind::Struct {
                name: _,
                ref members,
            } => {
                return members
                    .iter()
                    .map(|member| get_type_layout(member.typ, types, &mut seen_types.clone()))
                    .sum();
            }
            tt::TypeKind::Enum {
                name: _,
                ref members,
            } => {
                let discrminant_layout = match members.len() {
                    ..=0xFF => tt::Layout {
                        align: const { NonZeroUsize::new(align_of::<u8>()).unwrap() },
                        size: size_of::<u8>(),
                    },
                    0x100..=0xFFFF => tt::Layout {
                        align: const { NonZeroUsize::new(align_of::<u16>()).unwrap() },
                        size: size_of::<u16>(),
                    },
                    0x10000..=0xFFFFFFFF => tt::Layout {
                        align: const { NonZeroUsize::new(align_of::<u32>()).unwrap() },
                        size: size_of::<u32>(),
                    },
                    0x100000000..=0xFFFFFFFFFFFFFFFF => tt::Layout {
                        align: const { NonZeroUsize::new(align_of::<u64>()).unwrap() },
                        size: size_of::<u64>(),
                    },
                    _ => panic!("Internal Compiler Error: unsupported number of enum variants"),
                };
                return std::iter::once(Ok(Some(discrminant_layout)))
                    .chain(
                        members.iter().map(|member| {
                            get_type_layout(member.typ, types, &mut seen_types.clone())
                        }),
                    )
                    .sum();
            }
            tt::TypeKind::FunctionItem(_) => tt::Layout {
                align: NonZeroUsize::MIN,
                size: 0,
            },
            tt::TypeKind::U8 | tt::TypeKind::I8 => tt::Layout {
                align: const { NonZeroUsize::new(align_of::<u8>()).unwrap() },
                size: size_of::<u8>(),
            },
            tt::TypeKind::U16 | tt::TypeKind::I16 => tt::Layout {
                align: const { NonZeroUsize::new(align_of::<u16>()).unwrap() },
                size: size_of::<u16>(),
            },
            tt::TypeKind::U32 | tt::TypeKind::I32 => tt::Layout {
                align: const { NonZeroUsize::new(align_of::<u32>()).unwrap() },
                size: size_of::<u32>(),
            },
            tt::TypeKind::U64 | tt::TypeKind::I64 => tt::Layout {
                align: const { NonZeroUsize::new(align_of::<u64>()).unwrap() },
                size: size_of::<u64>(),
            },
            tt::TypeKind::ISize | tt::TypeKind::USize => tt::Layout {
                align: const { NonZeroUsize::new(align_of::<usize>()).unwrap() },
                size: size_of::<usize>(),
            },
            tt::TypeKind::Runtime => tt::Layout {
                align: NonZeroUsize::MIN,
                size: 0,
            },
        }))
    } else {
        Err(TypeCheckError {
            location,
            kind: TypeCheckErrorKind::TypeHasInfiniteSize { typ },
        })
    }
}
