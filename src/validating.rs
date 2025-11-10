use crate::{
    ast,
    ids::Id,
    interning::InternedStr,
    lexing::{SourceLocation, TokenKind},
    syntax_tree as st,
};
use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct SyntaxTreeValidationError {
    pub location: SourceLocation,
    pub kind: SyntaxTreeValidationErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum SyntaxTreeValidationErrorKind {
    #[display("Expected a value")]
    ExpectedValue,
    #[display("Expected a type")]
    ExpectedType,
    #[display("Expected a pattern")]
    ExpectedPattern,
    #[display("Cannot have multiple builtin attributes on the same item")]
    MultipleBuiltinAttributes,
    #[display("Unknown builtin {_0:?}")]
    UnknownBuiltin(InternedStr),
    #[display("#builtin type annotation must not have a type")]
    BuiltinTypeAnnotationMustNotHaveType,
    #[display("Function must have a body")]
    FunctionMustHaveBody,
    #[display("#builtin function must not have a body")]
    BuiltinFunctionMustNotHaveBody,
    #[display("Labels can only be used on blocks or loops")]
    LabelsCanOnlyBeUsedOnBlocksOrLoops,
}

pub fn validate_items(items: &[st::Item]) -> Result<Box<[ast::Item]>, SyntaxTreeValidationError> {
    items.iter().map(validate_item).collect()
}

pub fn validate_item(item: &st::Item) -> Result<ast::Item, SyntaxTreeValidationError> {
    let mut builtin_attribute = None;
    for attribute in &item.attributes {
        match attribute.kind {
            st::AttributeKind::Builtin {
                ref builtin_token,
                open_parenthesis_token: _,
                ref string_token,
                close_parenthesis_token: _,
            } => {
                if builtin_attribute.is_some() {
                    return Err(SyntaxTreeValidationError {
                        location: builtin_token.location,
                        kind: SyntaxTreeValidationErrorKind::MultipleBuiltinAttributes,
                    });
                }

                let TokenKind::String(builtin_name) = string_token.kind else {
                    unreachable!("{string_token:?}")
                };
                builtin_attribute = Some((string_token.location, builtin_name));
            }
        }
    }

    Ok(ast::Item {
        location: item.location,
        kind: match item.kind {
            st::ItemKind::Struct {
                struct_token: _,
                ref name_token,
                members:
                    st::Members {
                        open_brace_token: _,
                        ref members,
                        close_brace_token: _,
                    },
            } => ast::ItemKind::Struct {
                builtin: if let Some((location, builtin_name)) = builtin_attribute {
                    Some(match builtin_name.as_str() {
                        "Unit" => ast::BuiltinStruct::Unit,

                        _ => {
                            return Err(SyntaxTreeValidationError {
                                location,
                                kind: SyntaxTreeValidationErrorKind::UnknownBuiltin(builtin_name),
                            });
                        }
                    })
                } else {
                    None
                },
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
                members: members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::Member {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!("{name_token:?}")
                                    };
                                    name
                                },
                                typ: validate_type(typ)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ItemKind::Enum {
                enum_token: _,
                ref name_token,
                members:
                    st::Members {
                        open_brace_token: _,
                        ref members,
                        close_brace_token: _,
                    },
            } => ast::ItemKind::Enum {
                builtin: if let Some((location, builtin_name)) = builtin_attribute {
                    Some(match builtin_name.as_str() {
                        "Never" => ast::BuiltinEnum::Never,

                        "Bool" => ast::BuiltinEnum::Bool,

                        _ => {
                            return Err(SyntaxTreeValidationError {
                                location,
                                kind: SyntaxTreeValidationErrorKind::UnknownBuiltin(builtin_name),
                            });
                        }
                    })
                } else {
                    None
                },
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
                members: members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::Member {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!("{name_token:?}")
                                    };
                                    name
                                },
                                typ: validate_type(typ)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ItemKind::Type {
                ref type_token,
                ref name_token,
                ref typ,
            } => ast::ItemKind::Type {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
                typ: Box::new(if let Some((location, builtin_name)) = builtin_attribute {
                    if let Some(typ) = typ {
                        return Err(SyntaxTreeValidationError {
                            location: typ.equals_token.location,
                            kind:
                                SyntaxTreeValidationErrorKind::BuiltinTypeAnnotationMustNotHaveType,
                        });
                    }

                    match builtin_name.as_str() {
                        "I32" => ast::Type {
                            location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::I32),
                        },

                        "Runtime" => ast::Type {
                            location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::Runtime),
                        },

                        _ => {
                            return Err(SyntaxTreeValidationError {
                                location,
                                kind: SyntaxTreeValidationErrorKind::UnknownBuiltin(builtin_name),
                            });
                        }
                    }
                } else if let Some(typ) = typ {
                    validate_type(&typ.typ)?
                } else {
                    ast::Type {
                        location: type_token.location,
                        kind: ast::TypeKind::Opaque {
                            name: {
                                let TokenKind::Name(name) = name_token.kind else {
                                    unreachable!("{name_token:?}")
                                };
                                name
                            },
                        },
                    }
                }),
            },

            st::ItemKind::Function {
                ref fn_token,
                ref name_token,
                parameters:
                    st::FunctionParameters {
                        open_parenthesis_token: _,
                        ref parameters,
                        ref close_parenthesis_token,
                    },
                ref return_type,
                ref body,
            } => ast::ItemKind::Function {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
                parameters: parameters
                    .iter()
                    .map(
                        |st::FunctionParameter {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::FunctionParameter {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!("{name_token:?}")
                                    };
                                    name
                                },
                                typ: validate_type(typ)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
                return_type: Box::new(if let Some(return_type) = return_type {
                    validate_type(&return_type.typ)?
                } else {
                    ast::Type {
                        location: close_parenthesis_token.location,
                        kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                    }
                }),
                body: if let Some((location, builtin_name)) = builtin_attribute {
                    if let Some(body) = body {
                        return Err(SyntaxTreeValidationError {
                            location: body.location,
                            kind: SyntaxTreeValidationErrorKind::BuiltinFunctionMustNotHaveBody,
                        });
                    }

                    match builtin_name.as_str() {
                        "print_i32" => ast::FunctionBody::Builtin(ast::BuiltinFunction::PrintI32),

                        _ => {
                            return Err(SyntaxTreeValidationError {
                                location,
                                kind: SyntaxTreeValidationErrorKind::UnknownBuiltin(builtin_name),
                            });
                        }
                    }
                } else {
                    let Some(body) = body else {
                        return Err(SyntaxTreeValidationError {
                            location: fn_token.location,
                            kind: SyntaxTreeValidationErrorKind::FunctionMustHaveBody,
                        });
                    };
                    ast::FunctionBody::Expression(Box::new(validate_expression(body)?))
                },
            },
        },
    })
}

pub fn validate_statement(
    statement: &st::Statement,
) -> Result<ast::Statement, SyntaxTreeValidationError> {
    Ok(ast::Statement {
        location: statement.location,
        kind: match statement.kind {
            st::StatementKind::Item(ref item) => {
                ast::StatementKind::Item(Box::new(validate_item(item)?))
            }

            st::StatementKind::Expression(ref expression) => {
                ast::StatementKind::Expression(Box::new(validate_expression(expression)?))
            }

            st::StatementKind::Assignment {
                ref pattern,
                equals_token: _,
                ref value,
            } => ast::StatementKind::Assignment {
                pattern: Box::new(validate_pattern(pattern)?),
                value: Box::new(validate_expression(value)?),
            },
        },
    })
}

pub fn validate_expression(
    expression: &st::Expression,
) -> Result<ast::Expression, SyntaxTreeValidationError> {
    if let Some(ref label) = expression.label
        && !matches!(
            expression.kind,
            st::ExpressionKind::Block { .. } | st::ExpressionKind::While { .. }
        )
    {
        return Err(SyntaxTreeValidationError {
            location: label.lifetime_token.location,
            kind: SyntaxTreeValidationErrorKind::LabelsCanOnlyBeUsedOnBlocksOrLoops,
        });
    }

    Ok(ast::Expression {
        location: expression.location,
        kind: match expression.kind {
            st::ExpressionKind::Block {
                open_brace_token: _,
                ref statements,
                ref close_brace_token,
            } => {
                let mut statements = statements
                    .iter()
                    .map(validate_statement)
                    .collect::<Result<Vec<_>, _>>()?;

                let last_expression = if let Some(last_statement) = statements
                    .pop_if(|statement| matches!(statement.kind, ast::StatementKind::Expression(_)))
                {
                    let ast::StatementKind::Expression(expression) = last_statement.kind else {
                        unreachable!("{last_statement:?}")
                    };
                    expression
                } else {
                    Box::new(unit_expression(close_brace_token.location))
                };

                ast::ExpressionKind::Block {
                    label: Id::new(),
                    label_name: expression.label.as_ref().map(|label| {
                        let TokenKind::Lifetime(name) = label.lifetime_token.kind else {
                            unreachable!("{:?}", label.lifetime_token)
                        };
                        name
                    }),
                    statements: statements.into_boxed_slice(),
                    last_expression,
                }
            }

            st::ExpressionKind::Integer { ref integer_token } => {
                ast::ExpressionKind::Constant(ast::Constant::Integer({
                    let TokenKind::Integer(value) = integer_token.kind else {
                        unreachable!("{integer_token:?}")
                    };
                    value
                }))
            }

            st::ExpressionKind::Path(ref path) => {
                ast::ExpressionKind::Place(ast::Place::Path(Box::new(validate_path(path)?)))
            }

            st::ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token: _,
                ref expression,
                close_parenthesis_token: _,
            } => return validate_expression(expression),

            st::ExpressionKind::Call {
                ref operand,
                arguments:
                    st::CallArguments {
                        open_parenthesis_token: _,
                        ref arguments,
                        close_parenthesis_token: _,
                    },
            } => ast::ExpressionKind::Call {
                operand: Box::new(validate_expression(operand)?),
                arguments: arguments
                    .iter()
                    .map(validate_expression)
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ExpressionKind::Constructor {
                ref typ,
                arguments:
                    st::ConstructorArguments {
                        open_brace_token: _,
                        ref arguments,
                        close_brace_token: _,
                    },
            } => ast::ExpressionKind::Constructor {
                typ: Box::new(validate_type(typ)?),
                arguments: arguments
                    .iter()
                    .map(
                        |st::ConstructorArgument {
                             name_token,
                             colon_token: _,
                             value,
                         }| {
                            Ok(ast::ConstructorArgument {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!("{name_token:?}")
                                    };
                                    name
                                },
                                value: validate_expression(value)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ExpressionKind::MemberAccess {
                ref operand,
                dot_token: _,
                ref name_token,
            } => ast::ExpressionKind::Place(ast::Place::MemberAccess {
                operand: Box::new(validate_expression(operand)?),
                member_name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
            }),

            st::ExpressionKind::Match {
                match_token: _,
                ref scruitnee,
                body:
                    st::MatchBody {
                        open_brace_token: _,
                        ref arms,
                        close_brace_token: _,
                    },
            } => ast::ExpressionKind::Match {
                scruitnee: Box::new(validate_expression(scruitnee)?),
                arms: arms
                    .iter()
                    .map(
                        |st::MatchArm {
                             pattern,
                             fat_right_arrow_token,
                             value,
                         }| {
                            Ok(ast::MatchArm {
                                location: fat_right_arrow_token.location,
                                pattern: validate_pattern(pattern)?,
                                value: validate_expression(value)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ExpressionKind::If {
                ref if_token,
                ref condition,
                ref then_body,
                ref else_body,
            } => if_expression(
                validate_expression(condition)?,
                validate_expression(then_body)?,
                if let Some(else_body) = else_body {
                    validate_expression(&else_body.else_block)?
                } else {
                    unit_expression(if_token.location)
                },
            ),

            st::ExpressionKind::Break {
                ref break_token,
                ref lifetime_token,
                ref value,
            } => ast::ExpressionKind::Break {
                label: {
                    let TokenKind::Lifetime(name) = lifetime_token.kind else {
                        unreachable!("{lifetime_token:?}")
                    };
                    ast::Label::Name(name)
                },
                value: if let Some(value) = value {
                    Box::new(validate_expression(value)?)
                } else {
                    Box::new(unit_expression(break_token.location))
                },
            },

            st::ExpressionKind::Continue {
                continue_token: _,
                ref lifetime_token,
            } => ast::ExpressionKind::Continue {
                label: {
                    let TokenKind::Lifetime(name) = lifetime_token.kind else {
                        unreachable!("{lifetime_token:?}")
                    };
                    ast::Label::Name(name)
                },
            },

            st::ExpressionKind::While {
                ref while_token,
                ref condition,
                ref body,
            } => {
                let label = Id::new();
                ast::ExpressionKind::Block {
                    label,
                    label_name: expression.label.as_ref().map(|label| {
                        let TokenKind::Lifetime(name) = label.lifetime_token.kind else {
                            unreachable!("{:?}", label.lifetime_token)
                        };
                        name
                    }),
                    statements: Box::new([]),
                    last_expression: Box::new(ast::Expression {
                        location: while_token.location,
                        kind: if_expression(
                            validate_expression(condition)?,
                            ast::Expression {
                                location: while_token.location,
                                kind: ast::ExpressionKind::Block {
                                    label: Id::new(),
                                    label_name: None,
                                    statements: Box::new([ast::Statement {
                                        location: body.location,
                                        kind: ast::StatementKind::Expression(Box::new(
                                            validate_expression(body)?,
                                        )),
                                    }]),
                                    last_expression: Box::new(ast::Expression {
                                        location: while_token.location,
                                        kind: ast::ExpressionKind::Continue {
                                            label: ast::Label::Id(label),
                                        },
                                    }),
                                },
                            },
                            unit_expression(while_token.location),
                        ),
                    }),
                }
            }

            st::ExpressionKind::Discard { .. } | st::ExpressionKind::Let { .. } => {
                return Err(SyntaxTreeValidationError {
                    location: expression.location,
                    kind: SyntaxTreeValidationErrorKind::ExpectedValue,
                });
            }
        },
    })
}

fn if_expression(
    condition: ast::Expression,
    then_body: ast::Expression,
    else_body: ast::Expression,
) -> ast::ExpressionKind {
    ast::ExpressionKind::Match {
        scruitnee: Box::new(condition),
        arms: Box::new([
            ast::MatchArm {
                location: then_body.location,
                pattern: ast::Pattern {
                    location: then_body.location,
                    kind: ast::PatternKind::Deconstructor {
                        typ: Box::new(ast::Type {
                            location: then_body.location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::Bool),
                        }),
                        arguments: Box::new([ast::DeconstructorArgument {
                            location: then_body.location,
                            name: "true".into(),
                            pattern: unit_pattern(then_body.location),
                        }]),
                    },
                },
                value: then_body,
            },
            ast::MatchArm {
                location: else_body.location,
                pattern: ast::Pattern {
                    location: else_body.location,
                    kind: ast::PatternKind::Deconstructor {
                        typ: Box::new(ast::Type {
                            location: else_body.location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::Bool),
                        }),
                        arguments: Box::new([ast::DeconstructorArgument {
                            location: else_body.location,
                            name: "false".into(),
                            pattern: unit_pattern(else_body.location),
                        }]),
                    },
                },
                value: else_body,
            },
        ]),
    }
}

fn unit_expression(location: SourceLocation) -> ast::Expression {
    ast::Expression {
        location,
        kind: ast::ExpressionKind::Constructor {
            typ: Box::new(ast::Type {
                location,
                kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
            }),
            arguments: Box::new([]),
        },
    }
}

fn unit_pattern(location: SourceLocation) -> ast::Pattern {
    ast::Pattern {
        location,
        kind: ast::PatternKind::Deconstructor {
            typ: Box::new(ast::Type {
                location,
                kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
            }),
            arguments: Box::new([]),
        },
    }
}

pub fn validate_type(expression: &st::Expression) -> Result<ast::Type, SyntaxTreeValidationError> {
    Ok(ast::Type {
        location: expression.location,
        kind: match expression.kind {
            st::ExpressionKind::Path(ref path) => {
                ast::TypeKind::Path(Box::new(validate_path(path)?))
            }

            st::ExpressionKind::Discard { discard_token: _ } => ast::TypeKind::Infer,

            st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::ParenthesizedExpression { .. }
            | st::ExpressionKind::Call { .. }
            | st::ExpressionKind::Constructor { .. }
            | st::ExpressionKind::MemberAccess { .. }
            | st::ExpressionKind::Let { .. }
            | st::ExpressionKind::Match { .. }
            | st::ExpressionKind::If { .. }
            | st::ExpressionKind::Break { .. }
            | st::ExpressionKind::Continue { .. }
            | st::ExpressionKind::While { .. } => {
                return Err(SyntaxTreeValidationError {
                    location: expression.location,
                    kind: SyntaxTreeValidationErrorKind::ExpectedType,
                });
            }
        },
    })
}

pub fn validate_pattern(
    pattern: &st::Expression,
) -> Result<ast::Pattern, SyntaxTreeValidationError> {
    Ok(ast::Pattern {
        location: pattern.location,
        kind: match pattern.kind {
            st::ExpressionKind::Discard { discard_token: _ } => ast::PatternKind::Discard,

            st::ExpressionKind::Path(ref path) => {
                ast::PatternKind::Place(ast::Place::Path(Box::new(validate_path(path)?)))
            }

            st::ExpressionKind::Integer { ref integer_token } => {
                ast::PatternKind::Constant(ast::Constant::Integer({
                    let TokenKind::Integer(value) = integer_token.kind else {
                        unreachable!("{integer_token:?}")
                    };
                    value
                }))
            }

            st::ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token: _,
                ref expression,
                close_parenthesis_token: _,
            } => return validate_pattern(expression),

            st::ExpressionKind::Constructor {
                ref typ,
                arguments:
                    st::ConstructorArguments {
                        open_brace_token: _,
                        ref arguments,
                        close_brace_token: _,
                    },
            } => ast::PatternKind::Deconstructor {
                typ: Box::new(validate_type(typ)?),
                arguments: arguments
                    .iter()
                    .map(
                        |st::ConstructorArgument {
                             name_token,
                             colon_token: _,
                             value,
                         }| {
                            Ok(ast::DeconstructorArgument {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!("{name_token:?}")
                                    };
                                    name
                                },
                                pattern: validate_pattern(value)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            st::ExpressionKind::MemberAccess {
                ref operand,
                dot_token: _,
                ref name_token,
            } => ast::PatternKind::Place(ast::Place::MemberAccess {
                operand: Box::new(validate_expression(operand)?),
                member_name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
            }),

            st::ExpressionKind::Let {
                ref let_token,
                ref name_token,
                ref typ,
            } => ast::PatternKind::Let {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!("{name_token:?}")
                    };
                    name
                },
                typ: Box::new(if let Some(typ) = typ {
                    validate_type(&typ.typ)?
                } else {
                    ast::Type {
                        location: let_token.location,
                        kind: ast::TypeKind::Infer,
                    }
                }),
            },

            st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Call { .. }
            | st::ExpressionKind::Match { .. }
            | st::ExpressionKind::If { .. }
            | st::ExpressionKind::Break { .. }
            | st::ExpressionKind::Continue { .. }
            | st::ExpressionKind::While { .. } => {
                return Err(SyntaxTreeValidationError {
                    location: pattern.location,
                    kind: SyntaxTreeValidationErrorKind::ExpectedPattern,
                });
            }
        },
    })
}

pub fn validate_path(path: &st::Path) -> Result<ast::Path, SyntaxTreeValidationError> {
    Ok(ast::Path {
        location: path.location,
        name: {
            let TokenKind::Name(name) = path.name_token.kind else {
                unreachable!("{:?}", path.name_token)
            };
            name
        },
    })
}
