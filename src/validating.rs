use crate::{
    ast,
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
    #[display("Expected a type")]
    ExpectedType,
    #[display("Expected a value")]
    ExpectedValue,
    #[display("Cannot have multiple builtin attributes on the same item")]
    MultipleBuiltinAttributes,
    #[display("Unknown builtin {_0:?}")]
    UnknownBuiltin(InternedStr),
    #[display("Type annotation must have a type")]
    TypeAnnotationMustHaveType,
    #[display("#builtin type annotation must not have a type")]
    BuiltinTypeAnnotationMustNotHaveType,
    #[display("Function must have a body")]
    FunctionMustHaveBody,
    #[display("#builtin function must not have a body")]
    BuiltinFunctionMustNotHaveBody,
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
                    st::StructMembers {
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
                        |st::StructMember {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::StructMember {
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
                } else {
                    let Some(typ) = typ else {
                        return Err(SyntaxTreeValidationError {
                            location: type_token.location,
                            kind: SyntaxTreeValidationErrorKind::TypeAnnotationMustHaveType,
                        });
                    };
                    validate_type(&typ.typ)?
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
        },
    })
}

pub fn validate_expression(
    expression: &st::Expression,
) -> Result<ast::Expression, SyntaxTreeValidationError> {
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
                    Box::new(ast::Expression {
                        location: close_brace_token.location,
                        kind: ast::ExpressionKind::Constructor {
                            typ: Box::new(ast::Type {
                                location: close_brace_token.location,
                                kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                            }),
                            arguments: Box::new([]),
                        },
                    })
                };

                ast::ExpressionKind::Block {
                    statements: statements.into_boxed_slice(),
                    last_expression,
                }
            }

            st::ExpressionKind::Integer { ref integer_token } => ast::ExpressionKind::Integer({
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!("{integer_token:?}")
                };
                value
            }),

            st::ExpressionKind::Path(ref path) => {
                ast::ExpressionKind::Path(Box::new(validate_path(path)?))
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

            st::ExpressionKind::Discard { discard_token: _ } => {
                return Err(SyntaxTreeValidationError {
                    location: expression.location,
                    kind: SyntaxTreeValidationErrorKind::ExpectedValue,
                });
            }
        },
    })
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
            | st::ExpressionKind::Constructor { .. } => {
                return Err(SyntaxTreeValidationError {
                    location: expression.location,
                    kind: SyntaxTreeValidationErrorKind::ExpectedType,
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
