use crate::{
    interning::InternedStr,
    lexing::{Lexer, LexerErrorKind, LexingError, SourceLocation, Token, TokenKind},
    syntax_tree::{
        Attribute, AttributeKind, CallArguments, ColonType, ConstructorArgument,
        ConstructorArguments, Else, EqualsType, Expression, ExpressionKind, FunctionParameter,
        FunctionParameters, FunctionReturnType, Item, ItemKind, MatchArm, MatchBody, Member,
        Members, Path, Statement, StatementKind,
    },
};
use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct ParsingError {
    pub location: SourceLocation,
    pub kind: ParsingErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum ParsingErrorKind {
    #[display("{_0}")]
    LexerError(LexerErrorKind),
    #[display("Unexpected {_0}")]
    UnexpectedToken(Token),
}

impl From<LexingError> for ParsingError {
    fn from(error: LexingError) -> Self {
        Self {
            location: error.location,
            kind: ParsingErrorKind::LexerError(error.kind),
        }
    }
}

macro_rules! peek {
    ($lexer:ident, $kind:pat $(, $name:ident)*) => {
        match $lexer.peek_token() {
            #[expect(clippy::double_parens)]
            Ok(token @ Token { kind: $kind, .. }) => Some((token $(, $name)*)),
            _ => None,
        }
    };
}

macro_rules! consume {
    ($lexer:ident, $kind:pat $(, $name:ident)*) => {{
        let mut lexer = $lexer.clone();
        match lexer.next_token() {
            Ok(token @ Token { kind: $kind, .. }) => {
                *$lexer = lexer;
                #[expect(clippy::double_parens)]
                Some((token $(, $name)*))
            },
            _ => None,
        }
    }};
}

macro_rules! expect {
    ($lexer:ident, $kind:pat $(, $name:ident)*) => {
        match $lexer.next_token() {
            #[expect(clippy::double_parens)]
            Ok(token @ Token { kind: $kind, .. }) => Ok((token $(, $name)*)),
            Ok(token) => Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            }),
            Err(error) => Err(error.into()),
        }
    };
}

pub fn parse_file(filepath: InternedStr, source: &str) -> Result<Box<[Item]>, ParsingError> {
    let lexer = &mut Lexer::new(filepath, source);

    let mut items = vec![];
    loop {
        while consume!(lexer, TokenKind::Newline).is_some() {}
        if lexer.is_empty() {
            break;
        }

        items.push(parse_item(lexer)?);
        expect!(lexer, TokenKind::Newline)?;
    }
    Ok(items.into_boxed_slice())
}

pub fn parse_item(lexer: &mut Lexer) -> Result<Item, ParsingError> {
    let mut attributes = vec![];
    loop {
        while consume!(lexer, TokenKind::Newline).is_some() {}

        attributes.push(match lexer.peek_token()? {
            builtin_token @ Token {
                location,
                kind: TokenKind::BuiltinDirective,
            } => {
                lexer.next_token()?;
                Attribute {
                    location,
                    kind: AttributeKind::Builtin {
                        builtin_token,
                        open_parenthesis_token: expect!(lexer, TokenKind::OpenParenthesis)?,
                        string_token: expect!(lexer, TokenKind::String(_))?,
                        close_parenthesis_token: expect!(lexer, TokenKind::CloseParenthesis)?,
                    },
                }
            }

            _ => break,
        });
    }
    let attributes = attributes.into_boxed_slice();

    Ok(match lexer.next_token()? {
        struct_token @ Token {
            location,
            kind: TokenKind::StructKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Struct {
                struct_token,
                name_token: expect!(lexer, TokenKind::Name(_))?,

                members: {
                    let open_brace_token = expect!(lexer, TokenKind::OpenBrace)?;
                    let mut members = vec![];
                    let close_brace_token = loop {
                        while consume!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }

                        members.push(Member {
                            name_token: expect!(lexer, TokenKind::Name(_))?,
                            colon_token: expect!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer, true)?,
                        });

                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }
                        expect!(lexer, TokenKind::Comma)?;
                    };

                    Members {
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        enum_token @ Token {
            location,
            kind: TokenKind::EnumKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Enum {
                enum_token,
                name_token: expect!(lexer, TokenKind::Name(_))?,

                members: {
                    let open_brace_token = expect!(lexer, TokenKind::OpenBrace)?;
                    let mut members = vec![];
                    let close_brace_token = loop {
                        while consume!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }

                        members.push(Member {
                            name_token: expect!(lexer, TokenKind::Name(_))?,
                            colon_token: expect!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer, true)?,
                        });

                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }
                        expect!(lexer, TokenKind::Comma)?;
                    };

                    Members {
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        type_token @ Token {
            location,
            kind: TokenKind::TypeKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Type {
                type_token,
                name_token: expect!(lexer, TokenKind::Name(_))?,
                typ: match consume!(lexer, TokenKind::Equals) {
                    Some(equals_token) => Some(Box::new(EqualsType {
                        equals_token,
                        typ: parse_expression(lexer, true)?,
                    })),
                    None => None,
                },
            },
        },

        fn_token @ Token {
            location,
            kind: TokenKind::FnKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Function {
                fn_token,
                name_token: expect!(lexer, TokenKind::Name(_))?,

                parameters: {
                    let open_parenthesis_token = expect!(lexer, TokenKind::OpenParenthesis)?;
                    let mut parameters = vec![];
                    let close_parenthesis_token = loop {
                        while consume!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_parenthesis_token) =
                            consume!(lexer, TokenKind::CloseParenthesis)
                        {
                            break close_parenthesis_token;
                        }

                        parameters.push(FunctionParameter {
                            name_token: expect!(lexer, TokenKind::Name(_))?,
                            colon_token: expect!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer, true)?,
                        });

                        if let Some(close_parenthesis_token) =
                            consume!(lexer, TokenKind::CloseParenthesis)
                        {
                            break close_parenthesis_token;
                        }
                        expect!(lexer, TokenKind::Comma)?;
                    };

                    FunctionParameters {
                        open_parenthesis_token,
                        parameters: parameters.into_boxed_slice(),
                        close_parenthesis_token,
                    }
                },

                return_type: match consume!(lexer, TokenKind::RightArrow) {
                    Some(right_arrow_token) => Some(Box::new(FunctionReturnType {
                        right_arrow_token,
                        typ: parse_expression(lexer, false)?,
                    })),
                    None => None,
                },

                body: if peek!(lexer, TokenKind::OpenBrace).is_some() {
                    Some(Box::new(parse_expression(lexer, true)?))
                } else {
                    None
                },
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            });
        }
    })
}

pub fn parse_statement(lexer: &mut Lexer) -> Result<Statement, ParsingError> {
    Ok(match lexer.peek_token()? {
        Token {
            location,
            kind:
                TokenKind::StructKeyword
                | TokenKind::EnumKeyword
                | TokenKind::TypeKeyword
                | TokenKind::FnKeyword
                | TokenKind::ConstKeyword
                | TokenKind::BuiltinDirective,
        } => Statement {
            location,
            kind: StatementKind::Item(Box::new(parse_item(lexer)?)),
        },

        Token {
            location: start_location,
            ..
        } => {
            let expression = Box::new(parse_expression(lexer, true)?);
            if let Some(equals_token) = consume!(lexer, TokenKind::Equals) {
                Statement {
                    location: equals_token.location,
                    kind: StatementKind::Assignment {
                        pattern: expression,
                        equals_token,
                        value: Box::new(parse_expression(lexer, true)?),
                    },
                }
            } else {
                Statement {
                    location: start_location,
                    kind: StatementKind::Expression(expression),
                }
            }
        }
    })
}

pub fn parse_expression(
    lexer: &mut Lexer,
    postfix_brace_allowed: bool,
) -> Result<Expression, ParsingError> {
    let mut expression = match lexer.next_token()? {
        open_brace_token @ Token {
            location,
            kind: TokenKind::OpenBrace,
        } => Expression {
            location,
            kind: {
                let mut statements = vec![];
                let close_brace_token = loop {
                    while consume!(lexer, TokenKind::Newline).is_some() {}
                    if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                        break close_brace_token;
                    }

                    statements.push(parse_statement(lexer)?);

                    if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                        break close_brace_token;
                    }
                    expect!(lexer, TokenKind::Newline)?;
                };

                ExpressionKind::Block {
                    open_brace_token,
                    statements: statements.into_boxed_slice(),
                    close_brace_token,
                }
            },
        },

        integer_token @ Token {
            location,
            kind: TokenKind::Integer(_),
        } => Expression {
            location,
            kind: ExpressionKind::Integer { integer_token },
        },

        name_token @ Token {
            location,
            kind: TokenKind::Name(_),
        } => Expression {
            location,
            kind: ExpressionKind::Path(Box::new(Path {
                location,
                name_token,
            })),
        },

        discard_token @ Token {
            location,
            kind: TokenKind::Discard,
        } => Expression {
            location,
            kind: ExpressionKind::Discard { discard_token },
        },

        open_parenthesis_token @ Token {
            location,
            kind: TokenKind::OpenParenthesis,
        } => Expression {
            location,
            kind: ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token,
                expression: Box::new(parse_expression(lexer, true)?),
                close_parenthesis_token: expect!(lexer, TokenKind::CloseParenthesis)?,
            },
        },

        let_token @ Token {
            location,
            kind: TokenKind::LetKeyword,
        } => Expression {
            location,
            kind: ExpressionKind::Let {
                let_token,
                name_token: expect!(lexer, TokenKind::Name(_))?,
                typ: if let Some(colon_token) = consume!(lexer, TokenKind::Colon) {
                    Some(Box::new(ColonType {
                        colon_token,
                        typ: parse_expression(lexer, postfix_brace_allowed)?,
                    }))
                } else {
                    None
                },
            },
        },

        match_token @ Token {
            location,
            kind: TokenKind::MatchKeyword,
        } => Expression {
            location,
            kind: ExpressionKind::Match {
                match_token,
                scruitnee: Box::new(parse_expression(lexer, false)?),
                body: {
                    let open_brace_token = expect!(lexer, TokenKind::OpenBrace)?;

                    let mut arms = vec![];
                    let close_brace_token = loop {
                        while consume!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }

                        arms.push(MatchArm {
                            pattern: parse_expression(lexer, true)?,
                            fat_right_arrow_token: expect!(lexer, TokenKind::FatRightArrow)?,
                            value: parse_expression(lexer, true)?,
                        });

                        if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }
                        expect!(lexer, TokenKind::Comma)?;
                    };

                    MatchBody {
                        open_brace_token,
                        arms: arms.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        if_token @ Token {
            location,
            kind: TokenKind::IfKeyword,
        } => Expression {
            location,
            kind: ExpressionKind::If {
                if_token,
                condition: Box::new(parse_expression(lexer, false)?),
                then_body: Box::new(parse_expression(lexer, true)?),
                else_body: if let Some(else_token) = consume!(lexer, TokenKind::ElseKeyword) {
                    Some(Box::new(Else {
                        else_token,
                        else_block: parse_expression(lexer, true)?,
                    }))
                } else {
                    None
                },
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            });
        }
    };

    loop {
        expression = match lexer.peek_token().ok() {
            Some(
                open_parenthesis_token @ Token {
                    location,
                    kind: TokenKind::OpenParenthesis,
                },
            ) => Expression {
                location,
                kind: ExpressionKind::Call {
                    operand: Box::new(expression),
                    arguments: {
                        lexer.next_token()?;

                        let mut arguments = vec![];
                        let close_parenthesis_token = loop {
                            while consume!(lexer, TokenKind::Newline).is_some() {}
                            if let Some(close_parenthesis_token) =
                                consume!(lexer, TokenKind::CloseParenthesis)
                            {
                                break close_parenthesis_token;
                            }

                            arguments.push(parse_expression(lexer, true)?);

                            if let Some(close_parenthesis_token) =
                                consume!(lexer, TokenKind::CloseParenthesis)
                            {
                                break close_parenthesis_token;
                            }
                            expect!(lexer, TokenKind::Comma)?;
                        };

                        CallArguments {
                            open_parenthesis_token,
                            arguments: arguments.into_boxed_slice(),
                            close_parenthesis_token,
                        }
                    },
                },
            },

            Some(
                open_brace_token @ Token {
                    location,
                    kind: TokenKind::OpenBrace,
                },
            ) if postfix_brace_allowed => Expression {
                location,
                kind: ExpressionKind::Constructor {
                    typ: Box::new(expression),
                    arguments: {
                        lexer.next_token()?;

                        let mut arguments = vec![];
                        let close_brace_token = loop {
                            while consume!(lexer, TokenKind::Newline).is_some() {}
                            if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace)
                            {
                                break close_brace_token;
                            }

                            arguments.push(ConstructorArgument {
                                name_token: expect!(lexer, TokenKind::Name(_))?,
                                colon_token: expect!(lexer, TokenKind::Colon)?,
                                value: parse_expression(lexer, true)?,
                            });

                            if let Some(close_brace_token) = consume!(lexer, TokenKind::CloseBrace)
                            {
                                break close_brace_token;
                            }
                            expect!(lexer, TokenKind::Comma)?;
                        };

                        ConstructorArguments {
                            open_brace_token,
                            arguments: arguments.into_boxed_slice(),
                            close_brace_token,
                        }
                    },
                },
            },

            Some(
                dot_token @ Token {
                    location,
                    kind: TokenKind::Dot,
                },
            ) => Expression {
                location,
                kind: ExpressionKind::MemberAccess {
                    operand: Box::new(expression),
                    dot_token,
                    name_token: {
                        lexer.next_token()?;

                        expect!(lexer, TokenKind::Name(_))?
                    },
                },
            },

            _ => break,
        };
    }

    Ok(expression)
}
