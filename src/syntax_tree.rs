use crate::lexing::{SourceLocation, Token};

#[derive(Debug)]
pub struct Attribute {
    pub location: SourceLocation,
    pub kind: AttributeKind,
}

#[derive(Debug)]
pub enum AttributeKind {
    Builtin {
        builtin_token: Token,
        open_parenthesis_token: Token,
        string_token: Token,
        close_parenthesis_token: Token,
    },
}

#[derive(Debug)]
pub struct Item {
    pub attributes: Box<[Attribute]>,
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Struct {
        struct_token: Token,
        name_token: Token,
        members: StructMembers,
    },
    Type {
        type_token: Token,
        name_token: Token,
        typ: Option<Box<EqualsType>>,
    },
    Function {
        fn_token: Token,
        name_token: Token,
        parameters: FunctionParameters,
        return_type: Option<Box<FunctionReturnType>>,
        body: Option<Box<Expression>>,
    },
}

#[derive(Debug)]
pub struct StructMembers {
    pub open_brace_token: Token,
    pub members: Box<[StructMember]>,
    pub close_brace_token: Token,
}

#[derive(Debug)]
pub struct StructMember {
    pub name_token: Token,
    pub colon_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct EqualsType {
    pub equals_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct FunctionParameters {
    pub open_parenthesis_token: Token,
    pub parameters: Box<[FunctionParameter]>,
    pub close_parenthesis_token: Token,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub name_token: Token,
    pub colon_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct FunctionReturnType {
    pub right_arrow_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Item(Box<Item>),
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Block {
        open_brace_token: Token,
        statements: Box<[Statement]>,
        close_brace_token: Token,
    },
    Integer {
        integer_token: Token,
    },
    Path(Box<Path>),
    Discard {
        discard_token: Token,
    },
    ParenthesizedExpression {
        open_parenthesis_token: Token,
        expression: Box<Expression>,
        close_parenthesis_token: Token,
    },
    Call {
        operand: Box<Expression>,
        arguments: CallArguments,
    },
    Constructor {
        typ: Box<Expression>,
        arguments: ConstructorArguments,
    },
    MemberAccess {
        operand: Box<Expression>,
        dot_token: Token,
        name_token: Token,
    },
}

#[derive(Debug)]
pub struct Path {
    pub location: SourceLocation,
    pub name_token: Token,
}

#[derive(Debug)]
pub struct CallArguments {
    pub open_parenthesis_token: Token,
    pub arguments: Box<[Expression]>,
    pub close_parenthesis_token: Token,
}

#[derive(Debug)]
pub struct ConstructorArguments {
    pub open_brace_token: Token,
    pub arguments: Box<[ConstructorArgument]>,
    pub close_brace_token: Token,
}

#[derive(Debug)]
pub struct ConstructorArgument {
    pub name_token: Token,
    pub colon_token: Token,
    pub value: Expression,
}
