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
        members: Members,
    },
    Enum {
        enum_token: Token,
        name_token: Token,
        members: Members,
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
pub struct Members {
    pub open_brace_token: Token,
    pub members: Box<[Member]>,
    pub close_brace_token: Token,
}

#[derive(Debug)]
pub struct Member {
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
pub struct ColonType {
    pub colon_token: Token,
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
    Assignment {
        pattern: Box<Expression>,
        equals_token: Token,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub label: Option<Box<Label>>,
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub struct Label {
    pub lifetime_token: Token,
    pub colon_token: Token,
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
    Let {
        let_token: Token,
        name_token: Token,
        typ: Option<Box<ColonType>>,
    },
    Match {
        match_token: Token,
        scruitnee: Box<Expression>,
        body: MatchBody,
    },
    If {
        if_token: Token,
        condition: Box<Expression>,
        then_body: Box<Expression>,
        else_body: Option<Box<Else>>,
    },
    Break {
        break_token: Token,
        lifetime_token: Token,
        value: Option<Box<Expression>>,
    },
    Continue {
        continue_token: Token,
        lifetime_token: Token,
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

#[derive(Debug)]
pub struct MatchBody {
    pub open_brace_token: Token,
    pub arms: Box<[MatchArm]>,
    pub close_brace_token: Token,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Expression,
    pub fat_right_arrow_token: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Else {
    pub else_token: Token,
    pub else_block: Expression,
}
