use crate::{
    ids::{Id, IdMap},
    interning::InternedStr,
    lexing::SourceLocation,
};

#[derive(Debug)]
pub struct Function {
    pub location: SourceLocation,
    pub parameter_types: Box<[Id<Type>]>,
    pub return_type: Id<Type>,
    pub typ: Id<Type>,
}

#[derive(Debug)]
pub enum FunctionBody {
    Builtin(BuiltinFunction),
    Body {
        variables: IdMap<Variable>,
        parameters: Box<[Id<Variable>]>,
        expression: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum BuiltinFunction {
    PrintI32,
}

#[derive(Debug)]
pub struct Variable {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub typ: Id<Type>,
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Struct { members: Box<[StructMember]> },
    FunctionItem(Id<Function>),
    I32,
    Runtime,
}

#[derive(Debug)]
pub struct StructMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Id<Type>,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub typ: Id<Type>,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Variable(Id<Variable>),
    Function(Id<Function>),
    Integer(u64),
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    StructConstructor {
        arguments: Box<[StructConstructorArgument]>,
    },
    StructMemberAccess {
        operand: Box<Expression>,
        member_index: usize,
    },
}

#[derive(Debug)]
pub struct StructConstructorArgument {
    pub member_index: usize,
    pub value: Expression,
}
