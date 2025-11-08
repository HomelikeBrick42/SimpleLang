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
    Struct { members: Box<[Member]> },
    Enum { members: Box<[Member]> },
    FunctionItem(Id<Function>),
    I32,
    Runtime,
}

#[derive(Debug)]
pub struct Member {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Id<Type>,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expression>),
    Assignment {
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub typ: Id<Type>,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Place(Place),
    Constant(Constant),
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Constructor {
        arguments: Box<[ConstructorArgument]>,
    },
    Match {
        scruitnee: Box<Expression>,
        arms: Box<[MatchArm]>,
    },
}

#[derive(Debug)]
pub enum Place {
    Variable(Id<Variable>),
    Function(Id<Function>),
    StructMemberAccess {
        operand: Box<Expression>,
        member_index: usize,
    },
}

#[derive(Debug)]
pub enum Constant {
    Integer(u64),
}

#[derive(Debug)]
pub struct ConstructorArgument {
    pub member_index: usize,
    pub value: Expression,
}

#[derive(Debug)]
pub struct MatchArm {
    pub location: SourceLocation,
    pub pattern: Pattern,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub typ: Id<Type>,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Discard,
    Place(Place),
    Constant(Constant),
    Deconstructor {
        arguments: Box<[DeconstructorArgument]>,
    },
    Let {
        variable: Id<Variable>,
    },
}

#[derive(Debug)]
pub struct DeconstructorArgument {
    pub member_index: usize,
    pub pattern: Pattern,
}
