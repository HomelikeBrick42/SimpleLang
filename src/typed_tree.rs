use crate::{
    ids::{Id, IdMap},
    interning::InternedStr,
    lexing::SourceLocation,
};

#[derive(Debug)]
pub struct Function {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Id<Type>,
    pub parameter_types: Box<[Id<Type>]>,
    pub return_type: Id<Type>,
}

#[derive(Debug)]
pub enum FunctionBody {
    Builtin(BuiltinFunction),
    Body {
        variables: IdMap<Variable>,
        parameter_variables: Box<[Id<Variable>]>,
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
        label: Id<Label>,
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
    EnumConstructor {
        arguments: Box<EnumConstructorArgument>,
    },
    Match {
        scruitnee: Box<Expression>,
        arms: Box<[MatchArm]>,
    },
    Break {
        label: Id<Label>,
        value: Box<Expression>,
    },
    Continue {
        label: Id<Label>,
    },
}

#[derive(Debug)]
pub enum Place {
    Function(Id<Function>),
    Variable(Id<Variable>),
    StructMemberAccess {
        operand: Box<Expression>,
        member_index: usize,
    },
    EnumMemberAccess {
        operand: Box<Expression>,
        variant_index: usize,
    },
}

#[derive(Debug)]
pub enum Constant {
    I32(i32),
}

#[derive(Debug)]
pub struct StructConstructorArgument {
    pub location: SourceLocation,
    pub member_index: usize,
    pub value: Expression,
}

#[derive(Debug)]
pub struct EnumConstructorArgument {
    pub location: SourceLocation,
    pub variant_index: usize,
    pub value: Expression,
}

#[derive(Debug)]
pub struct MatchArm {
    pub location: SourceLocation,
    pub pattern: Pattern,
    pub value: Expression,
}

pub struct Label;

#[derive(Debug)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Expression(Box<Expression>),
    Assignment {
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub typ: Id<Type>,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Place(Place),
    Constant(Constant),
    StructDeconstructor {
        arguments: Box<[StructDeconstructorArgument]>,
    },
    EnumDeconstructor {
        arguments: Box<EnumDeconstructorArgument>,
    },
    Let {
        variable: Id<Variable>,
    },
}

#[derive(Debug)]
pub struct StructDeconstructorArgument {
    pub location: SourceLocation,
    pub member_index: usize,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub struct EnumDeconstructorArgument {
    pub location: SourceLocation,
    pub variant_index: usize,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Struct {
        name: InternedStr,
        members: Box<[Member]>,
    },
    Enum {
        name: InternedStr,
        members: Box<[Member]>,
    },
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
