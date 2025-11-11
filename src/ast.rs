use crate::{ids::Id, interning::InternedStr, lexing::SourceLocation};

#[derive(Debug)]
pub struct Item {
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Struct {
        builtin: Option<BuiltinStruct>,
        name: InternedStr,
        members: Box<[Member]>,
    },
    Enum {
        builtin: Option<BuiltinEnum>,
        name: InternedStr,
        members: Box<[Member]>,
    },
    Type {
        name: InternedStr,
        typ: Box<Type>,
    },
    Function {
        name: InternedStr,
        parameters: Box<[FunctionParameter]>,
        return_type: Box<Type>,
        body: FunctionBody,
    },
}

#[derive(Debug)]
pub enum BuiltinStruct {
    Unit,
}

#[derive(Debug)]
pub enum BuiltinEnum {
    Never,
    Bool,
}

#[derive(Debug)]
pub struct Member {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Type,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Type,
}

#[derive(Debug)]
pub enum FunctionBody {
    Expression(Box<Expression>),
    Builtin(BuiltinFunction),
}

#[derive(Debug)]
pub enum BuiltinFunction {
    PrintI32,
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
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Place(Place),
    Constant(Constant),
    Block {
        label: Id<Label>,
        label_name: Option<InternedStr>,
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Constructor {
        typ: Box<Type>,
        arguments: Box<[ConstructorArgument]>,
    },
    Match {
        scruitnee: Box<Expression>,
        arms: Box<[MatchArm]>,
    },
    Break {
        label: Label,
        value: Box<Expression>,
    },
    Continue {
        label: Label,
    },
}

#[derive(Debug)]
pub enum Place {
    Path(Box<Path>),
    MemberAccess {
        operand: Box<Expression>,
        member_name: InternedStr,
    },
}

#[derive(Debug)]
pub enum Constant {
    Integer(u128),
}

#[derive(Debug)]
pub enum Label {
    Id(Id<Label>),
    Name(InternedStr),
}

#[derive(Debug)]
pub struct ConstructorArgument {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub value: Expression,
}

#[derive(Debug)]
pub struct MatchArm {
    pub location: SourceLocation,
    pub pattern: Pattern,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Infer,
    Path(Box<Path>),
    Opaque { name: InternedStr },
    Builtin(BuiltinType),
}

#[derive(Debug)]
pub enum BuiltinType {
    Unit,
    Never,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    USize,
    ISize,
    Runtime,
    Bool,
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Discard,
    Place(Place),
    Constant(Constant),
    Deconstructor {
        typ: Box<Type>,
        arguments: Box<[DeconstructorArgument]>,
    },
    Let {
        name: InternedStr,
        typ: Box<Type>,
    },
}

#[derive(Debug)]
pub struct DeconstructorArgument {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub struct Path {
    pub location: SourceLocation,
    pub name: InternedStr,
}
