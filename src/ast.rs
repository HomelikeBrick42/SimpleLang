use crate::{interning::InternedStr, lexing::SourceLocation};

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
        members: Box<[StructMember]>,
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
pub struct StructMember {
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
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Path(Box<Path>),
    Integer(u64),
    Block {
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
}

#[derive(Debug)]
pub struct ConstructorArgument {
    pub location: SourceLocation,
    pub name: InternedStr,
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
    Builtin(BuiltinType),
}

#[derive(Debug)]
pub enum BuiltinType {
    Unit,
    I32,
    Runtime,
}

#[derive(Debug)]
pub struct Path {
    pub location: SourceLocation,
    pub name: InternedStr,
}
