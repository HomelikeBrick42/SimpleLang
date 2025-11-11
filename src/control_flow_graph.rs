use crate::{
    ids::{Id, IdMap},
    interning::InternedStr,
    typed_tree as tt,
};
use std::{num::NonZeroUsize, sync::Arc};

#[derive(Debug)]
pub enum Byte {
    Uninit,
    Init(u8),
}

#[derive(Debug)]
pub struct Allocation {
    pub align: NonZeroUsize,
    pub bytes: Box<[Byte]>,
}

#[derive(Debug)]
pub struct Function {
    pub variables: IdMap<Variable>,
    pub parameters: Box<[Id<Variable>]>,
    pub return_value: Id<Variable>,
    pub blocks: IdMap<Block>,
    pub start_block: Id<Block>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Option<InternedStr>,
    pub typ: Id<tt::Type>,
}

#[derive(Debug)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub jump: Jump,
}

#[derive(Debug)]
pub struct CopyOperand {
    pub variable: Id<Variable>,
    pub offset: usize,
    pub size: usize,
}

#[derive(Debug)]
pub enum Instruction {
    Const {
        destination: Id<Variable>,
        bytes: Arc<[Byte]>,
    },
    Copy {
        source: CopyOperand,
        destination: CopyOperand,
    },
    PrintI32 {
        variable: Id<Variable>,
    },
}

#[derive(Debug)]
pub enum Jump {
    Unreachable,
    Return,
    Block(Id<Block>),
    Match {
        scruitnee: Id<Variable>,
        arms: MatchArms,
    },
    Call {
        function: Id<Function>,
        arguments: Box<[Id<Variable>]>,
        return_to: Id<Block>,
    },
}

#[derive(Debug)]
pub enum MatchArms {
    U8 {
        arms: Box<[MatchArm<u8>]>,
        otherwise: Id<Block>,
    },
    U16 {
        arms: Box<[MatchArm<u16>]>,
        otherwise: Id<Block>,
    },
    U32 {
        arms: Box<[MatchArm<u32>]>,
        otherwise: Id<Block>,
    },
    U64 {
        arms: Box<[MatchArm<u64>]>,
        otherwise: Id<Block>,
    },
}

#[derive(Debug)]
pub struct MatchArm<T> {
    pub value: T,
    pub jump: Id<Block>,
}
