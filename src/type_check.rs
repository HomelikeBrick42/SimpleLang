use crate::{
    ids::{IdMap, IdSecondaryMap},
    inferring::InferResult,
    lexing::SourceLocation,
    typed_tree as tt,
};

#[derive(Debug, Clone)]
pub struct TypeCheckError {
    pub location: SourceLocation,
    pub kind: TypeCheckErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeCheckErrorKind {}

pub struct TypeCheckResult {
    pub types: IdMap<tt::Type>,
    pub functions: IdMap<tt::Function>,
    pub function_bodies: IdSecondaryMap<tt::Function, tt::FunctionBody>,
    pub errors: Vec<TypeCheckError>,
}

pub fn type_check(infer_result: &InferResult) -> TypeCheckResult {
    let types = IdMap::new();
    let functions = IdMap::new();
    let function_bodies = IdSecondaryMap::new();
    let errors = vec![];

    _ = infer_result;

    TypeCheckResult {
        types,
        functions,
        function_bodies,
        errors,
    }
}
