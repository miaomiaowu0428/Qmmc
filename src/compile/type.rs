#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
    None,
    I32,
    Bool,
    F32,
    FunctionType { _type: FunctionType },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<RawType>,
    pub return_type: Box<RawType>,
}

