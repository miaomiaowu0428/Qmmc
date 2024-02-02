#![allow(non_camel_case_types)]

use std::fmt::{Display, Formatter};


pub enum Type {
    None,
    I32,
    Bool,
    F32,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Type::None => "None",
            Type::I32 => "I32",
            Type::Bool => "Bool",
            Type::F32 => "F32",
        };
        write!(f, "{}", string)
    }
}