#![allow(non_camel_case_types)]

use std::fmt::{Display, Formatter};

use Type::Unknown;

use crate::evaluate::Type::Bool;
use crate::evaluate::Type::F32;
use crate::evaluate::Type::I32;

pub enum Type {
    Unknown,
    None,
    I32,
    Bool,
    F32,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Unknown => "Unknown",
            Type::None => "None",
            I32 => "I32",
            Bool => "Bool",
            F32 => "F32",
        };
        write!(f, "{}", string)
    }
}