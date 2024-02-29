use TokenType::{FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};

use crate::analyze::lex::{Token, TokenType};
use crate::compile::binary_operator::BinaryOperator;
use crate::compile::r#type::{FunctionType, RawType};
use crate::compile::unary_operator::UnaryOperator;

#[derive(Debug, Clone)]
pub enum ByteCode {
    Statement {
        expression: Box<ByteCode>,
    },
    Literal {
        value: ConstExpr,
    },
    Unary {
        op: UnaryOperator,
        operand: Box<ByteCode>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<ByteCode>,
        right: Box<ByteCode>,
    },
    Block {
        expressions: Vec<ByteCode>,
    },
    Identifier {
        identifier: Token,
    },
    VarDeclare {
        identifier: Token,
        expression: Box<ByteCode>,
    },
    Assignment {
        identifier: Token,
        expression: Box<ByteCode>,
    },
    If {
        condition: Box<ByteCode>,
        then: Box<ByteCode>,
        r#else: Option<Box<ByteCode>>,
    },
    Loop {
        body: Box<ByteCode>,
    },
    While {
        condition: Box<ByteCode>,
        body: Box<ByteCode>,
    },
    Break,
    Continue,
    FunctionDeclaration {
        name: Token,
        function: FunctionObj,
    },
    Call {
        name: Token,
        function: FunctionType,
        arguments: Vec<ByteCode>,
    },
    CallBuiltIn {
        name: Token,
        arguments: Vec<ByteCode>,
    },
    Return {
        expression: Box<ByteCode>,
    },
    FunType {
        _type: FunctionType,
    },

}


#[derive(Debug, Clone)]
pub struct Parameter {
    pub(crate) name: Token,
    pub(crate) r#type: RawType,
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.name.text == other.name.text && self.r#type == other.r#type
    }
}


#[derive(Debug, Clone)]
pub enum ConstExpr {
    None,
    I32(i32),
    Bool(bool),
    F32(f32),
    Function { fun: FunctionObj },
}

impl From<Token> for ConstExpr {
    fn from(value: Token) -> Self {
        match value.token_type {
            IntegerToken => ConstExpr::I32(value.text.parse().unwrap()),
            FloatPointToken => ConstExpr::F32(value.text.parse().unwrap()),
            TrueKeyword => ConstExpr::Bool(true),
            FalseKeyword => ConstExpr::Bool(false),
            _ => panic!("Invalid literal token type")
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionObj {
    pub _type: FunctionType,
    pub param_names: Vec<String>,
    pub body: Box<ByteCode>,
}

