use colored::Colorize;
use TokenType::{
    CharToken, FalseKeyword, FloatPointToken, IntegerToken, LiteralStringToken, TrueKeyword,
};

use crate::analyze::lex::{Token, TokenType};
use crate::compile::binary_operator::BinaryOperator;
use crate::compile::r#type::{FunctionDeclare, FunctionType, RawType};
use crate::compile::unary_operator::UnaryOperator;

#[derive(Debug, Clone)]
pub enum CheckedExpression {
    Statement {
        expression: Box<CheckedExpression>,
    },
    Literal {
        value: LiteralExpr,
    },
    Unary {
        op: UnaryOperator,
        operand: Box<CheckedExpression>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<CheckedExpression>,
        right: Box<CheckedExpression>,
    },
    Block {
        expressions: Vec<CheckedExpression>,
    },
    VariableName {
        name: Token,
    },
    Type {
        _type: RawType,
    },
    VarDeclare {
        name: Token,
        _type: RawType,
        init_expr: Box<CheckedExpression>,
    },
    Assignment {
        aim_expr: Box<CheckedExpression>,
        expression: Box<CheckedExpression>,
    },
    Conditional {
        condition: Box<CheckedExpression>,
        then: Box<CheckedExpression>,
        else_ifs: Vec<CheckedExpression>,
        else_expr: Option<Box<CheckedExpression>>,
        _type: RawType,
    },
    If {
        condition: Box<CheckedExpression>,
        body: Box<CheckedExpression>,
        r#else: Option<Box<CheckedExpression>>,
    },
    ElseIf {
        condition: Box<CheckedExpression>,
        body: Box<CheckedExpression>,
    },
    Else {
        body: Box<CheckedExpression>,
    },
    Loop {
        body: Box<CheckedExpression>,
    },
    While {
        condition: Box<CheckedExpression>,
        body: Box<CheckedExpression>,
    },
    Break,
    Continue,
    FunctionDeclaration {
        name: Token,
        function: Box<FunctionDeclare>,
    },
    Call {
        name: Token,
        function: FunctionType,
        arguments: Vec<CheckedExpression>,
    },
    CallBuiltIn {
        name: Token,
        arguments: Vec<CheckedExpression>,
    },
    Return {
        expression: Box<CheckedExpression>,
    },
    FunType {
        _type: FunctionType,
    },
    EmptyExpr
}

impl CheckedExpression {
    pub fn unwrap(self) -> Vec<CheckedExpression> {
        match self {
            CheckedExpression::Block { expressions } => expressions,
            _ => {
                vec![self]
            }
        }
    }
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
pub enum LiteralExpr {
    None,
    I32(i32),
    Bool(bool),
    F32(f32),
    Byte(char),
    Str(String),
}

impl From<Token> for LiteralExpr {
    fn from(value: Token) -> Self {
        match value.token_type {
            IntegerToken => LiteralExpr::I32(value.text.parse().unwrap()),
            FloatPointToken => LiteralExpr::F32(value.text.parse().unwrap()),
            TrueKeyword => LiteralExpr::Bool(true),
            FalseKeyword => LiteralExpr::Bool(false),
            CharToken => LiteralExpr::Byte(value.text.chars().next().unwrap()),
            LiteralStringToken => LiteralExpr::Str(value.text),
            _ => {
                println!(
                    "Invalid literal token type: {}",
                    format!("{:?}", value.token_type).red()
                );
                LiteralExpr::None
            }
        }
    }
}
