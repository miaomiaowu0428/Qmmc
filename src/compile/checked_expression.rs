use TokenType::{FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};

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
        value: ConstExpr,
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
    Identifier {
        name: Token,
    },
    VarDeclare {
        name: Token,
        _type: RawType,
        init_expr: Box<CheckedExpression>,
    },
    Assignment {
        identifier: Token,
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
}

// impl CheckedExpression {
//     pub fn position(&self) -> (usize, usize) {
//         match self {
//             CheckedExpression::Statement { expression } => expression.position(),
//             CheckedExpression::Literal { value } => value.position(),
//             CheckedExpression::Unary { op, operand } => op.position(),
//             CheckedExpression::Binary { op, left, right } => op.position(),
//             CheckedExpression::Block { expressions } => expressions[0].position(),
//             CheckedExpression::Identifier { identifier } => identifier.position(),
//             CheckedExpression::VarDeclare { name, init_expr } => name.position(),
//             CheckedExpression::Assignment { identifier, expression } => identifier.position(),
//             CheckedExpression::If { condition, then, r#else } => condition.position(),
//             CheckedExpression::Loop { body } => body.position(),
//             CheckedExpression::While { condition, body } => condition.position(),
//             CheckedExpression::Break => (0, 0),
//             CheckedExpression::Continue => (0, 0),
//             CheckedExpression::FunctionDeclaration { name, function } => name.position(),
//             CheckedExpression::Call { name, function, arguments } => name.position(),
//             CheckedExpression::CallBuiltIn { name, arguments } => name.position(),
//             CheckedExpression::Return { expression } => expression.position(),
//             CheckedExpression::FunType { _type } => _type.position(),
//         }
//     }
// }

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
}

impl From<Token> for ConstExpr {
    fn from(value: Token) -> Self {
        match value.token_type {
            IntegerToken => ConstExpr::I32(value.text.parse().unwrap()),
            FloatPointToken => ConstExpr::F32(value.text.parse().unwrap()),
            TrueKeyword => ConstExpr::Bool(true),
            FalseKeyword => ConstExpr::Bool(false),
            _ => panic!("Invalid literal token type"),
        }
    }
}
