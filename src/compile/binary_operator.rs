use lazy_static::lazy_static;

use TokenType::{AndKeyword, BangEqualsToken, GreatThanToken, OrKeyword};
use TokenType::EqualsEqualsToken;
use TokenType::LessThanToken;
use TokenType::MinusToken;
use TokenType::PercentToken;
use TokenType::PlusToken;
use TokenType::SlashToken;
use TokenType::StarToken;

use crate::analyze::lex::TokenType;
use crate::compile::r#type::RawType;

lazy_static! {
    static ref BINARY_OPERATORS: Vec<BinaryOperator> = vec![
        BinaryOperator {
            operator_type: BinaryOperatorType::Addition,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::I32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Subtraction,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::I32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Multiplication,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::I32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Division,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::I32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Remainder,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::I32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Equals,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::NotEquals,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LogicalAnd,
            left_type: RawType::Bool,
            right_type: RawType::Bool,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LogicalOr,
            left_type: RawType::Bool,
            right_type: RawType::Bool,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::GreaterThan,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LessThan,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::GreaterThanOrEqual,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LessThanOrEqual,
            left_type: RawType::I32,
            right_type: RawType::I32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Addition,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::F32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Subtraction,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::F32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Multiplication,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::F32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Division,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::F32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Remainder,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::F32,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::Equals,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::NotEquals,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LogicalAnd,
            left_type: RawType::Bool,
            right_type: RawType::Bool,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LogicalOr,
            left_type: RawType::Bool,
            right_type: RawType::Bool,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::GreaterThan,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LessThan,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::GreaterThanOrEqual,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
        BinaryOperator {
            operator_type: BinaryOperatorType::LessThanOrEqual,
            left_type: RawType::F32,
            right_type: RawType::F32,
            res_type: RawType::Bool,
        },
    ];
}


#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub operator_type: BinaryOperatorType,
    pub left_type: RawType,
    pub right_type: RawType,
    pub res_type: RawType,
}

impl BinaryOperator {
    pub fn check(op: TokenType, left_type: &RawType, right_type: &RawType) -> Option<Self> {
        let opt = BinaryOperatorType::from(op);
        BINARY_OPERATORS
            .iter()
            .find(|op| op.operator_type == opt && op.left_type == *left_type && op.right_type == *right_type)
            .map(|op| op.clone())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperatorType {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Equals,
    NotEquals,
    LogicalAnd,
    LogicalOr,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

impl From<TokenType> for BinaryOperatorType {
    fn from(value: TokenType) -> Self {
        match value {
            PlusToken => BinaryOperatorType::Addition,
            MinusToken => BinaryOperatorType::Subtraction,
            StarToken => BinaryOperatorType::Multiplication,
            SlashToken => BinaryOperatorType::Division,
            PercentToken => BinaryOperatorType::Remainder,
            EqualsEqualsToken => BinaryOperatorType::Equals,
            BangEqualsToken => BinaryOperatorType::NotEquals,
            AndKeyword => BinaryOperatorType::LogicalAnd,
            OrKeyword => BinaryOperatorType::LogicalOr,
            GreatThanToken => BinaryOperatorType::GreaterThan,
            LessThanToken => BinaryOperatorType::LessThan,
            _ => panic!("Invalid binary operator type"),
        }
    }
}