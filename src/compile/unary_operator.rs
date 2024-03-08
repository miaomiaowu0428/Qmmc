use lazy_static::lazy_static;

use crate::analyze::lex::TokenType;
use crate::compile::r#type::RawType;

lazy_static! {
    static ref UNARY_OPERATORS: Vec<UnaryOperator> = vec![
        UnaryOperator {
            operator_type: UnaryOperatorType::Positive,
            operand_type: RawType::I32,
            res_type: RawType::I32,
        },
        UnaryOperator {
            operator_type: UnaryOperatorType::Negation,
            operand_type: RawType::I32,
            res_type: RawType::I32,
        },
        UnaryOperator {
            operator_type: UnaryOperatorType::LogicalNegation,
            operand_type: RawType::Bool,
            res_type: RawType::Bool,
        },
    ];
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub operator_type: UnaryOperatorType,
    pub operand_type: RawType,
    pub res_type: RawType,
}

impl UnaryOperator {
    pub(crate) fn check(op: TokenType, operand_type: &RawType) -> Option<Self> {
        let opt = UnaryOperatorType::from(op);
        UNARY_OPERATORS
            .iter()
            .find(|o| o.operator_type == opt && o.operand_type == *operand_type)
            .map(|o| o.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperatorType {
    Positive,
    Negation,
    LogicalNegation,
}

impl From<TokenType> for UnaryOperatorType {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::PlusToken => UnaryOperatorType::Positive,
            TokenType::MinusToken => UnaryOperatorType::Negation,
            TokenType::BangToken => UnaryOperatorType::LogicalNegation,
            _ => panic!("Invalid token type for unary operator"),
        }
    }
}
