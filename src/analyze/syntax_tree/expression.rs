#![allow(dead_code)]

use crate::analyze::lex::token::Token;
use Expression::{
    BinaryExpression, BracketedExpression, IdentifierExpression, LiteralExpression,
    ParenthesizedExpression, UnaryExpression,
};

pub enum Expression {
    LiteralExpression {
        literal_token: Token,
    },
    IdentifierExpression {
        identifier_token: Token,
    },
    UnaryExpression {
        operator_token: Token,
        operand: Box<Expression>,
    },
    BinaryExpression {
        left: Box<Expression>,
        operator_token: Token,
        right: Box<Expression>,
    },
    // {}
    BracketedExpression {
        left_b: Token,
        expression: Box<Expression>,
        right_b: Token,
    },
    // ()
    ParenthesizedExpression {
        left_p: Token,
        expression: Box<Expression>,
        right_p: Token,
    },
}

impl Expression {
    pub fn print(&self, indent: i32) {
        let mut indent_str = "".to_string();
        for _ in 0..=indent {
            indent_str.push_str("\t");
        }

        match self {
            LiteralExpression { literal_token } => {
                println!("{}{}", indent_str, literal_token)
            }
            IdentifierExpression { identifier_token } => {
                println!("{}{}", indent_str, identifier_token)
            }
            UnaryExpression {
                operator_token,
                operand,
            } => {
                println!("{}{}", indent_str, operator_token);
                operand.print(indent + 1);
            }
            BinaryExpression {
                left,
                operator_token,
                right,
            } => {
                println!("{}{}", indent_str, operator_token);
                left.print(indent + 1);
                right.print(indent + 1);
            }
            BracketedExpression {
                left_b,
                expression,
                right_b,
            } => {
                println!("{}{}", indent_str, left_b);
                expression.print(indent + 1);
                println!("{}{}", indent_str, right_b);
            }
            ParenthesizedExpression {
                left_p,
                expression,
                right_p,
            } => {
                println!("{}{}", indent_str, left_p);
                expression.print(indent + 1);
                println!("{}{}", indent_str, right_p);
            }
        }
    }
}
