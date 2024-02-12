#![allow(dead_code)]

use crate::analyze::lex::token::Token;
use Expression::{AssignmentExpression, BinaryExpression, BracketedExpression, IdentifierExpression, LiteralExpression, ParenthesizedExpression, Statement, UnaryExpression};
use TokenType::{FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};
use crate::analyze::lex::TokenType;
use crate::analyze::syntax_tree::block::Block;
use crate::analyze::syntax_tree::Expression::DeclarationExpression;
use crate::evaluate::Type;

pub enum Expression {
    Statement {
        expression: Box<Expression>,
        semicolon: Token,
    },
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
        block: Block,
        right_b: Token,
    },
    // ()
    ParenthesizedExpression {
        left_p: Token,
        expression: Box<Expression>,
        right_p: Token,
    },
    DeclarationExpression {
        declaration_token: Token,
        identifier_token: Token,
        equals_token: Token,
        expression: Box<Expression>,
        semicolon_token: Token,
    },
    AssignmentExpression {
        identifier_token: Token,
        equals_token: Token,
        expression: Box<Expression>,
        semicolon_token: Token,
    },
}

impl Expression {
    pub fn r#type(&self) -> Type {
        match self {
            Statement { .. } => {
                Type::None
            }
            LiteralExpression { literal_token } => match literal_token.token_type {
                IntegerToken => Type::I32,
                FloatPointToken => Type::F32,
                TrueKeyword | FalseKeyword => Type::Bool,
                _ => Type::None,
            },
            IdentifierExpression { identifier_token } => {
                Type::Unknown
            }
            UnaryExpression { operand, .. } => {
                operand.r#type()
            }
            BinaryExpression { .. } => {
                Type::Unknown
            }
            BracketedExpression { block, .. } => {
                block.expressions.borrow().last().unwrap().r#type()
            }
            ParenthesizedExpression { expression, .. } => {
                expression.r#type()
            }
            DeclarationExpression { expression, .. } => {
                Type::None
            }
            AssignmentExpression { expression, .. } => {
                Type::None
            }
        }
    }


    pub fn print(&self, indent: i32) {
        let mut indent_str = "".to_string();
        for _ in 0..=indent {
            indent_str.push_str("\t");
        }

        match self {
            Statement { expression, semicolon } => {
                expression.print(indent);
                println!("{}{}", indent_str, semicolon);
            }
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
                block: expression,
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
            DeclarationExpression {
                declaration_token,
                identifier_token,
                equals_token,
                expression,
                semicolon_token,
            } => {
                println!("{}{} {} {}", indent_str, declaration_token, identifier_token, equals_token);
                expression.print(indent + 1);
                println!("{}{}", indent_str, semicolon_token);
            }
            AssignmentExpression {
                identifier_token,
                equals_token,
                expression,
                semicolon_token
            } => {
                println!("{}{} {}", indent_str, identifier_token, equals_token);
                expression.print(indent + 1);
                println!("{}{}", indent_str, semicolon_token);
            }
        }
    }
}
