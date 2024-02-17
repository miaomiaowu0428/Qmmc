#![allow(dead_code)]

use Expression::AssignmentExpression;
use Expression::BinaryExpression;
use Expression::BracketedExpression;
use Expression::IdentifierExpression;
use Expression::LiteralExpression;
use Expression::ParenthesizedExpression;
use Expression::Statement;
use Expression::UnaryExpression;
use TokenType::FalseKeyword;
use TokenType::FloatPointToken;
use TokenType::IntegerToken;
use TokenType::TrueKeyword;
use Type::Unknown;

use crate::analyze::lex::token::Token;
use crate::analyze::lex::TokenType;
use crate::analyze::syntax_tree::block::Block;
use crate::analyze::syntax_tree::Expression::{BreakExpression, DeclarationExpression, LoopExpression};
use crate::analyze::syntax_tree::Expression::IfExpression;
use crate::analyze::syntax_tree::Expression::WhileExpression;
use crate::evaluate::Type;

#[derive(Debug, Clone)]
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
    },
    AssignmentExpression {
        identifier_token: Token,
        equals_token: Token,
        expression: Box<Expression>,
    },
    IfExpression {
        if_token: Token,
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        else_token: Option<Token>,
        false_expr: Option<Box<Expression>>,
    },
    WhileExpression {
        while_token: Token,
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    LoopExpression {
        loop_token: Token,
        body: Box<Expression>,
    },
    BreakExpression {
        break_token: Token,
    },
}

impl Expression {
    pub fn r#type(&self) -> Type {
        match self {
            Statement { .. } => Type::None,
            LiteralExpression { literal_token } => match literal_token.token_type {
                IntegerToken => Type::I32,
                FloatPointToken => Type::F32,
                TrueKeyword | FalseKeyword => Type::Bool,
                _ => Type::None,
            },
            IdentifierExpression { identifier_token } => Unknown,
            UnaryExpression { operand, .. } => operand.r#type(),
            BinaryExpression { .. } => Unknown,
            BracketedExpression { block, .. } => block.expressions.borrow().last().unwrap().r#type(),
            ParenthesizedExpression { expression, .. } => expression.r#type(),
            DeclarationExpression { expression, .. } => Type::None,
            AssignmentExpression { expression, .. } => Type::None,
            IfExpression { true_expr: then_block, .. } => then_block.r#type(),
            WhileExpression { body: block, .. } => Type::None,
            LoopExpression { .. } => Type::None,
            BreakExpression { .. } => Type::None,
        }
    }


    pub fn to_token_vec(&self) -> Vec<Token> {
        let mut res = Vec::new();
        match self {
            Statement { expression, semicolon } => {
                res.append(&mut expression.to_token_vec());
                res.push(semicolon.clone());
            }
            LiteralExpression { literal_token } => {
                res.push(literal_token.clone());
            }
            IdentifierExpression { identifier_token } => {
                res.push(identifier_token.clone());
            }
            UnaryExpression { operator_token, operand, } => {
                res.push(operator_token.clone());
                res.append(&mut operand.to_token_vec());
            }
            BinaryExpression { left, operator_token, right, } => {
                res.append(&mut left.to_token_vec());
                res.push(operator_token.clone());
                res.append(&mut right.to_token_vec());
            }
            BracketedExpression { left_b, block: expression, right_b, } => {
                res.push(left_b.clone());
                res.append(&mut expression.to_token_vec());
                res.push(right_b.clone());
            }
            ParenthesizedExpression { left_p, expression, right_p, } => {
                res.push(left_p.clone());
                res.append(&mut expression.to_token_vec());
                res.push(right_p.clone());
            }
            DeclarationExpression { declaration_token, identifier_token, equals_token, expression, } => {
                res.push(declaration_token.clone());
                res.push(identifier_token.clone());
                res.push(equals_token.clone());
                res.append(&mut expression.to_token_vec());
            }
            AssignmentExpression { identifier_token, equals_token, expression, } => {
                res.push(identifier_token.clone());
                res.push(equals_token.clone());
                res.append(&mut expression.to_token_vec());
            }
            IfExpression { if_token, condition, true_expr: then_block, else_token, false_expr: else_block, } => {
                res.push(if_token.clone());
                res.append(&mut condition.to_token_vec());
                res.append(&mut then_block.to_token_vec());
                match else_token {
                    Some(token) => {
                        res.push(token.clone());
                        res.append(&mut else_block.as_ref().unwrap().to_token_vec());
                    }
                    None => {}
                }
            }
            WhileExpression { while_token, condition, body: block, } => {
                res.push(while_token.clone());
                res.append(&mut condition.to_token_vec());
                res.append(&mut block.to_token_vec());
            }
            LoopExpression { loop_token, body: block, } => {
                res.push(loop_token.clone());
                res.append(&mut block.to_token_vec());
            }
            BreakExpression { break_token, } => {
                res.push(break_token.clone());
            }
        }
        res
    }

    pub fn print_as_line(&self, indent: i32) {
        let mut indent_str = "".to_string();
        for _ in 0..indent {
            indent_str.push_str("    ");
        }
        match self {
            Statement { expression, semicolon } => {
                expression.print_as_line(indent);
                println!("{}{}", indent_str, semicolon);
            }
            LiteralExpression { literal_token } => {
                print!("{}{}", indent_str, literal_token)
            }
            IdentifierExpression { identifier_token } => {
                print!("{}{}", indent_str, identifier_token)
            }
            UnaryExpression { operator_token, operand, } => {
                print!("{}{}", indent_str, operator_token);
                operand.print_inline(indent);
            }
            BinaryExpression { left, operator_token, right, } => {
                left.print_as_line(indent);
                print!(" {} ", operator_token);
                right.print_inline(indent);
            }
            BracketedExpression { left_b, block: expression, right_b, } => {
                println!("{}{}", indent_str, left_b);
                expression.print_as_line(indent + 1);
                print!("{}{}", indent_str, right_b);
            }
            ParenthesizedExpression { left_p, expression, right_p, } => {
                print!("{}{}", indent_str, left_p);
                expression.print_inline(indent);
                print!("{}{}", indent_str, right_p);
            }
            DeclarationExpression { declaration_token, identifier_token, equals_token, expression, } => {
                print!("{}{} {} {} ", indent_str, declaration_token, identifier_token, equals_token);
                expression.print_inline(indent);
            }
            AssignmentExpression { identifier_token, equals_token, expression, } => {
                print!("{}{} {} ", indent_str, identifier_token, equals_token);
                expression.print_inline(indent);
            }
            IfExpression { if_token, condition, true_expr: then_block, else_token, false_expr: else_block, } => {
                print!("{}{} ", indent_str, if_token);
                condition.print_inline(indent);
                print!(" ");
                then_block.print_inline(indent);
                match else_token {
                    Some(token) => {
                        print!(" {}", token);
                        else_block.as_ref().unwrap().print_inline(indent);
                    }
                    None => {}
                }
            }
            WhileExpression { while_token, condition, body: block, } => {
                print!("{}{} ", indent_str, while_token);
                condition.print_inline(indent);
                print!(" ");
                block.print_inline(indent);
            }
            LoopExpression { loop_token, body: block, } => {
                print!("{}{} ", indent_str, loop_token);
                block.print_inline(indent);
            }
            BreakExpression { break_token, } => {
                print!("{}{}", indent_str, break_token);
            }
        } //end match
    }
    pub fn print_inline(&self, indent: i32) {
        let mut indent_str = "".to_string();
        for _ in 0..indent {
            indent_str.push_str("    ");
        }
        match self {
            Statement { expression, semicolon } => {
                expression.print_as_line(indent);
                print!("{}", semicolon);
            }
            LiteralExpression { literal_token } => {
                print!("{}", literal_token)
            }
            IdentifierExpression { identifier_token } => {
                print!("{}", identifier_token)
            }
            UnaryExpression { operator_token, operand, } => {
                print!("{}", operator_token);
                operand.print_inline(indent);
            }
            BinaryExpression { left, operator_token, right, } => {
                left.print_inline(indent);
                print!(" {} ", operator_token);
                right.print_inline(indent);
                // println!()
            }
            BracketedExpression { left_b, block: expression, right_b, } => {
                println!("{}", left_b);
                expression.print_as_line(indent);
                print!("{}{}", indent_str, right_b);
            }
            ParenthesizedExpression { left_p, expression, right_p, } => {
                print!("{}", left_p);
                expression.print_as_line(indent);
                print!("{}", right_p);
            }
            DeclarationExpression { declaration_token, identifier_token, equals_token, expression, } => {
                print!("{} {} {} ", declaration_token, identifier_token, equals_token);
                expression.print_inline(indent);
                println!()
            }
            AssignmentExpression { identifier_token, equals_token, expression, } => {
                print!("{} {} ", identifier_token, equals_token);
                expression.print_inline(indent);
                println!()
            }
            IfExpression { if_token, condition, true_expr: then_block, else_token, false_expr: else_block, } => {
                print!(" {} ", if_token);
                condition.print_inline(indent);
                print!(" ");
                then_block.print_inline(indent);
                match else_token {
                    Some(token) => {
                        // println!();
                        print!(" {} ", token);
                        else_block.as_ref().unwrap().print_inline(0);
                    }
                    None => {}
                }
            }
            WhileExpression { while_token, condition, body: block, } => {
                print!(" {} ", while_token);
                condition.print_inline(indent);
                block.print_inline(indent);
            }
            LoopExpression { loop_token, body: block, } => {
                print!(" {} ", loop_token);
                block.print_inline(indent);
            }
            BreakExpression { break_token, } => {
                print!("{}{}", indent_str, break_token);
            }
        }
    }
}
