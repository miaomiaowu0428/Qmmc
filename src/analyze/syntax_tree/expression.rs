#![allow(dead_code)]

use std::fmt::Display;

use Expression::{AssignmentExpression, ContinueExpression, FunctionCallExpression};
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
use crate::analyze::syntax_tree::Expression::{BreakExpression, DeclarationExpression, FunctionDeclarationExpression, LoopExpression, ReturnExpression};
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
    ContinueExpression {
        continue_token: Token,
    },
    FunctionDeclarationExpression {
        fun_token: Token,
        identifier_token: Token,
        left_p: Token,
        parameters: Vec<Token>,
        right_p: Token,
        body: Box<Expression>,
    },
    ReturnExpression {
        return_token: Token,
        expression: Box<Expression>,
    },
    FunctionCallExpression {
        identifier_token: Token,
        left_p: Token,
        arguments: Vec<Expression>,
        right_p: Token,
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
            ContinueExpression { .. } => Type::None,
            FunctionDeclarationExpression { .. } => Unknown,
            FunctionCallExpression { .. } => Unknown,
            ReturnExpression { expression, .. } => expression.r#type(),
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
            ContinueExpression { continue_token, } => {
                res.push(continue_token.clone());
            }
            FunctionDeclarationExpression { fun_token, identifier_token, left_p, parameters, right_p, body, } => {
                res.push(fun_token.clone());
                res.push(identifier_token.clone());
                res.push(left_p.clone());
                res.append(&mut parameters.clone());
                res.push(right_p.clone());
                res.append(&mut body.to_token_vec());
            }
            ReturnExpression { return_token, expression, } => {
                res.push(return_token.clone());
                res.append(&mut expression.to_token_vec());
            }
            FunctionCallExpression { identifier_token, left_p, arguments, right_p, } => {
                res.push(identifier_token.clone());
                res.push(left_p.clone());
                for arg in arguments.iter() {
                    res.append(&mut arg.to_token_vec());
                }
                res.push(right_p.clone());
            }
        }
        res
    }
}


impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format_with_indent(f, 0)
    }
}

impl Expression {
    pub(crate) fn format_with_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: i32) -> std::fmt::Result {
        let mut indent_str = "".to_string();
        for _ in 0..indent {
            indent_str.push_str("    ");
        }
        match self {
            Statement { expression, semicolon } => {
                expression.format_with_indent(f, indent)?;
                writeln!(f, "{}{}", indent_str, semicolon)
            }
            LiteralExpression { literal_token } => {
                write!(f, "{}{}", indent_str, literal_token)
            }
            IdentifierExpression { identifier_token } => {
                write!(f, "{}{}", indent_str, identifier_token)
            }
            UnaryExpression { operator_token, operand, } => {
                write!(f, "{}{}", indent_str, operator_token)?;
                operand.format_inline(f, indent)
            }
            BinaryExpression { left, operator_token, right, } => {
                left.format_with_indent(f, indent)?;
                write!(f, " {} ", operator_token)?;
                right.format_inline(f, indent)
            }
            BracketedExpression { left_b, block: expression, right_b, } => {
                writeln!(f, "{}{}", indent_str, left_b)?;
                expression.format_with_indent(f, indent + 1)?;
                write!(f, "{}{}", indent_str, right_b)
            }
            ParenthesizedExpression { left_p, expression, right_p, } => {
                write!(f, "{}{}", indent_str, left_p)?;
                expression.format_inline(f, indent)?;
                write!(f, "{}{}", indent_str, right_p)
            }
            DeclarationExpression { declaration_token, identifier_token, equals_token, expression, } => {
                write!(f, "{}{} {} {} ", indent_str, declaration_token, identifier_token, equals_token)?;
                expression.format_inline(f, indent)
            }
            AssignmentExpression { identifier_token, equals_token, expression, } => {
                write!(f, "{}{} {} ", indent_str, identifier_token, equals_token)?;
                expression.format_inline(f, indent)
            }
            IfExpression { if_token, condition, true_expr: then_block, else_token, false_expr: else_block, } => {
                write!(f, "{}{} ", indent_str, if_token)?;
                condition.format_inline(f, indent)?;
                write!(f, " ")?;
                then_block.format_inline(f, indent)?;
                match else_token {
                    Some(token) => {
                        write!(f, " {}", token)?;
                        else_block.as_ref().unwrap().format_inline(f, indent)
                    }
                    None => { Ok(()) }
                }
            }
            WhileExpression { while_token, condition, body: block, } => {
                write!(f, "{}{} ", indent_str, while_token)?;
                condition.format_inline(f, indent)?;
                write!(f, " ")?;
                block.format_inline(f, indent)
            }
            LoopExpression { loop_token, body: block, } => {
                write!(f, "{}{} ", indent_str, loop_token)?;
                block.format_inline(f, indent)
            }
            BreakExpression { break_token, } => {
                write!(f, "{}{}", indent_str, break_token)
            }
            ContinueExpression { continue_token, } => {
                write!(f, "{}{}", indent_str, continue_token)
            }
            FunctionDeclarationExpression { fun_token, identifier_token, parameters, body, .. } => {
                write!(f, "{}{} {} ", indent_str, fun_token, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", parameters.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ") ", )?;
                body.format_inline(f, indent)
            }
            ReturnExpression { return_token, expression, } => {
                write!(f, "{}{} ", indent_str, return_token)?;
                expression.format_inline(f, indent)
            }
            FunctionCallExpression { identifier_token, arguments, .. } => {
                write!(f, "{}{} ", indent_str, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", arguments.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ")")
            }
        } //end match
    }

    fn format_inline(&self, f: &mut std::fmt::Formatter<'_>, indent: i32) -> std::fmt::Result {
        let mut indent_str = "".to_string();
        for _ in 0..indent {
            indent_str.push_str("    ");
        }
        match self {
            Statement { expression, semicolon } => {
                expression.format_with_indent(f, indent)?;
                write!(f, "{}", semicolon)
            }
            LiteralExpression { literal_token } => write!(f, "{}", literal_token),
            IdentifierExpression { identifier_token } => write!(f, "{}", identifier_token),
            UnaryExpression { operator_token, operand, } => {
                write!(f, "{}", operator_token)?;
                operand.format_inline(f, indent)
            }
            BinaryExpression { left, operator_token, right, } => {
                left.format_inline(f, indent)?;
                write!(f, " {} ", operator_token)?;
                right.format_inline(f, indent)
            }
            BracketedExpression { left_b, block: expression, right_b, } => {
                writeln!(f, "{}", left_b)?;
                expression.format_with_indent(f, indent + 1)?;
                write!(f, "{}{}", indent_str, right_b)
            }
            ParenthesizedExpression { left_p, expression, right_p, } => {
                write!(f, "{}", left_p)?;
                expression.format_inline(f, indent)?;
                write!(f, "{}", right_p)
            }
            DeclarationExpression { declaration_token, identifier_token, equals_token, expression, } => {
                write!(f, "{}{} {} {} ", indent_str, declaration_token, identifier_token, equals_token)?;
                expression.format_inline(f, indent)
            }
            AssignmentExpression { identifier_token, equals_token, expression, } => {
                write!(f, "{}{} {} ", indent_str, identifier_token, equals_token)?;
                expression.format_inline(f, indent)
            }
            IfExpression { if_token, condition, true_expr: then_block, else_token, false_expr: else_block, } => {
                write!(f, "{}{} ", indent_str, if_token)?;
                condition.format_inline(f, indent)?;
                write!(f, " ")?;
                then_block.format_inline(f, indent)?;
                match else_token {
                    Some(token) => {
                        write!(f, " {}", token)?;
                        else_block.as_ref().unwrap().format_inline(f, indent)
                    }
                    None => { Ok(()) }
                }
            }
            WhileExpression { while_token, condition, body: block, } => {
                write!(f, "{}{} ", indent_str, while_token)?;
                condition.format_inline(f, indent)?;
                block.format_inline(f, indent)
            }
            LoopExpression { loop_token, body: block, } => {
                write!(f, "{}{} ", indent_str, loop_token)?;
                block.format_inline(f, indent)
            }
            BreakExpression { break_token, } => write!(f, "{}{}", indent_str, break_token),
            ContinueExpression { continue_token, } => write!(f, "{}{}", indent_str, continue_token),
            FunctionDeclarationExpression { fun_token, identifier_token, parameters, body, .. } => {
                write!(f, "{} {} ", fun_token, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", parameters.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ") ", )?;
                body.format_inline(f, indent)
            }
            ReturnExpression { return_token, expression, } => {
                write!(f, "{}{} ", indent_str, return_token)?;
                expression.format_inline(f, indent)
            }
            FunctionCallExpression { identifier_token, arguments, .. } => {
                write!(f, "{}{} ", indent_str, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", arguments.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ")")
            }
        }
    }
}









