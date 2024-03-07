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

use crate::analyze::lex::token::Token;
use crate::analyze::parse::block::Block;
use crate::analyze::parse::Expression::{BreakExpression, VarDeclarationExpression, FunctionDeclarationExpression, FunctionTypeExpression, LoopExpression, ReturnExpression};
use crate::analyze::parse::Expression::IfExpression;
use crate::analyze::parse::Expression::WhileExpression;

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
    VarDeclarationExpression {
        declaration_token: Token,
        identifier_token: Token,
        equals_token: Option<Token>,
        assigment_expr: Option<Box<Expression>>,
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
        parameters: Vec<IdentifierTypePair>,
        right_p: Token,
        arrow_token: Token,
        res_type_description: Box<Expression>,
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
    FunctionTypeExpression {
        lp: Token,
        parameter_types: Vec<Expression>,
        rp: Token,
        arrow: Token,
        return_type: Box<Expression>,
    },
}

impl Expression {
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
            VarDeclarationExpression { declaration_token, identifier_token, equals_token, assigment_expr: expression, } => {
                res.push(declaration_token.clone());
                res.push(identifier_token.clone());
                match equals_token {
                    Some(token) => {
                        res.push(token.clone());
                        res.append(&mut expression.as_ref().unwrap().to_token_vec());
                    }
                    None => {}
                }
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
            FunctionDeclarationExpression {
                fun_token,
                identifier_token,
                left_p,
                parameters,
                right_p, arrow_token,
                res_type_description: type_description,
                body,
            } => {
                res.push(fun_token.clone());
                res.push(identifier_token.clone());
                res.push(left_p.clone());
                for param in parameters.iter() {
                    res.push(param.name_token().clone());
                    res.push(param.colon_token().clone());
                    res.append(&mut param.type_description().to_token_vec());
                }
                res.push(right_p.clone());
                res.push(arrow_token.clone());
                res.append(&mut type_description.to_token_vec());
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
            FunctionTypeExpression { lp, parameter_types, rp, arrow, return_type, } => {
                res.push(lp.clone());
                for param in parameter_types.iter() {
                    res.append(&mut param.to_token_vec());
                }
                res.push(rp.clone());
                res.push(arrow.clone());
                res.append(&mut return_type.to_token_vec());
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
            VarDeclarationExpression { declaration_token, identifier_token, equals_token, assigment_expr: expression, } => {
                write!(f, "{}{} {} ", indent_str, declaration_token, identifier_token)?;
                match equals_token {
                    Some(token) => {
                        write!(f, "{} ", token)?;
                        expression.as_ref().unwrap().format_inline(f, indent)
                    }
                    None => Ok(()),
                }
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
            FunctionDeclarationExpression { fun_token, identifier_token, parameters, body, res_type_description: type_description,.. } => {
                write!(f, "{}{} {} ", indent_str, fun_token, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", parameters.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ") ", )?;
                write!(f,"-> ")?;
                write!(f,"{} ",type_description)?;
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
            FunctionTypeExpression { lp, parameter_types, rp, arrow, return_type, } => {
                write!(f, "{}{}", indent_str, lp)?;
                for param in parameter_types.iter() {
                    param.format_inline(f, indent)?;
                }
                write!(f, "{}", rp)?;
                write!(f, " {} ", arrow)?;
                return_type.format_inline(f, indent)
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
            VarDeclarationExpression { declaration_token, identifier_token, equals_token, assigment_expr: expression, } => {
                write!(f, "{}{} {} ", indent_str, declaration_token, identifier_token, )?;
                match equals_token {
                    Some(token) => {
                        write!(f, "{} ", token)?;
                        expression.as_ref().unwrap().format_inline(f, indent)
                    }
                    None => Ok(()),
                }
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
            FunctionDeclarationExpression { fun_token, identifier_token, parameters, body, res_type_description: type_description,.. } => {
                write!(f, "{} {} ", fun_token, identifier_token)?;
                write!(f, "(", )?;
                write!(f, "{}", parameters.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))?;
                write!(f, ") ", )?;
                write!(f,"-> ")?;
                write!(f,"{} ",type_description)?;
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
            FunctionTypeExpression { lp, parameter_types, rp, arrow, return_type } => {
                write!(f, "{}{}", indent_str, lp)?;
                for param in parameter_types.iter() {
                    param.format_inline(f, indent)?;
                }
                write!(f, "{}", rp)?;
                write!(f, " {} ", arrow)?;
                return_type.format_inline(f, indent)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierTypePair {
    pub name: Token,
    pub colon: Token,
    pub type_description: Expression,
}


impl IdentifierTypePair {
    pub fn new(name: Token, colon: Token, type_declaration: Expression) -> Self {
        Self { name, colon, type_description: type_declaration }
    }

    pub fn name_token(&self) -> &Token {
        &self.name
    }
    pub fn type_description(&self) -> &Expression {
        &self.type_description
    }

    pub fn colon_token(&self) -> &Token {
        &self.colon
    }
}

impl Display for IdentifierTypePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_description)
    }
}









