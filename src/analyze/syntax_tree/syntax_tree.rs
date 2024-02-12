#![allow(dead_code)]

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::lex::token::{Token, TokenType};
use crate::analyze::lex::SourceFile;
use crate::analyze::syntax_tree::expression::Expression;
use crate::analyze::syntax_tree::expression::Expression::{
    BinaryExpression, ParenthesizedExpression, UnaryExpression,
};
use std::cell::RefCell;
use Expression::{AssignmentExpression, DeclarationExpression, IdentifierExpression, LiteralExpression};
use TokenType::{FalseKeyword, IdentifierToken, IntegerToken, LeftBraceToken, LeftParenthesisToken, RightBraceToken, RightParenthesisToken, TrueKeyword, ValKeyword, VarKeyword};
use crate::analyze::lex::line::Line;
use crate::analyze::lex::TokenType::EqualsToken;
use crate::analyze::syntax_tree::block::Block;
use crate::analyze::syntax_tree::Expression::{BracketedExpression, Statement};

pub struct SyntaxTree {
    source_file: SourceFile,
    line_number: RefCell<usize>,
    pos: RefCell<usize>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl SyntaxTree {
    pub fn new(source_file: SourceFile) -> Self {
        Self {
            source_file,
            line_number: RefCell::from(0),
            pos: RefCell::from(0),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn parse_file(&self) -> Vec<Expression> {
        let mut res = Vec::new();
        while self.line_num() < self.source_file.len() {
            if self.current().token_type == LeftBraceToken {
                res.push(self.parse_block());
            } else {
                res.push(self.parse_line());
            }
            self.move_next_line();
        }
        res
    }

    pub fn parse_line(&self) -> Expression {
        *self.pos.borrow_mut() = 0;

        let expr;

        match self.current().token_type {
            LeftBraceToken => {
                expr = self.parse_block();
            }
            _ => {
                expr = self.parse_expression();
                self.move_next();
            }
        }

        match self.current().token_type {
            TokenType::SemicolonToken => {
                self.move_next();
                Statement {
                    expression: Box::new(expr),
                    semicolon: self.current(),
                }
            }
            _ => {
                expr
            }
        }
    }


    pub fn parse_block(&self) -> Expression {
        let lb = self.current();
        self.move_next_line();
        let block = Block::new();
        while self.line_num() < self.source_file.len()
            && self.current().token_type != RightBraceToken
        {
            block.expressions.borrow_mut().push(self.parse_line());
            self.move_next_line();
        }
        let rb = self.match_token(|t| t.token_type == RightBraceToken, vec![RightBraceToken]);
        self.move_next();
        self.check_block(&block);
        BracketedExpression {
            left_b: lb,
            block,
            right_b: rb,
        }
    }
    pub fn parse_expression(&self) -> Expression {
        match self.current().token_type {
            ValKeyword | VarKeyword => {
                let declaration_token = self.current();
                self.move_next();
                let identifier_token = self.current();
                self.move_next();
                let equals_token = self.match_token(|t| t.token_type == TokenType::EqualsToken, vec![TokenType::EqualsToken]);
                let expression = self.parse_operator_expression(0);
                let semicolon_token = self.match_token(|t| t.token_type == TokenType::SemicolonToken, vec![TokenType::SemicolonToken]);
                DeclarationExpression {
                    declaration_token,
                    identifier_token,
                    equals_token,
                    expression: Box::new(expression),
                    semicolon_token,
                }
            }
            IdentifierToken if self.peek(1).token_type == EqualsToken => {
                let identifier_token = self.current();
                self.move_next();
                let equals_token = self.match_token(|t| t.token_type == TokenType::EqualsToken, vec![TokenType::EqualsToken]);
                let expression = self.parse_operator_expression(0);
                let semicolon_token = self.match_token(|t| t.token_type == TokenType::SemicolonToken, vec![TokenType::SemicolonToken]);
                AssignmentExpression {
                    identifier_token,
                    equals_token,
                    expression: Box::new(expression),
                    semicolon_token,
                }
            }
            _ => {
                self.parse_operator_expression(0)
            }
        }
    }

    fn parse_operator_expression(&self, parent_priority: i32) -> Expression {
        let mut left;

        left = self.parse_unary_expression(parent_priority);

        loop {
            let binary_priority = self.current().token_type.get_binary_priority();
            if binary_priority == 0 || binary_priority <= parent_priority {
                break;
            }

            let op = self.current();
            self.move_next();
            let right = self.parse_operator_expression(binary_priority);
            left = BinaryExpression {
                left: Box::new(left),
                operator_token: op,
                right: Box::new(right),
            }
        }
        left
    }

    fn parse_unary_expression(&self, parent_priority: i32) -> Expression {
        let left;

        let unary_priority = self.current().token_type.get_unary_priority();
        if unary_priority == 0 || unary_priority < parent_priority {
            left = self.parse_literal_expression()
        } else {
            let op = self.current();
            self.move_next();
            let operand = self.parse_operator_expression(unary_priority);
            left = UnaryExpression {
                operator_token: op,
                operand: Box::new(operand),
            }
        }
        left
    }

    fn parse_literal_expression(&self) -> Expression {
        match self.current().token_type {
            IntegerToken | FloatPointToken | TrueKeyword | FalseKeyword => {
                let res = LiteralExpression {
                    literal_token: self.current(),
                };
                self.move_next();
                res
            }
            IdentifierToken => {
                let res = IdentifierExpression {
                    identifier_token: self.current(),
                };
                self.move_next();
                res
            }
            LeftParenthesisToken => {
                let left_p = self.current();
                self.move_next();
                let expr = self.parse_expression();

                let right_p = self.match_token(
                    |t| t.token_type == RightParenthesisToken,
                    vec![RightParenthesisToken],
                );
                ParenthesizedExpression {
                    left_p,
                    expression: Box::new(expr),
                    right_p,
                }
            }
            _ => LiteralExpression { literal_token: self.current() },
        }
    }

    fn match_token<F>(&self, predict: F, expected: Vec<TokenType>) -> Token
    where
        F: Fn(Token) -> bool,
    {
        if predict(self.current()) {
            self.move_next()
        } else {
            self.diagnostics.report_unexpected_token(self.current(), &expected, self.current_line(), self.line_num(), self.pos());
            Token::new(expected[0], "".to_string())
        }
    }

    fn peek(&self, offset: usize) -> Token {
        let line_num = self.line_num();
        let pos = self.pos();

        if pos + offset >= self.line_len() {
            self.current_line().get(self.line_len() - 1)
        } else {
            self.current_line().get(pos + offset)
        }
    }
    fn current(&self) -> Token {
        self.peek(0)
    }
    fn current_line(&self) -> Line {
        if self.line_num() >= self.source_file.lines.len() {
            Line::new(vec![Token::new(TokenType::EndLineToken, "".to_string())])
        } else {
            self.source_file.lines[self.line_num()].clone()
        }
    }

    fn line_num(&self) -> usize {
        self.line_number.borrow().clone()
    }
    fn line_len(&self) -> usize {
        if self.line_num() >= self.source_file.lines.len() {
            1
        } else {
            self.current_line()
                .tokens
                .borrow()
                .len()
        }
    }
    fn pos(&self) -> usize {
        self.pos.borrow().clone()
    }
    fn move_next(&self) -> Token {
        let c = self.current();
        *self.pos.borrow_mut() += 1;
        c
    }

    fn next_line(&self) -> Line {
        if self.line_num() + 1 >= self.source_file.lines.len() {
            Line::new(vec![Token::new(TokenType::EndLineToken, "".to_string())])
        } else {
            self.source_file.lines[self.line_num() + 1].clone()
        }
    }

    fn move_next_line(&self) {
        *self.line_number.borrow_mut() += 1;
        *self.pos.borrow_mut() = 0;
    }
    fn check_block(&self, block: &Block) {
        let expressions = block.expressions.borrow();
        if expressions.len() == 0 {
            return;
        }
        for i in 0..expressions.len() - 1 {
            match expressions.get(i).unwrap() {
                Statement { .. } => {},
                DeclarationExpression { .. } => {},
                AssignmentExpression { .. } => {},
                LiteralExpression { literal_token, .. } => {
                    self.diagnostics.report_unexpected_expression(self.source_file.lines[literal_token.line_num.borrow().clone()].clone());
                }
                IdentifierExpression { identifier_token } => {
                    self.diagnostics.report_unexpected_expression(self.source_file.lines[identifier_token.line_num.borrow().clone()].clone());
                }
                UnaryExpression { operator_token, .. } => {
                    self.diagnostics.report_unexpected_expression(self.source_file.lines[operator_token.line_num.borrow().clone()].clone());
                }
                BinaryExpression { operator_token: op, .. } => {
                    self.diagnostics.report_unexpected_expression(self.source_file.lines[op.line_num.borrow().clone()].clone());
                }
                BracketedExpression { block, .. } => {
                    self.check_block(block);
                }
                ParenthesizedExpression { left_p, .. } => {
                    self.diagnostics.report_unexpected_expression(self.source_file.lines[left_p.line_num.borrow().clone()].clone());
                }
            }
        }
    }
}
