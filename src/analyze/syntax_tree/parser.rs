#![allow(dead_code)]

use std::cell::RefCell;

use Expression::{IdentifierExpression, Statement};
use Expression::AssignmentExpression;
use Expression::DeclarationExpression;
use Expression::LiteralExpression;
use TokenType::{FalseKeyword, IfKeyword};
use TokenType::IdentifierToken;
use TokenType::IntegerToken;
use TokenType::LeftBraceToken;
use TokenType::LeftParenthesisToken;
use TokenType::RightBraceToken;
use TokenType::RightParenthesisToken;
use TokenType::TrueKeyword;
use TokenType::ValKeyword;
use TokenType::VarKeyword;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::token::Token;
use crate::analyze::lex::token::TokenType;
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::lex::TokenType::EqualsToken;
use crate::analyze::syntax_tree::block::Block;
use crate::analyze::syntax_tree::Expression::BracketedExpression;
use crate::analyze::syntax_tree::expression::Expression;
use crate::analyze::syntax_tree::expression::Expression::BinaryExpression;
use crate::analyze::syntax_tree::expression::Expression::ParenthesizedExpression;
use crate::analyze::syntax_tree::expression::Expression::UnaryExpression;

pub struct Parser {
    tokens: Vec<Token>,
    pos: RefCell<usize>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: RefCell::from(0),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn parse(&self) -> Vec<Expression> {
        let mut expressions = Vec::new();
        while self.current().token_type != TokenType::EndOfFileToken {
            expressions.push(self.parse_expression_with_semicolon_checking());
        }
        expressions
    }

    fn parse_expression_with_semicolon_checking(&self) -> Expression {
        let expr = self.parse_expression();

        if self.current().token_type == TokenType::SemicolonToken {
            let semicolon = self.move_next();
            Statement { expression: Box::new(expr), semicolon, }
        } else {
            expr
        }
    }

    fn parse_expression(&self) -> Expression {
        match self.current().token_type {
            ValKeyword | VarKeyword => self.parse_declaration_expression(),
            IdentifierToken if self.peek(1).token_type == EqualsToken => self.parse_assignment_expression(),
            IfKeyword => self.parse_if_expression(),
            _ => self.parse_single_expr_or_block(0),
        }
    }

    fn parse_block(&self) -> Expression {
        let lb = self.move_next();
        let block = Block::new();
        while self.current().token_type != RightBraceToken && self.current().token_type != TokenType::EndOfFileToken
        {
            block.expressions.borrow_mut().push(self.parse_expression_with_semicolon_checking());
        }
        let rb = self.match_token(|t| t.token_type == RightBraceToken, vec![RightBraceToken]);
        BracketedExpression {
            left_b: lb,
            block,
            right_b: rb,
        }
    }

    fn parse_if_expression(&self) -> Expression {
        let if_token = self.match_token(|t| t.token_type == IfKeyword, vec![IfKeyword]);
        // let if_token = self.move_next();
        let condition_expr = self.parse_single_expr_or_block(0);
        let true_expr = self.parse_if_expr_or_block();
        if self.current().token_type == TokenType::ElseKeyword {
            let else_token = Some(self.move_next());
            let false_expr = self.parse_if_expr_or_block();
            Expression::IfExpression {
                if_token,
                condition: Box::new(condition_expr),
                true_expr: Box::new(true_expr),
                else_token,
                false_expr: Some(Box::new(false_expr)),
            }
        } else {
            Expression::IfExpression {
                if_token,
                condition: Box::new(condition_expr),
                true_expr: Box::new(true_expr),
                else_token: None,
                false_expr: None,
            }
        }
    }

    fn parse_if_expr_or_block(&self) -> Expression {
        if self.current().token_type == LeftBraceToken {
            self.parse_block()
        } else {
            self.parse_if_expression()
        }
    }


    fn parse_assignment_expression(&self) -> Expression {
        let identifier_token = self.move_next();
        let equals_token = self.match_token(|t| t.token_type == EqualsToken, vec![EqualsToken]);
        let expression = self.parse_expression();
        AssignmentExpression { identifier_token, equals_token, expression: Box::new(expression) }
    }

    fn parse_single_expr_or_block(&self, priority: i32) -> Expression {
        match self.current().token_type {
            LeftBraceToken => self.parse_block(),
            ValKeyword | VarKeyword => self.parse_expression(),
            _ => self.parse_operator_expression(priority),
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
            let right = self.parse_single_expr_or_block(binary_priority);
            left = BinaryExpression {
                left: Box::new(left),
                operator_token: op,
                right: Box::new(right),
            }
        }
        left
    }


    fn parse_declaration_expression(&self) -> Expression {
        let declaration_token = self.move_next();
        let identifier_token = self.match_token(|t| t.token_type == IdentifierToken, vec![IdentifierToken]);
        let equals_token = self.match_token(|t| t.token_type == EqualsToken, vec![EqualsToken]);
        let expression = self.parse_expression();
        DeclarationExpression { declaration_token, identifier_token, equals_token, expression: Box::new(expression) }
    }

    fn parse_unary_expression(&self, parent_priority: i32) -> Expression {
        let left;

        let unary_priority = self.current().token_type.get_unary_priority();
        if unary_priority == 0 || unary_priority < parent_priority {
            left = self.parse_literal_expression()
        } else {
            let op = self.current();
            self.move_next();
            let operand = self.parse_single_expr_or_block(unary_priority);
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
                    literal_token: self.move_next()
                };

                res
            }
            IdentifierToken => {
                let res = IdentifierExpression {
                    identifier_token: self.move_next()
                };
                res
            }
            LeftParenthesisToken => {
                let left_p = self.move_next();
                let expr = self.parse_expression_with_semicolon_checking();

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
            _ => {
                self.diagnostics.report_unexpected_token(self.tokens_in_the_same_line(self.current()), self.current().clone(), &vec![IntegerToken, FloatPointToken, TrueKeyword, FalseKeyword, IdentifierToken, LeftParenthesisToken]);
                LiteralExpression { literal_token: self.move_next() }
            },
        }
    }

    fn tokens_in_the_same_line(&self, token: Token) -> Vec<&Token> {
        self.tokens
            .iter()
            .filter(|t| t.line_num == token.line_num)
            .collect::<Vec<&Token>>()
    }

    fn match_token<F>(&self, predict: F, expected: Vec<TokenType>) -> Token
    where
        F: Fn(Token) -> bool,
    {
        if predict(self.current()) {
            self.move_next()
        } else {
            let current = self.move_next();

            self.diagnostics.report_unexpected_token(self.tokens_in_the_same_line(current.clone()), current.clone(), &expected);
            Token::new(expected[0], current.text, current.line_num, current.column_num)
        }
    }

    fn peek(&self, offset: usize) -> Token {
        let pos = self.pos();

        if pos + offset >= self.tokens.len() {
            Token::new(TokenType::EndOfFileToken, "".to_string(), 0, 0)
        } else {
            self.tokens[pos + offset].clone()
        }
    }
    fn current(&self) -> Token {
        self.peek(0)
    }


    fn pos(&self) -> usize {
        self.pos.borrow().clone()
    }
    fn move_next(&self) -> Token {
        let c = self.current();
        *self.pos.borrow_mut() += 1;
        c
    }
}
