#![allow(dead_code)]

use colored::Colorize;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq,Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub text: String,
}

impl Token {
    pub fn new(token_type: TokenType, text: String) -> Self {
        Self { token_type, text }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self.token_type {
            TokenType::BadToken => {
                format!(
                    "{}{} {}",
                    self.token_type.to_string().red(),
                    ":".red(),
                    self.text.red()
                )
            }
            _ => format!("{}: {}", self.token_type, self.text),
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, Clone, PartialEq,Eq,Copy)]
pub enum TokenType {
    BadToken,
    WhitespaceToken,
    EndLineToken,

    IntegerToken,
    FloatPointToken,
    TrueKeyword,
    FalseKeyword,
    IdentifierToken,

    LeftParenthesisToken,
    RightParenthesisToken,

    LeftBraceToken,
    RightBraceToken,

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    EqualsToken,
    BangEqualsToken,
    EqualsEqualsToken,

    BangToken,
    AndKeyword,
    OrKeyword,
    SemicolonToken,
}

impl TokenType {
    pub fn get_unary_priority(&self) -> i32 {
        match self {
            TokenType::PlusToken => 6,
            TokenType::MinusToken => 6,
            TokenType::BangToken => 6,
            _ => 0,
        }
    }

    pub fn get_binary_priority(&self) -> i32 {
        match self {
            TokenType::StarToken => 5,
            TokenType::SlashToken => 5,
            TokenType::PlusToken => 4,
            TokenType::MinusToken => 4,
            TokenType::EqualsEqualsToken => 3,
            TokenType::BangEqualsToken => 3,
            TokenType::AndKeyword => 2,
            TokenType::OrKeyword => 1,
            _ => 0,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::BadToken => "BadToken",
            TokenType::WhitespaceToken => "Whitespace",
            TokenType::EndLineToken => "EOFToken",
            TokenType::IntegerToken => "Integer",
            TokenType::FloatPointToken => "FloatPoint",
            TokenType::TrueKeyword => "TrueKeyword",
            TokenType::FalseKeyword => "FalseKeyword",
            TokenType::IdentifierToken => "Identifier",
            TokenType::PlusToken => "PlusToken",
            TokenType::MinusToken => "MinusToken",
            TokenType::StarToken => "StarToken",
            TokenType::SlashToken => "SlashToken",
            TokenType::EqualsToken => "EqualsToken",
            TokenType::BangEqualsToken => "BangEqualsToken",
            TokenType::EqualsEqualsToken => "EqualsEqualsToken",
            TokenType::BangToken => "BangToken",
            TokenType::AndKeyword => "AndKeyword",
            TokenType::OrKeyword => "OrKeyword",
            TokenType::LeftParenthesisToken => "LPToken",
            TokenType::RightParenthesisToken => "RPToken",
            TokenType::LeftBraceToken => "LBToken",
            TokenType::RightBraceToken => "RBToken",
            TokenType::SemicolonToken => "SemicolonToken",
        };
        write!(f, "{}", string)
    }
}
