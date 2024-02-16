#![allow(dead_code)]

use std::fmt::{Debug, Display};

use colored::Colorize;

use TokenType::{ElseKeyword, FalseKeyword, FloatPointToken, IdentifierToken, IfKeyword, IntegerToken, TrueKeyword, ValKeyword, VarKeyword};

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub text: String,
    pub line_num: usize,
    pub column_num: usize,
}


impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.text == other.text
    }
}


impl Token {
    pub fn new(
        token_type: TokenType,
        text: String,
        line_num: usize,
        column_num: usize,
    ) -> Self {
        Self {
            token_type,
            text,
            line_num,
            column_num,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self.token_type {
            TokenType::BadToken => {
                format!(
                    "{}{} {}",
                    format!("{:?}", self.token_type).red(),
                    ":".red(),
                    self.text.red()
                )
            }
            _ => format!("{:?}: {:?}", self.token_type, self.text),
        };
        write!(f, "{}", string)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}", self.text)
        write!(f, "{}",
               match self.token_type {
                   IntegerToken | FloatPointToken | TrueKeyword | FalseKeyword => format!("{}", self.text.green()),
                   IdentifierToken => format!("{}", self.text.bold()),
                   ValKeyword | VarKeyword| IfKeyword| ElseKeyword => format!("{}", self.text.bold().yellow()),

                   _ => format!("{}", self.text),
               })
    }
}

#[derive(Clone, PartialEq, Eq, Copy)]
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
    ValKeyword,
    VarKeyword,
    EndOfFileToken,
    ElseKeyword,
    IfKeyword,
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

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::BadToken => "BadToken",
            TokenType::WhitespaceToken => "Whitespace",
            TokenType::EndLineToken => "EndLineToken",
            IntegerToken => "Integer",
            FloatPointToken => "FloatPoint",
            TrueKeyword => "TrueKeyword",
            FalseKeyword => "FalseKeyword",
            IdentifierToken => "Identifier",
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
            ValKeyword => "ValKeyword",
            VarKeyword => "VarKeyword",
            TokenType::EndOfFileToken => "EndOfFileToken",
            ElseKeyword => "ElseKeyword",
            IfKeyword => "IfKeyword",
        };
        write!(f, "{}", string)
    }
}


impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "", )
    }
}
