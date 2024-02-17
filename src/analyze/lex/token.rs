#![allow(dead_code)]

use std::fmt::{Debug, Display};

use colored::Colorize;

use TokenType::{AndKeyword, GreatThanToken, LessThanToken, PrecentToken, WhileKeyword};
use TokenType::BadToken;
use TokenType::BangEqualsToken;
use TokenType::BangToken;
use TokenType::BreakKeyword;
use TokenType::ElseKeyword;
use TokenType::EndLineToken;
use TokenType::EndOfFileToken;
use TokenType::EqualsEqualsToken;
use TokenType::EqualsToken;
use TokenType::FalseKeyword;
use TokenType::FloatPointToken;
use TokenType::IdentifierToken;
use TokenType::IfKeyword;
use TokenType::IntegerToken;
use TokenType::LeftBraceToken;
use TokenType::LeftParenthesisToken;
use TokenType::LoopKeyword;
use TokenType::MinusToken;
use TokenType::OrKeyword;
use TokenType::PlusToken;
use TokenType::RightBraceToken;
use TokenType::RightParenthesisToken;
use TokenType::SemicolonToken;
use TokenType::SlashToken;
use TokenType::StarToken;
use TokenType::TrueKeyword;
use TokenType::ValKeyword;
use TokenType::VarKeyword;
use TokenType::WhitespaceToken;

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
            BadToken => {
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
                   ValKeyword | VarKeyword | IfKeyword | ElseKeyword | WhileKeyword | LoopKeyword | BreakKeyword => format!("{}", self.text.bold().yellow()),

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
    LessThanToken,
    GreatThanToken,

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
    WhileKeyword,
    LoopKeyword,
    BreakKeyword,
    PrecentToken,
}

impl TokenType {
    pub fn get_unary_priority(&self) -> i32 {
        match self {
            PlusToken => 10,
            MinusToken => 10,
            BangToken => 10,
            _ => 0,
        }
    }

    pub fn get_binary_priority(&self) -> i32 {
        match self {
            StarToken => 7,
            SlashToken => 7,
            PlusToken => 6,
            MinusToken => 6,
            PrecentToken => 5,
            LessThanToken => 3,
            GreatThanToken => 3,
            EqualsEqualsToken => 3,
            BangEqualsToken => 3,
            AndKeyword => 2,
            OrKeyword => 1,
            _ => 0,
        }
    }
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            BadToken => "BadToken",
            WhitespaceToken => "Whitespace",
            EndLineToken => "EndLineToken",
            IntegerToken => "Integer",
            FloatPointToken => "FloatPoint",
            TrueKeyword => "TrueKeyword",
            FalseKeyword => "FalseKeyword",
            IdentifierToken => "Identifier",
            PlusToken => "PlusToken",
            MinusToken => "MinusToken",
            StarToken => "StarToken",
            SlashToken => "SlashToken",
            EqualsToken => "EqualsToken",
            BangEqualsToken => "BangEqualsToken",
            EqualsEqualsToken => "EqualsEqualsToken",
            BangToken => "BangToken",
            AndKeyword => "AndKeyword",
            OrKeyword => "OrKeyword",
            LeftParenthesisToken => "LPToken",
            RightParenthesisToken => "RPToken",
            LeftBraceToken => "LBToken",
            RightBraceToken => "RBToken",
            SemicolonToken => "SemicolonToken",
            ValKeyword => "ValKeyword",
            VarKeyword => "VarKeyword",
            EndOfFileToken => "EndOfFileToken",
            ElseKeyword => "ElseKeyword",
            IfKeyword => "IfKeyword",
            WhileKeyword => "WhileKeyword",
            LoopKeyword => "LoopKeyword",
            BreakKeyword => "BreakKeyword",
            LessThanToken => "LessThanToken",
            GreatThanToken => "GreatThanToken",
            PercentToken => "PercentTToken",
        };
        write!(f, "{}", string)
    }
}


impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "", )
    }
}