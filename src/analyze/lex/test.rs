#[cfg(test)]
mod tests {
    use crate::analyze::lex::Lexer;
    use crate::analyze::lex::token::Token;
    use crate::analyze::lex::token::TokenType::{AndKeyword, FalseKeyword, FloatPointToken, IntegerToken, LeftParenthesisToken, OrKeyword, PlusToken, RightParenthesisToken, SemicolonToken, SlashToken, StarToken, TrueKeyword};
    use crate::analyze::lex::TokenType::EndOfFileToken;

    #[test]
    fn test() {
        let input = r#"1*(2+3)/4;
                            true and false or true;
                            (1_0.0 + 2.5)/3.3;
                            1.3+4;;"#;
        let lexer = Lexer::new(&input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 30);
        assert_eq!(tokens[0], Token::new(IntegerToken, "1".to_string(), 0, 0));
        assert_eq!(tokens[1], Token::new(StarToken, "*".to_string(), 0, 0));
        assert_eq!(tokens[2], Token::new(LeftParenthesisToken, "(".to_string(), 0, 0));
        assert_eq!(tokens[3], Token::new(IntegerToken, "2".to_string(), 0, 0));
        assert_eq!(tokens[4], Token::new(PlusToken, "+".to_string(), 0, 0));
        assert_eq!(tokens[5], Token::new(IntegerToken, "3".to_string(), 0, 0));
        assert_eq!(tokens[6], Token::new(RightParenthesisToken, ")".to_string(), 0, 0));
        assert_eq!(tokens[7], Token::new(SlashToken, "/".to_string(), 0, 0));
        assert_eq!(tokens[8], Token::new(IntegerToken, "4".to_string(), 0, 0));
        assert_eq!(tokens[9], Token::new(SemicolonToken, ";".to_string(), 0, 0));
        assert_eq!(tokens[10], Token::new(TrueKeyword, "true".to_string(), 0, 0));
        assert_eq!(tokens[11], Token::new(AndKeyword, "and".to_string(), 0, 0));
        assert_eq!(tokens[12], Token::new(FalseKeyword, "false".to_string(), 0, 0));
        assert_eq!(tokens[13], Token::new(OrKeyword, "or".to_string(), 0, 0));
        assert_eq!(tokens[14], Token::new(TrueKeyword, "true".to_string(), 0, 0));
        assert_eq!(tokens[15], Token::new(SemicolonToken, ";".to_string(), 0, 0));
        assert_eq!(tokens[16], Token::new(LeftParenthesisToken, "(".to_string(), 0, 0));
        assert_eq!(tokens[17], Token::new(FloatPointToken, "10.0".to_string(), 0, 0));
        assert_eq!(tokens[18], Token::new(PlusToken, "+".to_string(), 0, 0));
        assert_eq!(tokens[19], Token::new(FloatPointToken, "2.5".to_string(), 0, 0));
        assert_eq!(tokens[20], Token::new(RightParenthesisToken, ")".to_string(), 0, 0));
        assert_eq!(tokens[21], Token::new(SlashToken, "/".to_string(), 0, 0));
        assert_eq!(tokens[22], Token::new(FloatPointToken, "3.3".to_string(), 0, 0, ));
        assert_eq!(tokens[23], Token::new(SemicolonToken, ";".to_string(), 0, 0));
        assert_eq!(tokens[24], Token::new(FloatPointToken, "1.3".to_string(), 0, 0));
        assert_eq!(tokens[25], Token::new(PlusToken, "+".to_string(), 0, 0));
        assert_eq!(tokens[26], Token::new(IntegerToken, "4".to_string(), 0, 0));
        assert_eq!(tokens[27], Token::new(SemicolonToken, ";".to_string(), 0, 0));
        assert_eq!(tokens[28], Token::new(SemicolonToken, ";".to_string(), 0, 0));
        assert_eq!(tokens[29], Token::new(EndOfFileToken, "".to_string(), 0, 0));
    }
}
