#[cfg(test)]
mod tests {
    use crate::analyze::lex::Lexer;
    use crate::analyze::lex::token::Token;
    use crate::analyze::lex::token::TokenType::{AndKeyword, FalseKeyword, FloatPointToken, IntegerToken, LeftParenthesisToken, OrKeyword, PlusToken, RightParenthesisToken, SemicolonToken, SlashToken, StarToken, TrueKeyword};

    #[test]
    fn test() {
        let input = r#"1*(2+3)/4;
                            true and false or true;
                            (1_0.0 + 2.5)/3.3;
                            1.3+4;;"#;
        let lexer = Lexer::new(&input);
        let source_file = lexer.lex();
        assert_eq!(source_file.len(), 5);

        let line1 = source_file.get(0).unwrap();
        assert_eq!(line1.len(), 10);
        assert_eq!(line1.get(0), Token::new(IntegerToken, "1".to_string()));
        assert_eq!(line1.get(1), Token::new(StarToken, "*".to_string()));
        assert_eq!(line1.get(2), Token::new(LeftParenthesisToken, "(".to_string()));
        assert_eq!(line1.get(3), Token::new(IntegerToken, "2".to_string()));
        assert_eq!(line1.get(4), Token::new(PlusToken, "+".to_string()));
        assert_eq!(line1.get(5), Token::new(IntegerToken, "3".to_string()));
        assert_eq!(line1.get(6), Token::new(RightParenthesisToken, ")".to_string()));
        assert_eq!(line1.get(7), Token::new(SlashToken, "/".to_string()));
        assert_eq!(line1.get(8), Token::new(IntegerToken, "4".to_string()));
        assert_eq!(line1.get(9), Token::new(SemicolonToken, ";".to_string()));
        // assert_eq!(line1.get(10), Token::new(EndLineToken, "".to_string()));

        let line2 = source_file.get(1).unwrap();
        assert_eq!(line2.len(), 6);
        assert_eq!(line2.get(0), Token::new(TrueKeyword, "true".to_string()));
        assert_eq!(line2.get(1), Token::new(AndKeyword, "and".to_string()));
        assert_eq!(line2.get(2), Token::new(FalseKeyword, "false".to_string()));
        assert_eq!(line2.get(3), Token::new(OrKeyword, "or".to_string()));
        assert_eq!(line2.get(4), Token::new(TrueKeyword, "true".to_string()));
        assert_eq!(line2.get(5), Token::new(SemicolonToken, ";".to_string()));
        // assert_eq!(line2.get(6), Token::new(EndLineToken, "".to_string()));

        let line3 = source_file.get(2).unwrap();
        assert_eq!(line3.len(), 8);
        assert_eq!(line3.get(0), Token::new(LeftParenthesisToken, "(".to_string()));
        assert_eq!(line3.get(1), Token::new(FloatPointToken, "10.0".to_string()));
        assert_eq!(line3.get(2), Token::new(PlusToken, "+".to_string()));
        assert_eq!(line3.get(3), Token::new(FloatPointToken, "2.5".to_string()));
        assert_eq!(line3.get(4), Token::new(RightParenthesisToken, ")".to_string()));
        assert_eq!(line3.get(5), Token::new(SlashToken, "/".to_string()));
        assert_eq!(line3.get(6), Token::new(FloatPointToken, "3.3".to_string()));
        assert_eq!(line3.get(7), Token::new(SemicolonToken, ";".to_string()));
        // assert_eq!(line3.get(8), Token::new(EndLineToken, "".to_string()));

        let line4 = source_file.get(3).unwrap();
        assert_eq!(line4.len(), 4);
        assert_eq!(line4.get(0), Token::new(FloatPointToken, "1.3".to_string()));
        assert_eq!(line4.get(1), Token::new(PlusToken, "+".to_string()));
        assert_eq!(line4.get(2), Token::new(IntegerToken, "4".to_string()));
        assert_eq!(line4.get(3), Token::new(SemicolonToken, ";".to_string()));
        // assert_eq!(line4.get(4), Token::new(EndLineToken, "".to_string()));


    }
}
