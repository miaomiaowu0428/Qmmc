#[cfg(test)]
mod tests {
    use crate::{Lexer, SyntaxTree};
    use crate::evaluate::Evaluator;

    #[test]
    fn test_evaluator() {
        let input = r#"
                            {
                                var a = 1_0.4;
                                val b = 20;
                                b = 30;
                                c = 40;
                                a = 50;
                                a
                            }

                            {
                                val t = true;
                                val f = false;
                                t and f
                            }
                            "#;
        let lexer = Lexer::new(input);
        let source_file = lexer.lex();
        let syntax_tree = SyntaxTree::new(source_file);
        let expressions = syntax_tree.parse_file();
        let evaluator = Evaluator::new();
        let res = evaluator.evaluate(expressions);
        assert_eq!(res.len(), 2);
        assert_eq!(res[0].to_string(), "50");
        assert_eq!(res[1].to_string(), "false");

        assert_eq!(evaluator.diagnostics.is_empty(), false);
        assert_eq!(evaluator.diagnostics.diagnostics.borrow().len(), 2);
        assert_eq!(evaluator.diagnostics.diagnostics.borrow()[0].message, "Assignment to immutable variable '\u{1b}[31mb\u{1b}[0m' because it's declared with 'val'. consider change it to var");
        assert_eq!(evaluator.diagnostics.diagnostics.borrow()[1].message, "Found no variable named '\u{1b}[31mc\u{1b}[0m'");
    }
}