#![allow(non_snake_case)]


use std::fs::File;
use std::io::Read;
use std::path::Path;
use crate::analyze::lex::Lexer;
use crate::analyze::syntax_tree::SyntaxTree;

mod analyze;

static PATH: &str = "G:/codefile/rust_source_file/Qmmc/resource/";

fn main() {
    let mut file = File::open(Path::new(&format!("{}{}", PATH, "test.qmm"))).expect("Could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Could not read file");
    let lexer = Lexer::new(&contents);
    let source_file = lexer.lex();
    // for line in source_file.lines.iter() {
    //     for token in line.tokens.borrow().iter() {
    //         println!("{:?}", token);
    //     }
    // }

    let syntax_tree = SyntaxTree::new(source_file);
    let expressions = syntax_tree.parse_file();
    syntax_tree.diagnostics.print();

    for expr in expressions.iter() {
        println!("------------------");
        expr.print(0);
        println!()
    }


}
