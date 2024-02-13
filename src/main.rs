#![allow(non_snake_case)]
// #![allow(dead_code)]
#![allow(unused_variables)]

pub use crate::analyze::lex::Lexer;
pub use crate::analyze::syntax_tree::SyntaxTree;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use evaluate::Evaluator;

mod analyze;
mod evaluate;

static PATH: &str = "G:/codefile/rust_source_file/Qmmc/resource/";

fn main() {
    let mut file =
        File::open(Path::new(&format!("{}{}", PATH, "test_scope.qmm"))).expect("Could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Could not read file");
    let lexer = Lexer::new(&contents);
    let source_file = lexer.lex();
    println!("Source File:");
    source_file.print();
    println!("==============================");

    let syntax_tree = SyntaxTree::new(source_file);
    let expressions = syntax_tree.parse_file();
    if !syntax_tree.diagnostics.is_empty() {
        println!("Parse Diagnostics: ");
        syntax_tree.diagnostics.print();
        println!("==============================");
    }

    // for expr in expressions.iter() {
    //     println!("------------------");
    //     expr.print(0);
    //     println!()
    // }

    let evaluator = Evaluator::new();
    let res = evaluator.evaluate(expressions);
    evaluator.diagnostics.print();
    println!("==============================");
    println!("Result: ");
    for v in res {
        println!("\t{}", v);
    }
    println!("==============================");

}
