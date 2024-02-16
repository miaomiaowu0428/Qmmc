#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::fs::File;
use std::io::Read;
use std::path::Path;

use evaluate::Evaluator;

pub use crate::analyze::lex::Lexer;
pub use crate::analyze::syntax_tree::Parser;
use crate::evaluate::{RuntimeScope, Value};

mod analyze;
mod evaluate;

static PATH: &str = "G:/codefile/rust_source_file/Qmmc/resource/";

fn main() {
    let mut file =
        File::open(Path::new(&format!("{}{}", PATH, "test_if_else.qmm"))).expect("Could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Could not read file");
    let lexer = Lexer::new(&contents);
    let source_file = lexer.lex();
    // println!("Source File:");
    // // source_file.print();
    // println!("todo!()");
    // println!("tokens: ");
    // for token in source_file.iter() {
    //     println!("\t{:?}", token);
    // }


    let syntax_tree = Parser::new(source_file);
    let expressions = syntax_tree.parse();
    if !syntax_tree.diagnostics.is_empty() {
        println!("Parse Diagnostics: ");
        syntax_tree.diagnostics.print();
        println!("==============================");
    }

    // for expr in expressions.iter() {
    //     println!("------------------\nExpr:");
    //     expr.print_as_line(0);
    //     println!()
    // }

    // for expr in expressions.iter() {
    //     println!("------------------\nExpr:");
    //     let tokens = expr.to_token_vec();
    //     for token in tokens.iter() {
    //         print!("{} ", token);
    //     }
    //     println!()
    // }

    // println!("{:#?}", expressions);

    let evaluator = Evaluator::new();
    let res = evaluator.evaluate(expressions.clone());
    evaluator.diagnostics.print();


    let expr_and_res = expressions.iter().zip(res.iter());

    let mut i = 1;
    for (expr, res) in expr_and_res {
        println!("------------------\nExpr: {i}{:>8}|-> {:?}","", res);
        i += 1;
        expr.print_as_line(0);
        println!()
    }


    // print_res(res);

    print_scope_variables(&evaluator.scope);
}

fn print_scope_variables(scope: &RuntimeScope) {
    println!("==============================\nVariables:\n{:?}", scope);
    println!("==============================");
}

fn print_res(res: Vec<Value>) {
    println!("Result: ");
    for v in res {
        println!("\t{}", v);
    }
    println!("==============================");
}
