#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::fs::File;
use std::io::Read;
use std::path::Path;

pub use crate::analyze::lex::Lexer;
use crate::analyze::syntax_tree::Expression;
pub use crate::analyze::syntax_tree::Parser;
use crate::compile::{ByteCode, Compiler};
use crate::runtime::{Runtime, RuntimeScope, Value};

mod analyze;
mod runtime;
mod compile;

static PATH: &str = "./resource/";

fn main() {
    let mut file =
        File::open(Path::new(&format!("{}{}", PATH, "test_function.qmm"))).expect("Could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Could not read file");
    let lexer = Lexer::new(&contents);
    let tokens = lexer.lex();


    let syntax_tree = Parser::new(tokens);
    let expressions = syntax_tree.parse();
    if !syntax_tree.diagnostics.is_empty() {
        println!("Parse Diagnostics: ");
        syntax_tree.diagnostics.print();
        println!("==============================");
    }

    // show_input(&expressions);


    let static_analyzer = Compiler::new();
    let checked_expressions = static_analyzer.analyse(expressions);

    // show_ByteCode(&checked_expressions);

    // show_static_scope(&static_analyzer);


    if !static_analyzer.diagnostics.is_empty() {
        println!("Static Analysis Diagnostics: ");
        static_analyzer.diagnostics.print();
        println!("==============================");
    } else {
        println!("Outputs:");
        let evaluator = Runtime::new();
        let values = evaluator.evaluate(checked_expressions);
        if !evaluator.diagnostics.is_empty() {
            println!("==============================");
            println!("Evaluation Diagnostics: ");
            evaluator.diagnostics.print();
            println!("==============================");
        }

        // println!("=================================\n{:#?}", evaluator.scope);
    }
}

fn show_input(expressions: &Vec<Expression>) {
    println!("Input Expressions: ");
    for expression in expressions {
        println!("{}", expression)
    }
    println!("==============================");
}

fn show_static_scope(static_analyzer: &Compiler) {
    println!("\n\nScope:\n{:#?}", static_analyzer.scope);
}

fn show_ByteCode(checked_expressions: &Vec<ByteCode>) {
    println!("\n\nByteCode:");
    for expression in checked_expressions {
        println!("{:#?}", expression);
    }
}

fn print_scope(scope: &RuntimeScope) {
    println!("==============================\nVariables:\n{}", scope.variables_to_string());
    println!("==============================");
}

fn print_res(res: Vec<Value>) {
    println!("Result: ");
    for v in res {
        println!("\t{}", v);
    }
    println!("==============================");
}
