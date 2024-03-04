#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::Command;

use inkwell::context::Context;
use inkwell::OptimizationLevel;

pub use crate::analyze::lex::Lexer;
use crate::analyze::syntax_tree::Expression;
pub use crate::analyze::syntax_tree::Parser;
use crate::compile::{CheckedExpression, StaticAnalyzer};
use crate::IR_building::IRBuilder;
use crate::runtime::{RuntimeScope, Value};

mod analyze;
mod runtime;
mod compile;
mod IR_building;

static PATH: &str = "./resource/";
static RES_PATH: &str = "./res/";

static FILE_NAME: &str = "main_ret_8";

fn main() {
    let mut file =
        File::open(Path::new(&format!("{}{}{}", PATH, FILE_NAME,".qmm"))).expect("Could not open file");
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


    let static_analyzer = StaticAnalyzer::new();
    let checked_expressions = static_analyzer.analyse(expressions);

    // show_ByteCode(&checked_expressions);

    // show_static_scope(&static_analyzer);


    if !static_analyzer.diagnostics.is_empty() {
        println!("Static Analysis Diagnostics: ");
        static_analyzer.diagnostics.print();
        println!("==============================");
    } else {
        println!("Outputs:\n");

        let context = Context::create();
        let module = context.create_module(FILE_NAME);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let ir_builder = IRBuilder::new(&context, module, builder, execution_engine);

        ir_builder.build_irs(checked_expressions);

        ir_builder.print_res();


        let test_path = format!("{}{}{}", RES_PATH, FILE_NAME,".ll");
        // ir_builder.save_as(&test_path);

        let output = Command::new(format!("{}{}", RES_PATH, FILE_NAME,))
            .output()
            .expect("Failed to execute command");
        let exit_code = output.status.code().unwrap();
        println!("Exit Code of Main(): {}\n\n\n", exit_code);

        ir_builder.diagnostics.print();
    }
}


// fn main() {
//     let mut file =
//         File::open(Path::new(&format!("{}{}", PATH, "test_function.qmm"))).expect("Could not open file");
//     let mut contents = String::new();
//     file.read_to_string(&mut contents)
//         .expect("Could not read file");
//     let lexer = Lexer::new(&contents);
//     let tokens = lexer.lex();
//
//
//     let syntax_tree = Parser::new(tokens);
//     let expressions = syntax_tree.parse();
//     if !syntax_tree.diagnostics.is_empty() {
//         println!("Parse Diagnostics: ");
//         syntax_tree.diagnostics.print();
//         println!("==============================");
//     }
//
//     // show_input(&expressions);
//
//
//     let static_analyzer = StaticAnalyzer::new();
//     let checked_expressions = static_analyzer.analyse(expressions);
//
//     // show_ByteCode(&checked_expressions);
//
//     // show_static_scope(&static_analyzer);
//
//
//     if !static_analyzer.diagnostics.is_empty() {
//         println!("Static Analysis Diagnostics: ");
//         static_analyzer.diagnostics.print();
//         println!("==============================");
//     } else {
//         println!("Outputs:");
//         let evaluator = Runtime::new();
//         let values = evaluator.evaluate(checked_expressions);
//         if !evaluator.diagnostics.is_empty() {
//             println!("==============================");
//             println!("Evaluation Diagnostics: ");
//             evaluator.diagnostics.print();
//             println!("==============================");
//         }
//
//         // println!("=================================\n{:#?}", evaluator.scope);
//     }
// }

fn show_input(expressions: &Vec<Expression>) {
    println!("Input Expressions: ");
    for expression in expressions {
        println!("{}", expression)
    }
    println!("==============================");
}

fn show_static_scope(static_analyzer: &StaticAnalyzer) {
    println!("\n\nScope:\n{:#?}", static_analyzer.scope);
}

fn show_ByteCode(checked_expressions: &Vec<CheckedExpression>) {
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
