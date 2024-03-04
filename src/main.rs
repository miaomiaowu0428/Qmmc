#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::Command;

use colored::Colorize;
use inkwell::context::Context;
use inkwell::OptimizationLevel;

pub use crate::analyze::lex::Lexer;
use crate::analyze::syntax_tree::Expression;
pub use crate::analyze::syntax_tree::Parser;
use crate::compile::{CheckedExpression, StaticAnalyzer};
use crate::IR_building::IRBuilder;

mod analyze;
mod runtime;
mod compile;
mod IR_building;

static PATH: &str = "./resource/";
static RES_PATH: &str = "./res/";

static FILE_NAME: &str = "main_ret_8";

fn main() {
    let mut file =
        File::open(Path::new(&format!("{}{}{}", PATH, FILE_NAME, ".qmm"))).expect("Could not open file");
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
        println!("IR:");

        let context = Context::create();
        let module = context.create_module(FILE_NAME);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let ir_builder = IRBuilder::new(&context, module, builder, execution_engine);

        ir_builder.build_irs(checked_expressions);

        ir_builder.print_res();


        {
            let RES_FILE = &*format!("{}{}", RES_PATH, FILE_NAME);

            let test_path = format!("{}{}", RES_FILE, ".ll");
            ir_builder.save_as(&test_path);

            let llc_source = format!("{}{}", RES_FILE, ".ll");
            let llc_output = Command::new("llc")
                .arg(llc_source)
                .output()
                .expect("Failed to execute llc command");

            if llc_output.status.success() {
                println!("{}", "successfully compiled to .ll".green());
            } else {
                eprintln!("llc command failed: {}", String::from_utf8_lossy(&llc_output.stderr));
            }


            let clang_source = format!("{}{}", RES_FILE, ".s");
            let clang_output = Command::new("clang")
                .arg(clang_source)
                .arg("-o")
                .arg(RES_FILE)
                .output()
                .expect("Failed to execute clang command");

            if clang_output.status.success() {
                println!("{}", "successfully compiled to .s".green());
            } else {
                eprintln!("clang command failed: {}", String::from_utf8_lossy(&clang_output.stderr));
            }

            let output = Command::new(RES_FILE)
                .output()
                .expect("Failed to execute command");
            let exit_code = output.status.code().unwrap();

            println!("\nExit Code of main(): {}\n\n\n", exit_code);
        }

        ir_builder.diagnostics.print();
    }
}

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
