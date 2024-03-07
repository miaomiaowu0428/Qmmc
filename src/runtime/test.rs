// #[cfg(test)]
// mod tests {
//     use std::fs::File;
//     use std::io::Read;
//     use std::path::Path;

//     use crate::{
//         runtime::Runtime,
//         Lexer,
//         Parser
//     };
//     use crate::runtime::Value;

//     static PATH: &str = "./resource/";

//     #[test]
//     //从resource下读取文件并解释,匹配结果
//     fn test_function_obj() {
//         let mut file =
//             File::open(Path::new(&format!("{}{}", PATH, "test_function_obj.qmm"))).expect("Could not open file");
//         let mut contents = String::new();
//         file.read_to_string(&mut contents)
//             .expect("Could not read file");
//         let lexer = Lexer::new(&contents);
//         let source_file = lexer.lex();
//         let parse = Parser::new(source_file);
//         let expressions = parse.parse();
//         let evaluator = Runtime::new();
//         let res = evaluator.evaluate(expressions.clone());

//         assert_eq!(res[0], Value::None);
//         assert_eq!(res[1], Value::None);
//         assert_eq!(res[2], Value::i32(2034324));
//     }

//     #[test]
//     fn test_function() {
//         let mut file =
//             File::open(Path::new(&format!("{}{}", PATH, "test_function.qmm"))).expect("Could not open file");
//         let mut contents = String::new();
//         file.read_to_string(&mut contents)
//             .expect("Could not read file");
//         let lexer = Lexer::new(&contents);
//         let source_file = lexer.lex();
//         let parse = Parser::new(source_file);
//         let expressions = parse.parse();
//         let evaluator = Runtime::new();
//         let res = evaluator.evaluate(expressions.clone());


//         assert_eq!(res[2], Value::None);
//         assert_eq!(res[3], Value::None);
//         assert_eq!(res[4], Value::None);
//         assert_eq!(res[5], Value::None);
//         assert_eq!(res[6], Value::i32(153));
//         assert_eq!(res[7], Value::i32(370));
//         assert_eq!(res[8], Value::i32(371));
//         assert_eq!(res[9], Value::i32(407));
//     }


//     #[test]
//     fn test_closure() {
//         let mut file =
//             File::open(Path::new(&format!("{}{}", PATH, "test_closure.qmm"))).expect("Could not open file");
//         let mut contents = String::new();
//         file.read_to_string(&mut contents)
//             .expect("Could not read file");
//         let lexer = Lexer::new(&contents);
//         let source_file = lexer.lex();
//         let parse = Parser::new(source_file);
//         let expressions = parse.parse();
//         let evaluator = Runtime::new();
//         let res = evaluator.evaluate(expressions.clone());

//         assert_eq!(res[1], Value::None);
//         assert_eq!(res[2], Value::None);
//         assert_eq!(res[4], Value::None);
//     }
// }