use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

use crate::evaluator::environtment::Environtment;
use crate::evaluator::eval;
use crate::evaluator::types::TObject;
use crate::lexer::Lexer;
use crate::parser::ast_nodes::AstNode;
use crate::parser::Parser;

fn print_parser_errors(errors: &Vec<String>) {
    if errors.len() > 0 {
        println!("There are some parsing errors: ");
    }
    for error in errors {
        println!("\t{}", error);
    }
}

pub fn start() {
    let env = Rc::new(RefCell::new(Environtment::new()));
    loop {
        print!(">> "); // Custom prompt
        io::stdout().flush().expect("Failed to flush stdout");

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        // Trim leading and trailing whitespaces
        let input = input.trim();

        if input == "exit" {
            println!("Goodbye!");
            break; // Exit the loop if the user types '!q'
        }

        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        print_parser_errors(&parser.errors);
        if parser.errors.len() == 0 {
            let evaluated = eval(AstNode::Program(program), env.clone());
            match evaluated {
                Ok(evaluation) =>  println!("{}\n", evaluation.inspect()),
                Err(err) => println!("{}\n", err.message)
            }
   
        }
    }
}
