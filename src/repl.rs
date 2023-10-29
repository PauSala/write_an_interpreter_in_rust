use std::io;
use std::io::Write;

use crate::lexer::Lexer;
use crate::parser::ast::Node;
use crate::parser::Parser;

fn print_parser_errors(errors: &Vec<String>) {
    println!("There are some parsing errors: ");
    for error in errors {
        println!("\t{}", error);
    }
}

pub fn start() {
    loop {
        print!(">> "); // Custom prompt
        io::stdout().flush().expect("Failed to flush stdout");

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        // Trim leading and trailing whitespaces
        let input = input.trim();

        if input == "!q" {
            println!("Goodbye!");
            break; // Exit the loop if the user types '!q'
        }

        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        print_parser_errors(&parser.errors);
        if parser.errors.len() == 0 {
            println!("{}\n", program.string());
        }
    }
}
