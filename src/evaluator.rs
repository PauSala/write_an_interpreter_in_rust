pub mod types;

use self::types::{Integer, Object};
use crate::parser::ast_nodes::{expressions::Expression, statements::Statement, AstNode};

pub fn eval_statements(statements: Vec<Box<Statement>>) -> Option<Box<dyn Object>> {
    let mut result: Option<Box<dyn Object>> = None;
    for statement in statements {
        result = eval(AstNode::Statement(*statement));
    }
    result
}

pub fn eval(node: AstNode) -> Option<Box<dyn Object>> {
    match node {
        AstNode::Program(program) => return eval_statements(program.statements),
        AstNode::Statement(statement) => match statement {
            Statement::ExpressionStatement(exp_stmt) => {
                match exp_stmt
                    .expression
                    .expect("Some expression expected!")
                    .as_ref()
                {
                    Expression::Identifier(_) => None,
                    Expression::BlockStatement(_) => None,
                    Expression::IntegerLiteral(integer_literal) => {
                        eval(AstNode::IntegerLiteral(integer_literal))
                    }
                    Expression::Boolean(_) => None,
                    Expression::PrefixExpression(_) => None,
                    Expression::InfixExpression(_) => None,
                    Expression::IfExpression(_) => None,
                    Expression::CallExpression(_) => None,
                    Expression::FunctionLiteral(_) => None,
                }
            }
            Statement::LetStatement(_) => None,
            Statement::ReturnStatement(_) => None,
        },
        AstNode::IntegerLiteral(integer_literal) => {
            println!("Parsing Integer literal");
            return Some(Box::new(Integer {
                value: integer_literal.value,
            }));
        }
        _ => return None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{ast_nodes::AstNode, Parser},
    };

    use super::{
        eval,
        types::{Integer, Object},
    };

    fn test_eval(input: &str) -> Option<Box<dyn Object>> {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        return eval(AstNode::Program(program));
    }

    fn test_integer_object(obj: Box<dyn Object>, expected: u64) {
        let result: &Integer = obj.as_any().downcast_ref().expect("Not an Integer object");
        assert_eq!(result.value, expected);
    }

    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: u64,
        }
        let tests: Vec<Test> = vec![
            Test {
                expected: 5,
                input: "5".to_string(),
            },
            Test {
                expected: 10,
                input: "10".to_string(),
            },
        ];
        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(evaluated, test.expected);
        }
    }
}
