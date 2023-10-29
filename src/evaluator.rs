pub mod types;


use self::types::{Boolean, Integer, Object};
use crate::parser::ast_nodes::{expressions::Expression, statements::Statement, AstNode};

pub fn eval_statements(statements: Vec<Box<Statement>>) -> Result<Object, String> {
    let mut result: Result<Object, String> = Err("No statement found".to_string());
    for statement in statements {
        result = eval(AstNode::Statement(*statement));
    }
    result
}

pub fn eval_bang_operator_expression(right: &Object) -> Result<Object, String> {
    match right {
        Object::Boolean(value) => match value.value {
            true => Ok(Object::Boolean(Boolean { value: false })),
            false => Ok(Object::Boolean(Boolean { value: true })),
        },
        _ => Ok(Object::Boolean(Boolean { value: false })),
    }
}

pub fn eval_minus_prefix_operator_expression(right: &Object) -> Result<Object, String> {
    match right {
        Object::Integer(integer) => Ok(Object::Integer(Integer {
            value: -integer.value,
        })),
        _ => Err(format!("Trying to evaluate a minus expression but {:?} was found in the right arm", right )),
    }
}

pub fn eval_prefix_expression(operator: &str, obj: &Object) -> Result<Object, String> {
    match operator {
        "!" => eval_bang_operator_expression(obj),
        "-" => eval_minus_prefix_operator_expression(obj),
        _ => Err(format!("Wrong prefix operator: {:?}", operator)),
    }
}

pub fn eval(node: AstNode) -> Result<Object, String> {
    match node {
        AstNode::Program(program) => return eval_statements(program.statements),
        AstNode::Statement(statement) => match statement {
            Statement::ExpressionStatement(exp_stmt) => eval(AstNode::Expression(
                exp_stmt
                    .expression
                    .expect("Some expression expected!")
                    .as_ref(),
            )),
            Statement::LetStatement(_) => todo!(),
            Statement::ReturnStatement(_) => todo!(),
        },
        AstNode::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => {
                eval(AstNode::IntegerLiteral(integer_literal))
            }
            Expression::PrefixExpression(prefix_expression) => {
                eval(AstNode::PrefixExpression(prefix_expression))
            }
            Expression::Boolean(boolean) => eval(AstNode::Boolean(boolean)),
            Expression::Identifier(_) => todo!(),
            Expression::BlockStatement(_) => todo!(),
            Expression::InfixExpression(_) => todo!(),
            Expression::IfExpression(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
        },
        AstNode::IntegerLiteral(integer_literal) => {
            return Ok(Object::Integer(Integer {
                value: integer_literal.value,
            }));
        }
        AstNode::Boolean(boolean) => {
            return Ok(Object::Boolean(Boolean {
                value: boolean.value,
            }));
        }
        AstNode::PrefixExpression(prefix_exp) => {
            let right = eval(AstNode::Expression(
                &prefix_exp
                    .right
                    .as_ref()
                    .expect("No right expression found"),
            ));
            return eval_prefix_expression(
                &prefix_exp.operator,
                &right.expect("Not an Object type"),
            );
        }
        _ => return Err(format!("Not implemented yet"))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{ast_nodes::AstNode, Parser},
    };

    use super::{eval, types::Object};

    fn test_eval(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        return eval(AstNode::Program(program));
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(result) => assert_eq!(result.value, expected),
            _ => panic!("Not an integer object: {:?}", obj),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: i64,
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
            Test {
                expected: -5,
                input: "-5".to_string(),
            },
            Test {
                expected: -10,
                input: "-10".to_string(),
            },
        ];
        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(&evaluated, test.expected);
        }
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(result) => assert_eq!(result.value, expected),
            _ => panic!("Not an integer object: {:?}", obj),
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Test {
            input: String,
            expected: bool,
        }
        let tests: Vec<Test> = vec![
            Test {
                expected: true,
                input: "true".to_string(),
            },
            Test {
                expected: false,
                input: "false".to_string(),
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_boolean_object(&evaluated, test.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        struct Test {
            input: String,
            expected: bool,
        }
        let tests: Vec<Test> = vec![
            Test {
                input: "!true".to_string(),
                expected: false,
            },
            Test {
                input: "!false".to_string(),
                expected: true,
            },
            Test {
                input: "!5".to_string(),
                expected: false,
            },
            Test {
                input: "!!true".to_string(),
                expected: true,
            },
            Test {
                input: "!!false".to_string(),
                expected: false,
            },
            Test {
                input: "!!5".to_string(),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_boolean_object(&evaluated, test.expected);
        }
    }
}
