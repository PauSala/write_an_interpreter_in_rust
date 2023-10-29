pub mod types;

use self::types::{Boolean, Integer, Null, Object, ReturnValue};
use crate::parser::ast_nodes::{
    expressions::{Expression, IfExpression},
    statements::Statement,
    AstNode,
};

pub fn eval_statements(statements: &Vec<Statement>) -> Result<Object, String> {
    let mut result: Result<Object, String> = Err("No statement found".to_string());
    for statement in statements {
        result = eval(AstNode::Statement(statement));
        if let Ok(uw) = &result {
            if let Object::ReturnValue(value) = uw {
                return Ok(*value.clone().value);
            }
        }
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

pub fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(Integer {
            value: -integer.value,
        }),
        _ => Object::Null(Null {}),
    }
}

pub fn eval_prefix_expression(operator: &str, obj: &Object) -> Result<Object, String> {
    match operator {
        "!" => eval_bang_operator_expression(obj),
        "-" => Ok(eval_minus_prefix_operator_expression(obj)),
        _ => Err(format!("Wrong prefix operator: {:?}", operator)),
    }
}

pub fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Object {
    match operator {
        "+" => Object::Integer(Integer {
            value: left.value + right.value,
        }),
        "-" => Object::Integer(Integer {
            value: left.value - right.value,
        }),
        "*" => Object::Integer(Integer {
            value: left.value * right.value,
        }),
        "/" => Object::Integer(Integer {
            value: left.value / right.value,
        }),
        "<" => Object::Boolean(Boolean {
            value: left.value < right.value,
        }),
        ">" => Object::Boolean(Boolean {
            value: left.value > right.value,
        }),
        "==" => Object::Boolean(Boolean {
            value: left.value == right.value,
        }),
        "!=" => Object::Boolean(Boolean {
            value: left.value != right.value,
        }),
        _ => Object::Null(Null {}),
    }
}

pub fn eval_infix_expression(
    operator: &str,
    left: &Object,
    right: &Object,
) -> Result<Object, String> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            return Ok(eval_integer_infix_expression(operator, left, right))
        }
        (Object::Boolean(left), Object::Boolean(right)) => match operator {
            "==" => {
                return Ok(Object::Boolean(Boolean {
                    value: left.value == right.value,
                }))
            }
            "!=" => {
                return Ok(Object::Boolean(Boolean {
                    value: left.value != right.value,
                }))
            }
            _ => return Ok(Object::Null(Null {})),
        },
        _ => Ok(Object::Null(Null {})),
    }
}

pub fn eval_if_expression(exp: &IfExpression) -> Result<Object, String> {
    let condition = eval(AstNode::Expression(
        exp.condition.as_ref().expect("Not an expression"),
    ));
    let condition = condition.expect("Expected a condition");
    if is_truthy(&condition) {
        return eval(AstNode::BlockStatement(
            exp.consequence.as_ref().expect("Expected an alternative"),
        ));
    } else if let Some(alternative) = &exp.alternative {
        return eval(AstNode::BlockStatement(&alternative));
    }

    Ok(Object::Null(Null {}))
}

pub fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null(_) => false,
        Object::Boolean(value) => value.value,
        _ => true,
    }
}

pub fn eval(node: AstNode) -> Result<Object, String> {
    match node {
        AstNode::Program(program) => return eval_statements(&program.statements),
        AstNode::Statement(statement) => match statement {
            Statement::ExpressionStatement(exp_stmt) => eval(AstNode::Expression(
                exp_stmt
                    .expression
                    .as_ref()
                    .expect("Some expression expected!")
                    .as_ref(),
            )),
            Statement::ReturnStatement(return_stmt) => {
                let result = eval(AstNode::Expression(
                    return_stmt.return_value.clone().unwrap().as_ref(),
                ));
                println!("ReturnStatement {:?}", result);
                Ok(Object::ReturnValue(ReturnValue {
                    value: Box::new(result.expect("Not a return statement")),
                }))
            }
            Statement::LetStatement(_) => todo!(),
        },
        AstNode::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => {
                eval(AstNode::IntegerLiteral(integer_literal))
            }
            Expression::PrefixExpression(prefix_exp) => eval(AstNode::PrefixExpression(prefix_exp)),
            Expression::Boolean(boolean) => eval(AstNode::Boolean(boolean)),
            Expression::InfixExpression(infix_exp) => eval(AstNode::InfixExpression(infix_exp)),
            Expression::IfExpression(if_exp) => eval(AstNode::IfExpression(if_exp)),
            Expression::BlockStatement(block_stmt) => eval(AstNode::BlockStatement(block_stmt)),
            Expression::Identifier(_) => todo!(),
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
        AstNode::InfixExpression(infix_exp) => {
            let left = eval(AstNode::Expression(
                infix_exp.left.as_ref().expect("Not a left Expression"),
            ));
            let right = eval(AstNode::Expression(
                infix_exp.right.as_ref().expect("Not a left Expression"),
            ));

            match (left, right) {
                (Ok(left), Ok(right)) => {
                    return eval_infix_expression(&infix_exp.operator, &left, &right)
                }
                (Err(_), _) | (_, Err(_)) => {
                    return Err("InfixExpression with error in some arms".to_string())
                }
            }
        }
        AstNode::BlockStatement(block_stmt) => eval_statements(&block_stmt.statements),
        AstNode::IfExpression(if_exp) => eval_if_expression(if_exp),
        _ => return Err(format!("Not implemented yet")),
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
                input: "5".to_string(),
                expected: 5,
            },
            Test {
                input: "-5".to_string(),
                expected: -5,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: 32,
            },
            Test {
                input: "20 + 2 * -10".to_string(),
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10".to_string(),
                expected: 60,
            },
            Test {
                input: "3 * (3 * 3) + 10".to_string(),
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: 50,
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
                input: "true".to_string(),
                expected: true,
            },
            Test {
                input: "false".to_string(),
                expected: false,
            },
            Test {
                input: "1 < 2".to_string(),
                expected: true,
            },
            Test {
                input: "1 > 2".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 1".to_string(),
                expected: true,
            },
            Test {
                input: "1 != 1".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 2".to_string(),
                expected: false,
            },
            Test {
                input: "1 != 2".to_string(),
                expected: true,
            },
            Test {
                input: "true != true".to_string(),
                expected: false,
            },
            Test {
                input: "true == true".to_string(),
                expected: true,
            },
            Test {
                input: "false == false".to_string(),
                expected: true,
            },
            Test {
                input: "true != false".to_string(),
                expected: true,
            },
            Test {
                input: "false != true".to_string(),
                expected: true,
            },
            Test {
                input: "(1 < 2) == true".to_string(),
                expected: true,
            },
            Test {
                input: "(1 < 2) == false".to_string(),
                expected: false,
            },
            Test {
                input: "(1 > 2) == true".to_string(),
                expected: false,
            },
            Test {
                input: "(1 > 2) == false".to_string(),
                expected: true,
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

    #[test]
    fn test_if_expressions() {
        struct Test {
            input: String,
            expected: Option<i64>,
        }

        let tests: Vec<Test> = vec![
            Test {
                input: "if (true) { 10 }".to_string(),
                expected: Some(10),
            },
            Test {
                input: "if (false) { 10 }".to_string(),
                expected: None,
            },
            Test {
                input: "if (1) { 10 }".to_string(),
                expected: Some(10),
            },
            Test {
                input: "if (1 < 2) { 10 }".to_string(),
                expected: Some(10),
            },
            Test {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: None,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: Some(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: Some(10),
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            match test.expected {
                Some(integer) => test_integer_object(&evaluated, integer),
                None => (),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test {
            input: String,
            expected: i64,
        }
        let tests: Vec<Test> = vec![
            Test {
                input: "return 10;".to_string(),
                expected: 10,
            },
            Test {
                input: "return 10; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;".to_string(),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(&evaluated, test.expected);
        }
    }
}
