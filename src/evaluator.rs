pub mod environtment;
pub mod eval_functions;
pub mod types;

use std::{cell::RefCell, rc::Rc};

use self::{
    environtment::Environtment,
    eval_functions::{
        apply_function, eval_block_statement, eval_expressions, eval_identifier,
        eval_if_expression, eval_infix_expression, eval_prefix_expression, eval_program, new_error,
    },
    types::{Boolean, Error, Function, Integer, Object, ReturnValue},
};
use crate::parser::ast_nodes::{expressions::Expression, statements::Statement, AstNode};

pub fn eval(node: AstNode, env: Rc<RefCell<Environtment>>) -> Result<Object, Error> {
    match node {
        AstNode::Program(program) => return eval_program(&program.statements, env),
        AstNode::Statement(statement) => match statement {
            Statement::ExpressionStatement(exp_stmt) => eval(
                AstNode::Expression(
                    exp_stmt
                        .expression
                        .as_ref()
                        .expect("Some expression expected!")
                        .as_ref(),
                ),
                env,
            ),
            Statement::ReturnStatement(return_stmt) => {
                let result = eval(
                    AstNode::Expression(return_stmt.return_value.clone().unwrap().as_ref()),
                    env,
                );
                Ok(Object::ReturnValue(ReturnValue {
                    value: Box::new(result.expect("Not a return statement")),
                }))
            }
            Statement::LetStatement(let_stmt) => {
                let value = eval(
                    AstNode::Expression(let_stmt.value.clone().unwrap().as_ref()),
                    env.clone(),
                );
                match value {
                    Err(err) => Err(err),
                    Ok(value) => {
                        env.borrow_mut().set(&let_stmt.name.value, value.clone());
                        Ok(value)
                    }
                }
            }
        },
        AstNode::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => {
                eval(AstNode::IntegerLiteral(integer_literal), env)
            }
            Expression::PrefixExpression(prefix_exp) => {
                eval(AstNode::PrefixExpression(prefix_exp), env)
            }
            Expression::Boolean(boolean) => eval(AstNode::Boolean(boolean), env),
            Expression::InfixExpression(infix_exp) => {
                eval(AstNode::InfixExpression(infix_exp), env)
            }
            Expression::IfExpression(if_exp) => eval(AstNode::IfExpression(if_exp), env),
            Expression::BlockStatement(block_stmt) => {
                eval(AstNode::BlockStatement(block_stmt), env)
            }
            Expression::Identifier(identifier) => eval(AstNode::Identifier(identifier), env),
            Expression::FunctionLiteral(function) => eval(AstNode::FunctionLiteral(function), env),
            Expression::CallExpression(call_expe) => eval(AstNode::CallExpression(call_expe), env),
        },
        AstNode::IntegerLiteral(integer_literal) => {
            return Ok(Object::Integer(Integer {
                value: integer_literal.value,
            }));
        }
        AstNode::Identifier(identifier) => {
            return eval_identifier(identifier, env);
        }
        AstNode::Boolean(boolean) => {
            return Ok(Object::Boolean(Boolean {
                value: boolean.value,
            }));
        }
        AstNode::PrefixExpression(prefix_exp) => {
            let right = eval(
                AstNode::Expression(
                    &prefix_exp
                        .right
                        .as_ref()
                        .expect("No right expression found"),
                ),
                env,
            );
            return eval_prefix_expression(
                &prefix_exp.operator,
                &right.expect("Not an Object type"),
            );
        }
        AstNode::InfixExpression(infix_exp) => {
            let left = eval(
                AstNode::Expression(infix_exp.left.as_ref().expect("Not a left Expression")),
                env.clone(),
            );
            let right = eval(
                AstNode::Expression(infix_exp.right.as_ref().expect("Not a left Expression")),
                env,
            );

            match (left, right) {
                (Ok(left), Ok(right)) => {
                    return eval_infix_expression(&infix_exp.operator, &left, &right)
                }
                (Err(_), _) | (_, Err(_)) => {
                    return Err(new_error(&format!(
                        "InfixExpression with error in some arms"
                    )));
                }
            }
        }
        AstNode::BlockStatement(block_stmt) => eval_block_statement(&block_stmt, env),
        AstNode::CallExpression(call_exp) => {
            let function = eval(AstNode::Expression(call_exp.function.as_ref()), env.clone());
            match function {
                Err(fun) => return Err(fun),
                Ok(node) => match node {
                    Object::Function(function) => {
                        let args = eval_expressions(&call_exp.args, env);
                        match args {
                            Err(e) => return Err(e),
                            Ok(val) => return apply_function(&Object::Function(function), &val),
                        }
                    }
                    _ => return Err(new_error("Not a function")),
                },
            }
        }
        AstNode::IfExpression(if_exp) => eval_if_expression(if_exp, env),
        AstNode::FunctionLiteral(function) => {
            let params = &function.parameters;
            let body = function.body.clone();
            return Ok(Object::Function(Function {
                params: params.to_vec(),
                body: Rc::new(body.unwrap()),
                env,
            }));
        }
        _ => return Err(new_error(&format!("Not implemented yet"))),
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        lexer::Lexer,
        parser::{
            ast_nodes::{AstNode, Node},
            Parser,
        },
    };

    use super::{
        environtment::Environtment,
        eval,
        types::{Error, Object},
    };

    fn test_eval(input: &str) -> Result<Object, Error> {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Environtment::new();
        return eval(AstNode::Program(program), Rc::new(RefCell::new(env)));
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
            Test {
                input: "if (10 > 1) {
                    if (10 > 1) {
                      return 10;
                    }
                    return 1;
                  }"
                .to_string(),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(&evaluated, test.expected);
        }
    }

    #[test]
    fn test_error_handling() {
        struct Test {
            input: String,
            expected: String,
        }
        let tests: Vec<Test> = vec![
            Test {
                input: "5 + true;".to_string(),
                expected: "type mismatch: INTEGER + BOOLEAN".to_string(),
            },
            Test {
                input: "-true".to_string(),
                expected: "unknown operator: -BOOLEAN".to_string(),
            },
            Test {
                input: "5; true + false; 6".to_string(),
                expected: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "if (10 > 1) { true + false; }".to_string(),
                expected: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "foobar".to_string(),
                expected: "identifier not found: foobar".to_string(),
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input);
            match evaluated {
                Ok(_) => panic!("Error expected {:?}", evaluated),
                Err(err) => assert_eq!(err.message, test.expected),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            expected: i64,
        }
        let tests: Vec<Test> = vec![
            Test {
                input: "let a = 5; a;".to_string(),
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;".to_string(),
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;".to_string(),
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;".to_string(),
                expected: 15,
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(&evaluated, test.expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        match evaluated {
            Err(_) => panic!("Not a function!"),
            Ok(function) => match function {
                Object::Function(f) => {
                    assert_eq!(f.params.len(), 1);
                    assert_eq!(f.params[0].string(), "x");
                    assert_eq!(f.body.as_ref().string(), "(x + 2)");
                }
                _ => panic!("Not a function!"),
            },
        }
    }

    #[test]
    fn test_function_application() {
        struct Test {
            input: String,
            expected: i64,
        }
        let tests: Vec<Test> = vec![
            Test {
                input: "let identity = fn(x) { x; }; identity(5);".to_string(),
                expected: 5,
            },
            Test {
                input: "let identity = fn(x) { return x; }; identity(5);".to_string(),
                expected: 5,
            },
            Test {
                input: "let double = fn(x) { x * 2; }; double(5);".to_string(),
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);".to_string(),
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(),
                expected: 20,
            },
            Test {
                input: "fn(x) { x; }(5)".to_string(),
                expected: 5,
            },
        ];

        for test in tests {
            let evaluated = test_eval(&test.input).expect("Get None instead of an Object");
            test_integer_object(&evaluated, test.expected);
        }
    }
}
