use std::{rc::Rc, cell::RefCell};

use crate::parser::ast_nodes::{statements::Statement, AstNode, expressions::{BlockStatement, IfExpression, Identifier, Expression}};

use super::{environtment::{Environtment, enclosed_environtment}, types::{Object, Error, Boolean, Integer, Null, Function}, eval};

pub fn eval_program(statements: &Vec<Statement>, env: Rc<RefCell<Environtment>>) -> Result<Object, Error> {
    let mut result: Result<Object, Error> = Err(new_error(&format!("No statement found")));
    for statement in statements {
        result = eval(AstNode::Statement(statement), env.clone());
        match &result {
            Ok(uw) => {
                if let Object::ReturnValue(value) = uw {
                    return Ok(*value.clone().value);
                }
            }
            Err(_) => return result,
        }
    }
    result
}

pub fn eval_block_statement(
    block: &BlockStatement,
    env: Rc<RefCell<Environtment>>,
) -> Result<Object, Error> {
    let mut result: Result<Object, Error> = Err(new_error(&format!("No statement found")));
    for statement in &block.statements {
        result = eval(AstNode::Statement(&statement), env.clone());
        match &result {
            Ok(uw) => match uw {
                Object::ReturnValue(return_value) => {
                    return Ok(Object::ReturnValue(return_value.clone()))
                },
                _ => ()
            },
            Err(_) => return result,
        }
    }
    result
}

pub fn eval_bang_operator_expression(right: &Object) -> Result<Object, Error> {
    match right {
        Object::Boolean(value) => match value.value {
            true => Ok(Object::Boolean(Boolean { value: false })),
            false => Ok(Object::Boolean(Boolean { value: true })),
        },
        _ => Ok(Object::Boolean(Boolean { value: false })),
    }
}

pub fn eval_minus_prefix_operator_expression(right: &Object) -> Result<Object, Error> {
    match right {
        Object::Integer(integer) => Ok(Object::Integer(Integer {
            value: -integer.value,
        })),
        _ => Err(new_error(&format!(
            "unknown operator: -{}",
            right.str_type()
        ))),
    }
}

pub fn eval_prefix_expression(operator: &str, right: &Object) -> Result<Object, Error> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(new_error(&format!(
            "unknown operator: {} {:?}",
            operator,
            right.str_type()
        ))),
    }
}

pub fn eval_integer_infix_expression(
    operator: &str,
    left: &Integer,
    right: &Integer,
) -> Result<Object, Error> {
    match operator {
        "+" => Ok(Object::Integer(Integer {
            value: left.value + right.value,
        })),
        "-" => Ok(Object::Integer(Integer {
            value: left.value - right.value,
        })),
        "*" => Ok(Object::Integer(Integer {
            value: left.value * right.value,
        })),
        "/" => Ok(Object::Integer(Integer {
            value: left.value / right.value,
        })),
        "<" => Ok(Object::Boolean(Boolean {
            value: left.value < right.value,
        })),
        ">" => Ok(Object::Boolean(Boolean {
            value: left.value > right.value,
        })),
        "==" => Ok(Object::Boolean(Boolean {
            value: left.value == right.value,
        })),
        "!=" => Ok(Object::Boolean(Boolean {
            value: left.value != right.value,
        })),
        _ => Err(new_error(&format!("unknown operator: {}", operator,))),
    }
}

pub fn eval_infix_expression(
    operator: &str,
    left: &Object,
    right: &Object,
) -> Result<Object, Error> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            return eval_integer_infix_expression(operator, left, right)
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
            _ => {
                return Err(new_error(&format!(
                    "unknown operator: {} {} {}",
                    left.str_type(),
                    operator,
                    right.str_type()
                )))
            }
        },
        (l, r) => {
            if l.str_type() != r.str_type() {
                Err(new_error(&format!(
                    "type mismatch: {} {} {}",
                    left.str_type(),
                    operator,
                    right.str_type()
                )))
            } else {
                Err(new_error(&format!(
                    "unknown operator: {} {} {}",
                    left.str_type(),
                    operator,
                    right.str_type()
                )))
            }
        }
    }
}

pub fn eval_if_expression(exp: &IfExpression, env: Rc<RefCell<Environtment>>) -> Result<Object, Error> {
    let condition = eval(
        AstNode::Expression(exp.condition.as_ref().expect("Not an expression")),
        env.clone(),
    );
    let condition = condition.expect("Expected a condition");
    if is_truthy(&condition) {
        return eval(
            AstNode::BlockStatement(exp.consequence.as_ref().expect("Expected an alternative")),
            env,
        );
    } else if let Some(alternative) = &exp.alternative {
        return eval(AstNode::BlockStatement(&alternative), env);
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

pub fn new_error(str: &str) -> Error {
    Error {
        message: str.to_string(),
    }
}

pub fn eval_identifier(node: &Identifier, env: Rc<RefCell<Environtment>>) -> Result<Object, Error> {
    let val = env.borrow_mut().get(&node.value);
    match val {
        None => Err(new_error(&format!("identifier not found: {}", node.value))),
        Some(value) => Ok(value.clone().clone()),
    }
}

pub fn eval_expressions(
    exps: &Vec<Rc<Expression>>,
    env: Rc<RefCell<Environtment>>,
) -> Result<Vec<Object>, Error> {
    let mut result: Vec<Object> = vec![];
    for exp in exps {
        let evaluated = eval(AstNode::Expression(&exp), env.clone());
        match evaluated {
            Err(e) => return Err(e),
            Ok(res) => result.push(res),
        }
    }
    Ok(result)
}

pub fn apply_function( func: &Object, args: &Vec<Object>) -> Result<Object, Error> {
    if let Object::Function(function) = func {
        let extended_env = extend_function_env(function, args);
        let evaluated = eval(AstNode::BlockStatement(function.body.as_ref()), Rc::new(RefCell::new(extended_env)));
        if let Ok(object) = evaluated {
            return unwrap_return_value(object);
        }else{
            Err(new_error(&format!("not an object {:?}", func.str_type())))
        }
    }else{
        Err(new_error(&format!("not a function {:?}", func.str_type())))
    }
}

pub fn extend_function_env(func: &Function, args: &Vec<Object>) -> Environtment {
    let mut env = enclosed_environtment(func.env.clone());
    for (index, param) in func.params.iter().enumerate() {
        let val = &args[index];
        env.set(&param.value, val.clone());
    }   
    env
}

pub fn unwrap_return_value(obj: Object) -> Result<Object, Error> {
    match obj {
        Object::ReturnValue(returnvalue) => Ok(*returnvalue.clone().value),
        _ => Ok(obj)
    }
}
