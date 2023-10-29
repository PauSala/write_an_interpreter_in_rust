use std::rc::Rc;

use crate::token::Token;

use super::{expressions::{Expression, Identifier}, Node};

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Rc<Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
       self.token.literal.clone()
    }
    fn string(&self) -> String {
        match &self.expression {
            None => "".to_string(),
            Some(value) => value.string(),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Rc<Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
       self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        let value = self.token_literal();
        let name = self.name.string();
        buffer.push_str(&format!("{} ", value.clone()));
        buffer.push_str(&name);
        buffer.push_str("=");
        if let Some(value) = &self.value {
            buffer.push_str(&value.string());
        }
        buffer.push_str(";");
        buffer
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Rc<Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        let value = self.token_literal();
        buffer.push_str(&format!("{} ", value.clone()));
        if let Some(value) = &self.return_value {
            buffer.push_str(&value.string());
        }
        buffer.push_str(";");
        buffer
    }
}

#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::ExpressionStatement(inner) => inner.token_literal(),
            Statement::LetStatement(inner) => inner.token_literal(),
            Statement::ReturnStatement(inner) => inner.token_literal(),
        }
    }
    fn string(&self) -> String {
        match self {
            Statement::ExpressionStatement(inner) => inner.string(),
            Statement::LetStatement(inner) => inner.string(),
            Statement::ReturnStatement(inner) => inner.string(),
        }
    }
}
