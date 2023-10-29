use std::rc::Rc;

use crate::token::Token;

use super::{statements::Statement, Node};

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token, // the token.IDENT token
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Rc<Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str("(");
        buffer.push_str(&self.operator);
        if let Some(rigth) = &self.right {
            buffer.push_str(&rigth.string())
        }
        buffer.push_str(")");
        buffer
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Rc<Expression>>,
    pub operator: String,
    pub right: Option<Rc<Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str("(");
        if let Some(left) = &self.left {
            buffer.push_str(&left.string());
        }
        buffer.push_str(" ");
        buffer.push_str(&self.operator);
        buffer.push_str(" ");
        if let Some(rigth) = &self.right {
            buffer.push_str(&rigth.string())
        }
        buffer.push_str(")");
        buffer
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        for statement in &self.statements {
            buffer.push_str(&statement.string())
        }
        buffer
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Option<Rc<Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str("if");
        if let Some(condition) = &self.condition {
            buffer.push_str(&condition.string());
        }
        buffer.push_str(" ");
        if let Some(consequence) = &self.consequence {
            buffer.push_str(&consequence.string());
        }
        buffer.push_str(" ");
        if let Some(alternative) = &self.alternative {
            buffer.push_str("else ");
            buffer.push_str(&alternative.string());
        }
        buffer
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.token_literal());
        buffer.push_str("(");
        let params = self
            .parameters
            .iter()
            .map(|p| p.string())
            .collect::<Vec<String>>()
            .join(",");
        buffer.push_str(&params);
        buffer.push_str(")");
        buffer.push_str(&self.body.as_ref().expect("Not a string").string());
        buffer
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Rc<Expression>,
    pub args:  Vec<Rc<Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.function.string());
        buffer.push_str("(");
        let params = self
            .args
            .iter()
            .map(|p| p.string())
            .collect::<Vec<String>>()
            .join(",");
        buffer.push_str(&params);
        buffer.push_str(")");
        buffer
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    BlockStatement(BlockStatement),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    CallExpression(CallExpression),
    FunctionLiteral(FunctionLiteral),
}

impl Node for Expression{

    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(inner) => inner.token_literal(),
            Expression::BlockStatement(inner) => inner.token_literal(),
            Expression::IntegerLiteral(inner) => inner.token_literal(),
            Expression::Boolean(inner) => inner.token_literal(),
            Expression::PrefixExpression(inner) => inner.token_literal(),
            Expression::InfixExpression(inner) => inner.token_literal(),
            Expression::IfExpression(inner) => inner.token_literal(),
            Expression::CallExpression(inner) => inner.token_literal(),
            Expression::FunctionLiteral(inner) => inner.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(inner) => inner.string(),
            Expression::BlockStatement(inner) => inner.string(),
            Expression::IntegerLiteral(inner) => inner.string(),
            Expression::Boolean(inner) => inner.string(),
            Expression::PrefixExpression(inner) => inner.string(),
            Expression::InfixExpression(inner) => inner.string(),
            Expression::IfExpression(inner) => inner.string(),
            Expression::CallExpression(inner) => inner.string(),
            Expression::FunctionLiteral(inner) => inner.string(),
        }
    }
}
