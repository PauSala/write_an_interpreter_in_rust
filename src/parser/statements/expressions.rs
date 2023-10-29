use std::{any::Any, rc::Rc};

use crate::{
    parser::ast::{self, Expression, Node, NodeType, Statement},
    token::Token,
};

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token, // the token.IDENT token
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        self.value.clone()
    }
    fn node_type(&self) -> NodeType {
        NodeType::Identifier
    }
}

impl ast::Expression for Identifier {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token, // the token.IDENT token
    pub value: u64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        self.token.literal.to_string()
    }
    fn node_type(&self) -> NodeType {
        NodeType::IntegerLiteral
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token, // the token.IDENT token
    pub operator: String,
    pub right: Option<Rc<dyn Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
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
    fn node_type(&self) -> NodeType {
        NodeType::PrefixExpression
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token, // the token.IDENT token
    pub left: Option<Rc<dyn Expression>>,
    pub operator: String,
    pub right: Option<Rc<dyn Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
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
    fn node_type(&self) -> NodeType {
        NodeType::InfixExpression
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token, // the token.IDENT token
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        self.token.literal.to_string()
    }
    fn node_type(&self) -> NodeType {
        NodeType::Boolean
    }
}

impl Expression for Boolean {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token, // the token.IDENT token
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        for statement in &self.statements {
            buffer.push_str(&statement.string())
        }
        buffer
    }
    fn node_type(&self) -> NodeType {
        NodeType::BlockStatement
    }
}

impl Expression for BlockStatement {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token, // the token.IDENT token
    pub condition: Option<Rc<dyn Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
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
    fn node_type(&self) -> NodeType {
        NodeType::IfExpression
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token, // the token.IDENT token
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.token_literal().unwrap());
        buffer.push_str("(");
        let params = self
            .parameters
            .iter()
            .map(|p| p.string())
            .collect::<Vec<String>>()
            .join(",");
        buffer.push_str(&params);
        buffer.push_str(")");
        buffer.push_str(&self.body.as_ref().unwrap().string());
        buffer
    }
    fn node_type(&self) -> NodeType {
        NodeType::FunctionLiteral
    }
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token, // the token.IDENT token
    pub function: Rc<dyn Expression>,
    pub args: Vec<Rc<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
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
    fn node_type(&self) -> NodeType {
        NodeType::CallExpression
    }
}

impl Expression for CallExpression {
    fn expression_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
