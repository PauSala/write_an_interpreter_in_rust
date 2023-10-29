pub mod expressions;
pub mod statements;
pub mod program;
use std::rc::Rc;

use self::{
    expressions::{
        BlockStatement, Boolean, CallExpression, FunctionLiteral, Identifier, IfExpression,
        InfixExpression, IntegerLiteral, PrefixExpression, Expression,
    },
    statements::{ExpressionStatement, LetStatement, ReturnStatement, Statement}, program::Program,
};

use super::Parser;



pub trait Node: std::fmt::Debug {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub enum AstNode<'a> {
    Program(Program),
    ExpressionStatement(ExpressionStatement),
    IntegerLiteral(&'a IntegerLiteral),
    Boolean(&'a Boolean),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    Identifier(Identifier),
    PrefixExpression(&'a PrefixExpression),
    InfixExpression(&'a InfixExpression),
    BlockStatement(BlockStatement),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    Statement(Statement),
    Expression(&'a Expression)
}

impl Node for AstNode<'_>{

    fn token_literal(&self) -> String {
        match self {
            AstNode::Program(inner) => inner.token_literal(),
            AstNode::ExpressionStatement(inner) => inner.token_literal(),
            AstNode::IntegerLiteral(inner) => inner.token_literal(),
            AstNode::Boolean(inner) => inner.token_literal(),
            AstNode::LetStatement(inner) => inner.token_literal(),
            AstNode::ReturnStatement(inner) => inner.token_literal(),
            AstNode::Identifier(inner) => inner.token_literal(),
            AstNode::PrefixExpression(inner) => inner.token_literal(),
            AstNode::InfixExpression(inner) => inner.token_literal(),
            AstNode::BlockStatement(inner) => inner.token_literal(),
            AstNode::IfExpression(inner) => inner.token_literal(),
            AstNode::FunctionLiteral(inner) => inner.token_literal(),
            AstNode::CallExpression(inner) => inner.token_literal(),
            AstNode::Statement(inner) => inner.token_literal(), 
            AstNode::Expression(inner) => inner.token_literal()
        }
    }

    fn string(&self) -> String {
        match self {
            AstNode::Program(inner) => inner.string(),
            AstNode::ExpressionStatement(inner) => inner.string(),
            AstNode::IntegerLiteral(inner) => inner.string(),
            AstNode::Boolean(inner) => inner.string(),
            AstNode::LetStatement(inner) => inner.string(),
            AstNode::ReturnStatement(inner) => inner.string(),
            AstNode::Identifier(inner) => inner.string(),
            AstNode::PrefixExpression(inner) => inner.string(),
            AstNode::InfixExpression(inner) => inner.string(),
            AstNode::BlockStatement(inner) => inner.string(),
            AstNode::IfExpression(inner) => inner.string(),
            AstNode::FunctionLiteral(inner) => inner.string(),
            AstNode::CallExpression(inner) => inner.string(),
            AstNode::Statement(inner) => inner.string(), 
            AstNode::Expression(inner) => inner.string()
        }
    }
}


pub type PrefixParseFn = fn(parser: &mut Parser) -> Option<Rc<Expression>>;
pub type InfixParseFn =
    fn(parser: &mut Parser, left: Rc<Expression>) -> Option<Rc<Expression>>;
