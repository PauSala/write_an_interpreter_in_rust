use std::{any::Any, rc::Rc};

use super::Parser;

pub trait Node: std::fmt::Debug {
    fn token_literal(&self) -> Option<String>;
    fn string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) -> Option<String>;
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self) -> Option<String>;
    fn as_any(&self) -> &dyn Any;
}

pub type PrefixParseFn = fn(parser: &mut Parser) -> Option<Rc<dyn Expression>>;
pub type InfixParseFn =
    fn(parser: &mut Parser, left: Rc<dyn Expression>) -> Option<Rc<dyn Expression>>;
