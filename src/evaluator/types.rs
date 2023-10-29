use std::any::Any;

pub enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}
pub trait Object: std::fmt::Debug {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct Integer {
    pub value: u64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn object_type(&self) -> ObjectType {
        ObjectType::INTEGER
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn object_type(&self) -> ObjectType {
        ObjectType::BOOLEAN
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn inspect(&self) -> String {
        format!("NULL")
    }

    fn object_type(&self) -> ObjectType {
        ObjectType::NULL
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
