

pub trait TObject: std::fmt::Debug {
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl TObject for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}

impl TObject for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Null {}

impl TObject for Null {
    fn inspect(&self) -> String {
        format!("NULL")
    }
}


#[derive(Debug)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl TObject for ReturnValue {
    fn inspect(&self) -> String {
        format!("{}", self.value.inspect())
    }
}

#[derive(Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(ReturnValue),
    Null(Null),
}

impl TObject for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(inner) => inner.inspect(),
            Object::Boolean(inner) => inner.inspect(),
            Object::ReturnValue(inner) => inner.inspect(),
            Object::Null(inner) => inner.inspect(),
        }
    }
}
