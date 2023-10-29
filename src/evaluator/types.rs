
pub trait TObject: std::fmt::Debug {
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl TObject for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean{
    pub fn str_type(&self) -> String {
        "BOOLEAN".to_string()
    }
}

impl TObject for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Null {}

impl TObject for Null {
    fn inspect(&self) -> String {
        format!("NULL")
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
}

impl TObject for Error {
    fn inspect(&self) -> String {
        format!("Error: {}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl TObject for ReturnValue {
    fn inspect(&self) -> String {
        format!("{}", self.value.inspect())
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(ReturnValue),
    Null(Null),
    Error(Error),
}

impl Object {
    pub fn str_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::ReturnValue(_) => "RETURN_VALUE".to_string(),
            Object::Null(_) => "NULL".to_string(),
            Object::Error(_) => "ERROR".to_string(),
        }
    }
}

impl TObject for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(inner) => inner.inspect(),
            Object::Boolean(inner) => inner.inspect(),
            Object::ReturnValue(inner) => inner.inspect(),
            Object::Null(inner) => inner.inspect(),
            Object::Error(inner) => inner.inspect(),
        }
    }
}
