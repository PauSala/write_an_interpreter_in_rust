#[derive(Debug, PartialEq, Eq, PartialOrd, Hash, Clone, Copy)]
pub enum Precedence {
    __ = 0,
    LOWEST = 1,
    EQUALS = 2,      // ==
    LESSGREATER = 3, // > or <
    SUM = 4,         // +
    PRODUCT = 5,     // *
    PREFIX = 6,      // -X or !X
    CALL = 7,        // myFunction(X)
}
