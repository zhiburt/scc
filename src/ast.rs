#[derive(Debug)]
pub enum BinOp {
    BitwiseXor,
    BitwiseOr,
    BitwiseAnd,
    Addition,
    Sub,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    BitwiseLeftShift,
    BitwiseRightShift,
}

#[derive(Debug)]
pub enum Const {
    Int(i64)
}

#[derive(Debug)]
pub enum UnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum Exp {
    Assign(String, Box<Exp>),
    Var(String),
    Const(Const),
    UnOp(UnOp, Box<Exp>),
    BinOp(BinOp, Box<Exp>, Box<Exp>),
}

pub enum Statement {
    Return{exp: Exp},
    Declare{name: String, exp: Option<Exp>},
    Exp{exp: Exp},
}

pub enum Declaration {
    Func{name: String, statements: Vec<Statement>},
}

// Add block node which is decl
pub struct Program(pub Declaration);