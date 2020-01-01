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
    IncrementPrefix,
    IncrementPostfix,
    DecrementPrefix,
    DecrementPostfix,
}

#[derive(Debug)]
pub enum AssignmentOp {
    Plus,
    Sub,
    Mul,
    Div,
    Mod,
    BitLeftShift,
    BitRightShift,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Debug)]
pub enum Exp {
    Assign(String, Box<Exp>),
    Var(String),
    Const(Const),
    UnOp(UnOp, Box<Exp>),
    BinOp(BinOp, Box<Exp>, Box<Exp>),
    AssignOp(String, AssignmentOp, Box<Exp>),
    CondExp(Box<Exp>, Box<Exp>, Box<Exp>),
    FuncCall(String, Vec<Exp>),
}

pub enum Statement {
    Return{exp: Exp},
    Exp{exp: Option<Exp>},
    Conditional{cond_expr: Exp, if_block: Box<Statement>, else_block: Option<Box<Statement>>},
    Compound{list: Option<Vec<BlockItem>>},
    For{exp1: Option<Exp>, exp2: Exp, exp3: Option<Exp>, statement: Box<Statement>},
    ForDecl{decl: Declaration, exp2: Exp, exp3: Option<Exp>, statement: Box<Statement>},
    While{exp: Exp, statement: Box<Statement>},
    Do{statement: Box<Statement>, exp: Exp},
    Break,
    Continue,
}

pub enum Declaration {
    Declare{name: String, exp: Option<Exp>},
}

pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

pub struct FuncDecl{
    pub name: String,
    pub parameters: Vec<String>,
    pub blocks: Option<Vec<BlockItem>>,
}

// Add block node which is decl
pub struct Program(pub Vec<FuncDecl>);