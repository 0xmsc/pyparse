#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i64),
    Identifier(String),
    Boolean(bool),
    String(String),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    FunctionDef {
        name: String,
        body: Vec<Statement>,
    },
    Assign {
        name: String,
        value: Expression,
    },
    If {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
    },
    Return(Option<Expression>),
    Pass,
    Expr(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
