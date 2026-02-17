#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i64),
    Identifier(String),
    Boolean(bool),
    String(String),
    List(Vec<Expression>),
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
    },
    Attribute {
        object: Box<Expression>,
        name: String,
    },
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
    LessThan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    ClassDef {
        name: String,
        body: Vec<Statement>,
    },
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Vec<Statement>,
    },
    Assign {
        target: AssignTarget,
        value: Expression,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
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
pub enum AssignTarget {
    Name(String),
    Index { name: String, index: Expression },
    Attribute { object: Expression, name: String },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
