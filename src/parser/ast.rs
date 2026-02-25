//! Shared syntax tree used by all execution backends.
//!
//! The parser builds these nodes once, then the interpreter walks them directly
//! while the VM lowers them into bytecode.

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i64),
    Identifier(String),
    Boolean(bool),
    String(String),
    List(Vec<Expression>),
    Dict(Vec<(Expression, Expression)>),
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
    For {
        target: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    If {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
    },
    Try {
        body: Vec<Statement>,
        except_body: Option<Vec<Statement>>,
        finally_body: Option<Vec<Statement>>,
    },
    Raise(Expression),
    Return(Option<Expression>),
    Pass,
    Expr(Expression),
}

/// Assignment target forms accepted by the parser.
///
/// Note: `Index` currently stores only a base variable name (`name[index]`), not
/// an arbitrary expression (`expr[index] = value`). This matches the parser's
/// current assignment-target restriction.
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
