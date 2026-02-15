use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{AssignTarget, BinaryOperator, Expression, Program, Statement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    PushInt(i64),
    PushBool(bool),
    PushString(String),
    BuildList(usize),
    PushNone,
    LoadName(String),
    StoreName(String),
    DefineFunction {
        name: String,
        symbol: String,
    },
    DefineClass {
        name: String,
        methods: Vec<(String, String)>,
    },
    Add,
    Sub,
    LessThan,
    LoadAttr(String),
    LoadIndex,
    StoreIndex(String),
    Call {
        argc: usize,
    },
    JumpIfFalse(isize),
    Jump(isize),
    Pop,
    Return,
    ReturnValue,
}

type CompiledBlock = Vec<Instruction>;

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub params: Vec<String>,
    pub code: CompiledBlock,
}

#[derive(Debug, Clone)]
pub struct CompiledProgram {
    pub functions: HashMap<String, CompiledFunction>,
    pub main: CompiledBlock,
}

pub fn compile(program: &Program) -> Result<CompiledProgram> {
    let mut functions = HashMap::new();
    let mut main = Vec::new();

    for statement in &program.statements {
        match statement {
            Statement::FunctionDef { name, params, body } => {
                if functions.contains_key(name) {
                    bail!("Duplicate function definition '{name}'");
                }
                let compiled = compile_function(params, body)?;
                functions.insert(name.to_string(), compiled);
                main.push(Instruction::DefineFunction {
                    name: name.to_string(),
                    symbol: name.to_string(),
                });
            }
            Statement::ClassDef { name, body } => {
                let methods = compile_class_methods(name, body, &mut functions)?;
                main.push(Instruction::DefineClass {
                    name: name.clone(),
                    methods,
                });
            }
            _ => main.extend(compile_statement(statement, false)?),
        }
    }

    Ok(CompiledProgram { functions, main })
}

fn compile_function(params: &[String], body: &[Statement]) -> Result<CompiledFunction> {
    let mut code = compile_block(body, true)?;
    code.push(Instruction::Return);

    Ok(CompiledFunction {
        params: params.to_vec(),
        code,
    })
}

fn compile_block(statements: &[Statement], in_function: bool) -> Result<CompiledBlock> {
    let mut code = Vec::new();
    for statement in statements {
        code.extend(compile_statement(statement, in_function)?);
    }
    Ok(code)
}

fn compile_statement(statement: &Statement, in_function: bool) -> Result<CompiledBlock> {
    let mut code = Vec::new();
    match statement {
        Statement::ClassDef { .. } => {
            if in_function {
                bail!("Class definitions inside functions are not supported in the VM");
            } else {
                bail!("Unexpected class definition during statement compilation");
            }
        }
        Statement::Assign { target, value } => match target {
            AssignTarget::Name(name) => {
                code.extend(compile_expression(value)?);
                code.push(Instruction::StoreName(name.to_string()));
            }
            AssignTarget::Index { name, index } => {
                code.extend(compile_expression(index)?);
                code.extend(compile_expression(value)?);
                code.push(Instruction::StoreIndex(name.to_string()));
            }
        },
        Statement::If {
            condition,
            then_body,
            else_body,
        } => {
            let condition_code = compile_expression(condition)?;
            let then_code = compile_block(then_body, in_function)?;
            let else_code = compile_block(else_body, in_function)?;
            let then_len = then_code.len();
            let else_len = else_code.len();

            code.extend(condition_code);
            if else_body.is_empty() {
                code.push(Instruction::JumpIfFalse(then_len as isize));
                code.extend(then_code);
            } else {
                let jump_to_else_offset = (then_len + 1) as isize;
                let jump_to_end_offset = else_len as isize;
                code.push(Instruction::JumpIfFalse(jump_to_else_offset));
                code.extend(then_code);
                code.push(Instruction::Jump(jump_to_end_offset));
                code.extend(else_code);
            }
        }
        Statement::While { condition, body } => {
            let condition_code = compile_expression(condition)?;
            let body_code = compile_block(body, in_function)?;
            let condition_len = condition_code.len();
            let body_len = body_code.len();

            code.extend(condition_code);
            code.push(Instruction::JumpIfFalse((body_len + 1) as isize));
            code.extend(body_code);
            let jump_back_offset = -((condition_len + body_len + 2) as isize);
            code.push(Instruction::Jump(jump_back_offset));
        }
        Statement::Return(value) => {
            if !in_function {
                bail!("Return outside of function is not supported in the VM");
            }
            if let Some(value) = value {
                code.extend(compile_expression(value)?);
            } else {
                code.push(Instruction::PushNone);
            }
            code.push(Instruction::ReturnValue);
        }
        Statement::Pass => {}
        Statement::Expr(expr) => {
            code.extend(compile_expression(expr)?);
            code.push(Instruction::Pop);
        }
        Statement::FunctionDef { .. } => {
            if in_function {
                bail!("Nested function definitions are not supported in the VM");
            } else {
                bail!("Unexpected function definition during compilation");
            }
        }
    }
    Ok(code)
}

fn compile_class_methods(
    class_name: &str,
    body: &[Statement],
    functions: &mut HashMap<String, CompiledFunction>,
) -> Result<Vec<(String, String)>> {
    let mut methods = Vec::new();
    for statement in body {
        match statement {
            Statement::FunctionDef {
                name: method_name,
                params,
                body,
            } => {
                let symbol = class_method_symbol(class_name, method_name);
                if functions.contains_key(&symbol) {
                    bail!("Duplicate function definition '{symbol}'");
                }
                let compiled = compile_function(params, body)?;
                functions.insert(symbol.clone(), compiled);
                methods.push((method_name.clone(), symbol));
            }
            Statement::Pass => {}
            _ => {
                bail!(
                    "Unsupported class body statement in class '{class_name}': only method definitions and pass are allowed"
                );
            }
        }
    }
    Ok(methods)
}

fn class_method_symbol(class_name: &str, method_name: &str) -> String {
    format!("__class_method::{class_name}::{method_name}")
}

fn compile_expression(expr: &Expression) -> Result<CompiledBlock> {
    let mut code = Vec::new();
    match expr {
        Expression::Integer(value) => {
            code.push(Instruction::PushInt(*value));
        }
        Expression::Boolean(value) => {
            code.push(Instruction::PushBool(*value));
        }
        Expression::String(value) => {
            code.push(Instruction::PushString(value.clone()));
        }
        Expression::List(elements) => {
            for element in elements {
                code.extend(compile_expression(element)?);
            }
            code.push(Instruction::BuildList(elements.len()));
        }
        Expression::Identifier(name) => {
            code.push(Instruction::LoadName(name.to_string()));
        }
        Expression::Attribute { object, name } => {
            code.extend(compile_expression(object)?);
            code.push(Instruction::LoadAttr(name.to_string()));
        }
        Expression::Index { object, index } => {
            code.extend(compile_expression(object)?);
            code.extend(compile_expression(index)?);
            code.push(Instruction::LoadIndex);
        }
        Expression::BinaryOp { left, op, right } => {
            code.extend(compile_expression(left)?);
            code.extend(compile_expression(right)?);
            match op {
                BinaryOperator::Add => code.push(Instruction::Add),
                BinaryOperator::Sub => code.push(Instruction::Sub),
                BinaryOperator::LessThan => code.push(Instruction::LessThan),
            }
        }
        Expression::Call { callee, args } => {
            code.extend(compile_expression(callee)?);
            for arg in args {
                code.extend(compile_expression(arg)?);
            }
            code.push(Instruction::Call { argc: args.len() });
        }
    }
    Ok(code)
}

#[cfg(test)]
mod tests {
    use super::{Instruction, compile};
    use crate::ast::{AssignTarget, Expression, Program, Statement};

    fn function(name: &str, body: Vec<Statement>) -> Statement {
        Statement::FunctionDef {
            name: name.to_string(),
            params: vec![],
            body,
        }
    }

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Identifier(name.to_string())),
            args,
        }
    }

    #[test]
    fn compiles_main_and_function_separately() {
        let program = Program {
            statements: vec![
                function("foo", vec![Statement::Return(Some(Expression::Integer(7)))]),
                Statement::Expr(call("foo", vec![])),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(compiled.functions.len(), 1);

        let function = compiled
            .functions
            .get("foo")
            .expect("expected compiled function 'foo'");
        assert!(function.params.is_empty());
        assert_eq!(
            function.code,
            vec![
                Instruction::PushInt(7),
                Instruction::ReturnValue,
                Instruction::Return
            ]
        );

        assert_eq!(
            compiled.main,
            vec![
                Instruction::DefineFunction {
                    name: "foo".to_string(),
                    symbol: "foo".to_string(),
                },
                Instruction::LoadName("foo".to_string()),
                Instruction::Call { argc: 0 },
                Instruction::Pop
            ]
        );
    }

    #[test]
    fn compiles_print_without_argument() {
        let program = Program {
            statements: vec![Statement::Expr(call("print", vec![]))],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert!(compiled.functions.is_empty());
        assert_eq!(
            compiled.main,
            vec![
                Instruction::LoadName("print".to_string()),
                Instruction::Call { argc: 0 },
                Instruction::Pop
            ]
        );
    }

    #[test]
    fn errors_on_duplicate_function_definitions() {
        let program = Program {
            statements: vec![function("dup", vec![]), function("dup", vec![])],
        };

        let error = compile(&program).expect_err("compile should fail");
        assert_eq!(
            error.to_string(),
            "Duplicate function definition 'dup'".to_string()
        );
    }

    #[test]
    fn errors_on_return_outside_function() {
        let program = Program {
            statements: vec![Statement::Return(None)],
        };

        let error = compile(&program).expect_err("compile should fail");
        assert_eq!(
            error.to_string(),
            "Return outside of function is not supported in the VM".to_string()
        );
    }

    #[test]
    fn errors_on_nested_function_definitions() {
        let program = Program {
            statements: vec![function(
                "outer",
                vec![function("inner", vec![Statement::Return(None)])],
            )],
        };

        let error = compile(&program).expect_err("compile should fail");
        assert_eq!(
            error.to_string(),
            "Nested function definitions are not supported in the VM".to_string()
        );
    }

    #[test]
    fn compiles_multi_argument_function_call() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "sum2".to_string(),
                    params: vec!["a".to_string(), "b".to_string()],
                    body: vec![Statement::Return(Some(Expression::BinaryOp {
                        left: Box::new(Expression::Identifier("a".to_string())),
                        op: crate::ast::BinaryOperator::Add,
                        right: Box::new(Expression::Identifier("b".to_string())),
                    }))],
                },
                Statement::Expr(call(
                    "sum2",
                    vec![Expression::Integer(1), Expression::Integer(2)],
                )),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let function = compiled
            .functions
            .get("sum2")
            .expect("expected compiled function 'sum2'");
        assert_eq!(function.params, vec!["a".to_string(), "b".to_string()]);
        assert_eq!(
            function.code,
            vec![
                Instruction::LoadName("a".to_string()),
                Instruction::LoadName("b".to_string()),
                Instruction::Add,
                Instruction::ReturnValue,
                Instruction::Return
            ]
        );
        assert_eq!(
            compiled.main,
            vec![
                Instruction::DefineFunction {
                    name: "sum2".to_string(),
                    symbol: "sum2".to_string(),
                },
                Instruction::LoadName("sum2".to_string()),
                Instruction::PushInt(1),
                Instruction::PushInt(2),
                Instruction::Call { argc: 2 },
                Instruction::Pop
            ]
        );
    }

    #[test]
    fn compiles_list_literal_expression() {
        let program = Program {
            statements: vec![Statement::Expr(call(
                "print",
                vec![Expression::List(vec![
                    Expression::Integer(1),
                    Expression::Integer(2),
                ])],
            ))],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(
            compiled.main,
            vec![
                Instruction::LoadName("print".to_string()),
                Instruction::PushInt(1),
                Instruction::PushInt(2),
                Instruction::BuildList(2),
                Instruction::Call { argc: 1 },
                Instruction::Pop
            ]
        );
    }

    #[test]
    fn compiles_list_index_assignment_and_read() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![Expression::Integer(1), Expression::Integer(2)]),
                },
                Statement::Assign {
                    target: AssignTarget::Index {
                        name: "values".to_string(),
                        index: Expression::Integer(1),
                    },
                    value: Expression::Integer(7),
                },
                Statement::Expr(call(
                    "print",
                    vec![Expression::Index {
                        object: Box::new(Expression::Identifier("values".to_string())),
                        index: Box::new(Expression::Integer(0)),
                    }],
                )),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(
            compiled.main,
            vec![
                Instruction::PushInt(1),
                Instruction::PushInt(2),
                Instruction::BuildList(2),
                Instruction::StoreName("values".to_string()),
                Instruction::PushInt(1),
                Instruction::PushInt(7),
                Instruction::StoreIndex("values".to_string()),
                Instruction::LoadName("print".to_string()),
                Instruction::LoadName("values".to_string()),
                Instruction::PushInt(0),
                Instruction::LoadIndex,
                Instruction::Call { argc: 1 },
                Instruction::Pop
            ]
        );
    }

    #[test]
    fn compiles_method_call_on_named_receiver() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![]),
                },
                Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Attribute {
                        object: Box::new(Expression::Identifier("values".to_string())),
                        name: "append".to_string(),
                    }),
                    args: vec![Expression::Integer(3)],
                }),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(
            compiled.main,
            vec![
                Instruction::BuildList(0),
                Instruction::StoreName("values".to_string()),
                Instruction::LoadName("values".to_string()),
                Instruction::LoadAttr("append".to_string()),
                Instruction::PushInt(3),
                Instruction::Call { argc: 1 },
                Instruction::Pop
            ]
        );
    }
}
