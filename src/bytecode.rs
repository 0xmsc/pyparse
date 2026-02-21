use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{AssignTarget, BinaryOperator, Expression, Program, Statement};

/// Stack-machine instruction set consumed by the VM and JIT backends.
///
/// Instructions follow a Python-like operand-stack model where expression
/// evaluation pushes values and operators/calls consume them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    PushInt(i64),
    PushBool(bool),
    PushString(String),
    BuildList(usize),
    BuildDict(usize),
    PushNone,
    LoadName(String),
    StoreName(String),
    DefineFunction {
        name: String,
        callable_id: u32,
    },
    DefineClass {
        name: String,
        methods: Vec<(String, u32)>,
    },
    Add,
    Sub,
    LessThan,
    LoadAttr(String),
    StoreAttr(String),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CompileScope {
    TopLevel,
    FunctionBody,
}

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub params: Vec<String>,
    pub code: CompiledBlock,
}

#[derive(Debug, Clone)]
pub struct CompiledCallable {
    pub name: String,
    pub function: CompiledFunction,
}

/// Fully compiled program: top-level code plus function/class callable bodies.
#[derive(Debug, Clone)]
pub struct CompiledProgram {
    pub callables: Vec<CompiledCallable>,
    pub main: CompiledBlock,
}

/// Compiles AST into bytecode plus callable metadata consumed by VM and JIT backends.
pub fn compile(program: &Program) -> Result<CompiledProgram> {
    let mut callable_ids = HashMap::new();
    let mut callables = Vec::new();
    let mut main = Vec::new();

    for statement in &program.statements {
        match statement {
            Statement::FunctionDef { name, params, body } => {
                let callable_id = define_callable(
                    &mut callable_ids,
                    &mut callables,
                    name.to_string(),
                    params,
                    body,
                )?;
                main.push(Instruction::DefineFunction {
                    name: name.to_string(),
                    callable_id,
                });
            }
            Statement::ClassDef { name, body } => {
                let methods = compile_class_methods(name, body, &mut callable_ids, &mut callables)?;
                main.push(Instruction::DefineClass {
                    name: name.clone(),
                    methods,
                });
            }
            _ => main.extend(compile_statement(statement, CompileScope::TopLevel)?),
        }
    }

    Ok(CompiledProgram { callables, main })
}

/// Compiles one function body and assigns a stable callable ID within this program.
fn define_callable(
    callable_ids: &mut HashMap<String, u32>,
    callables: &mut Vec<CompiledCallable>,
    name: String,
    params: &[String],
    body: &[Statement],
) -> Result<u32> {
    if callable_ids.contains_key(&name) {
        bail!("Duplicate function definition '{name}'");
    }
    let callable_id =
        u32::try_from(callables.len()).map_err(|_| anyhow::anyhow!("Too many callables"))?;
    let function = compile_function(params, body)?;
    callables.push(CompiledCallable {
        name: name.clone(),
        function,
    });
    callable_ids.insert(name, callable_id);
    Ok(callable_id)
}

/// Compiles a function body and appends implicit `Return` fallthrough behavior.
fn compile_function(params: &[String], body: &[Statement]) -> Result<CompiledFunction> {
    let mut code = compile_block(body, CompileScope::FunctionBody)?;
    code.push(Instruction::Return);

    Ok(CompiledFunction {
        params: params.to_vec(),
        code,
    })
}

/// Compiles a statement list under the same control-flow scope.
fn compile_block(statements: &[Statement], scope: CompileScope) -> Result<CompiledBlock> {
    let mut code = Vec::new();
    for statement in statements {
        code.extend(compile_statement(statement, scope)?);
    }
    Ok(code)
}

/// Compiles a single statement while enforcing scope-specific validity rules.
fn compile_statement(statement: &Statement, scope: CompileScope) -> Result<CompiledBlock> {
    let mut code = Vec::new();
    match statement {
        Statement::ClassDef { .. } => {
            if scope == CompileScope::FunctionBody {
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
            AssignTarget::Attribute { object, name } => {
                code.extend(compile_expression(value)?);
                code.extend(compile_expression(object)?);
                code.push(Instruction::StoreAttr(name.to_string()));
            }
        },
        Statement::If {
            condition,
            then_body,
            else_body,
        } => {
            let condition_code = compile_expression(condition)?;
            let then_code = compile_block(then_body, scope)?;
            let else_code = compile_block(else_body, scope)?;
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
            let body_code = compile_block(body, scope)?;
            let condition_len = condition_code.len();
            let body_len = body_code.len();

            code.extend(condition_code);
            code.push(Instruction::JumpIfFalse((body_len + 1) as isize));
            code.extend(body_code);
            let jump_back_offset = -((condition_len + body_len + 2) as isize);
            code.push(Instruction::Jump(jump_back_offset));
        }
        Statement::Return(value) => {
            if scope != CompileScope::FunctionBody {
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
            if scope == CompileScope::FunctionBody {
                bail!("Nested function definitions are not supported in the VM");
            } else {
                bail!("Unexpected function definition during compilation");
            }
        }
    }
    Ok(code)
}

/// Compiles class methods into callable IDs and returns class method dispatch metadata.
fn compile_class_methods(
    class_name: &str,
    body: &[Statement],
    callable_ids: &mut HashMap<String, u32>,
    callables: &mut Vec<CompiledCallable>,
) -> Result<Vec<(String, u32)>> {
    let mut methods = Vec::new();
    for statement in body {
        match statement {
            Statement::FunctionDef {
                name: method_name,
                params,
                body,
            } => {
                let symbol = class_method_symbol(class_name, method_name);
                let callable_id = define_callable(callable_ids, callables, symbol, params, body)?;
                methods.push((method_name.clone(), callable_id));
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

/// Builds a unique callable symbol for class methods in compiled metadata.
fn class_method_symbol(class_name: &str, method_name: &str) -> String {
    format!("__class_method::{class_name}::{method_name}")
}

/// Compiles an expression to stack-machine bytecode.
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
        Expression::Dict(entries) => {
            for (key, value) in entries {
                code.extend(compile_expression(key)?);
                code.extend(compile_expression(value)?);
            }
            code.push(Instruction::BuildDict(entries.len()));
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
        assert_eq!(compiled.callables.len(), 1);

        let callable = compiled
            .callables
            .first()
            .expect("expected compiled callable");
        assert_eq!(callable.name, "foo");
        let function = &callable.function;
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
                    callable_id: 0,
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
        assert!(compiled.callables.is_empty());
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
        let callable = compiled
            .callables
            .iter()
            .find(|callable| callable.name == "sum2")
            .expect("expected compiled callable 'sum2'");
        let function = &callable.function;
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
                    callable_id: 0,
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
    fn compiles_dict_literal_expression() {
        let program = Program {
            statements: vec![Statement::Expr(call(
                "print",
                vec![Expression::Dict(vec![
                    (Expression::String("a".to_string()), Expression::Integer(1)),
                    (Expression::String("b".to_string()), Expression::Integer(2)),
                ])],
            ))],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(
            compiled.main,
            vec![
                Instruction::LoadName("print".to_string()),
                Instruction::PushString("a".to_string()),
                Instruction::PushInt(1),
                Instruction::PushString("b".to_string()),
                Instruction::PushInt(2),
                Instruction::BuildDict(2),
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
    fn compiles_attribute_assignment() {
        let program = Program {
            statements: vec![Statement::Assign {
                target: AssignTarget::Attribute {
                    object: Expression::Identifier("obj".to_string()),
                    name: "value".to_string(),
                },
                value: Expression::Integer(7),
            }],
        };

        let compiled = compile(&program).expect("compile should succeed");
        assert_eq!(
            compiled.main,
            vec![
                Instruction::PushInt(7),
                Instruction::LoadName("obj".to_string()),
                Instruction::StoreAttr("value".to_string())
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
