use anyhow::{Context, Result, bail};
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::backend::Backend;

const C_HEADERS: &str = r#"#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

"#;

const C_VALUE_TYPES: &str = r#"typedef enum { VAL_INT, VAL_NONE } ValueTag;
typedef struct { ValueTag tag; int64_t int_value; } Value;

static Value make_int(int64_t v) { Value value = { VAL_INT, v }; return value; }
static Value make_none(void) { Value value = { VAL_NONE, 0 }; return value; }

"#;

const C_EXPECT_INT: &str = r#"static int64_t expect_int(Value value) {
    if (value.tag != VAL_INT) {
        fprintf(stderr, "Runtime error: expected int\n");
        exit(1);
    }
    return value.int_value;
}

"#;

const C_BINARY_OPS: &str = r#"static Value binary_add(Value lhs, Value rhs) {
    return make_int(expect_int(lhs) + expect_int(rhs));
}
static Value binary_sub(Value lhs, Value rhs) {
    return make_int(expect_int(lhs) - expect_int(rhs));
}

"#;

const C_PRINT: &str = r#"static void print_value(Value value) {
    if (value.tag == VAL_INT) {
        printf("%" PRId64, value.int_value);
    } else {
        printf("None");
    }
}
static Value builtin_print0(void) {
    printf("\n");
    return make_none();
}
static Value builtin_print1(Value value) {
    print_value(value);
    printf("\n");
    return make_none();
}

"#;

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(&self, program: &Program) -> Result<String> {
        let mut output = String::new();
        let (functions, main_statements) = self.split_program(program)?;
        let globals = self.collect_globals(&main_statements);

        output.push_str(C_HEADERS);
        output.push_str(C_VALUE_TYPES);
        output.push_str(C_EXPECT_INT);
        output.push_str(C_BINARY_OPS);
        output.push_str(C_PRINT);

        for (name, _) in &functions {
            output.push_str(&format!("static Value {name}(void);\n"));
        }
        if !functions.is_empty() {
            output.push('\n');
        }

        for name in &globals {
            output.push_str(&format!("static Value {name} = {{ VAL_NONE, 0 }};\n"));
        }
        if !globals.is_empty() {
            output.push('\n');
        }

        for (name, body) in &functions {
            let locals = self.collect_locals(body)?;
            output.push_str(&format!("static Value {name}(void) {{\n"));
            for local in &locals {
                output.push_str(&format!("    Value {local} = {{ VAL_NONE, 0 }};\n"));
            }
            if !locals.is_empty() {
                output.push('\n');
            }
            for stmt in *body {
                self.emit_statement(stmt, 1, &mut output, true)?;
            }
            output.push_str("    return make_none();\n");
            output.push_str("}\n\n");
        }

        output.push_str("int main(void) {\n");
        for stmt in main_statements {
            self.emit_statement(stmt, 1, &mut output, false)?;
        }
        output.push_str("    return 0;\n");
        output.push_str("}\n");

        Ok(output)
    }

    fn split_program<'a>(
        &self,
        program: &'a Program,
    ) -> Result<(Vec<(&'a str, &'a Vec<Statement>)>, Vec<&'a Statement>)> {
        let mut functions = Vec::new();
        let mut main_statements = Vec::new();
        for statement in &program.statements {
            match statement {
                Statement::FunctionDef { name, body } => {
                    functions.push((name.as_str(), body));
                }
                _ => main_statements.push(statement),
            }
        }
        Ok((functions, main_statements))
    }

    fn collect_globals(&self, statements: &[&Statement]) -> HashSet<String> {
        let mut globals = HashSet::new();
        for statement in statements {
            if let Statement::Assign { name, .. } = statement {
                globals.insert(name.to_string());
            }
        }
        globals
    }

    fn collect_locals(&self, statements: &[Statement]) -> Result<HashSet<String>> {
        let mut locals = HashSet::new();
        for statement in statements {
            match statement {
                Statement::Assign { name, .. } => {
                    locals.insert(name.to_string());
                }
                Statement::FunctionDef { .. } => {
                    bail!("Nested function definitions are not supported in the transpiler")
                }
                Statement::Expr(_) => {}
            }
        }
        Ok(locals)
    }

    fn emit_statement(
        &self,
        statement: &Statement,
        indent: usize,
        output: &mut String,
        in_function: bool,
    ) -> Result<()> {
        match statement {
            Statement::Assign { name, value } => {
                let expr = self.emit_expression(value)?;
                self.push_line(output, indent, &format!("{name} = {expr};"));
            }
            Statement::Expr(expr) => {
                let expr = self.emit_expression(expr)?;
                self.push_line(output, indent, &format!("{expr};"));
            }
            Statement::FunctionDef { .. } => {
                if in_function {
                    bail!("Nested function definitions are not supported in the transpiler");
                } else {
                    bail!("Unexpected function definition in statement emission");
                }
            }
        }
        Ok(())
    }

    fn emit_expression(&self, expr: &Expression) -> Result<String> {
        match expr {
            Expression::Integer(value) => Ok(format!("make_int({value})")),
            Expression::Identifier(name) => Ok(name.to_string()),
            Expression::BinaryOp { left, op, right } => {
                let left = self.emit_expression(left)?;
                let right = self.emit_expression(right)?;
                match op {
                    BinaryOperator::Add => Ok(format!("binary_add({left}, {right})")),
                    BinaryOperator::Sub => Ok(format!("binary_sub({left}, {right})")),
                }
            }
            Expression::Call { callee, args } => {
                if args.len() > 1 {
                    bail!("Transpiler only supports zero or one call argument");
                }
                match callee.as_ref() {
                    Expression::Identifier(name) if name == "print" => {
                        if let Some(arg) = args.get(0) {
                            let arg = self.emit_expression(arg)?;
                            Ok(format!("builtin_print1({arg})"))
                        } else {
                            Ok("builtin_print0()".to_string())
                        }
                    }
                    Expression::Identifier(name) => {
                        if !args.is_empty() {
                            bail!("Function '{name}' does not accept arguments");
                        }
                        Ok(format!("{name}()"))
                    }
                    _ => bail!("Can only call identifiers in the transpiler"),
                }
            }
        }
    }

    fn push_line(&self, output: &mut String, indent: usize, line: &str) {
        for _ in 0..indent {
            output.push_str("    ");
        }
        output.push_str(line);
        output.push('\n');
    }

    fn write_temp_file(&self, contents: &str, suffix: &str) -> Result<(PathBuf, PathBuf)> {
        let mut dir = std::env::temp_dir();
        dir.push("pyparse");
        fs::create_dir_all(&dir).context("Creating temp directory")?;

        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let file_stem = format!("transpile_{nanos}");
        let source_path = dir.join(format!("{file_stem}.c"));
        let binary_path = dir.join(format!("{file_stem}{suffix}"));

        fs::write(&source_path, contents).context("Writing C source")?;
        Ok((source_path, binary_path))
    }
}

impl Backend for Transpiler {
    fn name(&self) -> &'static str {
        "transpiler"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        let source = self.transpile(program)?;
        let (source_path, binary_path) = self.write_temp_file(&source, "")?;

        let compile = Command::new("cc")
            .arg(&source_path)
            .arg("-std=c99")
            .arg("-O2")
            .arg("-o")
            .arg(&binary_path)
            .output()
            .context("Running C compiler")?;
        if !compile.status.success() {
            let stderr = String::from_utf8_lossy(&compile.stderr);
            bail!("C compilation failed: {stderr}");
        }

        let output = Command::new(&binary_path)
            .output()
            .context("Running transpiled program")?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("Transpiled program failed: {stderr}");
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}
