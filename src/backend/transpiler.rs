use anyhow::{Result, bail};
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::backend::c_runtime::{
    C_BINARY_OPS, C_EXPECT_INT, C_HEADERS, C_PRINT, C_TRUTHY, C_VALUE_TYPES, compile_source,
    escape_c_string, run_compiled_binary,
};
use crate::backend::{Backend, PreparedBackend};

pub struct Transpiler;

pub struct PreparedTranspiler {
    source_path: PathBuf,
    binary_path: PathBuf,
}

type ProgramFunctionsAndMain<'a> = (Vec<(&'a str, &'a Vec<Statement>)>, Vec<&'a Statement>);

impl Transpiler {
    pub fn transpile(&self, program: &Program) -> Result<String> {
        let mut output = String::new();
        let (functions, main_statements) = self.split_program(program)?;
        let globals = self.collect_globals(&main_statements);

        output.push_str(C_HEADERS);
        output.push_str(C_VALUE_TYPES);
        output.push_str(C_EXPECT_INT);
        output.push_str(C_BINARY_OPS);
        output.push_str(C_TRUTHY);
        output.push_str(C_PRINT);

        for (name, _) in &functions {
            output.push_str(&format!("static Value {name}(void);\n"));
        }
        if !functions.is_empty() {
            output.push('\n');
        }

        for name in &globals {
            output.push_str(&format!(
                "static Value {name} = {{ VAL_NONE, 0, 0, NULL }};\n"
            ));
        }
        if !globals.is_empty() {
            output.push('\n');
        }

        for (name, body) in &functions {
            let locals = self.collect_locals(body)?;
            output.push_str(&format!("static Value {name}(void) {{\n"));
            for local in &locals {
                output.push_str(&format!(
                    "    Value {local} = {{ VAL_NONE, 0, 0, NULL }};\n"
                ));
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

    fn split_program<'a>(&self, program: &'a Program) -> Result<ProgramFunctionsAndMain<'a>> {
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
            self.collect_assignments(statement, &mut globals);
        }
        globals
    }

    fn collect_locals(&self, statements: &[Statement]) -> Result<HashSet<String>> {
        let mut locals = HashSet::new();
        for statement in statements {
            self.collect_assignments_in_function(statement, &mut locals)?;
        }
        Ok(locals)
    }

    fn collect_assignments(&self, statement: &Statement, names: &mut HashSet<String>) {
        match statement {
            Statement::Assign { name, .. } => {
                names.insert(name.to_string());
            }
            Statement::If {
                then_body,
                else_body,
                ..
            } => {
                for stmt in then_body {
                    self.collect_assignments(stmt, names);
                }
                for stmt in else_body {
                    self.collect_assignments(stmt, names);
                }
            }
            Statement::While { body, .. } => {
                for stmt in body {
                    self.collect_assignments(stmt, names);
                }
            }
            _ => {}
        }
    }

    fn collect_assignments_in_function(
        &self,
        statement: &Statement,
        names: &mut HashSet<String>,
    ) -> Result<()> {
        match statement {
            Statement::Assign { name, .. } => {
                names.insert(name.to_string());
            }
            Statement::If {
                then_body,
                else_body,
                ..
            } => {
                for stmt in then_body {
                    self.collect_assignments_in_function(stmt, names)?;
                }
                for stmt in else_body {
                    self.collect_assignments_in_function(stmt, names)?;
                }
            }
            Statement::While { body, .. } => {
                for stmt in body {
                    self.collect_assignments_in_function(stmt, names)?;
                }
            }
            Statement::FunctionDef { .. } => {
                bail!("Nested function definitions are not supported in the transpiler")
            }
            Statement::Expr(_) | Statement::Return(_) | Statement::Pass => {}
        }
        Ok(())
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
            Statement::While { condition, body } => {
                let condition = self.emit_expression(condition)?;
                self.push_line(
                    output,
                    indent,
                    &format!("while (is_truthy({condition})) {{"),
                );
                for stmt in body {
                    self.emit_statement(stmt, indent + 1, output, in_function)?;
                }
                self.push_line(output, indent, "}");
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                let condition = self.emit_expression(condition)?;
                self.push_line(output, indent, &format!("if (is_truthy({condition})) {{"));
                for stmt in then_body {
                    self.emit_statement(stmt, indent + 1, output, in_function)?;
                }
                self.push_line(output, indent, "}");
                if !else_body.is_empty() {
                    self.push_line(output, indent, "else {");
                    for stmt in else_body {
                        self.emit_statement(stmt, indent + 1, output, in_function)?;
                    }
                    self.push_line(output, indent, "}");
                }
            }
            Statement::Return(value) => {
                if !in_function {
                    bail!("Return outside of function is not supported in the transpiler");
                }
                let expr = if let Some(value) = value {
                    self.emit_expression(value)?
                } else {
                    "make_none()".to_string()
                };
                self.push_line(output, indent, &format!("return {expr};"));
            }
            Statement::Pass => {}
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
            Expression::Boolean(value) => Ok(format!("make_bool({})", if *value { 1 } else { 0 })),
            Expression::String(value) => {
                let escaped = escape_c_string(value);
                Ok(format!("make_str(\"{escaped}\")"))
            }
            Expression::Identifier(name) => Ok(name.to_string()),
            Expression::BinaryOp { left, op, right } => {
                let left = self.emit_expression(left)?;
                let right = self.emit_expression(right)?;
                match op {
                    BinaryOperator::Add => Ok(format!("binary_add({left}, {right})")),
                    BinaryOperator::Sub => Ok(format!("binary_sub({left}, {right})")),
                    BinaryOperator::LessThan => Ok(format!("binary_lt({left}, {right})")),
                }
            }
            Expression::Call { callee, args } => {
                if args.len() > 1 {
                    bail!("Transpiler only supports zero or one call argument");
                }
                match callee.as_ref() {
                    Expression::Identifier(name) if name == "print" => {
                        if let Some(arg) = args.first() {
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
}

impl Backend for Transpiler {
    fn name(&self) -> &'static str {
        "transpiler"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        let source = self.transpile(program)?;
        let (source_path, binary_path) =
            compile_source(&source, "", "C compilation failed in prepare phase")?;
        Ok(Box::new(PreparedTranspiler {
            source_path,
            binary_path,
        }))
    }
}

impl PreparedBackend for PreparedTranspiler {
    fn run(&self) -> Result<String> {
        run_compiled_binary(&self.binary_path, "Transpiled program failed")
    }
}

impl Drop for PreparedTranspiler {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.source_path);
        let _ = fs::remove_file(&self.binary_path);
    }
}
