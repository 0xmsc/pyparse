use anyhow::{Result, bail, anyhow};
use std::collections::{BTreeSet, VecDeque};

use crate::ast::Program;
use crate::backend::Backend;
use crate::backend::bytecode::{CompiledProgram, Instruction, compile};
use crate::backend::c_runtime::{
    C_BINARY_OPS, C_EXPECT_INT, C_HEADERS, C_PRINT, C_TRUTHY, C_VALUE_TYPES, compile_and_run,
    escape_c_string,
};

const C_RUNTIME_ERROR: &str = r#"static void runtime_error(const char *message) {
    fprintf(stderr, "Runtime error: %s\n", message);
    exit(1);
}

"#;

pub struct JIT;

impl JIT {
    pub fn new() -> Self {
        Self
    }

    fn compile_to_c(&self, program: &CompiledProgram) -> Result<String> {
        let mut output = String::new();
        let globals = collect_store_names(&program.main);
        let mut function_names: Vec<&String> = program.functions.keys().collect();
        function_names.sort();
        let function_set: BTreeSet<String> =
            function_names.iter().map(|name| (*name).clone()).collect();

        output.push_str(C_HEADERS);
        output.push_str(C_VALUE_TYPES);
        output.push_str(C_EXPECT_INT);
        output.push_str(C_BINARY_OPS);
        output.push_str(C_TRUTHY);
        output.push_str(C_PRINT);
        output.push_str(C_RUNTIME_ERROR);

        for name in &function_names {
            output.push_str(&format!("static Value {}(void);\n", function_c_name(name)));
        }
        if !function_names.is_empty() {
            output.push('\n');
        }

        for name in &globals {
            let c_name = global_c_name(name);
            output.push_str(&format!(
                "static Value {c_name} = {{ VAL_NONE, 0, 0, NULL }};\n"
            ));
            output.push_str(&format!("static int {c_name}_set = 0;\n"));
        }
        if !globals.is_empty() {
            output.push('\n');
        }

        for name in &function_names {
            let function = program
                .functions
                .get(*name)
                .ok_or_else(|| anyhow!("Missing function '{name}'"))?;
            self.emit_function(name, &function.code, &globals, &function_set, &mut output)?;
        }

        self.emit_main(&program.main, &globals, &function_set, &mut output)?;
        output.push_str("int main(void) {\n");
        output.push_str("    run_main();\n");
        output.push_str("    return 0;\n");
        output.push_str("}\n");

        Ok(output)
    }

    fn emit_function(
        &self,
        name: &str,
        code: &[Instruction],
        globals: &BTreeSet<String>,
        functions: &BTreeSet<String>,
        output: &mut String,
    ) -> Result<()> {
        let locals = collect_store_names(code);
        let stack_size = max_stack_depth(code)?.max(1);

        self.push_line(output, 0, &format!("static Value {}(void) {{", function_c_name(name)));
        self.push_line(output, 1, &format!("Value stack[{stack_size}];"));
        self.push_line(output, 1, "int sp = 0;");
        self.push_line(output, 1, "Value tmp;");
        self.push_line(output, 1, "Value tmp2;");
        for local in &locals {
            let c_name = local_c_name(local);
            self.push_line(
                output,
                1,
                &format!("Value {c_name} = {{ VAL_NONE, 0, 0, NULL }};"),
            );
            self.push_line(output, 1, &format!("int {c_name}_set = 0;"));
        }
        if !locals.is_empty() {
            output.push('\n');
        }

        self.emit_code(code, &locals, globals, functions, output, 1, ReturnKind::Value)?;
        self.push_line(output, 1, "return make_none();");
        self.push_line(output, 0, "}");
        output.push('\n');

        Ok(())
    }

    fn emit_main(
        &self,
        code: &[Instruction],
        globals: &BTreeSet<String>,
        functions: &BTreeSet<String>,
        output: &mut String,
    ) -> Result<()> {
        let stack_size = max_stack_depth(code)?.max(1);

        self.push_line(output, 0, "static void run_main(void) {");
        self.push_line(output, 1, &format!("Value stack[{stack_size}];"));
        self.push_line(output, 1, "int sp = 0;");
        self.push_line(output, 1, "Value tmp;");
        self.push_line(output, 1, "Value tmp2;");

        let locals = BTreeSet::new();
        self.emit_code(code, &locals, globals, functions, output, 1, ReturnKind::Void)?;
        self.push_line(output, 0, "}");
        output.push('\n');

        Ok(())
    }

    fn emit_code(
        &self,
        code: &[Instruction],
        locals: &BTreeSet<String>,
        globals: &BTreeSet<String>,
        functions: &BTreeSet<String>,
        output: &mut String,
        indent: usize,
        return_kind: ReturnKind,
    ) -> Result<()> {
        for (idx, instruction) in code.iter().enumerate() {
            self.push_label(output, indent, &format!("L{idx}:"));
            self.emit_instruction(
                idx,
                code.len(),
                instruction,
                locals,
                globals,
                functions,
                output,
                indent + 1,
                return_kind,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_instruction(
        &self,
        idx: usize,
        code_len: usize,
        instruction: &Instruction,
        locals: &BTreeSet<String>,
        globals: &BTreeSet<String>,
        functions: &BTreeSet<String>,
        output: &mut String,
        indent: usize,
        return_kind: ReturnKind,
    ) -> Result<()> {
        match instruction {
            Instruction::PushInt(value) => {
                self.push_line(output, indent, &format!("stack[sp++] = make_int({value});"));
            }
            Instruction::PushBool(value) => {
                self.push_line(
                    output,
                    indent,
                    &format!(
                        "stack[sp++] = make_bool({});",
                        if *value { 1 } else { 0 }
                    ),
                );
            }
            Instruction::PushString(value) => {
                let escaped = escape_c_string(value);
                self.push_line(
                    output,
                    indent,
                    &format!("stack[sp++] = make_str(\"{escaped}\");"),
                );
            }
            Instruction::PushNone => {
                self.push_line(output, indent, "stack[sp++] = make_none();");
            }
            Instruction::LoadName(name) => {
                self.emit_load_name(name, locals, globals, output, indent);
            }
            Instruction::StoreName(name) => {
                self.push_line(output, indent, "tmp = stack[--sp];");
                if locals.contains(name) {
                    let local = local_c_name(name);
                    self.push_line(output, indent, &format!("{local} = tmp;"));
                    self.push_line(output, indent, &format!("{local}_set = 1;"));
                } else {
                    let global = global_c_name(name);
                    self.push_line(output, indent, &format!("{global} = tmp;"));
                    self.push_line(output, indent, &format!("{global}_set = 1;"));
                }
            }
            Instruction::Add => {
                self.push_line(output, indent, "tmp2 = stack[--sp];");
                self.push_line(output, indent, "tmp = stack[--sp];");
                self.push_line(output, indent, "stack[sp++] = binary_add(tmp, tmp2);");
            }
            Instruction::Sub => {
                self.push_line(output, indent, "tmp2 = stack[--sp];");
                self.push_line(output, indent, "tmp = stack[--sp];");
                self.push_line(output, indent, "stack[sp++] = binary_sub(tmp, tmp2);");
            }
            Instruction::LessThan => {
                self.push_line(output, indent, "tmp2 = stack[--sp];");
                self.push_line(output, indent, "tmp = stack[--sp];");
                self.push_line(output, indent, "stack[sp++] = binary_lt(tmp, tmp2);");
            }
            Instruction::CallBuiltinPrint0 => {
                self.push_line(output, indent, "stack[sp++] = builtin_print0();");
            }
            Instruction::CallBuiltinPrint1 => {
                self.push_line(output, indent, "tmp = stack[--sp];");
                self.push_line(output, indent, "stack[sp++] = builtin_print1(tmp);");
            }
            Instruction::CallFunction(name) => {
                if functions.contains(name) {
                    self.push_line(
                        output,
                        indent,
                        &format!("stack[sp++] = {}();", function_c_name(name)),
                    );
                } else {
                    self.push_line(
                        output,
                        indent,
                        &format!("runtime_error(\"Undefined function '{name}'\");"),
                    );
                }
            }
            Instruction::JumpIfFalse(target) => {
                if *target >= code_len {
                    bail!("Invalid jump target {target} at {idx}");
                }
                self.push_line(output, indent, "tmp = stack[--sp];");
                self.push_line(output, indent, &format!("if (!is_truthy(tmp)) {{"));
                self.push_line(output, indent + 1, &format!("goto L{target};"));
                self.push_line(output, indent, "}");
            }
            Instruction::Jump(target) => {
                if *target >= code_len {
                    bail!("Invalid jump target {target} at {idx}");
                }
                self.push_line(output, indent, &format!("goto L{target};"));
            }
            Instruction::Pop => {
                self.push_line(output, indent, "sp--;");
            }
            Instruction::Return => match return_kind {
                ReturnKind::Value => self.push_line(output, indent, "return make_none();"),
                ReturnKind::Void => self.push_line(output, indent, "return;"),
            },
            Instruction::ReturnValue => match return_kind {
                ReturnKind::Value => self.push_line(output, indent, "return stack[--sp];"),
                ReturnKind::Void => self.push_line(output, indent, "return;"),
            },
        }
        Ok(())
    }

    fn emit_load_name(
        &self,
        name: &str,
        locals: &BTreeSet<String>,
        globals: &BTreeSet<String>,
        output: &mut String,
        indent: usize,
    ) {
        if locals.contains(name) {
            let local = local_c_name(name);
            if globals.contains(name) {
                let global = global_c_name(name);
                self.push_line(output, indent, &format!("if ({local}_set) {{"));
                self.push_line(output, indent + 1, &format!("stack[sp++] = {local};"));
                self.push_line(output, indent, &format!("}} else if ({global}_set) {{"));
                self.push_line(output, indent + 1, &format!("stack[sp++] = {global};"));
                self.push_line(output, indent, "} else {");
                self.push_line(
                    output,
                    indent + 1,
                    &format!("runtime_error(\"Undefined variable '{name}'\");"),
                );
                self.push_line(output, indent, "}");
            } else {
                self.push_line(output, indent, &format!("if ({local}_set) {{"));
                self.push_line(output, indent + 1, &format!("stack[sp++] = {local};"));
                self.push_line(output, indent, "} else {");
                self.push_line(
                    output,
                    indent + 1,
                    &format!("runtime_error(\"Undefined variable '{name}'\");"),
                );
                self.push_line(output, indent, "}");
            }
        } else if globals.contains(name) {
            let global = global_c_name(name);
            self.push_line(output, indent, &format!("if ({global}_set) {{"));
            self.push_line(output, indent + 1, &format!("stack[sp++] = {global};"));
            self.push_line(output, indent, "} else {");
            self.push_line(
                output,
                indent + 1,
                &format!("runtime_error(\"Undefined variable '{name}'\");"),
            );
            self.push_line(output, indent, "}");
        } else {
            self.push_line(
                output,
                indent,
                &format!("runtime_error(\"Undefined variable '{name}'\");"),
            );
        }
    }

    fn push_line(&self, output: &mut String, indent: usize, line: &str) {
        for _ in 0..indent {
            output.push_str("    ");
        }
        output.push_str(line);
        output.push('\n');
    }

    fn push_label(&self, output: &mut String, indent: usize, label: &str) {
        for _ in 0..indent {
            output.push_str("    ");
        }
        output.push_str(label);
        output.push('\n');
    }
}

impl Default for JIT {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for JIT {
    fn name(&self) -> &'static str {
        "jit"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        let compiled = compile(program)?;
        let source = self.compile_to_c(&compiled)?;
        compile_and_run(&source, "", "JIT compilation failed", "JIT program failed")
    }
}

#[derive(Clone, Copy)]
enum ReturnKind {
    Value,
    Void,
}

fn collect_store_names(code: &[Instruction]) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for instruction in code {
        if let Instruction::StoreName(name) = instruction {
            names.insert(name.clone());
        }
    }
    names
}

fn function_c_name(name: &str) -> String {
    format!("fn_{name}")
}

fn local_c_name(name: &str) -> String {
    format!("local_{name}")
}

fn global_c_name(name: &str) -> String {
    format!("global_{name}")
}

fn max_stack_depth(code: &[Instruction]) -> Result<usize> {
    if code.is_empty() {
        return Ok(0);
    }

    let mut depths = vec![None; code.len()];
    let mut worklist = VecDeque::new();
    depths[0] = Some(0);
    worklist.push_back(0);
    let mut max_depth = 0;

    while let Some(ip) = worklist.pop_front() {
        let depth = depths[ip].unwrap_or(0);
        let instruction = &code[ip];
        let next_depth = depth + stack_effect(instruction);
        if next_depth < 0 {
            bail!("Bytecode stack underflow at {ip}");
        }
        max_depth = max_depth.max(next_depth as usize);

        match instruction {
            Instruction::Return | Instruction::ReturnValue => {}
            Instruction::Jump(target) => {
                propagate_depth(&mut depths, &mut worklist, *target, next_depth, code.len())?;
            }
            Instruction::JumpIfFalse(target) => {
                propagate_depth(&mut depths, &mut worklist, *target, next_depth, code.len())?;
                propagate_depth(&mut depths, &mut worklist, ip + 1, next_depth, code.len())?;
            }
            _ => {
                propagate_depth(&mut depths, &mut worklist, ip + 1, next_depth, code.len())?;
            }
        }
    }

    Ok(max_depth)
}

fn propagate_depth(
    depths: &mut [Option<i32>],
    worklist: &mut VecDeque<usize>,
    target: usize,
    depth: i32,
    code_len: usize,
) -> Result<()> {
    if target >= code_len {
        return Ok(());
    }
    if let Some(existing) = depths[target] {
        if existing != depth {
            bail!("Bytecode stack mismatch at {target}");
        }
        return Ok(());
    }
    depths[target] = Some(depth);
    worklist.push_back(target);
    Ok(())
}

fn stack_effect(instruction: &Instruction) -> i32 {
    match instruction {
        Instruction::PushInt(_)
        | Instruction::PushBool(_)
        | Instruction::PushString(_)
        | Instruction::PushNone
        | Instruction::LoadName(_)
        | Instruction::CallBuiltinPrint0
        | Instruction::CallFunction(_) => 1,
        Instruction::StoreName(_) | Instruction::Pop | Instruction::JumpIfFalse(_) => -1,
        Instruction::Add | Instruction::Sub | Instruction::LessThan => -1,
        Instruction::CallBuiltinPrint1 => 0,
        Instruction::Jump(_) | Instruction::Return => 0,
        Instruction::ReturnValue => -1,
    }
}
