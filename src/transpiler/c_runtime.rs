use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

pub const C_HEADERS: &str = r#"#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

"#;

pub const C_VALUE_TYPES: &str = r#"typedef enum { VAL_INT, VAL_BOOL, VAL_STR, VAL_NONE } ValueTag;
typedef struct {
    ValueTag tag;
    int64_t int_value;
    int bool_value;
    const char *str_value;
} Value;

static Value make_int(int64_t v) { Value value = { VAL_INT, v, 0, NULL }; return value; }
static Value make_bool(int v) { Value value = { VAL_BOOL, 0, v != 0, NULL }; return value; }
static Value make_str(const char *v) { Value value = { VAL_STR, 0, 0, v }; return value; }
static Value make_none(void) { Value value = { VAL_NONE, 0, 0, NULL }; return value; }

"#;

pub const C_EXPECT_INT: &str = r#"static int64_t expect_int(Value value) {
    if (value.tag != VAL_INT) {
        fprintf(stderr, "Runtime error: expected int\n");
        exit(1);
    }
    return value.int_value;
}

"#;

pub const C_BINARY_OPS: &str = r#"static Value binary_add(Value lhs, Value rhs) {
    return make_int(expect_int(lhs) + expect_int(rhs));
}

static Value binary_sub(Value lhs, Value rhs) {
    return make_int(expect_int(lhs) - expect_int(rhs));
}

static Value binary_lt(Value lhs, Value rhs) {
    return make_bool(expect_int(lhs) < expect_int(rhs));
}

"#;

pub const C_TRUTHY: &str = r#"static int is_truthy(Value value) {
    switch (value.tag) {
        case VAL_INT:
            return value.int_value != 0;
        case VAL_BOOL:
            return value.bool_value != 0;
        case VAL_STR:
            return value.str_value != NULL && value.str_value[0] != '\0';
        case VAL_NONE:
        default:
            return 0;
    }
}

"#;

pub const C_PRINT: &str = r#"static void print_value(Value value) {
    if (value.tag == VAL_INT) {
        printf("%" PRId64, value.int_value);
    } else if (value.tag == VAL_BOOL) {
        printf(value.bool_value ? "True" : "False");
    } else if (value.tag == VAL_STR) {
        printf("%s", value.str_value ? value.str_value : "");
    } else {
        printf("None");
    }
}

static Value builtin_print(Value *values, int64_t count) {
    for (int64_t i = 0; i < count; ++i) {
        if (i > 0) {
            printf(" ");
        }
        print_value(values[i]);
    }
    printf("\n");
    return make_none();
}

"#;

pub fn escape_c_string(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

pub fn write_temp_file(contents: &str, suffix: &str) -> Result<(PathBuf, PathBuf)> {
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

pub fn compile_source(
    source: &str,
    suffix: &str,
    compile_error: &str,
) -> Result<(PathBuf, PathBuf)> {
    let (source_path, binary_path) = write_temp_file(source, suffix)?;
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
        bail!("{compile_error}: {stderr}");
    }
    Ok((source_path, binary_path))
}

pub fn run_compiled_binary(binary_path: &Path, run_error: &str) -> Result<String> {
    let output = Command::new(binary_path)
        .output()
        .context("Running compiled program")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("{run_error}: {stderr}");
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
