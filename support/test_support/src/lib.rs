use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};
use serde::Deserialize;

#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CaseClass {
    RuntimeSuccess,
    FrontendError,
    BackendRuntimeError,
    UnsupportedFeature,
}

#[derive(Debug, Deserialize, Clone)]
pub struct BenchConfig {
    pub enabled: bool,
    pub tags: Vec<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ExpectedOutcome {
    pub exit_code: i32,
    pub stdout_file: Option<String>,
    pub stderr_file: Option<String>,
    pub stderr_contains_file: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct CaseSpec {
    pub class: CaseClass,
    #[allow(dead_code)]
    pub parity: bool,
    #[serde(default)]
    pub unsupported_backends: Vec<String>,
    pub bench: BenchConfig,
    pub expected: ExpectedOutcome,
}

#[derive(Debug, Clone)]
pub struct Case {
    pub name: String,
    pub dir: PathBuf,
    pub program_path: PathBuf,
    pub spec: CaseSpec,
}

impl Case {
    pub fn read_text(&self, relative_path: &str) -> Result<String> {
        fs::read_to_string(self.dir.join(relative_path))
            .with_context(|| format!("Reading {} fixture file {}", self.name, relative_path))
    }
}

pub fn load_cases(programs_dir: &Path) -> Result<Vec<Case>> {
    let mut cases = Vec::new();

    for entry in
        fs::read_dir(programs_dir).with_context(|| format!("Reading {}", programs_dir.display()))?
    {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let case_path = path.join("case.yaml");
        if !case_path.exists() {
            continue;
        }

        let program_path = path.join("program.py");
        ensure!(
            program_path.exists(),
            "Missing program.py for case {}",
            path.display()
        );

        let case_name = path
            .file_name()
            .and_then(|value| value.to_str())
            .map(str::to_string)
            .with_context(|| format!("Invalid case directory name {}", path.display()))?;
        let case_raw = fs::read_to_string(&case_path)
            .with_context(|| format!("Reading {}", case_path.display()))?;
        let spec: CaseSpec = serde_yaml::from_str(&case_raw)
            .with_context(|| format!("Parsing {}", case_path.display()))?;

        cases.push(Case {
            name: case_name,
            dir: path,
            program_path,
            spec,
        });
    }

    ensure!(
        !cases.is_empty(),
        "No test cases found in {}",
        programs_dir.display()
    );
    cases.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(cases)
}

pub fn normalize_output(output: &str) -> String {
    output.replace("\r\n", "\n").trim_end().to_string()
}

pub fn detect_python_interpreter() -> Option<String> {
    if let Ok(python) = std::env::var("PYTHON") {
        let status = Command::new(&python).arg("--version").status().ok()?;
        if status.success() {
            return Some(python);
        }
    }

    let status = Command::new("python3").arg("--version").status().ok()?;
    if status.success() {
        return Some("python3".to_string());
    }

    None
}

pub fn run_python_file(interpreter: &str, path: &Path) -> Result<String> {
    let output = Command::new(interpreter)
        .arg(path)
        .output()
        .with_context(|| format!("Running python file {}", path.display()))?;
    ensure!(
        output.status.success(),
        "python failed for {}: {}",
        path.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

pub fn run_python_startup(interpreter: &str) -> Result<()> {
    let status = Command::new(interpreter)
        .arg("-c")
        .arg("pass")
        .status()
        .with_context(|| format!("Running '{interpreter} -c pass'"))?;
    ensure!(status.success(), "python startup command failed");
    Ok(())
}

pub fn validate_unsupported_backends(case: &Case, known_backends: &[&str]) -> Result<()> {
    for backend in &case.spec.unsupported_backends {
        ensure!(
            known_backends.contains(&backend.as_str()),
            "Case {} contains unknown unsupported backend '{}'",
            case.name,
            backend
        );
    }
    Ok(())
}

pub fn is_backend_unsupported(case: &Case, backend_name: &str) -> bool {
    case.spec
        .unsupported_backends
        .iter()
        .any(|name| name == backend_name)
}
