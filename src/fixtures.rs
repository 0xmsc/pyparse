use std::fs;
use std::path::{Path, PathBuf};

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
    pub parity: bool,
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
