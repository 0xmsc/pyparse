use std::fs;
use std::io::{self, Read};

use anyhow::{Context, Result, bail};
use pyparse::{backend, lexer, parser};

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let mut backend_name = "interpreter".to_string();
    let mut input_path: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--backend" | "-b" => {
                backend_name = args
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("Missing backend name after {arg}"))?;
            }
            _ => {
                input_path = Some(arg);
                if args.next().is_some() {
                    bail!("Only one input file is supported");
                }
                break;
            }
        }
    }

    let source = if let Some(path) = input_path {
        fs::read_to_string(&path).with_context(|| format!("Reading {path}"))?
    } else {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .context("Reading stdin")?;
        buffer
    };

    let tokens = lexer::tokenize(&source)?;
    let program = parser::parse_tokens(tokens)?;

    for backend in backend::backends() {
        if backend.name() == backend_name {
            let output = backend.run(&program)?;
            if !output.is_empty() {
                print!("{output}");
            }
            return Ok(());
        }
    }

    bail!("Unknown backend '{backend_name}'")
}
