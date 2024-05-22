// Copyright 2023 Oxide Computer Company

use cargo_typify::{convert, CliArgs, ConvertResult};
use clap::Parser;

use color_eyre::eyre::{Context, Result};

#[derive(Parser)] // requires `derive` feature
#[command(name = "cargo")]
#[command(bin_name = "cargo")]
enum CargoCli {
    Typify(CliArgs),
}

fn main() -> Result<()> {
    env_logger::init();
    color_eyre::install()?;

    let cli = CargoCli::parse();
    let CargoCli::Typify(args) = cli;

    let ConvertResult { contents, tree } =
        convert(&args).wrap_err("Failed to convert JSON Schema to Rust code")?;

    if let Some(output_path) = &args.output_path() {
        std::fs::write(output_path, contents).wrap_err_with(|| {
            format!("Failed to write output to file: {}", output_path.display())
        })?;
    } else {
        print!("{contents}");
    }

    if let Some(tree) = tree {
        if let Some(output_tree_path) = &args.tree_out_path() {
            std::fs::write(output_tree_path, tree).wrap_err_with(|| {
                format!(
                    "Failed to write output to file: {}",
                    output_tree_path.display()
                )
            })?;
        } else {
            println!("{tree}");
        }
    }

    Ok(())
}
