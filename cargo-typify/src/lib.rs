// Copyright 2023 Oxide Computer Company

//! cargo command to generate Rust code from a JSON Schema.

#![deny(missing_docs)]

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use clap::{ArgGroup, Args};
use color_eyre::eyre::{Context, Result};
use petgraph::{Direction, Graph};
use ptree::graph::write_graph_with;
use ptree::PrintConfig;
use typify::{CrateVers, TypeSpace, TypeSpaceSettings, UnknownPolicy};

/// A CLI for the `typify` crate that converts JSON Schema files to Rust code.
#[derive(Args)]
#[command(author, version, about)]
#[command(group(
ArgGroup::new("build")
.args(["builder", "no_builder"]),
))]
pub struct CliArgs {
    /// The input file to read from
    pub input: PathBuf,

    /// Whether to include a builder-style interface, this is the default.
    #[arg(short, long, default_value = "false", group = "build")]
    pub builder: bool,

    /// Inverse of `--builder`. When set the builder-style interface will not
    /// be included.
    #[arg(short = 'B', long, default_value = "false", group = "build")]
    pub no_builder: bool,

    /// Add an additional derive macro to apply to all defined types.
    #[arg(short, long = "additional-derive", value_name = "derive")]
    pub additional_derives: Vec<String>,

    /// The output file to write to. If not specified, the input file name will
    /// be used with a `.rs` extension.
    ///
    /// If `-` is specified, the output will be written to stdout.
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Specify each crate@version that can be assumed to be in use for types
    /// found in the schema with the x-rust-type extension.
    #[arg(long = "crate")]
    crates: Vec<CrateSpec>,

    /// Specify the policy unknown crates found in schemas with the
    /// x-rust-type extension.
    #[arg(
  long = "unknown-crates",
  value_parser = ["generate", "allow", "deny"]
  )]
    unknown_crates: Option<String>,

    /// Record reference tree in file. Input file name will
    /// be used with a `.tree` extension.
    #[arg(long)]
    pub print_refs: bool,

    /// The output file to write tree to. If not specified, the input file name will be used with a `.tree` extension.
    /// 
    /// If `-` is specified, the tree will be written to stdout.
    #[arg(long)]
    pub output_tree: Option<PathBuf>,
}

impl CliArgs {
    /// Output path.
    pub fn output_path(&self) -> Option<PathBuf> {
        match &self.output {
            Some(output_path) => {
                if output_path == &PathBuf::from("-") {
                    None
                } else {
                    Some(output_path.clone())
                }
            }
            None => {
                let mut output = self.input.clone();
                output.set_extension("rs");
                Some(output)
            }
        }
    }

    /// Out tree path.
    pub fn tree_out_path(&self) -> Option<PathBuf> {
        match &self.output_tree {
            Some(output_path_tree) => {
                if output_path_tree == &PathBuf::from("-") {
                    None
                } else {
                    Some(output_path_tree.clone())
                }
            }
            None => {
                if !self.print_refs {
                    None
                } else {
                    let mut output = self.input.clone();
                    output.set_extension("tree");
                    Some(output)
                }
            }
        }
    }

    /// Whether builder-style interface was selected.
    pub fn use_builder(&self) -> bool {
        !self.no_builder
    }
}

#[derive(Debug, Clone)]
struct CrateSpec {
    name: String,
    version: CrateVers,
    rename: Option<String>,
}

impl std::str::FromStr for CrateSpec {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn is_crate(s: &str) -> bool {
            !s.contains(|cc: char| !cc.is_alphabetic() && cc != '-' && cc != '_')
        }

        fn convert(s: &str) -> Option<CrateSpec> {
            let (rename, s) = if let Some(ii) = s.find('=') {
                let rename = &s[..ii];
                let rest = &s[ii + 1..];
                if !is_crate(rename) {
                    return None;
                }
                (Some(rename.to_string()), rest)
            } else {
                (None, s)
            };

            let ii = s.find('@')?;
            let crate_str = &s[..ii];
            let vers_str = &s[ii + 1..];

            if !is_crate(crate_str) {
                return None;
            }
            let version = CrateVers::parse(vers_str)?;

            Some(CrateSpec {
                name: crate_str.to_string(),
                version,
                rename,
            })
        }

        convert(s).ok_or("crate specifier must be of the form 'cratename@version'")
    }
}

/// Result of `convert` function.
pub struct ConvertResult {
    /// Generated code.
    pub contents: String,
    /// Generated tree.
    pub tree: Option<String>,
}

/// Generate Rust code for the selected JSON Schema.
pub fn convert<'a>(args: &CliArgs) -> Result<ConvertResult> {
    let mut tree = None;
    let content = std::fs::read_to_string(&args.input)
        .wrap_err_with(|| format!("Failed to open input file: {}", &args.input.display()))?;

    let schema = serde_json::from_str::<schemars::schema::RootSchema>(&content)
        .wrap_err("Failed to parse input file as JSON Schema")?;

    let mut settings = TypeSpaceSettings::default();
    settings.with_struct_builder(args.use_builder());

    for derive in &args.additional_derives {
        settings.with_derive(derive.clone());
    }

    for CrateSpec {
        name,
        version,
        rename,
    } in &args.crates
    {
        settings.with_crate(name, version.clone(), rename.as_ref());
    }

    if let Some(unknown_crates) = &args.unknown_crates {
        let unknown_crates = match unknown_crates.as_str() {
            "generate" => UnknownPolicy::Generate,
            "allow" => UnknownPolicy::Allow,
            "deny" => UnknownPolicy::Deny,
            _ => unreachable!(),
        };
        settings.with_unknown_crates(unknown_crates);
    }

    let mut type_space = TypeSpace::new(&settings);
    type_space.with_path(&args.input);
    type_space
        .add_root_schema(schema)
        .wrap_err("Schema conversion failed")?;

    if args.print_refs || args.output_tree.is_some() {
        let g = construct_graph(type_space.get_tree());

        let mut root_indexes = vec![];
        for index in g
            .node_indices()
            .filter(|i| g.neighbors_directed(*i, Direction::Incoming).count() == 0)
        {
            root_indexes.push(index);
        }

        let mut s = FmtToIoWrite(String::new());

        for root_index in root_indexes {
            write_graph_with(&g, root_index, &mut s, &PrintConfig::from_env()).unwrap();
        }

        tree = Some(s.0);
    }

    let intro = "#![allow(clippy::redundant_closure_call)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::match_single_binding)]
#![allow(clippy::clone_on_copy)]

use serde::{Deserialize, Serialize};
";

    let contents = format!("{intro}\n{}", type_space.to_stream());

    let contents = rustfmt_wrapper::rustfmt(contents).wrap_err("Failed to format Rust code")?;

    Ok(ConvertResult { contents, tree })
}

fn construct_graph(packages: &HashMap<String, HashSet<String>>) -> Graph<&String, &String> {
    let nodes: HashSet<_> = packages
        .iter()
        .flat_map(|(name, dependency)| dependency.into_iter().chain(Some(name)))
        .collect();
    let mut deps = Graph::new();
    for nude in nodes {
        deps.add_node(nude);
    }
    for (name, dependencies) in packages {
        let root_node = deps.node_indices().find(|i| deps[*i] == name).unwrap();
        for dep in dependencies {
            let dep_node = deps.node_indices().find(|i| deps[*i] == dep).unwrap();
            deps.add_edge(root_node, dep_node, name);
        }
    }
    deps
}

struct FmtToIoWrite<T>(T);

impl<T> std::io::Write for FmtToIoWrite<T>
where
    T: std::fmt::Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write_str(std::str::from_utf8(buf).unwrap()).unwrap();
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_parsing_stdout() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: Some(PathBuf::from("-")),
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert_eq!(args.output_path(), None);
    }

    #[test]
    fn test_output_parsing_file() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: Some(PathBuf::from("some_file.rs")),
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert_eq!(args.output_path(), Some(PathBuf::from("some_file.rs")));
    }

    #[test]
    fn test_output_parsing_default() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert_eq!(args.output_path(), Some(PathBuf::from("input.rs")));
    }

    #[test]
    fn test_builder_as_default_style() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert!(args.use_builder());
    }

    #[test]
    fn test_no_builder() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: true,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert!(!args.use_builder());
    }

    #[test]
    fn test_builder_opt_in() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: true,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: None,
        };

        assert!(args.use_builder());
    }

    #[test]
    fn test_output_tree_stdout() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: Some(PathBuf::from("-")),
        };

        assert_eq!(args.tree_out_path(), None);
    }

    #[test]
    fn test_tree_output_parsing_file() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: false,
            output_tree: Some(PathBuf::from("some_file.tree")),
        };

        assert_eq!(args.tree_out_path(), Some(PathBuf::from("some_file.tree")));
    }

    #[test]
    fn test_tree_output_parsing_default() {
        let args = CliArgs {
            input: PathBuf::from("input.json"),
            builder: false,
            additional_derives: vec![],
            output: None,
            no_builder: false,
            crates: vec![],
            unknown_crates: Default::default(),
            print_refs: true,
            output_tree: None,
        };

        assert_eq!(args.tree_out_path(), Some(PathBuf::from("input.tree")));
    }
}
