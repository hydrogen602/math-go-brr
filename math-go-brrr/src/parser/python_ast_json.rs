// use std::{fs::File, io::BufReader, path::Path};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Location {
    pub col_offset: u64,
    pub end_col_offset: u64,
    pub end_lineno: u64,
    pub lineno: u64,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "_type")]
pub enum PyJsonNode {
    Module {
        body: Vec<PyJsonNode>,
        type_ignores: Vec<PyJsonNode>,
    },
    FunctionDef {
        name: String,
        args: Box<PyJsonNode>,
        body: Vec<PyJsonNode>,
        decorator_list: Vec<PyJsonNode>,
        returns: Option<Box<PyJsonNode>>,
        type_comment: Option<Box<PyJsonNode>>,
        type_params: Vec<PyJsonNode>,
        #[serde(flatten)]
        location: Location,
    },
    #[serde(rename = "arguments")]
    Arguments {
        args: Vec<PyJsonNode>,
        defaults: Vec<PyJsonNode>,
        kw_defaults: Vec<PyJsonNode>,
        kwonlyargs: Vec<PyJsonNode>,
        kwarg: Option<Box<PyJsonNode>>,
        posonlyargs: Vec<PyJsonNode>,
        vararg: Option<Box<PyJsonNode>>,
    },
    #[serde(rename = "arg")]
    Arg {
        arg: String,
        annotation: Option<Box<PyJsonNode>>,
        #[serde(flatten)]
        location: Location,
        type_comment: Option<Box<PyJsonNode>>,
    },
    Return {
        value: Option<Box<PyJsonNode>>,
        #[serde(flatten)]
        location: Location,
    },
    BinOp {
        left: Box<PyJsonNode>,
        op: Box<PyJsonNode>,
        right: Box<PyJsonNode>,
        #[serde(flatten)]
        location: Location,
    },
    Add,
    Sub,
    Name {
        id: String,
        ctx: Box<PyJsonNode>,
        #[serde(flatten)]
        location: Location,
    },
    Load,
}

impl PyJsonNode {
    // pub fn load_from_file(path: impl AsRef<Path>) -> anyhow::Result<Self> {
    //     let file = File::open(path)?;
    //     let reader = BufReader::new(file);

    //     let node = serde_json::from_reader(reader)?;
    //     Ok(node)
    // }

    pub fn load_from_str(s: &str) -> anyhow::Result<Self> {
        let node = serde_json::from_str(s)?;
        Ok(node)
    }
}
