use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub struct PyLocation {
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
        // decorator_list: Vec<PyJsonNode>, don't care rn
        returns: Option<Box<PyJsonNode>>,
        type_comment: Option<Box<PyJsonNode>>,
        type_params: Vec<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
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
        location: PyLocation,
        type_comment: Option<Box<PyJsonNode>>,
    },
    Return {
        value: Option<Box<PyJsonNode>>,
        #[serde(flatten)]
        location: PyLocation,
    },
    BinOp {
        left: Box<PyJsonNode>,
        op: Box<PyJsonNode>,
        right: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    Add,
    Sub,
    Name {
        id: String,
        ctx: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    Load,
    Store,
    Constant {
        value: Option<serde_json::Value>,
        #[serde(flatten)]
        location: PyLocation,
        //kind: Option<String>, idk what these are
        //n: Option<i64>,
        //s: Option<i64>,
    },
    UnaryOp {
        op: Box<PyJsonNode>,
        operand: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    USub,
    Assign {
        targets: Vec<PyJsonNode>,
        value: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    If {
        body: Vec<PyJsonNode>,
        orelse: Vec<PyJsonNode>,
        test: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    Pass {
        #[serde(flatten)]
        location: PyLocation,
    },
    BoolOp {
        op: Box<PyJsonNode>,
        values: Vec<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    And,
    Or,
    Not,
    Compare {
        left: Box<PyJsonNode>,
        ops: Vec<PyJsonNode>,
        comparators: Vec<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    While {
        body: Vec<PyJsonNode>,
        test: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
    AugAssign {
        target: Box<PyJsonNode>,
        op: Box<PyJsonNode>,
        value: Box<PyJsonNode>,
        #[serde(flatten)]
        location: PyLocation,
    },
}

impl PyJsonNode {
    pub fn load_from_str(s: &str) -> anyhow::Result<Self> {
        let node = serde_json::from_str(s)?;
        Ok(node)
    }
}
