use inkwell::IntPredicate;

use crate::compiler::parser::python_ast::CompareOp;

impl From<CompareOp> for IntPredicate {
    fn from(op: CompareOp) -> Self {
        match op {
            CompareOp::Eq => IntPredicate::EQ,
            CompareOp::NotEq => IntPredicate::NE,
            CompareOp::Lt => IntPredicate::SLT,
            CompareOp::LtE => IntPredicate::SLE,
            CompareOp::Gt => IntPredicate::SGT,
            CompareOp::GtE => IntPredicate::SGE,
        }
    }
}
