use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::{
    anyhow_500, bail_400, bail_500, bail_type_err,
    compiler::{
        parser::{
            self,
            python_ast::{BoolBinOp, CompareOp, Located, LocatedAdder},
            CompileResult,
        },
        CompileOpts,
    },
    signature::Signature,
    Location,
};

use super::{
    super::{
        parser::python_ast::{Arg, BinOp, ConstantAST, ExpressionAST, FunctionAST, StatementAST},
        Type,
    },
    runtime_err::RuntimeError,
    Bool, Typed, I64,
};

/// This is currently for only one function
pub struct CodeGen<'ctx, 'm> {
    context: &'ctx Context,
    module: &'m Module<'ctx>,
    builder: Builder<'ctx>,

    /// Variables in the function
    variables: HashMap<String, VarData<'ctx>>,

    tmp_var_counter: u64,
    block_counter: u64,

    err_code: Option<PointerValue<'ctx>>,
}

#[derive(Debug, Clone, Copy)]
enum VarData<'ctx> {
    Const(Typed<IntValue<'ctx>, IntValue<'ctx>>),
    Var(Typed<PointerValue<'ctx>, PointerValue<'ctx>>),
}

#[derive(Debug, Clone, Copy)]
struct Expr<'ctx>(pub Typed<IntValue<'ctx>, IntValue<'ctx>>);

impl<'ctx> From<Typed<IntValue<'ctx>, IntValue<'ctx>>> for Expr<'ctx> {
    fn from(val: Typed<IntValue<'ctx>, IntValue<'ctx>>) -> Self {
        Self(val)
    }
}

impl<'ctx> Expr<'ctx> {
    fn into_dyn(&self) -> &dyn BasicValue<'ctx> {
        match &self.0 {
            Typed::I64(val) => val,
            Typed::Bool(val) => val,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IsTerminal {
    Terminal,
    NotTerminal,
}

impl<'ctx, 'm> CodeGen<'ctx, 'm> {
    fn new_tmp_var_name(&mut self) -> String {
        let name = format!("_tmp_var_{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;
        name
    }

    fn new_if_block_name(&mut self) -> (String, String) {
        let if_name = format!("_if_block_{}", self.block_counter);
        let else_name = format!("_else_block_{}", self.block_counter);
        self.block_counter += 1;
        (if_name, else_name)
    }

    fn new_while_block_name(&mut self) -> (String, String) {
        let condition_name = format!("_condition_block_{}", self.block_counter);
        let while_name = format!("_while_block_{}", self.block_counter);
        self.block_counter += 1;
        (condition_name, while_name)
    }

    fn new_generic_block_name(&mut self) -> String {
        let name = format!("_block_{}", self.block_counter);
        self.block_counter += 1;
        name
    }

    pub fn new(context: &'ctx Context, module: &'m Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            variables: HashMap::new(),
            tmp_var_counter: 0,
            block_counter: 0,
            err_code: None,
        }
    }

    /// If err is true, it will set an error pointer and return 0
    fn jit_compile_error_handling(
        &mut self,
        is_err: Bool<IntValue<'ctx>>,
        function: FunctionValue<'ctx>,
        err: RuntimeError,
        return_type: Type,
    ) -> CompileResult<()> {
        let (if_block_name, else_block_name) = self.new_if_block_name();

        let if_block = self.context.append_basic_block(function, &if_block_name);
        let else_block = self.context.append_basic_block(function, &else_block_name);

        self.builder
            .build_conditional_branch(is_err.0, if_block, else_block)?;

        self.builder.position_at_end(if_block);

        let err_val = I64::from_i64(&self.context, err.into_raw_value() as i64);
        let err_ptr = self.err_code.ok_or_else(|| {
            anyhow_500!("Error pointer not set, can't handle errors in this function")
        })?;

        self.builder.build_store(err_ptr, err_val.0)?;

        let ret_val: Expr = match return_type {
            Type::I64 => Typed::I64(self.context.i64_type().const_zero()).into(),
            Type::Bool => Typed::Bool(self.context.bool_type().const_zero()).into(),
        };
        self.builder.build_return(Some(ret_val.into_dyn()))?;

        self.builder.position_at_end(else_block);

        Ok(())
    }

    fn jit_compile_expr(
        &mut self,
        val: Located<ExpressionAST>,
        function: FunctionValue<'ctx>,
        return_type: Type,
    ) -> CompileResult<Expr<'ctx>> {
        let Located { value, location } = val;
        match value {
            ExpressionAST::BoolBinOp(lhs, op, rhs) => {
                let lhs = self.jit_compile_expr(*lhs, function, return_type)?;
                let rhs = self.jit_compile_expr(*rhs, function, return_type)?;
                let name = self.new_tmp_var_name();

                Ok(Expr(match (lhs.0, op, rhs.0) {
                    (Typed::Bool(lhs), BoolBinOp::And, Typed::Bool(rhs)) => {
                        Typed::Bool(self.builder.build_and(lhs, rhs, &name)?)
                    }
                    (Typed::Bool(lhs), BoolBinOp::Or, Typed::Bool(rhs)) => {
                        Typed::Bool(self.builder.build_or(lhs, rhs, &name)?)
                    }
                    (Typed::I64(_), _, _) => {
                        bail_type_err!("BoolBinOp not supported for i64" @ location)
                    }
                    (_, _, Typed::I64(_)) => {
                        bail_type_err!("BoolBinOp not supported for i64" @ location)
                    }
                }))
            }
            ExpressionAST::BinOp(lhs, op, rhs) => {
                let lhs = self.jit_compile_expr(*lhs, function, return_type)?;
                let rhs = self.jit_compile_expr(*rhs, function, return_type)?;
                let name = self.new_tmp_var_name();

                Ok(Expr(match (lhs.0, op, rhs.0) {
                    (Typed::I64(lhs), BinOp::Add, Typed::I64(rhs)) => {
                        Typed::I64(self.builder.build_int_add(lhs, rhs, &name)?)
                    }
                    (Typed::I64(lhs), BinOp::Sub, Typed::I64(rhs)) => {
                        Typed::I64(self.builder.build_int_sub(lhs, rhs, &name)?)
                    }
                    (Typed::I64(lhs), BinOp::Mult, Typed::I64(rhs)) => {
                        Typed::I64(self.builder.build_int_mul(lhs, rhs, &name)?)
                    }
                    (Typed::I64(lhs), BinOp::FloorDiv, Typed::I64(rhs)) => {
                        let zero = self.context.i64_type().const_zero();
                        let tmp_var = self.new_tmp_var_name();
                        let is_err = Bool(self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ,
                            rhs,
                            zero,
                            &tmp_var,
                        )?);
                        self.jit_compile_error_handling(
                            is_err,
                            function,
                            RuntimeError::zero_division_error(location),
                            return_type,
                        )?;

                        Typed::I64(self.builder.build_int_signed_div(lhs, rhs, &name)?)
                    }
                    (
                        Typed::Bool(_),
                        BinOp::Add | BinOp::Sub | BinOp::Mult | BinOp::FloorDiv,
                        _,
                    ) => {
                        bail_type_err!("Add/Sub not supported for bool" @ location)
                    }
                    (
                        _,
                        BinOp::Add | BinOp::Sub | BinOp::Mult | BinOp::FloorDiv,
                        Typed::Bool(_),
                    ) => {
                        bail_type_err!("Add/Sub not supported for bool" @ location)
                    }
                }))
            }
            ExpressionAST::Name(name) => {
                // only i64 for now. load var
                let data = self
                    .variables
                    .get(&name)
                    .copied()
                    .ok_or_else(|| anyhow_500!("Unknown variable: {}", name))?;

                Ok(Expr(match data {
                    VarData::Const(val) => val,
                    VarData::Var(Typed::I64(ptr)) => {
                        let tmp_var = self.new_tmp_var_name();
                        let ty = self.context.i64_type();
                        let BasicValueEnum::IntValue(value) =
                            self.builder.build_load(ty, ptr, &tmp_var)?
                        else {
                            bail_500!("Expected IntValue, this is a bug");
                        };
                        Typed::I64(value)
                    }
                    VarData::Var(Typed::Bool(ptr)) => {
                        let tmp_var = self.new_tmp_var_name();
                        let ty = self.context.bool_type();
                        let BasicValueEnum::IntValue(value) =
                            self.builder.build_load(ty, ptr, &tmp_var)?
                        else {
                            bail_500!("Expected IntValue, this is a bug");
                        };
                        Typed::Bool(value)
                    }
                }))
            }
            ExpressionAST::Constant(const_) => {
                match const_ {
                    ConstantAST::I64(val) => {
                        // idk about sign_extend, but we are passing in 64 bits.
                        // even though it says u64, it still does it signed
                        Ok(Expr(Typed::I64(
                            self.context.i64_type().const_int(val as u64, false),
                        )))
                    }
                    ConstantAST::Bool(val) => Ok(Expr(Typed::Bool(
                        self.context.bool_type().const_int(val as u64, false),
                    ))),
                }
            }
            ExpressionAST::UnaryOp(op, val) => {
                let val = self.jit_compile_expr(*val, function, return_type)?;
                let name = self.new_tmp_var_name();

                Ok(Expr(match (val.0, op) {
                    (Typed::I64(val), parser::python_ast::UnaryOp::USub) => {
                        Typed::I64(self.builder.build_int_neg(val, &name)?)
                    }
                    (Typed::Bool(val), parser::python_ast::UnaryOp::Not) => {
                        Typed::Bool(self.builder.build_not(val, &name)?)
                    }
                    (Typed::Bool(_), parser::python_ast::UnaryOp::USub) => {
                        bail_type_err!("Unary minus on bool not supported" @ location)
                    }
                    (Typed::I64(_), parser::python_ast::UnaryOp::Not) => {
                        bail_type_err!("Not on i64 not supported" @ location)
                    }
                }))
            }
            ExpressionAST::MultiOp(left, rest) => {
                let mut left = self.jit_compile_expr(*left, function, return_type)?;

                if rest.is_empty() {
                    bail_500!(
                        "MultiOp comparison with only one value is not allowed. This is a bug"
                    );
                }

                let mut compare_results: Vec<Bool<IntValue<'ctx>>> = Vec::new();
                for (op, right) in rest {
                    let right = self.jit_compile_expr(right, function, return_type)?;
                    let name = self.new_tmp_var_name();

                    compare_results.push(Bool(match (left.0, op, right.0) {
                        (
                            Typed::I64(lhs),
                            CompareOp::Eq
                            | CompareOp::NotEq
                            | CompareOp::Gt
                            | CompareOp::GtE
                            | CompareOp::Lt
                            | CompareOp::LtE,
                            Typed::I64(rhs),
                        ) => self.builder.build_int_compare(op.into(), lhs, rhs, &name)?,
                        (Typed::Bool(lhs), CompareOp::Eq | CompareOp::NotEq, Typed::Bool(rhs)) => {
                            self.builder.build_int_compare(op.into(), lhs, rhs, &name)?
                        }
                        (Typed::Bool(_), _, Typed::Bool(_)) => {
                            bail_type_err!(location, "Cannot apply op {} on bool", op)
                        }
                        (Typed::I64(_), _, Typed::Bool(_)) => {
                            bail_type_err!("Cannot mix types in comparison" @ location)
                        }
                        (Typed::Bool(_), _, Typed::I64(_)) => {
                            bail_type_err!("Cannot mix types in comparison" @ location)
                        }
                    }));

                    left = right;
                }

                // all conditions are and'ed together
                Ok(Expr(
                    compare_results
                        .into_iter()
                        .try_fold(Bool::get_true(&self.context), |prev, this| {
                            let var_name = self.new_tmp_var_name();
                            self.builder.build_and(prev.0, this.0, &var_name).map(Bool)
                        })?
                        .into(),
                ))
            }
        }
    }

    /// return true if stmt is terminal like return
    fn jit_compile_stmt(
        &mut self,
        val: Located<StatementAST>,
        return_type: Type,
        function: FunctionValue<'ctx>,
        is_conditional: bool,
    ) -> CompileResult<IsTerminal> {
        let Located { value, location } = val;
        Ok(match value {
            StatementAST::If {
                if_block: if_code,
                else_block: else_code,
                condition,
            } => {
                let condition = self.jit_compile_expr(condition, function, return_type)?;

                let cond = match condition.0 {
                    Typed::Bool(val) => val,
                    other => bail_type_err!(Type::Bool => other.into(), location),
                };

                let (if_block_name, else_block_name) = self.new_if_block_name();
                // FIXME: if a var is declared in the if block, it would be accessible in the else block even
                // if its not initialized, or worse, a garbage pointer
                // right now its enforced in the code analysis, but we should also enforce it here
                let if_block = self.context.append_basic_block(function, &if_block_name);
                let else_block = self.context.append_basic_block(function, &else_block_name);

                let afterwards_block = self
                    .context
                    .append_basic_block(function, &self.new_generic_block_name());

                self.builder
                    .build_conditional_branch(cond, if_block, else_block)?;

                self.builder.position_at_end(if_block);
                let got_term = self.jit_compile_body(if_code, return_type, function, true)?;

                if got_term == IsTerminal::NotTerminal {
                    self.builder.build_unconditional_branch(afterwards_block)?;
                }

                self.builder.position_at_end(else_block);
                let got_term = self.jit_compile_body(else_code, return_type, function, true)?;

                if got_term == IsTerminal::NotTerminal {
                    self.builder.build_unconditional_branch(afterwards_block)?;
                }

                self.builder.position_at_end(afterwards_block);

                IsTerminal::NotTerminal
            }
            StatementAST::Return(val) => {
                let val = val
                    .map(|val| self.jit_compile_expr(val, function, return_type))
                    .transpose()?;

                let val = match val {
                    Some(val) => match (return_type, val.0) {
                        (Type::I64, Typed::I64(_)) => val,
                        (Type::Bool, Typed::Bool(_)) => val,
                        (ret_ty, val) => bail_type_err!(ret_ty => val.into(), location),
                    },
                    None => Expr(match return_type {
                        Type::I64 => Typed::I64(self.context.i64_type().const_zero()).into(),
                        Type::Bool => Typed::Bool(self.context.bool_type().const_zero()).into(),
                    }),
                };

                self.builder.build_return(Some(val.into_dyn()))?;

                IsTerminal::Terminal
            }
            StatementAST::Assign { target, value } => {
                let existing_var = self.variables.get(&target).cloned();

                let value = self.jit_compile_expr(value, function, return_type)?;

                match existing_var {
                    Some(var) => match var {
                        VarData::Const(_) => {
                            bail_500!("Cannot assign to a constant. This is a bug")
                        }
                        VarData::Var(Typed::I64(ptr)) => {
                            let Ok(value) = value.0.into_i64() else {
                                bail_type_err!(Type::I64 => value.0.into(), location)
                            };

                            self.builder.build_store(ptr, value)?;
                        }
                        VarData::Var(Typed::Bool(ptr)) => {
                            let Ok(value) = value.0.into_bool() else {
                                bail_type_err!(Type::Bool => value.0.into(), location)
                            };

                            self.builder.build_store(ptr, value)?;
                        }
                    },
                    None => {
                        if is_conditional {
                            bail_500!("Cannot declare variables in conditional blocks. This is a bug and should have been caught earlier")
                        }

                        let typed = match value.0 {
                            Typed::I64(value) => {
                                let ptr = self
                                    .builder
                                    .build_alloca(self.context.i64_type(), &target)?;
                                self.builder.build_store(ptr, value)?;
                                Typed::I64(ptr)
                            }
                            Typed::Bool(value) => {
                                let ptr = self
                                    .builder
                                    .build_alloca(self.context.bool_type(), &target)?;
                                self.builder.build_store(ptr, value)?;
                                Typed::Bool(ptr)
                            }
                        };

                        self.variables.insert(target, VarData::Var(typed));
                    }
                }

                IsTerminal::NotTerminal
            }
            StatementAST::While { body, condition } => {
                let (condition_name, while_name) = self.new_while_block_name();

                let condition_block = self.context.append_basic_block(function, &condition_name);
                let while_block = self.context.append_basic_block(function, &while_name);

                let afterwards_block = self
                    .context
                    .append_basic_block(function, &self.new_generic_block_name());

                // jump to condition block
                self.builder.build_unconditional_branch(condition_block)?;

                // move builder to condition block
                self.builder.position_at_end(condition_block);

                let b = match self
                    .jit_compile_expr(condition, function, return_type)?
                    .0
                    .into_bool()
                {
                    Ok(b) => b,
                    Err(t) => bail_type_err!(
                        location,
                        "Expected bool in while condition but got {}",
                        t.py_type_name()
                    ),
                };

                self.builder
                    .build_conditional_branch(b, while_block, afterwards_block)?;

                // move builder to while block
                self.builder.position_at_end(while_block);

                let got_term = self.jit_compile_body(body, return_type, function, true)?;

                if got_term == IsTerminal::NotTerminal {
                    self.builder.build_unconditional_branch(condition_block)?;
                }

                // move builder to afterwards block
                self.builder.position_at_end(afterwards_block);

                IsTerminal::NotTerminal
            }
        })
    }

    fn jit_compile_body(
        &mut self,
        body: Vec<Located<StatementAST>>,
        return_type: Type,
        function: FunctionValue<'ctx>,
        is_conditional: bool,
    ) -> CompileResult<IsTerminal> {
        for stmt in body {
            let is_terminal = self.jit_compile_stmt(stmt, return_type, function, is_conditional)?;

            // if we hit a terminal statement, we should not continue
            // as everything else is dead code and llvm is picky about its
            // control flow
            if is_terminal == IsTerminal::Terminal {
                return Ok(IsTerminal::Terminal);
            }
        }

        // If we got here, it means we didn't hit a terminal statement
        Ok(IsTerminal::NotTerminal)
    }

    fn jit_compile_args(
        &mut self,
        args: Vec<Arg>,
        function: &FunctionValue<'ctx>,
        code_analysis: &CodeAnalysis,
    ) -> CompileResult<()> {
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();

        // set up error handling pointer
        let err_code = function
            .get_nth_param(0)
            .ok_or_else(|| anyhow_500!("Error handling pointer not found. This is a bug"))?
            .into_pointer_value();
        self.err_code = Some(err_code);

        for (i, arg) in args.into_iter().enumerate() {
            let i = i + 1; // first goes to error handling pointer

            let var_info = code_analysis
                .variable_info
                .get(&arg.arg)
                .ok_or_else(|| anyhow_500!("Variable info not found. This is a bug"))?;

            let raw_val = function
                .get_nth_param(i as u32)
                .ok_or_else(|| anyhow_500!("Argument count off. This is a bug"))?
                .into_pointer_value();

            let x_ptr = match arg.type_ {
                Type::I64 => Typed::I64(raw_val),
                Type::Bool => Typed::Bool(raw_val),
            };

            if var_info.is_assigned {
                let name = arg.arg.clone();
                self.variables.insert(name, VarData::Var(x_ptr));
            } else {
                let tmp_var = self.new_tmp_var_name();
                self.variables.insert(
                    arg.arg,
                    VarData::Const(match arg.type_ {
                        Type::I64 => Typed::I64(
                            self.builder
                                .build_load(
                                    i64_type,
                                    x_ptr.into_i64().expect("This is a bug"),
                                    &tmp_var,
                                )?
                                .into_int_value(),
                        ),
                        Type::Bool => Typed::Bool(
                            self.builder
                                .build_load(
                                    bool_type,
                                    x_ptr.into_bool().expect("This is a bug"),
                                    &tmp_var,
                                )?
                                .into_int_value(),
                        ),
                    }),
                );
            }
        }

        Ok(())
    }

    pub fn jit_compile_function(
        mut self,
        func: FunctionAST,
        compile_opts: CompileOpts,
    ) -> CompileResult<(FunctionValue<'ctx>, Signature)> {
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();
        // let ptr_type = self.context.ptr_type(6);

        // Context::ptr
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let code_analysis = CodeAnalysis::analyze_function(&func)?;

        let FunctionAST {
            name,
            args,
            body,
            return_type,
        } = func;

        // first arg is error handling pointer
        let mut arg_types_llvm = vec![ptr_type.into()];

        let mut arg_types = vec![];
        for arg in &args {
            arg_types_llvm.push(ptr_type.into());
            arg_types.push(arg.type_)
        }

        let sig = Signature::new(arg_types, return_type);

        let fn_type = match return_type {
            Type::I64 => i64_type.fn_type(&arg_types_llvm, false),
            Type::Bool => bool_type.fn_type(&arg_types_llvm, false),
        };

        let function = self.module.add_function(&name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        self.jit_compile_args(args, &function, &code_analysis)?;

        let got_term = self.jit_compile_body(body, return_type, function, false)?;

        if got_term == IsTerminal::NotTerminal {
            self.jit_compile_stmt(
                // this is default for now cause this is a made-up instructions
                // so it has no mapping to the source
                StatementAST::Return(None).with_loc(Location::default()),
                return_type,
                function,
                false,
            )?;
        }

        if compile_opts.dump_ir {
            eprintln!("");
            eprintln!(">>> IR for function {}", name);
            function.print_to_stderr();
            eprintln!("<<<");
            eprintln!("");
        }

        Ok((function, sig))
    }
}

struct CodeAnalysis {
    pub variable_info: HashMap<String, VariableInfo>,
}

#[derive(Debug)]
struct VariableInfo {
    /// false until proven otherwise
    is_assigned: bool,
}

impl CodeAnalysis {
    fn analyze_function(func: &FunctionAST) -> CompileResult<Self> {
        let FunctionAST { args, body, .. } = func;

        let mut variable_info: HashMap<String, VariableInfo> = HashMap::new();

        for arg in args {
            variable_info.insert(arg.arg.clone(), VariableInfo { is_assigned: false });
        }

        Self::analyze_body(body, &mut variable_info, false)?;

        Ok(Self { variable_info })
    }

    /// Since variables declared in conditional blocks are accessible outside the block,
    /// and a conditionally declared variable could lead to dangling pointers or
    /// uninitialized variables, right now, we do not permit declaring variables in conditional blocks
    fn analyze_body(
        body: &Vec<Located<StatementAST>>,
        variable_info: &mut HashMap<String, VariableInfo>,
        no_declaring_allowed: bool,
    ) -> CompileResult<()> {
        for Located {
            value: stmt,
            location: _,
        } in body
        {
            match stmt {
                StatementAST::Assign { target, .. } => {
                    if let Some(var) = variable_info.get_mut(target) {
                        var.is_assigned = true;
                    } else {
                        if no_declaring_allowed {
                            // TODO: attach location info to this
                            bail_400!("Cannot declare variables in conditional blocks")
                        }
                        variable_info.insert(target.clone(), VariableInfo { is_assigned: true });
                    }
                }
                StatementAST::Return(_) => {}
                StatementAST::If {
                    if_block,
                    else_block,
                    ..
                } => {
                    Self::analyze_body(if_block, variable_info, true)?;
                    Self::analyze_body(else_block, variable_info, true)?;
                }
                StatementAST::While { body, .. } => {
                    Self::analyze_body(body, variable_info, true)?;
                }
            }
        }
        Ok(())
    }
}
