use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate;

use std::borrow::Borrow;
use std::collections::HashMap;

use crate::parser::*;

pub struct OptionCodegen {
    display_ir: bool,
    show_optimization: bool,
}

impl OptionCodegen {
    pub fn new(display_ir: bool, show_optimization: bool) -> Self {
        Self {
            display_ir,
            show_optimization,
        }
    }
}

pub struct CodeGen<'a> {
    context: &'a Context,
    pub module: Module<'a>,
    builder: Builder<'a>,
    variables: HashMap<String, PointerValue<'a>>,
    pass_manager: Option<PassManager<FunctionValue<'a>>>,
    option_codegen: OptionCodegen,
}

impl<'a> CodeGen<'a> {
    pub fn new(
        context: &'a Context,
        builder: Builder<'a>,
        module: Module<'a>,
        pass_manager: Option<PassManager<FunctionValue<'a>>>,
        option_codegen: OptionCodegen,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            variables: HashMap::new(),
            pass_manager,
            option_codegen,
        }
    }

    /// Generate the IR code for a Program
    pub fn gencode_program(&mut self, program: Program) -> Result<(), String> {
        for stmt in program.stmts {
            match stmt {
                ASTNode::Function(f) => {
                    self.gencode_function(&f)?;
                }
                ASTNode::Extern(p) => {
                    self.gencode_prototype(&p)?;
                }
                ASTNode::Expression(e) => {
                    self.gencode_expr(&e)?;
                }
            }
        }

        Ok(())
    }

    /// Generate the IR code for a prototype
    pub fn gencode_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'a>, String> {
        //let args_types = Vec::<BasicTypeEnum>::with_capacity(proto.args.len());
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicTypeEnum>>();

        match self.module.get_function(proto.name.as_str()) {
            None => {
                let ftype = self
                    .context
                    .f64_type()
                    .fn_type(args_types.as_slice(), false);
                let f = self.module.add_function(proto.name.as_str(), ftype, None);

                for (i, arg) in f.get_param_iter().enumerate() {
                    arg.into_float_value().set_name(proto.args[i].as_str());
                }

                if self.option_codegen.display_ir {
                    f.print_to_stderr()
                }

                Ok(f)
            }
            Some(f) => {
                if f.get_basic_blocks().len() == 0 {
                    return Err("redefinition of function".to_owned());
                }

                if f.get_params().len() != proto.args.len() {
                    return Err("redefinition of function with different args".to_owned());
                }

                Err("Unknow error on redefinition of function".to_owned())
            }
        }
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    /// Helper function that ensures that the allocas are created in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str, function: &FunctionValue) -> PointerValue<'a> {
        let builder = self.context.create_builder();

        let entry_basic_block = function.get_first_basic_block().expect(&format!(
            "can't find first basic block for function: {}",
            name
        ));

        match entry_basic_block.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry_basic_block),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    /// Generate the IR code for a function
    pub fn gencode_function(&mut self, func: &Function) -> Result<FunctionValue<'a>, String> {
        let func_name = func.proto.name.as_str();

        if let Some(_) = self.module.get_function(func_name) {
            return Err(format!("Function: {} cannot be redefined", func_name));
        }

        // == prototype

        let proto_codegen = self.gencode_prototype(&func.proto)?;

        // return only compiled prototype if its an external function
        if func.body.is_none() {
            return Ok(proto_codegen);
        }

        let bb = self.context.append_basic_block(proto_codegen, "entry");
        self.builder.position_at_end(bb);

        self.variables.reserve(func.proto.args.len());

        for (arg, param) in func.proto.args.iter().zip(proto_codegen.get_param_iter()) {
            let alloca = self.create_entry_block_alloca(arg.as_str(), &proto_codegen);
            self.builder.build_store(alloca, param);
            self.variables.insert(arg.clone(), alloca);
        }

        // == body

        let body = self.gencode_expr(func.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // == verification by LLVM

        if proto_codegen.verify(true) {
            if self.option_codegen.display_ir {
                proto_codegen.print_to_stderr()
            }

            if let Some(pass_manager) = self.pass_manager.as_ref() {
                pass_manager.run_on(&proto_codegen);
            }

            if self.option_codegen.display_ir && self.option_codegen.show_optimization {
                proto_codegen.print_to_stderr()
            }

            Ok(proto_codegen)
        } else {
            unsafe {
                proto_codegen.delete();
            }

            Err(format!("Invalid generated function: {}", func_name))
        }
    }

    /// Generate the IR code for an Expression
    pub fn gencode_expr(&mut self, expr: &Expression) -> Result<FloatValue<'a>, String> {
        let res = match *expr {
            Expression::Number(value) => Ok(self.context.f64_type().const_float(value)),
            Expression::Variable(ref id) => match self.variables.get(id) {
                Some(var) => Ok(self.builder.build_load(*var, id).into_float_value()),
                None => Err("Could not find a matching variable.".to_owned()),
            },
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                let lhs_res = self.gencode_expr(lhs)?;
                let rhs_res = self.gencode_expr(rhs)?;

                match op {
                    BinaryOp::Plus => Ok(self.builder.build_float_add(lhs_res, rhs_res, "addtmp")),
                    BinaryOp::Minus => Ok(self.builder.build_float_sub(lhs_res, rhs_res, "subtmp")),
                    BinaryOp::Mult => Ok(self.builder.build_float_mul(lhs_res, rhs_res, "multmp")),
                    BinaryOp::GreaterThan => {
                        let fcmp = self.builder.build_float_compare(
                            FloatPredicate::UGT,
                            lhs_res,
                            rhs_res,
                            "cmptmp",
                        );
                        Ok(self.builder.build_unsigned_int_to_float(
                            fcmp,
                            self.context.f64_type(),
                            "tmpbool",
                        ))
                    }
                    BinaryOp::LowerThan => {
                        let fcmp = self.builder.build_float_compare(
                            FloatPredicate::ULT,
                            lhs_res,
                            rhs_res,
                            "cmptmp",
                        );
                        Ok(self.builder.build_unsigned_int_to_float(
                            fcmp,
                            self.context.f64_type(),
                            "tmpbool",
                        ))
                    }
                    BinaryOp::Equiv => {
                        let fcmp = self.builder.build_float_compare(
                            FloatPredicate::UEQ,
                            lhs_res,
                            rhs_res,
                            "cmptmp",
                        );
                        Ok(self.builder.build_unsigned_int_to_float(
                            fcmp,
                            self.context.f64_type(),
                            "tmpbool",
                        ))
                    }
                    BinaryOp::Eq => {
                        let var_name = match *lhs.borrow() {
                            Expression::Variable(ref var_name) => var_name,
                            _ => {
                                return Err(
                                    "Expected variable as left-hand operator of assignement."
                                        .to_owned(),
                                );
                            }
                        };

                        let var_val = self.gencode_expr(rhs)?;
                        let var = self
                            .variables
                            .get(var_name.as_str())
                            .ok_or("Undefined variable.")?;

                        self.builder.build_store(*var, var_val);
                        Ok(var_val)
                    }
                }
            }
            Expression::Call(ref name, ref args) => {
                match self.module.get_function(name) {
                    Some(func) => {
                        if func.count_params() as usize == args.len() {
                            let mut evaluated_args = Vec::new();

                            // When iterator<> map doesn't exist...
                            for a in args {
                                evaluated_args.push(self.gencode_expr(a)?);
                            }

                            let argsv: Vec<BasicValueEnum> = evaluated_args
                                .iter()
                                .by_ref()
                                .map(|&val| val.into())
                                .collect();

                            match self
                                .builder
                                .build_call(func, argsv.as_slice(), name)
                                .try_as_basic_value()
                                .left()
                            {
                                Some(value) => Ok(value.into_float_value()),
                                None => Err(format!("Error creating the call {}", name)),
                            }
                        } else {
                            Err(format!(
                                "Incorrect params expected = {:?} but got = {:?}",
                                func.get_params(),
                                args
                            ))
                        }
                    }
                    None => Err(format!("Unknow call {}", name)),
                }
            }
            Expression::Conditional(ref cond, ref e1, ref e2) => {
                let cond = self.gencode_expr(cond)?;
                let float_compare = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    self.context.f64_type().const_zero(),
                    "ifcond",
                );

                // == Build block then and else and ifcont
                let parent = if let Some(b) = self.builder.get_insert_block() {
                    b.get_parent().expect("Should have a parent")
                } else {
                    return Err(format!("no parent for conditional"));
                };

                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let ifcont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(float_compare, then_bb, else_bb);

                // === build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.gencode_expr(e1)?;
                self.builder.build_unconditional_branch(ifcont_bb);
                let then_bb = self.builder.get_insert_block().unwrap();

                // == build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.gencode_expr(&e2)?;
                self.builder.build_unconditional_branch(ifcont_bb);
                let else_bb = self.builder.get_insert_block().unwrap();

                // === build merge block
                self.builder.position_at_end(ifcont_bb);

                // === build phi SSA
                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }
            Expression::ForLoop(ref id, ref start, ref cond, ref step, ref body) => {
                let parent = if let Some(b) = self.builder.get_insert_block() {
                    b.get_parent().expect("Should have a parent")
                } else {
                    return Err(format!("no parent for conditional"));
                };
                let start_alloca = self.create_entry_block_alloca(id, &parent);
                let start = self.gencode_expr(start)?;
                self.builder.build_store(start_alloca, start);

                let old_val = self.variables.remove(id.as_str());
                self.variables.insert(id.to_owned(), start_alloca);

                let loop_bb = self.context.append_basic_block(parent, "loop");
                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                // == build body

                // Emit the body of the loop.  This, like any other expr, can change the
                // current BB.  Note that we ignore the value computed by the body, but
                // don't allow an error
                self.gencode_expr(&body)?;
                let step = match step {
                    Some(step) => self.gencode_expr(&step)?,
                    None => self.context.f64_type().const_float(1.0),
                };
                let cond = self.gencode_expr(&cond)?;

                let curr_var = self.builder.build_load(start_alloca, id);
                let next_var =
                    self.builder
                        .build_float_add(curr_var.into_float_value(), step, "nextvar");
                self.builder.build_store(start_alloca, next_var);

                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                let after_bb = self.context.append_basic_block(parent, "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);

                self.variables.remove(id);

                if let Some(val) = old_val {
                    self.variables.insert(id.to_owned(), val);
                }

                Ok(self.context.f64_type().const_float(0.0))
            }
            Expression::WhileLoop(ref cond, ref body) => {
                let cond = self.gencode_expr(cond)?;

                let parent = if let Some(b) = self.builder.get_insert_block() {
                    b.get_parent().expect("Should have a parent")
                } else {
                    return Err(format!("no parent for conditional"));
                };

                let loop_bb = self.context.append_basic_block(parent, "loop");
                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                self.gencode_expr(&body)?;

                let after_bb = self.context.append_basic_block(parent, "afterloop");

                let cond_end = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    self.context.f64_type().const_zero(),
                    "loopcond",
                );

                self.builder
                    .build_conditional_branch(cond_end, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);

                Ok(self.context.f64_type().const_float(0.0))
            }
            Expression::VarIn(ref var_name, ref var_val, ref inbody) => {
                let parent = if let Some(b) = self.builder.get_insert_block() {
                    b.get_parent().expect("Should have a parent")
                } else {
                    return Err(format!("no parent for conditional"));
                };

                let init_val = self.gencode_expr(&var_val)?;
                let alloca = self.create_entry_block_alloca(var_name, &parent);

                self.builder.build_store(alloca, init_val);
                self.variables.remove(var_name); // remove old bindings
                self.variables.insert(var_name.to_owned(), alloca);

                let body = self.gencode_expr(&inbody)?;

                Ok(body)
            }
        };

        if let Ok(e) = res {
            if self.option_codegen.display_ir {
                e.print_to_stderr()
            }
        }

        res
    }
}

#[cfg(test)]
mod test {
    use inkwell::context::Context;

    use super::{CodeGen, OptionCodegen};
    use crate::parser::{BinaryOp, Expression, Function, Prototype};
    use inkwell::OptimizationLevel;

    #[test]
    fn test_codegen_expr_call() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let mut codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let res = codegen.gencode_expr(&Expression::Call(
            "add".to_owned(),
            vec![
                Expression::Variable("x".to_owned()),
                Expression::Variable("y".to_owned()),
            ],
        ));

        assert!(res.is_err()); //because the method "add" is unknow for know
    }

    #[test]
    fn test_codegen_expr_op() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let mut codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let res = codegen.gencode_expr(&Expression::Binary(
            BinaryOp::Plus,
            Box::new(Expression::Number(1.0)),
            Box::new(Expression::Number(2.0)),
        ));
        assert!(res.is_ok());
    }

    #[test]
    fn test_codegen_prototype() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let prototype = Prototype {
            name: "add2".to_owned(),
            args: vec![String::from("x"), String::from("y")],
        };

        let res = codegen.gencode_prototype(&prototype);

        assert!(res.is_ok());
    }

    #[test]
    fn test_codegen_function() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("can't create the LLVM JIT");

        let mut codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let body = Expression::Binary(
            BinaryOp::Plus,
            Box::new(Expression::Variable("x".to_owned())),
            Box::new(Expression::Variable("y".to_owned())),
        );

        let prototype = Prototype {
            name: "add2".to_owned(),
            args: vec![String::from("x"), String::from("y")],
        };

        let res = codegen.gencode_function(&Function {
            proto: prototype,
            body: Some(body),
        });

        assert!(res.is_ok());

        let res = unsafe {
            // FIXME: LLVM ERROR: MCJIT::runFunction does not support full-featured argument passing.
            // Please use ExecutionEngine::getFunctionAddress and cast the result to the desired function pointer type.
            execution_engine.get_function::<unsafe extern "C" fn(f64, f64) -> f64>("add2")
        };

        match res {
            Ok(func) => unsafe {
                println!("{}", func.call(9.0, 2.0));
            },
            Err(e) => eprint!("{}", e),
        };
    }

    #[test]
    fn test_codegen_function_mult_expr() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let mut codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let body = Expression::Binary(
            BinaryOp::Plus,
            Box::new(Expression::Binary(
                BinaryOp::Plus,
                Box::new(Expression::Number(1.0)), //NOTE: Should found trivial constant folding
                Box::new(Expression::Number(2.0)),
            )),
            Box::new(Expression::Variable("x".to_owned())),
        );

        let prototype = Prototype {
            name: "test".to_owned(),
            args: vec![String::from("x")],
        };

        let res = codegen.gencode_function(&Function {
            proto: prototype,
            body: Some(body),
        });

        assert!(res.is_ok());
    }

    #[test]
    fn test_codegen_function_extern() {
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("can't create the LLVM JIT");

        let mut codegen = CodeGen::new(
            &context,
            builder,
            module,
            None,
            OptionCodegen::new(false, false),
        );

        let prototype = Prototype {
            name: "cos".to_owned(),
            args: vec![String::from("x")],
        };

        let res = codegen.gencode_function(&Function {
            proto: prototype,
            body: None,
        });

        assert!(res.is_ok());

        let res = unsafe {
            // FIXME: LLVM ERROR: MCJIT::runFunction does not support full-featured argument passing.
            // Please use ExecutionEngine::getFunctionAddress and cast the result to the desired function pointer type.
            execution_engine.get_function::<unsafe extern "C" fn(f64) -> f64>("cos")
        };

        match res {
            Ok(func) => unsafe {
                func.call(1.234);
            },
            Err(e) => eprint!("{}", e),
        };

        let call = Expression::Call("cos".to_owned(), vec![Expression::Number(1.234)]);

        let res = codegen.gencode_expr(&call);

        assert!(res.is_ok());
    }
}
