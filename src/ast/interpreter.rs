//! Tree-walking interpreter for the AST

pub mod native_funcs;

use super::{
    visit::{Visitable, Visitor},
    AssignExpr, BinaryExpr, BinaryOpKind, BlockStatement, CallExpr, ExprStatement,
    FunctionDeclaration, GroupingExpr, IfStatement, LiteralExpr, LogicalExpr, LogicalOpKind,
    PrintStatement, ReturnStatement, Statement, UnaryExpr, UnaryOpKind, VarDeclaration, VarExpr,
    WhileStatement,
};
use std::{fmt, io::Write, sync::Arc};

/// The AST interpreter
pub struct Interpreter {
    /// The environment, which holds the complete set of variable bindings
    pub env: Environment,
    /// The standard out buffer, used to print things to screen
    pub stdout: Box<dyn Write>,
}

impl Interpreter {
    /// Create a new `Interpreter` with the default set of `NativeFunction`s.
    pub fn new(stdout: Box<dyn Write>) -> Self {
        let mut env = Environment::default();

        for func_constructor in native_funcs::default_list() {
            let native_func = (func_constructor)();

            env.define(native_func.name, Value::NativeFunction(native_func))
        }

        Interpreter { env, stdout }
    }

    /// Visit the given AST fragments and evaluate them
    pub fn interpret(&mut self, statements: &[Statement]) -> Result<Value, RuntimeControlFlow> {
        for stmnt in statements {
            let v = stmnt.visit_with(self)?;

            assert_eq!(v, Value::Null, "Statement returned non-nil value!");
        }

        Ok(Value::Null)
    }

    /// Mutably access the environment of this `Interpreter`
    pub fn env(&mut self) -> &mut Environment {
        &mut self.env
    }

    /// Mutable access the stdout of this `Interpreter`
    pub fn stdout(&mut self) -> &mut dyn Write {
        &mut self.stdout
    }
}

impl fmt::Debug for Interpreter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut state = f.debug_struct("Interpreter");
        state.field("env", &self.env);
        state.field("stdout", &"[stdout]");
        state.finish()
    }
}

impl Visitor for Interpreter {
    type Output = Result<Value, RuntimeControlFlow>;

    fn default_output(&self) -> Self::Output {
        Ok(Value::Null)
    }

    // This combiner should only join outputs from null statements
    fn combine_output(&self, o1: Self::Output, o2: Self::Output) -> Self::Output {
        match (o1?, o2?) {
            (Value::Null, Value::Null) => Ok(Value::Null),
            _ => panic!("Unsupported operation"),
        }
    }

    fn visit_print_stmnt(&mut self, d: &PrintStatement) -> Self::Output {
        let PrintStatement { expr } = d;

        let value = expr.visit_with(self)?;

        let _ = writeln!(self.stdout, "{}", value).expect("failed to write to stdout");

        Ok(Value::Null)
    }

    fn visit_expr_stmnt(&mut self, d: &ExprStatement) -> Self::Output {
        let ExprStatement { expr } = d;

        let _ = expr.visit_with(self)?;

        Ok(Value::Null)
    }

    fn visit_var_decl(&mut self, d: &VarDeclaration) -> Self::Output {
        let VarDeclaration { name, initializer } = d;

        let value: Value = initializer
            .as_ref()
            .map_or(Ok(Value::Null), |e| e.visit_with(self))?;

        self.env().define(name, value);

        Ok(Value::Null)
    }

    fn visit_block_stmnt(&mut self, d: &BlockStatement) -> Self::Output {
        let BlockStatement { statements } = d;

        self.env().push_env();
        let outputs: Vec<_> = statements
            .iter()
            .map(|stmnt| stmnt.visit_with(self))
            .collect();
        self.env().pop_env();

        self.combine_many_output(outputs)
    }

    fn visit_if_stmnt(&mut self, d: &IfStatement) -> Self::Output {
        let IfStatement {
            condition,
            then_branch,
            else_branch,
        } = d;
        let condition_value = condition.visit_with(self)?;

        if condition_value.is_truthy() {
            then_branch.visit_with(self)
        } else {
            else_branch.visit_with(self)
        }
    }

    fn visit_while_stmnt(&mut self, d: &WhileStatement) -> Self::Output {
        let WhileStatement { condition, body } = d;

        loop {
            let condition_value = condition.visit_with(self)?;
            if !condition_value.is_truthy() {
                break;
            }

            let _ = body.visit_with(self)?;
        }

        Ok(Value::Null)
    }

    fn visit_func_decl(&mut self, d: &FunctionDeclaration) -> Self::Output {
        self.env().define(
            &d.name,
            Value::UserFunction(UserFunction {
                declaration: Arc::new(d.clone()),
            }),
        );

        Ok(Value::Null)
    }

    fn visit_return_stmnt(&mut self, d: &ReturnStatement) -> Self::Output {
        let ReturnStatement { value } = d;

        let value = if let Some(value) = value {
            value.visit_with(self)?
        } else {
            Value::Null
        };

        Err(RuntimeControlFlow::Return(value))
    }

    fn visit_binary_expr(&mut self, d: &BinaryExpr) -> Self::Output {
        let BinaryExpr {
            left,
            operator,
            right,
        } = d;

        let left_value = left.visit_with(self)?;
        let right_value = right.visit_with(self)?;

        // TODO: implement control flow boolean operators

        let v = match (operator, left_value, right_value) {
            (BinaryOpKind::Mult, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
            (BinaryOpKind::Add, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (BinaryOpKind::Add, Value::String(mut l), Value::String(r)) => {
                l.push_str(&r);

                Value::String(l)
            },
            (BinaryOpKind::Sub, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
            (BinaryOpKind::Div, Value::Number(l), Value::Number(r)) => Value::Number(l / r),

            // comparisons
            (BinaryOpKind::Greater, Value::Number(l), Value::Number(r)) => Value::Boolean(l > r),
            (BinaryOpKind::GreaterEqual, Value::Number(l), Value::Number(r)) => {
                Value::Boolean(l >= r)
            },
            (BinaryOpKind::Less, Value::Number(l), Value::Number(r)) => Value::Boolean(l < r),
            (BinaryOpKind::LessEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l <= r),

            // equality
            (BinaryOpKind::Equal, l, r) => Value::Boolean(l.eq(&r)),
            (BinaryOpKind::NotEqual, l, r) => Value::Boolean(l.ne(&r)),

            // operation undefined for value types
            (op, l, r) => {
                return Err(InvalidOperationForType {
                    operation: op.symbol(),
                    first_arg: Some(l.r#type()),
                    second_arg: Some(r.r#type()),
                }
                .into())
            },
        };

        Ok(v)
    }

    fn visit_grouping_expr(&mut self, d: &GroupingExpr) -> Self::Output {
        let GroupingExpr { inner } = d;

        inner.visit_with(self)
    }

    fn visit_literal_expr(&mut self, d: &LiteralExpr) -> Self::Output {
        Ok(Value::from(d))
    }

    fn visit_unary_expr(&mut self, d: &UnaryExpr) -> Self::Output {
        let UnaryExpr { operator, right } = d;

        let right_value = right.visit_with(self)?;

        let v = match (operator, right_value) {
            (UnaryOpKind::Negate, Value::Number(r)) => Value::Number(-r),
            (UnaryOpKind::Not, r) => Value::Boolean(!r.is_truthy()),

            // operation undefined for value types
            (op, r) => {
                return Err(InvalidOperationForType {
                    operation: op.symbol(),
                    first_arg: Some(r.r#type()),
                    second_arg: None,
                }
                .into())
            },
        };

        Ok(v)
    }

    fn visit_logical_expr(&mut self, d: &LogicalExpr) -> Self::Output {
        let LogicalExpr {
            left,
            right,
            operator,
        } = d;

        let left_value = left.visit_with(self)?;

        match operator {
            LogicalOpKind::And => {
                if !left_value.is_truthy() {
                    Ok(left_value)
                } else {
                    right.visit_with(self)
                }
            },
            LogicalOpKind::Or => {
                if left_value.is_truthy() {
                    Ok(left_value)
                } else {
                    right.visit_with(self)
                }
            },
        }
    }

    fn visit_var_expr(&mut self, d: &VarExpr) -> Self::Output {
        let VarExpr { name } = d;

        self.env().lookup(name.as_str())
    }

    fn visit_assign_expr(&mut self, d: &AssignExpr) -> Self::Output {
        let AssignExpr { name, value } = d;

        let value = value.visit_with(self)?;

        self.env().assign(name, value.clone())?;

        Ok(value)
    }

    fn visit_call_expr(&mut self, d: &CallExpr) -> Self::Output {
        let CallExpr { callee, arguments } = d;

        // Evaluate the callee (the thing to call) and the arguments

        let callee_value = callee.visit_with(self)?;
        let arg_values: Vec<_> = arguments
            .iter()
            .map(|e| e.visit_with(self))
            .collect::<Result<_, _>>()?;

        match callee_value {
            Value::NativeFunction(f) => f.call(arg_values),
            Value::UserFunction(f) => f.call(self, arg_values),
            x => Err(RuntimeControlFlow::CalledNonFunctionType(x)),
        }
    }
}

/// Errors that can occur during the course of interpretation
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum RuntimeControlFlow {
    // //////////////// //
    //  ERROR VARIANTS  //
    // //////////////// //
    /// An error that occurs when performing an operation between incompatible
    /// types
    #[error("{}", .0)]
    InvalidOperationForType(#[from] InvalidOperationForType),
    /// A variable lookup failed because the name is not bound
    #[error("undefined variable [{}]", .0)]
    UndefinedVariable(String),
    /// Attempted to call a non-function type failed
    #[error("attempted to call [{}] as a function", .0)]
    CalledNonFunctionType(Value),
    /// Attempted to a call a `Function` with too many or too few argumets
    #[error("A function [{}] expected [{}] arguments but got [{}]", .callee_name, .expected, .provided)]
    MismatchedArity {
        /// The function that attempted to call
        callee_name: String,
        /// The number of arguments provided
        provided: usize,
        /// The number of arguments expected
        expected: usize,
    },

    // //////////////// //
    // CONTROL VARIANTS //
    // //////////////// //
    /// A control flow construct that immediately returns from the function it
    /// is in
    #[error("return")]
    Return(Value),
}

/// An error that occurs when performing an operation between incompatible types
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub struct InvalidOperationForType {
    operation: &'static str,
    first_arg: Option<&'static str>,
    second_arg: Option<&'static str>,
}

// #[error("unsupported operand type(s) for {}: {:?}", .operation,
// .operand_types)]
impl fmt::Display for InvalidOperationForType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.first_arg, self.second_arg) {
            (None, None) => panic!("Unsupported error report with no arguments!"),
            (Some(v), None) | (None, Some(v)) => write!(
                f,
                "unsupported operand type for [{}]: [{}]",
                self.operation, v
            ),
            (Some(l), Some(r)) => write!(
                f,
                "unsupported operand type for [{}]: [{}] and [{}]",
                self.operation, l, r
            ),
        }
    }
}

/// A lox value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A boolean value
    Boolean(bool),
    /// A numeric value
    Number(f64),
    /// A string value (UTF-8)
    String(String),
    /// A null value
    Null,
    /// A callable value provided by the host environment
    NativeFunction(NativeFunction),
    /// A callable value defined by the user
    UserFunction(UserFunction),
}

impl Value {
    /// The statically known type of the value
    pub fn r#type(&self) -> &'static str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Null => "null",
            Value::NativeFunction(_) => "function",
            Value::UserFunction(_) => "function",
        }
    }

    /// Return true if the value is "truthy"
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Number(_)
            | Value::String(_)
            | Value::NativeFunction(_)
            | Value::UserFunction(_) => true,
            Value::Null => false,
        }
    }
}

impl From<LiteralExpr> for Value {
    fn from(src: LiteralExpr) -> Self {
        match src {
            LiteralExpr::Boolean(b) => Value::Boolean(b),
            LiteralExpr::Number(n) => Value::Number(n),
            LiteralExpr::String(s) => Value::String(s),
            LiteralExpr::Null => Value::Null,
        }
    }
}

impl From<&LiteralExpr> for Value {
    fn from(src: &LiteralExpr) -> Self {
        match src {
            LiteralExpr::Boolean(b) => Value::Boolean(*b),
            LiteralExpr::Number(n) => Value::Number(*n),
            LiteralExpr::String(s) => Value::String(s.clone()),
            LiteralExpr::Null => Value::Null,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Null => write!(f, "nil"),
            Value::NativeFunction(func) => write!(f, "{}", func),
            Value::UserFunction(func) => write!(f, "{}", func),
        }
    }
}

/// The set of bindings that are present in lexical scopes during execution.
///
/// This struct will serve the similar purpose as the stack in a compiled
/// program.
#[derive(Debug, Clone)]
pub struct Environment {
    stack: Vec<(String, Value)>,
    frame_size: Vec<usize>,
}

impl Environment {
    /// Define a variable, shadowing any variable with the same name in the
    /// environment
    pub fn define(&mut self, name: impl Into<String>, value: Value) {
        self.stack.push((name.into(), value));
        *self.frame_size.last_mut().unwrap() += 1;
    }

    /// Assign a new value to a variable, erroring if the variable has
    /// not been bound.
    pub fn assign(
        &mut self,
        name: impl Into<String>,
        value: Value,
    ) -> Result<(), RuntimeControlFlow> {
        let name = name.into();

        for (ref slot_name, slot_value) in self.stack.iter_mut().rev() {
            if slot_name.eq(&name) {
                *slot_value = value;

                return Ok(());
            }
        }

        Err(RuntimeControlFlow::UndefinedVariable(name))
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// produce an error if none are found
    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeControlFlow> {
        for (slot_name, v) in self.stack.iter().rev() {
            if slot_name.eq(name) {
                return Ok(v.clone());
            }
        }

        Err(RuntimeControlFlow::UndefinedVariable(name.into()))
    }

    /// Create a new lexical environment
    pub fn push_env(&mut self) {
        self.frame_size.push(0);
    }

    /// Destroy the current lexical environment, and move the previous one
    pub fn pop_env(&mut self) {
        let len = self.stack.len();
        let _ = self.stack.drain((len - *self.frame_size.last().unwrap())..);
        self.frame_size.pop();
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment {
            stack: vec![],
            frame_size: vec![0],
        }
    }
}

// TODO: rewrite this documentation
/// A `Value` that can evaluated with new additions to its environment.
#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    f: fn(Vec<Value>) -> Value,
    name: &'static str,
    arity: usize,
}

impl NativeFunction {
    /// Return the number of parameters of the `NativeFunction`
    pub fn arity(&self) -> usize {
        self.arity
    }

    /// Evaluate this `NativeFunction` with the provided arguments
    pub fn call(&self, arguments: Vec<Value>) -> Result<Value, RuntimeControlFlow> {
        if arguments.len() != self.arity {
            return Err(RuntimeControlFlow::MismatchedArity {
                callee_name: self.name.into(),
                expected: self.arity,
                provided: arguments.len(),
            });
        }

        Ok((self.f)(arguments))
    }
}

impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native-function [{}]>", self.name)
    }
}

/// A user defined function
#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
    declaration: Arc<FunctionDeclaration>,
}

impl UserFunction {
    /// Return the number of parameters of `UserFunction`.
    pub fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }

    /// Evaluate this `UserFunction` with the provided arguments and
    /// interpreter
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeControlFlow> {
        let decl = &self.declaration;
        let arity = decl.parameters.len();
        if arguments.len() != arity {
            return Err(RuntimeControlFlow::MismatchedArity {
                callee_name: decl.name.clone(),
                expected: arity,
                provided: arguments.len(),
            });
        }

        let statements = &decl.body;
        let param_bindings = decl.parameters.iter().zip(arguments.into_iter());

        interpreter.env().push_env();
        // Add all the parameters/argument bindins to the environment
        for (param_name, arg_value) in param_bindings {
            interpreter.env().define(param_name, arg_value);
        }

        let outputs: Result<Vec<_>, _> = statements
            .iter()
            .map(|stmnt| stmnt.visit_with(interpreter))
            .collect();
        interpreter.env().pop_env();

        match outputs {
            Ok(_) => Ok(Value::Null),
            Err(RuntimeControlFlow::Return(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }
}

impl fmt::Display for UserFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function [{}]>", self.declaration.name)
    }
}
