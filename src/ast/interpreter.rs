//! Tree-walking interpreter for the AST

use std::{collections::HashMap, fmt};

use super::{
    visit::{Visitable, Visitor},
    AssignExpr, BinaryExpr, BinaryOpKind, BlockStatement, ExprStatement, GroupingExpr, LiteralExpr,
    PrintStatement, Statement, UnaryExpr, UnaryOpKind, VarExpr, VarStatement,
};

/// The AST interpreter
#[derive(Debug)]
pub struct Interpreter {
    bindings: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    /// Visit the given AST fragments and evaluate them
    pub fn interpret(&mut self, statements: &[Statement]) -> Result<Value, RuntimeError> {
        for stmnt in statements {
            let v = stmnt.visit_with(self)?;

            assert_eq!(v, Value::Null, "Statement returned non-nil value!");
        }

        Ok(Value::Null)
    }

    /// Define a variable, overwritting any existing binding with the
    /// same name
    pub fn define(&mut self, name: impl Into<String>, value: Value) -> Option<Value> {
        self.bindings.last_mut().unwrap().insert(name.into(), value)
    }

    /// Assign a new value to a variable, erroring if the variable has
    /// not been bound.
    pub fn assign(&mut self, name: impl Into<String>, value: Value) -> Result<(), RuntimeError> {
        let name = name.into();

        for env in self.bindings.iter_mut().rev() {
            if env.contains_key(&name) {
                env.insert(name, value);

                return Ok(());
            }
        }

        Err(RuntimeError::UndefinedVariable(name))
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// produce an error if none are found
    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeError> {
        for env in self.bindings.iter().rev() {
            if let Some(v) = env.get(name) {
                return Ok(v.clone());
            }
        }

        Err(RuntimeError::UndefinedVariable(name.into()))
    }

    /// Create a new lexical environment
    pub fn push_env(&mut self) {
        self.bindings.push(HashMap::new())
    }

    /// Destroy the current lexical environment, and move the previous one
    pub fn pop_env(&mut self) {
        let _ = self
            .bindings
            .pop()
            .expect("should not pop all the environments");
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter {
            bindings: vec![HashMap::new()],
        }
    }
}

impl Visitor for Interpreter {
    type Output = Result<Value, RuntimeError>;

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

        println!("{}", value);

        Ok(Value::Null)
    }

    fn visit_expr_stmnt(&mut self, d: &ExprStatement) -> Self::Output {
        let ExprStatement { expr } = d;

        let _ = expr.visit_with(self)?;

        Ok(Value::Null)
    }

    fn visit_var_stmnt(&mut self, d: &VarStatement) -> Self::Output {
        let VarStatement { name, initializer } = d;

        let value: Value = initializer
            .as_ref()
            .map_or(Ok(Value::Null), |e| e.visit_with(self))?;

        self.define(name, value);

        Ok(Value::Null)
    }

    fn visit_block_stmnt(&mut self, d: &BlockStatement) -> Self::Output {
        let BlockStatement { statements } = d;

        self.push_env();
        let outputs: Vec<_> = statements
            .into_iter()
            .map(|stmnt| stmnt.visit_with(self))
            .collect();
        self.pop_env();

        self.combine_many_output(outputs)
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

    fn visit_var_expr(&mut self, d: &VarExpr) -> Self::Output {
        let VarExpr { name } = d;

        self.lookup(name.as_str())
    }

    fn visit_assign_expr(&mut self, d: &AssignExpr) -> Self::Output {
        let AssignExpr { name, value } = d;

        let value = value.visit_with(self)?;

        self.assign(name, value.clone())?;

        Ok(value)
    }
}

/// Errors that can occur during the course of interpretation
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum RuntimeError {
    /// An error that occurs when performing an operation between incompatible
    /// types
    #[error("{}", .0)]
    InvalidOperationForType(#[from] InvalidOperationForType),
    /// A variable lookup failed because the name is not bound
    #[error("undefined variable [{}]", .0)]
    UndefinedVariable(String),
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
}

impl Value {
    /// The statically known type of the value
    pub fn r#type(&self) -> &'static str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Null => "null",
        }
    }

    /// Return true if the value is "truthy"
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Number(_) | Value::String(_) => true,
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
        }
    }
}
