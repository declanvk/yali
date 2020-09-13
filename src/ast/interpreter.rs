//! Tree-walking interpreter for the AST

use std::fmt;

use super::{
    visit::{Visitable, Visitor},
    BinaryExpr, BinaryOpKind, ExprStatement, GroupingExpr, LiteralExpr, PrintStatement, Statement,
    UnaryExpr, UnaryOpKind,
};

/// Visit the given AST fragment and evaluate it
pub fn interpret(statements: &[Statement]) -> Result<Value, RuntimeError> {
    let mut interpreter = Interpreter::default();

    for stmnt in statements {
        let v = stmnt.visit_with(&mut interpreter)?;

        assert_eq!(v, Value::Null, "Statement returned non-nil value!");
    }

    Ok(Value::Null)
}

/// The AST interpreter
#[derive(Debug, Default)]
pub struct Interpreter;

impl Visitor for Interpreter {
    type Output = Result<Value, RuntimeError>;

    fn default_output(&self) -> Self::Output {
        Ok(Value::Null)
    }

    fn combine_output(&self, _o1: Self::Output, _o2: Self::Output) -> Self::Output {
        panic!("Unsupported operation")
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
}

/// Errors that can occur during the course of interpretation
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum RuntimeError {
    /// An error that occurs when performing an operation between incompatible
    /// types
    #[error("{}", .0)]
    InvalidOperationForType(#[from] InvalidOperationForType),
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
