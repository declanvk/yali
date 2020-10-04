use super::{Environment, Interpreter, RuntimeControlFlow, RuntimeException};
use crate::ast::{visit::Visitable, FunctionDeclaration, LiteralExpr};
use std::{fmt, io::Write, mem, rc::Rc};

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
    /// A class is an extensible collection of methods that is used to create
    /// objects
    Class(Class),
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
            Value::Class(_) => "object",
        }
    }

    /// Return true if the value is "truthy"
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Number(_)
            | Value::String(_)
            | Value::NativeFunction(_)
            | Value::UserFunction(_)
            | Value::Class(_) => true,
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
            Value::Boolean(b) => <bool as fmt::Display>::fmt(b, f),
            Value::Number(n) => <f64 as fmt::Display>::fmt(n, f),
            Value::String(s) => <String as fmt::Display>::fmt(s, f),
            Value::Null => write!(f, "nil"),
            Value::NativeFunction(func) => <NativeFunction as fmt::Display>::fmt(func, f),
            Value::UserFunction(func) => <UserFunction as fmt::Display>::fmt(func, f),
            Value::Class(class) => <Class as fmt::Display>::fmt(class, f),
        }
    }
}

/// A function that is defined by the interpreter
#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    /// The underlying implementation of the native function
    pub f: fn(Vec<Value>) -> Value,
    /// The name of the native function
    pub name: &'static str,
    /// The number of parameters of the native function
    pub arity: usize,
}

impl NativeFunction {
    /// Evaluate this `NativeFunction` with the provided arguments
    pub fn call(&self, arguments: Vec<Value>) -> Result<Value, RuntimeControlFlow> {
        if arguments.len() != self.arity {
            return Err(RuntimeException::MismatchedArity {
                callee_name: self.name.into(),
                expected: self.arity,
                provided: arguments.len(),
            }
            .into());
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
    /// The content of the function: parameters + body.
    pub declaration: Rc<FunctionDeclaration>,
    /// The parent environment of this closure, contains all possible variables
    /// bindings that are accessible to this function.
    pub closure: Environment,
}

impl UserFunction {
    /// Return the number of parameters of `UserFunction`.
    pub fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }

    /// Evaluate this `UserFunction` with the provided arguments and
    /// interpreter
    pub fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeControlFlow> {
        let decl = &self.declaration;
        let arity = decl.parameters.len();
        if arguments.len() != arity {
            return Err(RuntimeException::MismatchedArity {
                callee_name: decl.name.clone(),
                expected: arity,
                provided: arguments.len(),
            }
            .into());
        }

        let statements = &decl.body;
        let param_bindings = decl.parameters.iter().zip(arguments.into_iter());

        let new_env = Environment::new_child(&self.closure);
        let old_env = mem::replace(interpreter.env(), new_env);

        // Add all the parameters/argument bindins to the environment
        for (param_name, arg_value) in param_bindings {
            interpreter.env().define(param_name, arg_value);
        }

        let outputs: Result<Vec<_>, _> = statements
            .iter()
            .map(|stmnt| stmnt.visit_with(interpreter))
            .collect();
        *interpreter.env() = old_env;

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

/// A class is an extensible collection of methods that is used to create
/// objects
#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    /// The name of the class
    pub name: String,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
