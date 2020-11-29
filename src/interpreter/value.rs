use smol_str::SmolStr;

use super::{Environment, Interpreter, RuntimeControlFlow, RuntimeException};
use crate::ast::{visit::Visitable, ClassDeclaration, FunctionDeclaration, LiteralExpr, ThisExpr};
use std::{cell::RefCell, collections::HashMap, fmt, io::Write, mem, sync::Arc};

/// A lox value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A boolean value
    Boolean(bool),
    /// A numeric value
    Number(f64),
    /// A string value (UTF-8)
    String(SmolStr),
    /// A null value
    Null,
    /// A callable value provided by the host environment
    NativeFunction(NativeFunction),
    /// A callable value defined by the user
    UserFunction(UserFunction),
    /// A class is an extensible collection of methods that is used to create
    /// objects
    Class(Arc<Class>),
    /// An instantiation of a class
    Instance(Arc<Instance>),
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
            Value::Instance(_) => "instance",
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
            | Value::Class(_)
            | Value::Instance(_) => true,
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

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Boolean(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Number(v)
    }
}

impl From<SmolStr> for Value {
    fn from(v: SmolStr) -> Self {
        Value::String(v)
    }
}

impl From<Arc<Instance>> for Value {
    fn from(v: Arc<Instance>) -> Self {
        Value::Instance(v)
    }
}

impl From<Arc<Class>> for Value {
    fn from(v: Arc<Class>) -> Self {
        Value::Class(v)
    }
}

impl From<NativeFunction> for Value {
    fn from(v: NativeFunction) -> Self {
        Value::NativeFunction(v)
    }
}

impl From<UserFunction> for Value {
    fn from(v: UserFunction) -> Self {
        Value::UserFunction(v)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(b) => <bool as fmt::Display>::fmt(b, f),
            Value::Number(n) => <f64 as fmt::Display>::fmt(n, f),
            Value::String(s) => <SmolStr as fmt::Display>::fmt(s, f),
            Value::Null => write!(f, "nil"),
            Value::NativeFunction(func) => <NativeFunction as fmt::Display>::fmt(func, f),
            Value::UserFunction(func) => <UserFunction as fmt::Display>::fmt(func, f),
            Value::Class(class) => <Class as fmt::Display>::fmt(class, f),
            Value::Instance(inst) => <Instance as fmt::Display>::fmt(inst, f),
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
                callee_name: SmolStr::new(self.name),
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
    pub declaration: Arc<FunctionDeclaration>,
    /// The parent environment of this closure, contains all possible variables
    /// bindings that are accessible to this function.
    pub closure: Environment,
    /// The type of user function
    pub function_type: FunctionType,
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
            _ if matches!(self.function_type, FunctionType::Initializer) => self
                .closure
                .lookup(ThisExpr::VARIABLE_NAME)
                .map_err(Into::into),
            Ok(_) => Ok(Value::Null),
            Err(RuntimeControlFlow::Return(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }

    /// Create a new copy of the function that has the `this` variable defined
    /// as the a value of the given `Instance`.
    pub fn bind(&self, instance: &Arc<Instance>) -> UserFunction {
        let closure = Environment::new_child(&self.closure);

        closure.define(
            &SmolStr::new(ThisExpr::VARIABLE_NAME),
            Arc::clone(instance).into(),
        );

        UserFunction {
            declaration: Arc::clone(&self.declaration),
            closure,
            function_type: self.function_type,
        }
    }
}

impl fmt::Display for UserFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function [{}]>", self.declaration.name)
    }
}

/// Different types of functions
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionType {
    /// A normal function
    Function,
    /// A method defined inside of a class
    Method,
    /// A method defined with the name `init`.
    Initializer,
}

/// A class is an extensible collection of methods that is used to create
/// objects
#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    /// The name of the class
    pub name: SmolStr,
    /// The methods defined on this class
    pub methods: HashMap<SmolStr, UserFunction>,
    /// The superclass of this class, if any
    pub superclass: Option<Arc<Class>>,
}

impl Class {
    /// Create an instance of this class
    pub fn constructor(
        self: &Arc<Class>,
        interpreter: &mut Interpreter<impl Write>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeControlFlow> {
        let instance = Arc::new(Instance {
            class: Arc::clone(self),
            fields: RefCell::new(HashMap::new()),
        });

        if let Some(initializer) = self.find_method(ClassDeclaration::INITIALIZER_METHOD_NAME) {
            let _ = initializer.bind(&instance).call(interpreter, args)?;
        } else if args.len() != 0 {
            return Err(RuntimeException::MismatchedArity {
                callee_name: self.name.clone(),
                expected: 0,
                provided: args.len(),
            }
            .into());
        }

        Ok(instance.into())
    }

    /// Search for a method of this `Class` that has the given name.
    ///
    /// If not found directly on this `Class`, continues searching on the parent
    /// class (if it exists).
    pub fn find_method(&self, name: &str) -> Option<UserFunction> {
        if let Some(m) = self.methods.get(name) {
            return Some(m.clone());
        }

        return self
            .superclass
            .as_ref()
            .and_then(|sclass| sclass.find_method(name));
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// An instantiation of a class
#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    /// The `Class` that this instance is derived from
    pub class: Arc<Class>,
    /// The mapping from names to `Value`s for this instance
    pub fields: RefCell<HashMap<SmolStr, Value>>,
}

impl Instance {
    /// Access a property of this `Instance`
    pub fn get(self: &Arc<Self>, property: &SmolStr) -> Result<Value, RuntimeControlFlow> {
        let fields = self.fields.borrow();

        if let Some(val) = fields.get(property) {
            return Ok(val.clone());
        }

        if let Some(method) = self.class.find_method(property) {
            return Ok(method.bind(self).into());
        }

        Err(RuntimeException::AccessMissingField {
            field_name: property.clone(),
        }
        .into())
    }

    /// Set the value of a property on this `Instance`.
    pub fn set(&self, property: SmolStr, value: Value) {
        let mut fields = self.fields.borrow_mut();

        fields.insert(property, value);
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class,)
    }
}
