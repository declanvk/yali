use super::{RuntimeException, Value};
use std::{cell::RefCell, rc::Rc};

/// An identifier which tracks the current extent of the `Environment`.
#[derive(Debug, Copy, Clone, PartialEq)]
struct Frame(usize);

/// The set of bindings that are present in lexical scopes during execution.
///
/// This struct will serve the similar purpose as the stack in a compiled
/// program.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    inner: Rc<RefCell<EnvironmentInner>>,
    frame: Option<Frame>,
}

#[derive(Debug, PartialEq)]
struct EnvironmentInner {
    parent: Option<Environment>,
    bindings: Vec<(String, Value)>,
    is_global: bool,
}

impl Environment {
    /// Create a new global environment.
    ///
    /// The global environment has different rules than function or block
    /// environments, as it allows redefining and addition of bindings, even
    /// after `snapshot` is called.
    pub fn global() -> Self {
        Environment {
            inner: Rc::new(RefCell::new(EnvironmentInner {
                parent: None,
                bindings: vec![],
                is_global: true,
            })),
            frame: None,
        }
    }

    /// Create a new lexical environment that is a child of the given
    /// environment.
    ///
    /// This means that any variable binding not found in the current
    /// environment will continue searching for in the parent environment.
    pub fn new_child(parent: &Environment) -> Self {
        Environment {
            inner: Rc::new(RefCell::new(EnvironmentInner {
                parent: Some(parent.clone()),
                bindings: vec![],
                is_global: false,
            })),
            frame: None,
        }
    }

    /// Return a copy of the current environment such that new bindings will not
    /// be considered by later lookups.
    pub fn snapshot(&self) -> Self {
        if self.inner.borrow().is_global {
            Environment {
                inner: self.inner.clone(),
                frame: None,
            }
        } else {
            Environment {
                inner: self.inner.clone(),
                frame: Some(self.current_frame()),
            }
        }
    }

    /// Returns a marker for the current extent of the environment.
    fn current_frame(&self) -> Frame {
        self.frame
            .unwrap_or_else(|| Frame(self.inner.borrow().bindings.len()))
    }

    /// Define a variable, shadowing any variable with the same name in the
    /// environment.
    pub fn define(&mut self, name: impl Into<String>, value: Value) {
        let mut inner = self.inner.borrow_mut();

        inner.bindings.push((name.into(), value));
    }

    /// Assign a new value to a variable, erroring if the variable has
    /// not been bound.
    pub fn assign(
        &mut self,
        name: impl Into<String>,
        value: Value,
    ) -> Result<(), RuntimeException> {
        let from = self.current_frame();

        self.assign_inner(name, value, from)
    }

    fn assign_inner(
        &mut self,
        name: impl Into<String>,
        value: Value,
        from: Frame,
    ) -> Result<(), RuntimeException> {
        let name = name.into();
        let mut inner = self.inner.borrow_mut();

        for (ref slot_name, slot_value) in inner.bindings.iter_mut().take(from.0).rev() {
            if slot_name.eq(&name) {
                *slot_value = value;

                return Ok(());
            }
        }

        if let Some(ref mut parent) = inner.parent {
            parent.assign(name, value)
        } else {
            Err(RuntimeException::UndefinedVariable(name).into())
        }
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// produce an error if none are found.
    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeException> {
        let from = self.current_frame();

        self.lookup_inner(name, from)
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// starting the search from the given `Frame`, producing an error if none
    /// are found.
    fn lookup_inner(&self, name: &str, from: Frame) -> Result<Value, RuntimeException> {
        let inner = self.inner.borrow();
        for (slot_name, v) in inner.bindings.iter().take(from.0).rev() {
            if slot_name.eq(name) {
                return Ok(v.clone());
            }
        }

        if let Some(ref parent) = inner.parent {
            parent.lookup(name)
        } else {
            Err(RuntimeException::UndefinedVariable(name.into()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn top_level_env() -> Environment {
        let mut env = Environment::global();

        env.define("a", Value::Number(1.0));
        env.define("b", Value::Number(2.0));
        env.define("c", Value::Number(3.0));

        env
    }

    fn child_env() -> Environment {
        let mut env = Environment::new_child(&top_level_env());

        env.define("b", Value::Number(20.0));
        env.define("d", Value::Number(40.0));

        env
    }

    #[test]
    fn lookup_global_env() {
        let top = top_level_env();

        assert_eq!(top.lookup("a"), Ok(Value::Number(1.0)));
        assert_eq!(top.lookup("b"), Ok(Value::Number(2.0)));
        assert_eq!(top.lookup("c"), Ok(Value::Number(3.0)));
        assert_eq!(
            top.lookup("d"),
            Err(RuntimeException::UndefinedVariable("d".into()))
        );
    }

    #[test]
    fn lookup_child_env() {
        let env = child_env();

        assert_eq!(env.lookup("a"), Ok(Value::Number(1.0)));
        assert_eq!(env.lookup("b"), Ok(Value::Number(20.0)));
        assert_eq!(env.lookup("c"), Ok(Value::Number(3.0)));
        assert_eq!(env.lookup("d"), Ok(Value::Number(40.0)));
        assert_eq!(
            env.lookup("e"),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
    }

    #[test]
    fn modify_lookup_child_env() {
        let mut env = child_env();

        env.assign("a", Value::Number(100.0)).unwrap();
        env.assign("d", Value::Number(400.0)).unwrap();

        assert_eq!(env.lookup("a"), Ok(Value::Number(100.0)));
        assert_eq!(env.lookup("b"), Ok(Value::Number(20.0)));
        assert_eq!(env.lookup("c"), Ok(Value::Number(3.0)));
        assert_eq!(env.lookup("d"), Ok(Value::Number(400.0)));

        assert_eq!(
            env.assign("e", Value::Number(0.0)),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
        assert_eq!(
            env.lookup("e"),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
    }

    #[test]
    fn assign_to_shadowed_later() {
        let mut global = Environment::global();

        global.define("a", Value::Number(1.0));

        let mut block = Environment::new_child(&global);
        let mut closure = Environment::new_child(&block.snapshot());

        block.define("a", Value::Number(2.0));
        closure.assign("a", Value::Number(3.0)).unwrap();

        assert_eq!(block.lookup("a"), Ok(Value::Number(2.0)));
        assert_eq!(global.lookup("a"), Ok(Value::Number(3.0)));
    }
}
