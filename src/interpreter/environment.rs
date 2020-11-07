use super::{RuntimeException, Value};
use std::{
    cell::RefCell,
    fmt,
    rc::{Rc, Weak},
};

/// The set of bindings that are present in lexical scopes during execution.
#[derive(Debug, Clone)]
pub struct Environment(Rc<RefCell<EnvironmentInner>>);
type WeakEnvironment = Weak<RefCell<EnvironmentInner>>;

impl Environment {
    /// Create a new global environment.
    ///
    /// The global environment has different rules than function or block
    /// environments, as new definitions are visible to child environments, even
    /// though those child environments may be created earlier in the execution
    /// of the program.
    pub fn global() -> Self {
        let inner = EnvironmentInner {
            parent: None,
            children: vec![],
            bindings: vec![],
        };

        Environment(Rc::new(RefCell::new(inner)))
    }

    /// Create a new lexical environment that is a child of the given
    /// environment.
    ///
    /// This means that any variable binding not found in the current
    /// environment will continue searching for in the parent environment.
    pub fn new_child(parent: &Self) -> Self {
        let frame = { parent.0.borrow().current_frame() };

        let inner = EnvironmentInner {
            parent: Some((frame, Environment::clone(parent))),
            children: vec![],
            bindings: vec![],
        };

        let child_env = Environment(Rc::new(RefCell::new(inner)));

        parent.add_child(&child_env);

        child_env
    }

    /// Return a clone of the given `Environment`.
    pub fn clone(env: &Self) -> Self {
        Environment(Rc::clone(&env.0))
    }

    /// Define a variable, shadowing any variable with the same name in the
    /// environment.
    pub fn define(&self, name: impl Into<String>, value: Value) {
        let mut inner = self.0.borrow_mut();

        inner.bindings.push((name.into(), value));
    }

    /// Assign a new value to a variable, erroring if the variable has
    /// not been bound.
    pub fn assign(&self, name: &str, new_value: Value) -> Result<(), RuntimeException> {
        let frame = { self.0.borrow().current_frame() };

        self.assign_inner(name, new_value, frame)
    }

    fn assign_inner(
        &self,
        name: &str,
        new_value: Value,
        frame: usize,
    ) -> Result<(), RuntimeException> {
        let mut inner = self.0.borrow_mut();

        let frame = if inner.is_global() {
            // If the current environment is the global one, the usual lexical restriction
            // do not apply. Definitions made later in a program are still visible to items
            // defined earlier.
            inner.current_frame()
        } else {
            frame
        };

        for (ref slot_name, slot_value) in inner.bindings[..frame].iter_mut().rev() {
            if slot_name == name {
                *slot_value = new_value;

                return Ok(());
            }
        }

        if let Some((parent_frame, parent)) = &inner.parent {
            parent.assign_inner(name, new_value, *parent_frame)
        } else {
            Err(RuntimeException::UndefinedVariable(name.into()).into())
        }
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// produce an error if none are found.
    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeException> {
        let frame = { self.0.borrow().current_frame() };

        self.lookup_inner(name, frame)
    }

    /// Attempt to get the `Value` associated with the given variable name,
    /// starting the search from the given `Frame`, producing an error if none
    /// are found.
    fn lookup_inner(&self, name: &str, frame: usize) -> Result<Value, RuntimeException> {
        let inner = self.0.borrow();

        let frame = if inner.is_global() {
            // If the current environment is the global one, the usual lexical restriction
            // do not apply. Definitions made later in a program are still visible to items
            // defined earlier.
            inner.current_frame()
        } else {
            frame
        };

        for (slot_name, v) in inner.bindings[..frame].iter().rev() {
            if slot_name.eq(name) {
                return Ok(v.clone());
            }
        }

        if let Some((parent_frame, parent)) = &inner.parent {
            parent.lookup_inner(name, *parent_frame)
        } else {
            Err(RuntimeException::UndefinedVariable(name.into()))
        }
    }

    /// Return a copy of a child environment.
    pub fn get_child(&self, idx: usize) -> Option<Environment> {
        let inner = self.0.borrow();

        let child = inner.children.get(idx)?.upgrade()?;

        Some(Environment(child))
    }

    fn add_child(&self, child_env: &Environment) {
        let mut env_mut = child_env.0.borrow_mut();

        env_mut.children.push(Rc::downgrade(&child_env.0));
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

struct EnvironmentInner {
    pub parent: Option<(usize, Environment)>,
    pub children: Vec<WeakEnvironment>,
    pub bindings: Vec<(String, Value)>,
}

impl EnvironmentInner {
    /// Return true if this `Environment` has no parent
    pub fn is_global(&self) -> bool {
        self.parent.is_none()
    }

    /// Returns a marker for the current extent of the environment.
    pub fn current_frame(&self) -> usize {
        self.bindings.len()
    }
}

impl fmt::Debug for EnvironmentInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EnvironmentInner")
            .field(
                "bindings",
                &self
                    .bindings
                    .iter()
                    .map(|(name, _)| name)
                    .collect::<Vec<_>>(),
            )
            .field("parent", &self.parent)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn top_level_env() -> Environment {
        let env = Environment::global();

        env.define("a", 1.0.into());
        env.define("b", 2.0.into());
        env.define("c", 3.0.into());

        env
    }

    fn child_env() -> Environment {
        let env = Environment::new_child(&top_level_env());

        env.define("b", 20.0.into());
        env.define("d", 40.0.into());

        env
    }

    #[test]
    fn lookup_global_env() {
        let top = top_level_env();

        assert_eq!(top.lookup("a"), Ok(1.0.into()));
        assert_eq!(top.lookup("b"), Ok(2.0.into()));
        assert_eq!(top.lookup("c"), Ok(3.0.into()));
        assert_eq!(
            top.lookup("d"),
            Err(RuntimeException::UndefinedVariable("d".into()))
        );
    }

    #[test]
    fn lookup_child_env() {
        let env = child_env();

        assert_eq!(env.lookup("a"), Ok(1.0.into()));
        assert_eq!(env.lookup("b"), Ok(20.0.into()));
        assert_eq!(env.lookup("c"), Ok(3.0.into()));
        assert_eq!(env.lookup("d"), Ok(40.0.into()));
        assert_eq!(
            env.lookup("e"),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
    }

    #[test]
    fn modify_lookup_child_env() {
        let env = child_env();

        env.assign("a", 100.0.into()).unwrap();
        env.assign("d", 400.0.into()).unwrap();

        assert_eq!(env.lookup("a"), Ok(100.0.into()));
        assert_eq!(env.lookup("b"), Ok(20.0.into()));
        assert_eq!(env.lookup("c"), Ok(3.0.into()));
        assert_eq!(env.lookup("d"), Ok(400.0.into()));

        assert_eq!(
            env.assign("e", 0.0.into()),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
        assert_eq!(
            env.lookup("e"),
            Err(RuntimeException::UndefinedVariable("e".into()))
        );
    }

    #[test]
    fn assign_to_shadowed_later() {
        let global = Environment::global();

        global.define("a", 1.0.into());

        let block = Environment::new_child(&global);
        let closure = Environment::new_child(&block);

        block.define("a", 2.0.into());
        closure.assign("a", 3.0.into()).unwrap();

        assert_eq!(block.lookup("a"), Ok(2.0.into()));
        assert_eq!(global.lookup("a"), Ok(3.0.into()));
    }
}
