//! This module contains static analysis tools for lox AST that can detect
//! specific errors rather than encountering them at runtime.

use crate::{
    ast::{
        visit::{Visitable, Visitor},
        ClassDeclaration, FunctionDeclaration, ReturnStatement, SuperExpr, ThisExpr,
    },
    interpreter::FunctionType,
};

/// Traverse the AST and assert specific properties like no top level return
/// statements or no usage of the `this` variable outside methods
#[derive(Debug, Default, Clone)]
pub struct AstValidator {
    inside_function: Option<FunctionType>,
    inside_class: Option<ClassType>,
}

impl AstValidator {
    /// Perform checks on given AST chunk, returning errors that are found.
    pub fn validate(&mut self, ast: impl Visitable) -> Result<(), ValidationError> {
        ast.visit_with(self)
    }

    fn resolve_function(
        &mut self,
        function: &FunctionDeclaration,
        func_type: FunctionType,
    ) -> Result<(), ValidationError> {
        let enclosing_function = self.inside_function;
        self.inside_function = Some(func_type);

        function.body.visit_with(self)?;

        self.inside_function = enclosing_function;

        Ok(())
    }
}

impl Visitor for AstValidator {
    type Output = Result<(), ValidationError>;

    fn default_output(&self) -> Self::Output {
        Ok(())
    }

    fn combine_output(&self, o1: Self::Output, o2: Self::Output) -> Self::Output {
        match (o1, o2) {
            (Ok(_), Ok(_)) => Ok(()),
            (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err),
            (Err(err1), Err(_)) => Err(err1),
        }
    }

    fn visit_class_decl(&mut self, d: &ClassDeclaration) -> Self::Output {
        let ClassDeclaration {
            methods,
            superclass,
            name,
        } = d;

        let prev_inside_class = self.inside_class;
        self.inside_class = Some(ClassType::Class);

        if let Some(superclass) = superclass {
            if &superclass.name == name {
                return Err(ValidationError::InheritFromSelf);
            }
        }

        if superclass.is_some() {
            self.inside_class = Some(ClassType::Subclass);
        }

        for m in methods {
            let declaration = if name == ClassDeclaration::INITIALIZER_METHOD_NAME {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };

            self.resolve_function(m, declaration)?;
        }

        let o = methods.visit_with(self);

        self.inside_class = prev_inside_class;

        o
    }

    fn visit_func_decl(&mut self, d: &FunctionDeclaration) -> Self::Output {
        self.resolve_function(d, FunctionType::Function)?;

        Ok(())
    }

    fn visit_return_stmnt(&mut self, d: &ReturnStatement) -> Self::Output {
        let ReturnStatement { value } = d;

        if self.inside_function.is_none() {
            return Err(ValidationError::ReturnOutsideFunction);
        }

        if let Some(value) = value {
            if self.inside_function == Some(FunctionType::Initializer) {
                return Err(ValidationError::ReturnInsideInitializer);
            }

            value.visit_with(self)
        } else {
            Ok(())
        }
    }

    fn visit_this_expr(&mut self, _d: &ThisExpr) -> Self::Output {
        match (self.inside_class, self.inside_function) {
            (None, None) | (None, Some(_)) => Err(ValidationError::ThisUseOutsideMethod),
            (Some(_), None) => panic!(
                "This state should not occur where the 'this' variable is present outside of a \
                 method body in a class."
            ),
            (Some(_), Some(_)) => Ok(()),
        }
    }

    fn visit_super_expr(&mut self, _: &SuperExpr) -> Self::Output {
        if self.inside_class.is_none() || self.inside_class != Some(ClassType::Subclass) {
            Err(ValidationError::SuperUseOutsideSubClass)
        } else {
            Ok(())
        }
    }
}

/// Types of errors that occur when traversing the complete AST.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ValidationError {
    /// Attempted to access the `this` variable outside of a class method
    #[error("cannot access 'this' variable outside class method")]
    ThisUseOutsideMethod,
    /// Attempted to `return` outside of a function
    #[error("cannot 'return' outside of a function")]
    ReturnOutsideFunction,
    /// Attempted to `return` a value inside of a class `init` method
    #[error("cannot 'return' a value inside of a class 'init' method")]
    ReturnInsideInitializer,
    /// Attempted to have a class inherit from itself
    #[error("a class cannot inherit from itself")]
    InheritFromSelf,
    /// Attempted to redefine a variable that already exists
    #[error("cannot redefine a variable")]
    AlreadyDefinedVariable,
    /// Attempted to use `super` outside of a class that superclass
    #[error("cannot access 'super' outside of a method in a class with a superclass")]
    SuperUseOutsideSubClass,
}

/// This enum represents where the `AstValidator` is with respect to different
/// types of class.
#[derive(Debug, Copy, Clone, PartialEq)]
enum ClassType {
    Class,
    Subclass,
}
