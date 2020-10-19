//! This module contains static analysis tools for lox AST that can detect
//! specific errors rather than encountering them at runtime.

use crate::{
    ast::{
        visit::{Visitable, Visitor},
        ClassDeclaration, FunctionDeclaration, ReturnStatement, ThisExpr,
    },
    interpreter::FunctionType,
};

/// Traverse the AST and assert specific properties like no top level return
/// statements or no usage of the `this` variable outside methods
#[derive(Debug, Default, Clone)]
pub struct AstValidator {
    inside_function: Option<FunctionType>,
    inside_class: Option<()>,
}

impl AstValidator {
    /// Perform checks on given AST chunk, returning errors that are found.
    pub fn validate(&mut self, ast: impl Visitable) -> Result<(), ValidationError> {
        ast.visit_with(self)
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
        let ClassDeclaration { methods, .. } = d;

        let prev_inside_class = self.inside_class;
        self.inside_class = Some(());

        let o = methods.visit_with(self);

        self.inside_class = prev_inside_class;

        o
    }

    fn visit_func_decl(&mut self, d: &FunctionDeclaration) -> Self::Output {
        let FunctionDeclaration { name, body, .. } = d;

        let new_inside_func = match (self.inside_class, self.inside_function) {
            (None, None) | (None, Some(_)) | (Some(()), Some(_)) => FunctionType::Function,
            (Some(()), None) => {
                if name == ClassDeclaration::INITIALIZER_METHOD_NAME {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                }
            },
        };
        let prev_inside_func = self.inside_function;
        self.inside_function = Some(new_inside_func);

        let o = body.visit_with(self);

        self.inside_function = prev_inside_func;

        o
    }

    fn visit_return_stmnt(&mut self, d: &ReturnStatement) -> Self::Output {
        let ReturnStatement { value } = d;

        match self.inside_function {
            Some(FunctionType::Initializer) => {
                if value.is_some() {
                    Err(ValidationError::ReturnInsideInitializer)
                } else {
                    Ok(())
                }
            },
            None => Err(ValidationError::ReturnOutsideFunction),
            _ => Ok(()),
        }
    }

    fn visit_this_expr(&mut self, _d: &ThisExpr) -> Self::Output {
        match (self.inside_class, self.inside_function) {
            (None, None) | (None, Some(_)) => Err(ValidationError::ThisUseOutsideMethod),
            (Some(()), None) => panic!(
                "This state should not occur where the 'this' variable is present outside of a \
                 method body in a class."
            ),
            (Some(()), Some(_)) => Ok(()),
        }
    }
}

/// Types of errors that occur when traversing the complete AST.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ValidationError {
    /// Attempted to access the `this` variable outside of a class method
    #[error("attempted to access 'this' variable outside class method")]
    ThisUseOutsideMethod,
    /// Attempted to `return` outside of a function
    #[error("attempted to 'return' outside of a function")]
    ReturnOutsideFunction,
    /// Attempted to `return` a value inside of a class `init` method
    #[error("attempted to 'return' a value inside of a class 'init' method")]
    ReturnInsideInitializer,
}
