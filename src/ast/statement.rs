use super::{
    visit::{Visitable, Visitor},
    Expr,
};
use crate::span::Span;
use std::sync::Arc;

/// Syntax tree of lox statements, the main elements of lox scripts
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    /// The continuous block of code this statement covers
    pub span: Span,
    /// The type of statement
    pub kind: StatementKind,
}

impl Visitable for Statement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let Statement { kind, .. } = self;

        match kind {
            StatementKind::Expression(stmnt) => stmnt.visit_with(visitor),
            StatementKind::Print(stmnt) => stmnt.visit_with(visitor),
            StatementKind::Var(stmnt) => stmnt.visit_with(visitor),
        }
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_statement(self)
    }
}

/// Different types of statements
#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    /// A statement consisting a single expression followed by a semicolon
    Expression(ExprStatement),
    /// A print statement which outputs text to standard out
    Print(PrintStatement),
    /// A var statement declares and optionally initializes a variable binding
    Var(VarStatement),
}

impl From<ExprStatement> for StatementKind {
    fn from(v: ExprStatement) -> Self {
        StatementKind::Expression(v)
    }
}

impl From<PrintStatement> for StatementKind {
    fn from(v: PrintStatement) -> Self {
        StatementKind::Print(v)
    }
}

impl From<VarStatement> for StatementKind {
    fn from(v: VarStatement) -> Self {
        StatementKind::Var(v)
    }
}

/// A statement consisting a single expression followed by a semicolon
#[derive(Debug, Clone, PartialEq)]
pub struct ExprStatement {
    /// The expression to be evaluated
    pub expr: Arc<Expr>,
}

impl Visitable for ExprStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let ExprStatement { expr } = self;

        expr.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_expr_stmnt(self)
    }
}

/// A print statement which outputs text to standard out
#[derive(Debug, Clone, PartialEq)]
pub struct PrintStatement {
    /// The expression evaluated and printed
    pub expr: Arc<Expr>,
}

impl Visitable for PrintStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let PrintStatement { expr } = self;

        expr.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_print_stmnt(self)
    }
}

/// A var statement declares and optionally initializes a variable binding
#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement {
    /// The name of the variable binding
    pub name: String,
    /// The initial value of the variable
    pub initializer: Option<Expr>,
}

impl Visitable for VarStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let VarStatement { initializer, .. } = self;

        initializer.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_var_stmnt(self)
    }
}
