use super::{
    visit::{Visitable, Visitor},
    Expr,
};
use crate::span::Span;
use std::sync::Arc;

/// Syntax tree of lox statements, the main elements of lox scripts
#[derive(Debug)]
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
        }
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_statement(self)
    }
}

/// Different types of statements
#[derive(Debug)]
pub enum StatementKind {
    /// A statement consisting a single expression followed by a semicolon
    Expression(ExprStatement),
    /// A print statement which outputs text to standard out
    Print(PrintStatement),
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

/// A statement consisting a single expression followed by a semicolon
#[derive(Debug)]
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
#[derive(Debug)]
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
