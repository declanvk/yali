use super::{
    visit::{Visitable, Visitor},
    Expr,
};
use crate::span::Span;
use std::{iter, sync::Arc};

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
            StatementKind::Block(stmnt) => stmnt.visit_with(visitor),
            StatementKind::If(stmnt) => stmnt.visit_with(visitor),
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
    /// A block statement contains a list of other statements and defines a new
    /// lexical scope
    Block(BlockStatement),
    /// An if statement lets you conditionally execute statements
    If(IfStatement),
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

impl From<BlockStatement> for StatementKind {
    fn from(v: BlockStatement) -> Self {
        StatementKind::Block(v)
    }
}

impl From<IfStatement> for StatementKind {
    fn from(v: IfStatement) -> Self {
        StatementKind::If(v)
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

/// A block statement contains a list of other statements and defines a new
/// lexical scope
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    /// The statements that are present inside the block
    pub statements: Vec<Arc<Statement>>,
}

impl Visitable for BlockStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let BlockStatement { statements } = self;

        let outputs: Vec<_> = statements
            .iter()
            .map(|stmnt| stmnt.visit_with(visitor))
            .collect();

        visitor.combine_many_output(outputs)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_block_stmnt(self)
    }
}

/// An if statement lets you conditionally execute statements
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    /// The expression which decides which branch is taken
    pub condition: Expr,
    /// The statement representing the actions taken if the condition is true
    pub then_branch: Arc<Statement>,
    /// The statement representing the actions taken if the condition is false
    pub else_branch: Option<Arc<Statement>>,
}

impl Visitable for IfStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let IfStatement {
            condition,
            then_branch,
            else_branch,
        } = self;

        let o1 = condition.visit_with(visitor);
        let o2 = then_branch.visit_with(visitor);
        let o3 = else_branch.visit_with(visitor);

        visitor.combine_many_output(iter::once(o1).chain(iter::once(o2)).chain(iter::once(o3)))
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_if_stmnt(self)
    }
}
