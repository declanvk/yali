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
            StatementKind::While(stmnt) => stmnt.visit_with(visitor),
            StatementKind::Function(stmnt) => stmnt.visit_with(visitor),
            StatementKind::Return(stmnt) => stmnt.visit_with(visitor),
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
    /// A var declaration defines and optionally initializes a variable binding
    Var(VarDeclaration),
    /// A block statement contains a list of other statements and defines a new
    /// lexical scope
    Block(BlockStatement),
    /// An if statement lets you conditionally execute statements
    If(IfStatement),
    /// A while loop let you execute a statement multiple times depending on
    /// some condition
    While(WhileStatement),
    /// A function declaration defines the arguments and body of a user-defined
    /// function
    Function(FunctionDeclaration),
    /// A statement which immediately exits the containing function with a value
    Return(ReturnStatement),
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

impl From<VarDeclaration> for StatementKind {
    fn from(v: VarDeclaration) -> Self {
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

impl From<WhileStatement> for StatementKind {
    fn from(v: WhileStatement) -> Self {
        StatementKind::While(v)
    }
}

impl From<FunctionDeclaration> for StatementKind {
    fn from(v: FunctionDeclaration) -> Self {
        StatementKind::Function(v)
    }
}

impl From<ReturnStatement> for StatementKind {
    fn from(v: ReturnStatement) -> Self {
        StatementKind::Return(v)
    }
}

/// A statement consisting a single expression followed by a semicolon
#[derive(Debug, Clone, PartialEq)]
pub struct ExprStatement {
    /// The expression to be evaluated
    pub expr: Expr,
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
    pub expr: Expr,
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

/// A var declaration defines and optionally initializes a variable binding
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    /// The name of the variable binding
    pub name: String,
    /// The initial value of the variable
    pub initializer: Option<Expr>,
}

impl Visitable for VarDeclaration {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let VarDeclaration { initializer, .. } = self;

        initializer.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_var_decl(self)
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

/// A while loop let you execute a statement multiple times depending on some
/// condition
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    /// The condition controls the execution of the while loop, once it
    /// evaluates to false the loop ends
    pub condition: Expr,
    /// The part of the while loop that gets executed multiple times
    pub body: Arc<Statement>,
}

impl Visitable for WhileStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let WhileStatement { condition, .. } = self;

        let o1 = condition.visit_with(visitor);
        let o2 = condition.visit_with(visitor);

        visitor.combine_output(o1, o2)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_while_stmnt(self)
    }
}

/// A function declaration defines the arguments and body of a user-defined
/// function
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    /// The name of the function
    pub name: String,
    /// The names of the parameters to this function
    pub parameters: Vec<String>,
    /// The body of the function
    pub body: Vec<Arc<Statement>>,
}

impl Visitable for FunctionDeclaration {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let FunctionDeclaration { body, .. } = self;

        body.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_func_decl(self)
    }
}

/// A statement which immediately exits the containing function with a value
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    /// The optional return value
    pub value: Option<Expr>,
}

impl Visitable for ReturnStatement {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let ReturnStatement { value } = self;

        value.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_return_stmnt(self)
    }
}
