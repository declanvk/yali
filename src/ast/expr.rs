use crate::{
    ast::visit::{Visitable, Visitor},
    scanner::Literal,
    span::Span,
};
use std::{fmt, sync::Arc};

/// Syntax tree of a lox expression
#[derive(Debug)]
pub struct Expr {
    /// The type of expression
    pub kind: ExprKind,
    /// The continuous block of code this expression covers
    pub span: Span,
}

/// Different types of expressions
#[derive(Debug)]
pub enum ExprKind {
    /// A binary operation
    Binary(BinaryExpr),
    /// Different types of binary operations
    Grouping(GroupingExpr),
    /// A literal value
    Literal(LiteralExpr),
    /// A unary operation
    Unary(UnaryExpr),
}

impl Visitable for Expr {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let Expr { kind, .. } = self;

        match kind {
            ExprKind::Binary(e) => e.visit_with(visitor),
            ExprKind::Grouping(e) => e.visit_with(visitor),
            ExprKind::Literal(e) => e.visit_with(visitor),
            ExprKind::Unary(e) => e.visit_with(visitor),
        }
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_expr(self)
    }
}

/// A binary operation
#[derive(Debug)]
pub struct BinaryExpr {
    /// The first argument to the operation
    pub left: Arc<Expr>,
    /// The type of binary operation
    pub operator: BinaryOpKind,
    /// The second argument to the operation
    pub right: Arc<Expr>,
}

/// Different types of binary operations
#[derive(Debug)]
pub enum BinaryOpKind {
    /// Multiplication operation, symbolized by `*`
    Mult,
}

impl fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOpKind::Mult => "*",
        };

        write!(f, "{}", s)
    }
}

impl Visitable for BinaryExpr {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let BinaryExpr { left, right, .. } = self;

        let o1 = left.visit_with(visitor);
        let o2 = right.visit_with(visitor);

        visitor.combine_output(o1, o2)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_binary_expr(self)
    }
}

/// An expression that is delimited by parentheses
#[derive(Debug)]
pub struct GroupingExpr {
    /// The inner expression
    pub inner: Arc<Expr>,
}

impl Visitable for GroupingExpr {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let GroupingExpr { inner, .. } = self;

        inner.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_grouping_expr(self)
    }
}

/// A literal value
#[derive(Debug)]
pub struct LiteralExpr {
    /// The literal value of the expression
    pub value: Literal,
}

impl Visitable for LiteralExpr {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.default_output()
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_literal_expr(self)
    }
}

/// A unary operation
#[derive(Debug)]
pub struct UnaryExpr {
    /// The type of unary operation
    pub operator: UnaryOpKind,
    /// The argument to the operation
    pub right: Arc<Expr>,
}

/// Different types of unary operations
#[derive(Debug)]
pub enum UnaryOpKind {
    /// Multiplication operation, symbolized by `*`
    Negate,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOpKind::Negate => "-",
        };

        write!(f, "{}", s)
    }
}

impl Visitable for UnaryExpr {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let UnaryExpr { right, .. } = self;

        right.visit_with(visitor)
    }

    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_unary_expr(self)
    }
}
