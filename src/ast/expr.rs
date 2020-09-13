use crate::{
    ast::visit::{Visitable, Visitor},
    scanner::{Literal, TokenType},
    span::Span,
};
use std::{convert::TryFrom, fmt, sync::Arc};

/// Errors that occur when converting to AST elements
#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ConversionError {
    /// An error that occurs when converting from a raw `TokenType` to a
    /// specific `{Binary,Unary}OpKind`
    #[error("[{:?}] does not correspond to any operation.", .0)]
    Op(TokenType),

    /// An error that occurs when mapping token literals to expression literals
    #[error("the literal [{:?}] has no equivalent expression.", .0)]
    Literal(Literal),
}

/// Syntax tree of a lox expression
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    /// The type of expression
    pub kind: ExprKind,
    /// The continuous block of code this expression covers
    pub span: Span,
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

/// Different types of expressions
#[derive(Debug, Clone, PartialEq)]
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

impl From<BinaryExpr> for ExprKind {
    fn from(v: BinaryExpr) -> Self {
        ExprKind::Binary(v)
    }
}

impl From<UnaryExpr> for ExprKind {
    fn from(v: UnaryExpr) -> Self {
        ExprKind::Unary(v)
    }
}

impl From<LiteralExpr> for ExprKind {
    fn from(v: LiteralExpr) -> Self {
        ExprKind::Literal(v)
    }
}

impl From<GroupingExpr> for ExprKind {
    fn from(v: GroupingExpr) -> Self {
        ExprKind::Grouping(v)
    }
}

/// A binary operation
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    /// The first argument to the operation
    pub left: Arc<Expr>,
    /// The type of binary operation
    pub operator: BinaryOpKind,
    /// The second argument to the operation
    pub right: Arc<Expr>,
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

/// Different types of binary operations
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOpKind {
    /// Multiplication operation, symbolized by `*`
    Mult,
    /// Addition operation, symbolized by `+`
    Add,
    /// Subtraction operation, symbolized by `-`
    Sub,
    /// Division operation, symbolized by `/`
    Div,

    /// Boolean AND operation, symbolized by `and`
    And,
    /// Boolean OR operation, symbolized by `or`
    Or,

    /// Comparison equal operation, symbolized by `==`
    Equal,
    /// Comparison not equal operation, symbolized by `!=`
    NotEqual,
    /// Comparison greater than operation, symbolized by `>`
    Greater,
    /// Comparison less than operation, symbolized by `<`
    Less,
    /// Comparison greater than or equal operation, symbolized by `>=`
    GreaterEqual,
    /// Comparison less than or equal operation, symbolized by `<=`
    LessEqual,
}

impl BinaryOpKind {
    /// Return a static string which symbolizes this operation
    pub fn symbol(&self) -> &'static str {
        match self {
            BinaryOpKind::Mult => "*",
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Div => "/",
            BinaryOpKind::And => "and",
            BinaryOpKind::Or => "or",
            BinaryOpKind::Equal => "==",
            BinaryOpKind::NotEqual => "!=",
            BinaryOpKind::Greater => ">",
            BinaryOpKind::Less => "<",
            BinaryOpKind::GreaterEqual => ">=",
            BinaryOpKind::LessEqual => "<=",
        }
    }
}

impl TryFrom<TokenType> for BinaryOpKind {
    type Error = ConversionError;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            // TokenType::Dot => {}
            TokenType::Minus => Ok(BinaryOpKind::Sub),
            TokenType::Plus => Ok(BinaryOpKind::Add),
            TokenType::Slash => Ok(BinaryOpKind::Div),
            TokenType::Star => Ok(BinaryOpKind::Mult),

            TokenType::BangEqual => Ok(BinaryOpKind::NotEqual),
            TokenType::EqualEqual => Ok(BinaryOpKind::Equal),
            TokenType::Greater => Ok(BinaryOpKind::Greater),
            TokenType::GreaterEqual => Ok(BinaryOpKind::GreaterEqual),
            TokenType::Less => Ok(BinaryOpKind::Less),
            TokenType::LessEqual => Ok(BinaryOpKind::LessEqual),

            TokenType::And => Ok(BinaryOpKind::And),
            TokenType::Or => Ok(BinaryOpKind::Or),

            t => Err(ConversionError::Op(t)),
        }
    }
}

impl fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

/// An expression that is delimited by parentheses
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    /// A boolean value
    Boolean(bool),
    /// A numeric value
    Number(f64),
    /// A string value (UTF-8)
    String(String),
    /// A null value
    Null,
}

impl fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralExpr::Boolean(b) => write!(f, "{}", b),
            LiteralExpr::Number(n) => write!(f, "{}", n),
            LiteralExpr::String(s) => write!(f, "\"{}\"", s),
            LiteralExpr::Null => write!(f, "nil"),
        }
    }
}

impl TryFrom<Literal> for LiteralExpr {
    type Error = ConversionError;

    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Number(f) => Ok(LiteralExpr::Number(f)),
            Literal::String(s) => Ok(LiteralExpr::String(s)),
            Literal::Identifier(_) => Err(ConversionError::Literal(value)),
        }
    }
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
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    /// The type of unary operation
    pub operator: UnaryOpKind,
    /// The argument to the operation
    pub right: Arc<Expr>,
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

/// Different types of unary operations
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOpKind {
    /// Negation operation, symbolized by `-`
    Negate,
    /// Boolean not operation, symbolized by `!`,
    Not,
}

impl UnaryOpKind {
    /// Return a static string which symbolizes this operation
    pub fn symbol(&self) -> &'static str {
        match self {
            UnaryOpKind::Negate => "-",
            UnaryOpKind::Not => "!",
        }
    }
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOpKind::Negate => "-",
            UnaryOpKind::Not => "!",
        };

        write!(f, "{}", s)
    }
}

impl TryFrom<TokenType> for UnaryOpKind {
    type Error = ConversionError;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Minus => Ok(UnaryOpKind::Negate),
            TokenType::Bang => Ok(UnaryOpKind::Not),

            t => Err(ConversionError::Op(t)),
        }
    }
}
