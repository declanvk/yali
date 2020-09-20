use super::{Cursor, ParseError};
use crate::{
    ast::{AssignExpr, BinaryExpr, Expr, ExprKind, GroupingExpr, LiteralExpr, UnaryExpr, VarExpr},
    scanner,
    scanner::{Token, TokenType},
    span::Span,
};
use std::{convert::TryInto, sync::Arc};

/// Parse an expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn expression(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    assignment(c)
}

/// Parse an assignment expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn assignment(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let expr = equality(c)?;

    if let Some(equal) = c.advance_if(&[TokenType::Equal]) {
        let value = assignment(c)?;

        if let ExprKind::Var(VarExpr { name }) = expr.kind {
            return Ok(Expr {
                span: Span::envelop([&expr.span, &equal.span, &value.span].iter().copied()),
                kind: AssignExpr {
                    name,
                    value: Arc::new(value),
                }
                .into(),
            });
        }

        Err(ParseError::InvalidAssignmentTarget { target: expr })
    } else {
        Ok(expr)
    }
}

/// Parse an equality expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn equality(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = comparison(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::BangEqual, TokenType::EqualEqual][..]) {
        let right = comparison(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
}

/// Parse a comparison expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn comparison(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = addition(c)?;

    while let Some(tok) = c.advance_if(
        &[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ][..],
    ) {
        let right = addition(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
}

/// Parse an addition or subtraction expression
pub fn addition(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = multiplication(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::Plus, TokenType::Minus][..]) {
        let right = multiplication(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
}

/// Parse a multiplication or division expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn multiplication(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = unary(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::Star, TokenType::Slash][..]) {
        let right = unary(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
}

/// Parse a unary operation expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn unary(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    if let Some(tok) = c.advance_if(&[TokenType::Bang, TokenType::Minus]) {
        let right = unary(c)?;

        Ok(Expr {
            span: Span::envelop([&tok.span, &right.span].iter().copied()),
            kind: UnaryExpr {
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        })
    } else {
        primary(c)
    }
}

/// Parse a primary expression: a literal or a grouping expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn primary(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let lit: Option<LiteralExpr> = {
        let tok = c.peek().ok_or(ParseError::InputRequired {
            failed_in: "primary",
            required: Some(1),
        })?;

        match tok.r#type {
            TokenType::String | TokenType::Number => Some(
                tok.literal
                    .as_ref()
                    .ok_or(ParseError::MissingLiteral)?
                    .clone()
                    .try_into()?,
            ),
            TokenType::False => Some(LiteralExpr::Boolean(false)),
            TokenType::Nil => Some(LiteralExpr::Null),
            TokenType::True => Some(LiteralExpr::Boolean(true)),
            _ => None,
        }
    };

    if let Some(lit) = lit {
        let tok = c.advance().unwrap();

        return Ok(Expr {
            span: tok.span.clone(),
            kind: lit.into(),
        });
    }

    if let Some(tok) = c.advance_if(&[TokenType::Identifier][..]) {
        let name = match tok.literal.ok_or(ParseError::MissingLiteral)? {
            scanner::Literal::Identifier(s) => s,
            lit => panic!(
                "An `Identifier` token type should guarantee a `Literal::Identifier` value. \
                 Actual [{}]",
                lit
            ),
        };

        return Ok(Expr {
            span: tok.span.clone(),
            kind: VarExpr { name }.into(),
        });
    }

    if let Some(_) = c.advance_if(&[TokenType::LeftParen][..]) {
        let inner = expression(c)?;
        let _ = c.consume(TokenType::RightParen, "Expect ')' after expression.")?;

        Ok(Expr {
            span: inner.span.clone(),
            kind: GroupingExpr {
                inner: Arc::new(inner),
            }
            .into(),
        })
    } else {
        Err(ParseError::MisplacedToken {
            failed_in: "primary",
            token: c.peek().cloned(),
        })
    }
}
