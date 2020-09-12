use super::{Cursor, ParseError};
use crate::{
    ast::{BinaryExpr, Expr, ExprKind, LiteralExpr, UnaryExpr},
    scanner::{Token, TokenType},
    span::Span,
};
use std::{convert::TryInto, sync::Arc};

/// Parse an equality expression
pub fn equality(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = comparison(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::BangEqual, TokenType::EqualEqual][..]) {
        let right = comparison(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: ExprKind::Binary(BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }),
        };
    }

    Ok(expr)
}

/// Parse a comparison expression
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
            kind: ExprKind::Binary(BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }),
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
            kind: ExprKind::Binary(BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }),
        };
    }

    Ok(expr)
}

/// Parse a multiplication or division expression
pub fn multiplication(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = unary(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::Star, TokenType::Slash][..]) {
        let right = unary(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: ExprKind::Binary(BinaryExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }),
        };
    }

    Ok(expr)
}

/// Parse a unary operation expression
pub fn unary(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    if let Some(tok) = c.advance_if(&[TokenType::Bang, TokenType::Minus]) {
        let right = unary(c)?;

        Ok(Expr {
            span: Span::envelop([&tok.span, &right.span].iter().copied()),
            kind: ExprKind::Unary(UnaryExpr {
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }),
        })
    } else {
        primary(c)
    }
}

/// Parse a primary expression: a literal or a grouping expression
pub fn primary(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let tok = c.peek().ok_or(ParseError::InputRequired {
        failed_in: "primary",
        required: Some(1),
    })?;

    let lit: LiteralExpr = match tok.r#type {
        TokenType::String | TokenType::Number => tok
            .literal
            .as_ref()
            .ok_or(ParseError::MissingLiteral)?
            .clone()
            .try_into()?,
        TokenType::False => LiteralExpr::Boolean(false),
        TokenType::Nil => LiteralExpr::Null,
        TokenType::True => LiteralExpr::Boolean(true),
        _ => {
            return Err(ParseError::MisplacedToken {
                failed_in: "primary",
                token: tok.clone(),
            })
        },
    };

    Ok(Expr {
        span: tok.span.clone(),
        kind: ExprKind::Literal(lit),
    })
}
