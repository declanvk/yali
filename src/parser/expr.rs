use super::{Cursor, ParseError};
use crate::{
    ast::{
        AssignExpr, BinaryExpr, CallExpr, Expr, ExprKind, GroupingExpr, LiteralExpr, LogicalExpr,
        UnaryExpr, VarExpr,
    },
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
    let expr = or(c)?;

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

        Err(ParseError::InvalidAssignmentTarget)
    } else {
        Ok(expr)
    }
}

/// Parse an OR logical expression
pub fn or(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = and(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::Or][..]) {
        let right = and(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: LogicalExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
}

/// Parse an AND logical expression
pub fn and(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = equality(c)?;

    while let Some(tok) = c.advance_if(&[TokenType::And][..]) {
        let right = equality(c)?;

        expr = Expr {
            span: Span::envelop([&expr.span, &tok.span, &right.span].iter().copied()),
            kind: LogicalExpr {
                left: Arc::new(expr),
                right: Arc::new(right),
                operator: tok.r#type.try_into()?,
            }
            .into(),
        };
    }

    Ok(expr)
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
        call(c)
    }
}

/// Parse a function call expression
#[tracing::instrument(level = "debug", skip(c))]
pub fn call(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut callee = primary(c)?;

    loop {
        if let Some(open_paren) = c.advance_if(&[TokenType::LeftParen][..]) {
            let mut arguments = Vec::new();

            if !c.check(TokenType::RightParen) {
                'args: loop {
                    arguments.push(expression(c)?);

                    if c.advance_if(&[TokenType::Comma][..]).is_none() {
                        break 'args;
                    }
                }
            }

            let close_paren = c.consume(TokenType::RightParen, "expected ')' after arguments")?;

            callee = Expr {
                span: Span::envelop(
                    [&callee.span, &open_paren.span, &close_paren.span]
                        .iter()
                        .copied(),
                ),
                kind: CallExpr {
                    callee: Arc::new(callee),
                    arguments: arguments.into_iter().map(Arc::new).collect(),
                }
                .into(),
            };
        } else {
            break Ok(callee);
        }
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
            span: tok.span,
            kind: lit.into(),
        });
    }

    if let Some(tok) = c.advance_if(&[TokenType::Identifier][..]) {
        let span = tok.span.clone();
        let name = tok.unwrap_identifier_name();
        return Ok(Expr {
            span,
            kind: VarExpr { name }.into(),
        });
    }

    if c.advance_if(&[TokenType::LeftParen][..]).is_some() {
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
        if let Some(Token { error: Some(e), .. }) = c.peek() {
            Err(e.clone().into())
        } else {
            Err(ParseError::MisplacedToken {
                failed_in: "primary",
                token: c.peek().cloned(),
            })
        }
    }
}
