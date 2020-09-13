use std::sync::Arc;

use super::{expression, Cursor, ParseError};
use crate::{
    ast::{ExprStatement, PrintStatement, Statement},
    scanner::{Token, TokenType},
};

/// Parse a statement
pub fn statement(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Statement, ParseError> {
    if matches!(c.advance_if(&[TokenType::Print][..]), Some(_)) {
        print_statement(c)
    } else {
        expr_statement(c)
    }
}

/// Parse a print statement
pub fn print_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<Statement, ParseError> {
    let value = expression(c)?;
    let _ = c.consume(TokenType::Semicolon, "Expected ';' after value")?;

    Ok(Statement {
        span: value.span.clone(),
        kind: PrintStatement {
            expr: Arc::new(value),
        }
        .into(),
    })
}

/// Parse an expression statement
pub fn expr_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<Statement, ParseError> {
    let value = expression(c)?;
    let _ = c.consume(TokenType::Semicolon, "Expected ';' after expression")?;

    Ok(Statement {
        span: value.span.clone(),
        kind: ExprStatement {
            expr: Arc::new(value),
        }
        .into(),
    })
}
