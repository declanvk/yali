use std::sync::Arc;

use super::{expression, Cursor, ParseError};
use crate::{
    ast::{ExprStatement, PrintStatement, Statement, VarStatement},
    scanner::{self, Token, TokenType},
    span::Span,
};

// NOTE: Declaration vs statement
// It seems (based on the Crafting Interpreters book), that declarations are
// simply statements that introduce new names into the set of variables,
// functions, or classes.

/// Parse a declaration or fall through to a normal statement
pub fn declaration(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Statement, ParseError> {
    if matches!(c.advance_if(&[TokenType::Var][..]), Some(_)) {
        var_declaration(c)
    } else {
        statement(c)
    }
}

/// Parse a variable declaraction
pub fn var_declaration(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<Statement, ParseError> {
    let name = c.consume(TokenType::Identifier, "expected variable name")?;

    let name = match name.literal.ok_or(ParseError::MissingLiteral)? {
        scanner::Literal::Identifier(s) => s,
        lit => panic!(
            "An `Identifier` token type should guarantee a `Literal::Identifier` value. Actual \
             [{}]",
            lit
        ),
    };

    let initializer = if matches!(c.advance_if(&[TokenType::Equal][..]), Some(_)) {
        Some(expression(c)?)
    } else {
        None
    };

    let _ = c.consume(TokenType::Semicolon, "expected ';' after declaration")?;

    Ok(Statement {
        span: Span::envelop(&[][..]),
        kind: VarStatement { name, initializer }.into(),
    })
}

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
    let _ = c.consume(TokenType::Semicolon, "expected ';' after value")?;

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
    let _ = c.consume(TokenType::Semicolon, "expected ';' after expression")?;

    Ok(Statement {
        span: value.span.clone(),
        kind: ExprStatement {
            expr: Arc::new(value),
        }
        .into(),
    })
}
