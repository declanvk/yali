//! Parser implementation

mod expr;
mod statement;

use crate::{
    ast::{ConversionError, Statement},
    scanner::{Cursor, MissingTokenError, ScanError, Token, TokenType},
};
pub use expr::*;
pub use statement::*;

/// An error that can occur during the parsing process
#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    /// The scanner produced some error while creating `Token`s
    #[error("scan error: {}", .0)]
    Scanning(#[from] ScanError),
    /// Converting from a `TokenType` to an `{Unary,Binary}OpKind` failed
    #[error("convert from a token to an operation failed")]
    OpConversion(#[from] ConversionError),
    /// Parsing failed because the end of the token stream was reached
    #[error("parsing in [{}] failed, require [{:?}] more input tokens", .failed_in, .required)]
    InputRequired {
        /// The parse function that the error was produced in
        failed_in: &'static str,
        /// The minimum number of tokens required, if known
        required: Option<usize>,
    },
    /// An error which occurs because a `Literal` was not present in a
    /// `Token`
    #[error("token was missing the `literal` field")]
    MissingLiteral,
    /// An error which occurs because the token was unexpected in the context
    #[error("parsing in [{}] failed because unexpected token [{:?}]", .failed_in, .token)]
    MisplacedToken {
        /// The parse function that the error was procued in
        failed_in: &'static str,
        /// The token which was unexpected
        token: Option<Token>,
    },
    /// An error produced by the `Cursor` when an expected `Token` is not found
    #[error("Missing token: {}", .0.msg)]
    MissingToken(#[from] MissingTokenError),
    /// An error produced when the assignment target was illegal
    #[error("invalid assignment target")]
    InvalidAssignmentTarget,
}

/// Take the current state of the `Cursor` and attempt to fast-forward until a
/// reasonable parse boundary is found
pub fn synchronize(c: &mut Cursor<impl Iterator<Item = Token>>) {
    while let Some(prev) = c.advance() {
        if prev.r#type == TokenType::Semicolon {
            return;
        }

        let next = if let Some(t) = c.peek() {
            t
        } else {
            return;
        };

        match next.r#type {
            TokenType::Class
            | TokenType::Fun
            | TokenType::For
            | TokenType::If
            | TokenType::Print
            | TokenType::Return
            | TokenType::Var
            | TokenType::While => return,
            _ => {},
        }
    }
}

/// Parse `lox` source
#[tracing::instrument(level = "debug", skip(tokens))]
pub fn parse(tokens: impl IntoIterator<Item = Token>) -> Result<Vec<Statement>, Vec<ParseError>> {
    let mut c = Cursor::new(tokens);
    let mut statements = Vec::new();
    let mut errors = Vec::new();

    loop {
        match statement::declaration(&mut c) {
            Ok(stmnt) => {
                statements.push(stmnt);
            },
            Err(err) => {
                let is_end = matches!(err, ParseError::InputRequired { .. });

                if is_end {
                    if errors.is_empty() {
                        return Ok(statements);
                    } else {
                        errors.push(err);
                        return Err(errors);
                    }
                } else {
                    errors.push(err);
                }

                synchronize(&mut c);
            },
        }
    }
}
