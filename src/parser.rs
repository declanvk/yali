//! Parser implementation

mod expr;
mod statement;

use crate::{
    ast::{ConversionError, Statement},
    scanner::{ScanError, Token, TokenType},
    util::peek::Peekable1,
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
    #[error("Missing token: {}", .msg)]
    MissingToken {
        /// The accompanying message to the error
        msg: &'static str,
    },
    /// An error produced when the assignment target was illegal
    #[error("invalid assignment target")]
    InvalidAssignmentTarget,
}

/// A struct which manages the state of the `Token` iterator and provides common
/// utilities
#[derive(Debug, Clone)]
pub struct Cursor<I: Iterator<Item = Token>> {
    tokens: Peekable1<I>,
    previous: Option<Token>,
}

impl<I> Cursor<I>
where
    I: Iterator<Item = Token>,
{
    /// Create a new `Cursor`
    pub fn new(tokens: impl IntoIterator<Item = Token, IntoIter = I>) -> Self {
        Cursor {
            tokens: Peekable1::new(tokens.into_iter()),
            previous: None,
        }
    }

    /// Look at the next token without advancing
    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek(0)
    }

    /// Advance the token stream
    pub fn advance(&mut self) -> Option<Token> {
        let o = self.tokens.next();
        self.previous = o.clone();
        o
    }

    /// Return true if the next token type matches the provided type
    pub fn check(&mut self, r#type: TokenType) -> bool {
        self.peek().map(|t| t.r#type == r#type).unwrap_or(false)
    }

    /// Advance the token stream, if the next token matches one of the provided
    /// types
    pub fn advance_if(&mut self, types: &[TokenType]) -> Option<Token> {
        for r#type in types {
            if self.check(*r#type) {
                return self.advance();
            }
        }

        None
    }

    /// Return a reference to the last `Token` that was produced, if it exists
    pub fn previous(&self) -> Option<&Token> {
        self.previous.as_ref()
    }

    /// Advance the token stream if the next token matches the provided type,
    /// otherwise throw an error
    pub fn consume(&mut self, r#type: TokenType, msg: &'static str) -> Result<Token, ParseError> {
        if self.check(r#type) {
            Ok(self.advance().unwrap())
        } else {
            Err(ParseError::MissingToken { msg })
        }
    }

    /// Return true if the token stream is empty
    pub fn is_empty(&mut self) -> bool {
        self.tokens.peek(0).is_none()
    }
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
