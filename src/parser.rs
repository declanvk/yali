//! Parser implementation

mod expr;

use crate::{
    ast::ConversionError,
    context::ErrorReport,
    scanner::{Token, TokenType},
    util::peek::Peekable2,
};
pub use expr::*;

/// An error that can occur during the parsing process
#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    /// The scanner produced some error while creating `Token`s
    #[error("the scanner failed to process some text")]
    Scanning(#[from] ErrorReport),
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
        token: Token,
    },
}

/// A struct which manages the state of the `Token` iterator and provides common
/// utilities
#[derive(Debug, Clone)]
pub struct Cursor<I: Iterator<Item = Token>> {
    tokens: Peekable2<I>,
    previous: Option<Token>,
}

impl<I> Cursor<I>
where
    I: Iterator<Item = Token>,
{
    /// Create a new `Cursor`
    pub fn new(tokens: impl IntoIterator<Item = Token, IntoIter = I>) -> Self {
        Cursor {
            tokens: Peekable2::new(tokens.into_iter()),
            previous: None,
        }
    }

    /// Look at the next token without advancing
    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek(0)
    }

    /// Look at the token after the next without advancing
    pub fn peek2(&mut self) -> Option<&Token> {
        self.tokens.peek(1)
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

        return None;
    }

    /// Return a reference to the last `Token` that was produced, if it exists
    pub fn previous(&self) -> Option<&Token> {
        self.previous.as_ref()
    }
}
