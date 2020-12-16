use super::{binary, grouping, literal, number, unary, ParseRule};
use crate::scanner::{Token, TokenType};

/// A relative measure of priority used while parsing.
///
/// The ordering is from lowest (`Precedence::None`) to highest
/// (`Precedence::Primary`). Parse rules with higher precedence will bind more
/// tightly than low precedence rules.
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash)]
#[repr(u16)]
pub enum Precedence {
    /// Least precedence
    None = 0,
    /// Assignment expression, `x = 10`.
    Assignment,
    /// Logical or operation, `true or false`
    Or,
    /// Logical and operation, `true and false`
    And,
    /// Equality comparison operation, `1 == 2` or `2 != 1`
    Equality,
    /// Numeric comparison operation, `1 >= 2` or `2 < 1`
    Comparison,
    /// Addition or subtraction
    Term,
    /// Multiplication or division
    Factor,
    /// Numeric or logical negation
    Unary,
    /// Function call or property access
    Call,
    /// Highest precedence level
    Primary,
}

impl Precedence {
    /// Produce the next highest `Precedence` level, saturating at
    /// `Predence::Primary`.
    pub fn next_highest(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }

    /// Return the `ParseRule` for the specific `TokenType`.
    #[rustfmt::skip]
    pub fn get_rule<I>(token_type: TokenType) -> ParseRule<I>
    where
        I: Iterator<Item = Token>,
    {
        use TokenType::*;
        match token_type {
            LeftParen =>    ParseRule { prefix_fn_impl: Some(grouping), infix_fn_impl: None, precedence: Precedence::None },
            RightParen =>   ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            LeftBrace =>    ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            RightBrace =>   ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Comma =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Dot =>          ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Minus =>        ParseRule { prefix_fn_impl: Some(unary), infix_fn_impl: Some(binary), precedence: Precedence::Term },
            Plus =>         ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Term },
            Semicolon =>    ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Slash =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Factor },
            Star =>         ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Factor },
            Bang =>         ParseRule { prefix_fn_impl: Some(unary), infix_fn_impl: None, precedence: Precedence::None },
            BangEqual =>    ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Equality },
            Equal =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            EqualEqual =>   ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Equality },
            Greater =>      ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Comparison },
            GreaterEqual => ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Comparison },
            Less =>         ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Comparison },
            LessEqual =>    ParseRule { prefix_fn_impl: None, infix_fn_impl: Some(binary), precedence: Precedence::Comparison },
            Identifier =>   ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            String =>       ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Number =>       ParseRule { prefix_fn_impl: Some(number), infix_fn_impl: None, precedence: Precedence::None },
            And =>          ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Class =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Else =>         ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            False =>        ParseRule { prefix_fn_impl: Some(literal), infix_fn_impl: None, precedence: Precedence::None },
            Fun =>          ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            For =>          ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            If =>           ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Nil =>          ParseRule { prefix_fn_impl: Some(literal), infix_fn_impl: None, precedence: Precedence::None },
            Or =>           ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Print =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Return =>       ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Super =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            This =>         ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            True =>         ParseRule { prefix_fn_impl: Some(literal), infix_fn_impl: None, precedence: Precedence::None },
            Var =>          ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            While =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
            Error =>        ParseRule { prefix_fn_impl: None, infix_fn_impl: None, precedence: Precedence::None },
        }
    }
}

impl Default for Precedence {
    fn default() -> Self {
        Precedence::None
    }
}
