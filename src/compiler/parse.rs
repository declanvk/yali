use super::{Compiler, CompilerError, Precedence};
use crate::{
    scanner::{Literal, MissingTokenError, Token, TokenType},
    vm::{OpCode, Value},
};

/// Compile an expression.
pub fn expression(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    parse_precedence(c, Precedence::Assignment)
}

/// Attempt to compile a numeric literal, having already observed a `Number`
/// token.
pub fn number(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    // This is non-`None` because the core of the parser will prime the iterator or
    // return earlier if it was empty.
    let num_token = c.cursor.previous().unwrap();
    let value: Value = match num_token
        .literal
        .as_ref()
        .ok_or(CompilerError::MissingLiteral)?
    {
        Literal::Number(n) => Value::from(*n),
        // This branch should never run because the `parse_precedence` should never dispatch to this
        // function unless the previous `TokenType` is a `Number`.
        _ => unreachable!(),
    };

    c.current
        .constant_inst(value, num_token.span.line() as usize);

    Ok(())
}

/// Attempt to compile a literal (boolean or nil) expression, having already
/// observed a literal (boolean or nil) token.
pub fn literal(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let lit_token = c.cursor.previous().unwrap();
    let line_number = lit_token.span.line();

    let op = match lit_token.r#type {
        TokenType::False => OpCode::False,
        TokenType::Nil => OpCode::Nil,
        TokenType::True => OpCode::True,
        // This branch should never run because the `parse_precedence` should never dispatch to this
        // function (`literal`) unless the previous `TokenType` is one of the above.
        _ => unreachable!(),
    };

    c.current.simple_inst(op, line_number as usize);

    Ok(())
}

/// Attempt to compile a grouped expression, having already observed a `(`
/// token.
pub fn grouping(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    expression(c)?;

    c.cursor
        .consume(TokenType::RightParen, "expected ')' after expression")?;

    Ok(())
}

/// Attempt to compile a unary operation, having already observed a `-` or `!`
/// token.
pub fn unary(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let prev_token = c.cursor.previous().unwrap().clone();

    parse_precedence(c, Precedence::Unary)?;

    let line_number = prev_token.span.line() as usize;

    match prev_token.r#type {
        TokenType::Minus => {
            c.current.simple_inst(OpCode::Negate, line_number);
            Ok(())
        },
        TokenType::Bang => {
            c.current.simple_inst(OpCode::Not, line_number);
            Ok(())
        },
        x => Err(CompilerError::UnexpectedToken {
            actual: x,
            expected: "`-` or `!`",
        }),
    }
}

/// Attempt to compile a binary operation, having observed a requisite starting
/// token.
pub fn binary<I>(c: &mut Compiler<I>) -> Result<(), CompilerError>
where
    I: Iterator<Item = Token>,
{
    let prev_token = c.cursor.previous().unwrap().clone();

    let rule = Precedence::get_rule::<I>(prev_token.r#type);
    parse_precedence(c, rule.precedence.next_highest())?;
    let line_number = prev_token.span.line() as usize;

    match prev_token.r#type {
        TokenType::Plus => {
            c.current.simple_inst(OpCode::Add, line_number);
        },
        TokenType::Minus => {
            c.current.simple_inst(OpCode::Subtract, line_number);
        },
        TokenType::Star => {
            c.current.simple_inst(OpCode::Multiply, line_number);
        },
        TokenType::Slash => {
            c.current.simple_inst(OpCode::Divide, line_number);
        },
        TokenType::EqualEqual => {
            c.current.simple_inst(OpCode::Equal, line_number);
        },
        TokenType::BangEqual => {
            c.current
                .simple_inst(OpCode::Equal, line_number)
                .simple_inst(OpCode::Not, line_number);
        },
        TokenType::Greater => {
            c.current.simple_inst(OpCode::Greater, line_number);
        },
        TokenType::GreaterEqual => {
            c.current
                .simple_inst(OpCode::Less, line_number)
                .simple_inst(OpCode::Not, line_number);
        },
        TokenType::Less => {
            c.current.simple_inst(OpCode::Less, line_number);
        },
        TokenType::LessEqual => {
            c.current
                .simple_inst(OpCode::Greater, line_number)
                .simple_inst(OpCode::Not, line_number);
        },
        x => {
            return Err(CompilerError::UnexpectedToken {
                actual: x,
                expected: "`+`, `-`, `*`, or `/`",
            })
        },
    }

    Ok(())
}

/// Attempt to parse a string expression, having observed a string token.
pub fn string<I>(c: &mut Compiler<I>) -> Result<(), CompilerError>
where
    I: Iterator<Item = Token>,
{
    let tok = c.cursor.previous().unwrap();
    let line_number = tok.span.line() as usize;

    match tok.literal.as_ref().unwrap() {
        Literal::String(s) => {
            c.current.constant_string_inst(s.to_string(), line_number);
        },
        // This branch should never run because the `parse_precedence` should never dispatch to this
        // function (`literal`) unless the previous `TokenType` is `TokenType::String` and the
        // literal payload is also `Literal::String`.
        _ => unreachable!(),
    }

    Ok(())
}

/// Parse the next token, dispatching to a more specific parse rule based on the
/// `TokenType` and the `Precedence` given.
pub fn parse_precedence<I>(
    c: &mut Compiler<I>,
    precendence: Precedence,
) -> Result<(), CompilerError>
where
    I: Iterator<Item = Token>,
{
    let tok = c.cursor.advance();
    let rule = match tok {
        Some(tok) => Precedence::get_rule(tok.r#type),
        None => {
            return Err(CompilerError::MissingToken(MissingTokenError {
                msg: "expected any token",
            }))
        },
    };

    (rule.prefix_fn_impl.expect("missing prefix parse impl"))(c)?;

    while precendence
        <= c.cursor
            .peek()
            .map(|tok| Precedence::get_rule::<I>(tok.r#type).precedence)
            .unwrap_or_else(|| Precedence::None)
    {
        let tok = c.cursor.advance();
        let rule = match tok {
            Some(tok) => Precedence::get_rule::<I>(tok.r#type),
            None => {
                return Err(CompilerError::MissingToken(MissingTokenError {
                    msg: "expected any token",
                }))
            },
        };

        match rule.infix_fn_impl {
            Some(parse_impl) => (parse_impl)(c)?,
            None => return Ok(()),
        }
    }

    Ok(())
}
