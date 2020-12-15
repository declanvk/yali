//! Utilities for the single-pass compiler

use crate::{
    scanner::{Cursor, Literal, MissingTokenError, ScanError, Token, TokenType},
    vm::{ChunkBuilder, OpCode, Value},
};

/// A single-pass compiler into `lox` bytecode.
pub struct Compiler<I: Iterator<Item = Token>> {
    /// The stream of token from the source code.
    pub cursor: Cursor<I>,
    /// The chunk being built.
    pub current: ChunkBuilder,
}

/// A rule for parsing in the case of a specific `TokenType`.
pub struct ParseRule<I: Iterator<Item = Token>> {
    /// The function that will be used to parse a prefix instance of the
    /// `TokenType`.
    pub prefix_fn_impl: Option<fn(&mut Compiler<I>) -> Result<(), CompilerError>>,
    /// The function that will be used to parse an infix instance of the
    /// `TokenType`.
    pub infix_fn_impl: Option<fn(&mut Compiler<I>) -> Result<(), CompilerError>>,
    /// The priority of this rule.
    pub precedence: Precedence,
}

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

/// Errors that occur during the course of parsing and emitting bytecode.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CompilerError {
    /// An error which occurs because a `Literal` was not present in a
    /// `Token`
    #[error("token was missing the `literal` field")]
    MissingLiteral,
    /// An error which occurs when encountering an unexpected `TokenType`
    #[error("encountered unexpected token [{:?}], expected {}", .actual, .expected)]
    UnexpectedToken {
        /// The `TokenType` encountered in the stream.
        actual: TokenType,
        /// The expected `TokenType` in a static message.
        expected: &'static str,
    },
    /// An error which occurs because a specific `TokenType` was not found.
    #[error("{}", .0)]
    MissingToken(#[from] MissingTokenError),
    /// An error which occurs because of something in the scanning process.
    #[error("{}", .0)]
    ScanError(#[from] ScanError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{scanner::Scanner, vm::Chunk};

    macro_rules! assert_instructions {
        ($chunk:ident => {$($op:expr $(, [$($data:expr)*])? ;)+}) => {
            {
                let mut instructions = $chunk.into_iter();
                $(
                    {
                        let (_, inst) = instructions.next().expect("unable to get next instruction");
                        let inst = inst.expect("error in chunk iter");
                        assert_eq!(inst.op, $op);
                        $(
                            assert_eq!(inst.arguments, &[$($data)*][..]);
                        )?

                    }
                )+
            }

        };
    }

    fn compile_expression(src: &str) -> Chunk {
        let tokens = Scanner::new(src);
        let mut compiler = Compiler {
            cursor: Cursor::new(tokens),
            current: ChunkBuilder::default(),
        };

        expression(&mut compiler).expect("unable to parse expression from tokens");

        compiler.current.return_inst(1);

        compiler
            .current
            .build()
            .expect("unable to build compiled chunk")
    }

    #[test]
    fn simple_arith_compile() {
        let chunk = compile_expression("10 + 20");
        assert_eq!(&*chunk.constants, &[10.0.into(), 20.0.into()][..]);
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Add;
        });
    }

    #[test]
    fn paren_arith_compile() {
        let chunk = compile_expression("10 * (20 + (30 - 2))");
        assert_eq!(
            &*chunk.constants,
            &[10.0.into(), 20.0.into(), 30.0.into(), 2.0.into()][..]
        );
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Constant, [2];
            OpCode::Constant, [3];
            OpCode::Subtract;
            OpCode::Add;
            OpCode::Multiply;
        });
    }

    #[test]
    fn comparison_compile() {
        let chunk = compile_expression("(10.0 < 2) == ((1.2 - 3.2) <= 0)");
        assert_eq!(
            &*chunk.constants,
            &[10.0.into(), 2.0.into(), 1.2.into(), 3.2.into(), 0.0.into()][..]
        );
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Less;
            OpCode::Constant, [2];
            OpCode::Constant, [3];
            OpCode::Subtract;
            OpCode::Constant, [4];
            OpCode::Greater;
            OpCode::Not;
            OpCode::Equal;
        });
    }

    #[test]
    fn negation_compile() {
        let chunk = compile_expression("!(5 - 4 > 3 * 2 == !nil)");
        assert_eq!(
            &*chunk.constants,
            &[5.0.into(), 4.0.into(), 3.0.into(), 2.0.into()][..]
        );
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Subtract;
            OpCode::Constant, [2];
            OpCode::Constant, [3];
            OpCode::Multiply;
            OpCode::Greater;
            OpCode::Nil;
            OpCode::Not;
            OpCode::Equal;
            OpCode::Not;
        });
    }
}
