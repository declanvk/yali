use super::{Compiler, CompilerError, Precedence, VariableRef};
use crate::{
    parser::synchronize,
    scanner::{Literal, MissingTokenError, Token, TokenType},
    span::Span,
    vm::{OpCode, Value},
};

/// Compile a declaration.
#[tracing::instrument(level = "debug", skip(c))]
pub fn declaration(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let res = if c.cursor.advance_if(&[TokenType::Var][..]).is_some() {
        var_declaration(c)
    } else {
        statement(c)
    };

    if res.is_err() {
        synchronize(&mut c.cursor);
    }

    res
}

/// Compile a variable declaration
#[tracing::instrument(level = "debug", skip(c))]
pub fn var_declaration(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    // 1. Read variable name
    let ident = c
        .cursor
        .consume(TokenType::Identifier, "expected variable name")?;
    let line_number = ident.span.line();

    // declare the variable ahead of initialization
    let variable_ref = c.declare_variable(ident.unwrap_identifier_name())?;

    // Write initialization expression to struct
    if c.cursor.advance_if(&[TokenType::Equal][..]).is_some() {
        expression(c)?;
    } else {
        c.current.simple_inst(OpCode::Nil, line_number as usize);
    }

    // Expected semicolon
    c.cursor
        .consume(TokenType::Semicolon, "expected ';' after value")?;

    // now that the variable has been initialized, define it
    c.define_variable(variable_ref, line_number as usize);

    Ok(())
}

/// Compile a statement.
#[tracing::instrument(level = "debug", skip(c))]
pub fn statement(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    if c.cursor.advance_if(&[TokenType::Print][..]).is_some() {
        print_statement(c)
    } else if c.cursor.advance_if(&[TokenType::If][..]).is_some() {
        if_statement(c)
    } else if c.cursor.advance_if(&[TokenType::While][..]).is_some() {
        while_statement(c)
    } else if c.cursor.advance_if(&[TokenType::LeftBrace][..]).is_some() {
        c.begin_scope();
        let block_result = block_statement(c);
        c.end_scope();

        block_result
    } else {
        expression_statement(c)
    }
}

/// Attempt to compile a print statement.
#[tracing::instrument(level = "debug", skip(c))]
pub fn print_statement(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let line_number = c.cursor.previous().unwrap().span.line();
    expression(c)?;
    c.cursor
        .consume(TokenType::Semicolon, "expected ';' after value")?;
    c.current.simple_inst(OpCode::Print, line_number as usize);

    Ok(())
}

/// Attempt to compile a while statement.
#[tracing::instrument(level = "debug", skip(c))]
pub fn while_statement(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let while_line_number = c.cursor.previous().unwrap().span.line();

    let loop_start = c.current.prepare_loop();

    c.cursor
        .consume(TokenType::LeftParen, "expected '(' after 'while'")?;
    expression(c)?;
    c.cursor
        .consume(TokenType::RightParen, "expected ')' after condition")?;

    let exit_patch = c
        .current
        .jump_inst(OpCode::JumpIfFalse, while_line_number as usize);

    c.current
        .simple_inst(OpCode::Pop, while_line_number as usize);
    statement(c)?;
    let while_end_line_number = c.current.get_last_line();
    c.current.loop_inst(loop_start, while_end_line_number);

    c.current.complete_patch(exit_patch);
    let exit_line_number = c.current.get_last_line();
    c.current.simple_inst(OpCode::Pop, exit_line_number);

    Ok(())
}

/// Attempt to compile an if statement
#[tracing::instrument(level = "debug", skip(c))]
pub fn if_statement(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    let if_line_number = c.cursor.previous().unwrap().span.line();

    c.cursor
        .consume(TokenType::LeftParen, "expected '(' after 'if'")?;
    expression(c)?;
    c.cursor
        .consume(TokenType::RightParen, "expected ')' after condition")?;

    let then_patch = c
        .current
        .jump_inst(OpCode::JumpIfFalse, if_line_number as usize);

    // used to clear the stack from the result of the condition
    c.current.simple_inst(OpCode::Pop, if_line_number as usize);
    // "then" branch - the condition was true
    statement(c)?;

    // This jump is unconditional so that if there is an `else` branch we can jump
    // directly to it. Otherwise, we just jump to the next instruction, basically a
    // no-op
    let then_branch_last_line = c.current.get_last_line();
    let else_patch = c.current.jump_inst(OpCode::Jump, then_branch_last_line);

    c.current.complete_patch(then_patch);

    if c.cursor.advance_if(&[TokenType::Else][..]).is_some() {
        let else_line_number = c.cursor.previous().unwrap().span.line();
        // used to clear the stack from the result of the condition
        c.current
            .simple_inst(OpCode::Pop, else_line_number as usize);
        // "else" branch - the condition was false
        statement(c)?;
    }

    c.current.complete_patch(else_patch);

    Ok(())
}

/// Compile an expression statement
#[tracing::instrument(level = "debug", skip(c))]
pub fn expression_statement(
    c: &mut Compiler<impl Iterator<Item = Token>>,
) -> Result<(), CompilerError> {
    expression(c)?;
    let semi_tok = c
        .cursor
        .consume(TokenType::Semicolon, "expected ';' after value")?;
    c.current
        .simple_inst(OpCode::Pop, semi_tok.span.line() as usize);

    Ok(())
}

/// Compile an block statement
#[tracing::instrument(level = "debug", skip(c))]
pub fn block_statement(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    loop {
        if c.cursor.check(TokenType::RightBrace) || c.cursor.peek().is_none() {
            break;
        }

        declaration(c)?
    }

    c.cursor
        .consume(TokenType::RightBrace, "expected '}' after block")?;

    Ok(())
}

/// Compile an expression.
#[tracing::instrument(level = "debug", skip(c))]
pub fn expression(c: &mut Compiler<impl Iterator<Item = Token>>) -> Result<(), CompilerError> {
    parse_precedence(c, Precedence::Assignment)
}

/// Attempt to compile a numeric literal, having already observed a `Number`
/// token.
#[tracing::instrument(level = "debug", skip(c))]
pub fn number(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
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
#[tracing::instrument(level = "debug", skip(c))]
pub fn literal(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
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
#[tracing::instrument(level = "debug", skip(c))]
pub fn grouping(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
    expression(c)?;

    c.cursor
        .consume(TokenType::RightParen, "expected ')' after expression")?;

    Ok(())
}

/// Attempt to compile a unary operation, having already observed a `-` or `!`
/// token.
#[tracing::instrument(level = "debug", skip(c))]
pub fn unary(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
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
#[tracing::instrument(level = "debug", skip(c))]
pub fn binary<I>(c: &mut Compiler<I>, can_assign: bool) -> Result<(), CompilerError>
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
            c.current.simple_inst(OpCode::Equal, line_number);
            c.current.simple_inst(OpCode::Not, line_number);
        },
        TokenType::Greater => {
            c.current.simple_inst(OpCode::Greater, line_number);
        },
        TokenType::GreaterEqual => {
            c.current.simple_inst(OpCode::Less, line_number);
            c.current.simple_inst(OpCode::Not, line_number);
        },
        TokenType::Less => {
            c.current.simple_inst(OpCode::Less, line_number);
        },
        TokenType::LessEqual => {
            c.current.simple_inst(OpCode::Greater, line_number);
            c.current.simple_inst(OpCode::Not, line_number);
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

/// Attempt to parse a variable expression, having observed an identifier token.
#[tracing::instrument(level = "debug", skip(c))]
pub fn variable(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
    let tok = c.cursor.previous().cloned().unwrap();
    let line_number = tok.span.line() as usize;
    let variable_ref = c.resolve_variable(tok.unwrap_identifier_name());

    let (get_op, set_op, argument) = match variable_ref {
        VariableRef::Global(global_idx) => (OpCode::GetGlobal, OpCode::SetGlobal, global_idx),
        VariableRef::Local(local_idx) => (OpCode::GetLocal, OpCode::SetLocal, local_idx as u8),
    };

    if can_assign && c.cursor.advance_if(&[TokenType::Equal][..]).is_some() {
        expression(c)?;

        c.current.variable_inst(set_op, argument, line_number);
    } else {
        c.current.variable_inst(get_op, argument, line_number);
    }

    Ok(())
}

/// Attempt to parse a string expression, having observed a string token.
#[tracing::instrument(level = "debug", skip(c))]
pub fn string(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
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

/// Attempt to parse an "and" logical infix expression.
#[tracing::instrument(level = "debug", skip(c))]
pub fn and(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
    let line_number = c.cursor.previous().unwrap().span.line();
    let end_patch = c
        .current
        .jump_inst(OpCode::JumpIfFalse, line_number as usize);

    c.current.simple_inst(OpCode::Pop, line_number as usize);
    parse_precedence(c, Precedence::And)?;

    c.current.complete_patch(end_patch);

    Ok(())
}

/// Attempt to parse an "or" logical infix expression.
#[tracing::instrument(level = "debug", skip(c))]
pub fn or(
    c: &mut Compiler<impl Iterator<Item = Token>>,
    can_assign: bool,
) -> Result<(), CompilerError> {
    let line_number = c.cursor.previous().unwrap().span.line();
    let else_patch = c
        .current
        .jump_inst(OpCode::JumpIfFalse, line_number as usize);
    let end_patch = c.current.jump_inst(OpCode::Jump, line_number as usize);

    c.current.complete_patch(else_patch);
    c.current.simple_inst(OpCode::Pop, line_number as usize);

    parse_precedence(c, Precedence::Or)?;
    c.current.complete_patch(end_patch);

    Ok(())
}

/// Parse the next token, dispatching to a more specific parse rule based on the
/// `TokenType` and the `Precedence` given.
#[tracing::instrument(level = "debug", skip(c))]
pub fn parse_precedence<I>(c: &mut Compiler<I>, precedence: Precedence) -> Result<(), CompilerError>
where
    I: Iterator<Item = Token>,
{
    let tok = match c.cursor.advance() {
        Some(tok) => tok,
        None => {
            tracing::error!("unexpected eof");
            return Err(CompilerError::MissingToken(MissingTokenError {
                msg: "expected any token",
                span: Span::dummy(),
            }));
        },
    };
    tracing::trace!(?tok.r#type, "Finding rule for token type");
    let rule = Precedence::get_rule(tok.r#type);

    let can_assign = precedence <= Precedence::Assignment;
    tracing::trace!(?precedence, "Current expression precedence level");

    (rule
        .prefix_fn_impl
        .unwrap_or_else(|| panic!("missing prefix parse impl for [{:?}]", tok)))(c, can_assign)?;

    while precedence
        <= c.cursor
            .peek()
            .map(|tok| Precedence::get_rule::<I>(tok.r#type).precedence)
            .unwrap_or_else(|| Precedence::None)
    {
        let tok = c.cursor.advance();
        let rule = match tok {
            Some(tok) => Precedence::get_rule::<I>(tok.r#type),
            None => {
                tracing::error!("unexpected eof");
                return Err(CompilerError::MissingToken(MissingTokenError {
                    msg: "expected any token",
                    span: Span::dummy(),
                }));
            },
        };

        match rule.infix_fn_impl {
            Some(parse_impl) => (parse_impl)(c, can_assign)?,
            None => return Ok(()),
        }
    }

    if can_assign && c.cursor.advance_if(&[TokenType::Equal][..]).is_some() {
        return Err(CompilerError::InvalidAssignmentTarget);
    }

    Ok(())
}
