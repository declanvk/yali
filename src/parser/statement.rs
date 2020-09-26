use super::{expression, Cursor, ParseError};
use crate::{
    ast::{
        BlockStatement, Expr, ExprStatement, FunctionDeclaration, IfStatement, LiteralExpr,
        PrintStatement, ReturnStatement, Statement, VarDeclaration, WhileStatement,
    },
    scanner::{self, Token, TokenType},
    span::Span,
};
use std::sync::Arc;

// NOTE: Declaration vs statement
// It seems (based on the Crafting Interpreters book), that declarations are
// simply statements that introduce new names into the set of variables,
// functions, or classes.

/// Parse a declaration or fall through to a normal statement
pub fn declaration(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Statement, ParseError> {
    if let Some(var_token) = c.advance_if(&[TokenType::Var][..]) {
        var_declaration(c, var_token)
    } else if let Some(fun_token) = c.advance_if(&[TokenType::Fun][..]) {
        function_declaration(c, fun_token)
    } else {
        statement(c)
    }
}

/// Parse a variable declaraction
pub fn var_declaration(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    var_token: Token,
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

    let initializer = if c.advance_if(&[TokenType::Equal][..]).is_some() {
        Some(expression(c)?)
    } else {
        None
    };

    let semi = c.consume(TokenType::Semicolon, "expected ';' after declaration")?;

    Ok(Statement {
        span: Span::envelop([&var_token.span, &semi.span].iter().copied()),
        kind: VarDeclaration { name, initializer }.into(),
    })
}

/// Parse a statement
pub fn statement(c: &mut Cursor<impl Iterator<Item = Token>>) -> Result<Statement, ParseError> {
    if c.advance_if(&[TokenType::Print][..]).is_some() {
        print_statement(c)
    } else if let Some(return_token) = c.advance_if(&[TokenType::Return][..]) {
        return_statement(c, return_token)
    } else if let Some(open_brace) = c.advance_if(&[TokenType::LeftBrace][..]) {
        let (statements, close_brace) = block(c)?;

        Ok(Statement {
            span: Span::envelop(
                statements
                    .iter()
                    .map(|stmnt| &stmnt.span)
                    .chain([&open_brace.span, &close_brace.span].iter().copied()),
            ),
            kind: BlockStatement {
                statements: statements.into_iter().map(Arc::new).collect(),
            }
            .into(),
        })
    } else if let Some(if_token) = c.advance_if(&[TokenType::If][..]) {
        if_statement(c, if_token)
    } else if let Some(while_token) = c.advance_if(&[TokenType::While][..]) {
        while_statement(c, while_token)
    } else if let Some(for_token) = c.advance_if(&[TokenType::For][..]) {
        for_statement(c, for_token)
    } else {
        expr_statement(c)
    }
}

/// Parse a print statement
pub fn print_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<Statement, ParseError> {
    let expr = expression(c)?;
    let semi = c.consume(TokenType::Semicolon, "expected ';' after value")?;

    Ok(Statement {
        span: Span::envelop([&expr.span, &semi.span].iter().copied()),
        kind: PrintStatement { expr }.into(),
    })
}

fn return_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    return_token: Token,
) -> Result<Statement, ParseError> {
    let value = if !c.check(TokenType::Semicolon) {
        Some(expression(c)?)
    } else {
        None
    };
    let semi = c.consume(TokenType::Semicolon, "expected ';' after return value")?;

    Ok(Statement {
        span: Span::envelop([&return_token.span, &semi.span].iter().copied()),
        kind: ReturnStatement { value }.into(),
    })
}

/// Parse an expression statement
pub fn expr_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<Statement, ParseError> {
    let expr = expression(c)?;
    let semi = c.consume(TokenType::Semicolon, "expected ';' after expression")?;

    Ok(Statement {
        span: Span::envelop([&expr.span, &semi.span].iter().copied()),
        kind: ExprStatement { expr }.into(),
    })
}

/// Parse a block of statements
pub fn block(
    c: &mut Cursor<impl Iterator<Item = Token>>,
) -> Result<(Vec<Statement>, Token), ParseError> {
    let mut statements = Vec::new();

    while !c.check(TokenType::RightBrace) {
        statements.push(declaration(c)?)
    }

    let close_brace = c.consume(TokenType::RightBrace, "expected '}' after block")?;

    Ok((statements, close_brace))
}

/// Parse an if statement
pub fn if_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    if_token: Token,
) -> Result<Statement, ParseError> {
    let _ = c.consume(TokenType::LeftParen, "expected '(' after 'if'")?;
    let condition = expression(c)?;
    let _ = c.consume(TokenType::RightParen, "expected ')' after if condition")?;

    let then_branch = statement(c)?;
    let else_branch = if c.advance_if(&[TokenType::Else][..]).is_some() {
        Some(statement(c)?)
    } else {
        None
    };

    Ok(Statement {
        span: Span::envelop(
            [
                &if_token.span,
                &then_branch.span,
                &else_branch
                    .as_ref()
                    .map(|stmnt| stmnt.span.clone())
                    .unwrap_or_else(Span::dummy),
            ]
            .iter()
            .copied(),
        ),
        kind: IfStatement {
            condition,
            then_branch: Arc::new(then_branch),
            else_branch: else_branch.map(Arc::new),
        }
        .into(),
    })
}

/// Parse a while statement
pub fn while_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    while_token: Token,
) -> Result<Statement, ParseError> {
    let _ = c.consume(TokenType::LeftParen, "expected '(' after 'while'")?;
    let condition = expression(c)?;
    let _ = c.consume(TokenType::RightParen, "expected '(' after 'while'")?;
    let body = statement(c)?;

    Ok(Statement {
        span: Span::envelop([&while_token.span, &body.span].iter().copied()),
        kind: WhileStatement {
            condition,
            body: Arc::new(body),
        }
        .into(),
    })
}

/// Parse a for statement
pub fn for_statement(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    for_token: Token,
) -> Result<Statement, ParseError> {
    let _ = c.consume(TokenType::LeftParen, "expected '(' after 'for'")?;

    let initializer = if c.advance_if(&[TokenType::Semicolon][..]).is_some() {
        None
    } else if let Some(var_token) = c.advance_if(&[TokenType::Var][..]) {
        Some(var_declaration(c, var_token)?)
    } else {
        Some(expr_statement(c)?)
    };

    let condition = if let Some(Token {
        r#type: TokenType::Semicolon,
        span,
        ..
    }) = c.peek()
    {
        Expr {
            span: Span::new(span.line(), span.range().start..span.range().start),
            kind: LiteralExpr::Boolean(true).into(),
        }
    } else {
        expression(c)?
    };

    let _ = c.consume(TokenType::Semicolon, "expected ';' after condition")?;
    let increment = if c.check(TokenType::Semicolon) {
        None
    } else {
        Some(expression(c)?)
    };

    let _ = c.consume(TokenType::RightParen, "expected ')' after for clauses")?;
    let body = statement(c)?;

    // Desugar for loop elements into while loop inside of blocks
    let for_span = Span::envelop([&for_token.span, &body.span].iter().copied());

    let while_body = if let Some(increment) = increment {
        let increment_stmnt = Statement {
            span: for_span.clone(),
            kind: ExprStatement { expr: increment }.into(),
        };

        Statement {
            span: for_span.clone(),
            kind: BlockStatement {
                statements: vec![Arc::new(body), Arc::new(increment_stmnt)],
            }
            .into(),
        }
    } else {
        body
    };

    let while_stmnt = Statement {
        span: for_span.clone(),
        kind: WhileStatement {
            condition,
            body: Arc::new(while_body),
        }
        .into(),
    };

    let outer_stmnt = if let Some(initializer) = initializer {
        Statement {
            span: for_span,
            kind: BlockStatement {
                statements: vec![Arc::new(initializer), Arc::new(while_stmnt)],
            }
            .into(),
        }
    } else {
        while_stmnt
    };

    Ok(outer_stmnt)
}

/// Parse a function declaraction
pub fn function_declaration(
    c: &mut Cursor<impl Iterator<Item = Token>>,
    fun_token: Token,
) -> Result<Statement, ParseError> {
    let name = c.consume(TokenType::Identifier, "expected function name")?;
    let _ = c.consume(TokenType::LeftParen, "expected '(' after function name")?;

    let mut parameters = Vec::new();
    if !c.check(TokenType::RightParen) {
        loop {
            tracing::info!("another parameter");

            let param_name_token = c.consume(TokenType::Identifier, "expected parameter name")?;
            let _ = param_name_token
                .literal
                .as_ref()
                .ok_or(ParseError::MissingLiteral)?;

            parameters.push(param_name_token.unwrap_identifier_name());

            if c.advance_if(&[TokenType::Comma][..]).is_none() {
                break;
            }
        }
    }
    let _ = c.consume(TokenType::RightParen, "expected ')' after parameters")?;

    let _ = c.consume(TokenType::LeftBrace, "expected '{' before function body")?;
    let (body, close_body_paren) = block(c)?;

    Ok(Statement {
        span: Span::envelop([&fun_token.span, &close_body_paren.span].iter().copied()),
        kind: FunctionDeclaration {
            parameters,
            name: name.unwrap_identifier_name(),
            body: body.into_iter().map(Arc::new).collect(),
        }
        .into(),
    })
}
