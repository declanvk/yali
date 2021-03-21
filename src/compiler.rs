//! Utilities for the single-pass compiler

mod parse;
mod precedence;

use crate::{
    scanner::{Cursor, MissingTokenError, ScanError, Token, TokenType},
    vm::{Chunk, ChunkBuilder, ChunkError, Heap},
};
pub use parse::{
    binary, declaration, expression, grouping, literal, number, parse_precedence, print_statement,
    statement, string, unary,
};
pub use precedence::Precedence;

/// A single-pass compiler into `lox` bytecode.
pub struct Compiler<'h, I: Iterator<Item = Token>> {
    /// The stream of token from the source code.
    pub cursor: Cursor<I>,
    /// The chunk being built.
    pub current: ChunkBuilder<'h>,
}

impl<'h, I> Compiler<'h, I>
where
    I: Iterator<Item = Token>,
{
    /// Create a new compiler for the given source of tokens.
    pub fn new(tokens: I, heap: &'h Heap) -> Self {
        Compiler {
            cursor: Cursor::new(tokens),
            current: ChunkBuilder::new(heap),
        }
    }
}

type ParseFunc<I> = Option<fn(&mut Compiler<I>) -> Result<(), CompilerError>>;

/// A rule for parsing in the case of a specific `TokenType`.
pub struct ParseRule<I: Iterator<Item = Token>> {
    /// The function that will be used to parse a prefix instance of the
    /// `TokenType`.
    pub prefix_fn_impl: ParseFunc<I>,
    /// The function that will be used to parse an infix instance of the
    /// `TokenType`.
    pub infix_fn_impl: ParseFunc<I>,
    /// The priority of this rule.
    pub precedence: Precedence,
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
    /// An error which occurs because of something in the chunk construction
    /// process.
    #[error("{}", .0)]
    ChunkError(#[from] ChunkError),
}

/// Compile `lox` source
#[tracing::instrument(level = "debug", skip(tokens))]
pub fn compile(
    tokens: impl IntoIterator<Item = Token>,
    heap: &Heap,
) -> Result<Chunk, Vec<CompilerError>> {
    let mut compiler = Compiler::new(tokens.into_iter(), heap);
    let mut errors = Vec::new();

    while !compiler.cursor.is_empty() {
        match declaration(&mut compiler) {
            Ok(()) => {},
            Err(e) => {
                errors.push(e);
            },
        }
    }

    if errors.is_empty() {
        // Emit a return on the last line, as we're only compiling single Chunk right
        // now
        let last_line = compiler
            .cursor
            .previous()
            .map(|prev| prev.span.line())
            .unwrap_or(0);
        compiler.current.return_inst(last_line as usize);

        match compiler.current.build() {
            Ok(c) => Ok(c),
            Err(e) => {
                errors.push(e.into());

                Err(errors)
            },
        }
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        scanner::Scanner,
        vm::{Chunk, OpCode, StringObject},
    };

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

    fn compile_expression(heap: &Heap, src: &str) -> Chunk {
        let tokens = Scanner::new(src);
        let mut compiler = Compiler::new(tokens, &heap);

        expression(&mut compiler).expect("unable to parse expression from tokens");

        compiler.current.return_inst(1);

        let chunk = compiler
            .current
            .build()
            .expect("unable to build compiled chunk");

        chunk
    }

    #[test]
    fn simple_arith_compile() {
        let heap = Heap::new();
        let chunk = compile_expression(&heap, "10 + 20");
        assert_eq!(&*chunk.constants, &[10.0.into(), 20.0.into()][..]);
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Add;
        });
    }

    #[test]
    fn paren_arith_compile() {
        let heap = Heap::new();
        let chunk = compile_expression(&heap, "10 * (20 + (30 - 2))");
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
        let heap = Heap::new();
        let chunk = compile_expression(&heap, "(10.0 < 2) == ((1.2 - 3.2) <= 0)");
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
        let heap = Heap::new();
        let chunk = compile_expression(&heap, "!(5 - 4 > 3 * 2 == !nil)");
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

    #[test]
    fn string_concat_compile() {
        let heap = Heap::new();
        let chunk = compile_expression(&heap, r##" "a" + "b" + "c" "##);

        assert_eq!(chunk.constants.len(), 3);
        assert_eq!(
            chunk.constants[0]
                .unwrap_object()
                .read::<StringObject>()
                .unwrap()
                .value
                .as_ref(),
            "a"
        );
        assert_eq!(
            chunk.constants[1]
                .unwrap_object()
                .read::<StringObject>()
                .unwrap()
                .value
                .as_ref(),
            "b"
        );
        assert_eq!(
            chunk.constants[2]
                .unwrap_object()
                .read::<StringObject>()
                .unwrap()
                .value
                .as_ref(),
            "c"
        );
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Add;
            OpCode::Constant, [2];
            OpCode::Add;
        });
    }
}
