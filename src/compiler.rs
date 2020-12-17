//! Utilities for the single-pass compiler

mod parse;
mod precedence;

use crate::{
    scanner::{Cursor, MissingTokenError, ScanError, Token, TokenType},
    vm::{ChunkBuilder, Heap},
};
pub use parse::{binary, expression, grouping, literal, number, parse_precedence, string, unary};
pub use precedence::Precedence;

/// A single-pass compiler into `lox` bytecode.
pub struct Compiler<I: Iterator<Item = Token>> {
    /// The stream of token from the source code.
    pub cursor: Cursor<I>,
    /// The chunk being built.
    pub current: ChunkBuilder,
    /// The heap that contains `Object`s allocated during compilation.
    pub heap: Heap,
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

    fn compile_expression(src: &str) -> (Heap, Chunk) {
        let heap = Heap::new();
        let tokens = Scanner::new(src);
        let mut compiler = Compiler {
            cursor: Cursor::new(tokens),
            current: ChunkBuilder::default(),
            heap,
        };

        expression(&mut compiler).expect("unable to parse expression from tokens");

        compiler.current.return_inst(1);

        let chunk = compiler
            .current
            .build()
            .expect("unable to build compiled chunk");

        (compiler.heap, chunk)
    }

    #[test]
    fn simple_arith_compile() {
        let (_heap, chunk) = compile_expression("10 + 20");
        assert_eq!(&*chunk.constants, &[10.0.into(), 20.0.into()][..]);
        assert_instructions!(chunk => {
            OpCode::Constant, [0];
            OpCode::Constant, [1];
            OpCode::Add;
        });
    }

    #[test]
    fn paren_arith_compile() {
        let (_heap, chunk) = compile_expression("10 * (20 + (30 - 2))");
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
        let (_heap, chunk) = compile_expression("(10.0 < 2) == ((1.2 - 3.2) <= 0)");
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
        let (_heap, chunk) = compile_expression("!(5 - 4 > 3 * 2 == !nil)");
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
        let (_heap, chunk) = compile_expression(r##" "a" + "b" + "c" "##);

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
