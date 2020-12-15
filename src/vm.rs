//! Virtual machine implementation

mod chunk;
mod op;

pub use chunk::{Chunk, ChunkBuilder, ChunkError, ChunkIter};
pub use op::{Instruction, OpCode, TryFromByteError};
use std::{fmt, io::Write};

/// The value type of the virtual machine
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    /// Numeric value: `12300`, `-1.23004`, etc
    Number(f64),
    /// `nil`
    Nil,
    /// `true` or `false`
    Bool(bool),
}

impl Value {
    /// Returns `true` if this `Value` is `false` or `false` equivalent.
    pub fn is_falsey(&self) -> bool {
        // return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
        match self {
            Value::Nil => true,
            Value::Bool(b) => !*b,
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(src: f64) -> Self {
        Value::Number(src)
    }
}

impl From<bool> for Value {
    fn from(src: bool) -> Self {
        Value::Bool(src)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => <f64 as fmt::Display>::fmt(n, f),
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => <bool as fmt::Display>::fmt(b, f),
        }
    }
}

/// The virtual machine that executions `Instructions`
pub struct VM<W: Write> {
    /// The stack of values
    pub stack: Vec<Value>,
    /// The currently executing chunk
    pub chunk: Chunk,
    /// The standard out buffer, used to print things to screen
    pub stdout: W,
    /// Instruction pointer
    pub ip: *const u8,
}

impl<W: Write> VM<W> {
    /// Safely execute the current `Chunk` to completion.
    pub fn interpret(&mut self) -> Result<(), RuntimeError> {
        // The `validate_instructions` will never return an empty list of errors
        self.chunk.validate_instructions().map_err(|errs| errs[0])?;

        let result = unsafe { self.interpret_inner_unchecked() };

        if result.is_err() {
            // Reset the stack on error
            self.stack.clear();
        }

        result
    }

    /// Execute the current `Chunk` to completion.
    ///
    /// # Safety
    ///
    /// If the `VM` is constructed with a malformed `Chunk` then this function
    /// runs a very real danger of performing out-of-bounds memory accesses
    /// and otherwise dangerous operations.
    unsafe fn interpret_inner_unchecked(&mut self) -> Result<(), RuntimeError> {
        macro_rules! binary_op {
            ($($p:pat => $e:expr;)+) => {{
                let rhs = self.stack.pop().unwrap();
                let lhs = self.stack.pop().unwrap();

                let output = match (lhs, rhs) {
                    $(
                        $p => $e,
                    )+
                    (a, b) => {
                        return Err(RuntimeError::IncompatibleTypes(format!(
                            "cannot perform '{}' on arguments of {:?} and {:?}",
                            stringify!($op),
                            a,
                            b
                        )))
                    },
                };

                self.stack.push(output);
            }};
        }

        loop {
            let (new_ip, inst) = unsafe { chunk::decode_instruction_at_unchecked(self.ip) };
            self.ip = new_ip;

            match inst.op {
                OpCode::Constant => self
                    .stack
                    .push(self.chunk.constants[inst.arguments[0] as usize]),
                OpCode::Add => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a + b);
                },
                OpCode::Subtract => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a - b);
                },
                OpCode::Multiply => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a * b);
                },
                OpCode::Divide => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a / b);
                },
                OpCode::Negate => {
                    let v = self.stack.pop().unwrap();
                    if let Value::Number(n) = v {
                        self.stack.push((-n).into());
                    } else {
                        return Err(RuntimeError::IncompatibleTypes(format!(
                            "cannot perform '-' on arguments of {:?}",
                            v
                        )));
                    }
                },
                OpCode::Not => {
                    let v = self.stack.pop().unwrap();
                    self.stack.push(v.is_falsey().into());
                },
                OpCode::Return => {
                    // Just in case `interpret` is called again.
                    self.ip = self.chunk.first_instruction_pointer();
                    return Ok(());
                },
                OpCode::True => self.stack.push(true.into()),
                OpCode::False => self.stack.push(false.into()),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Equal => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();

                    self.stack.push((rhs == lhs).into());
                },
                OpCode::Greater => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a > b);
                },
                OpCode::Less => binary_op! {
                    (Value::Number(a), Value::Number(b)) => Value::from(a < b);
                },
            }
        }
    }
}

/// Errors that can occur during the execution of bytecode.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum RuntimeError {
    /// Error that occured while validating a chunk before execution
    #[error("validation error: {}", .0)]
    Validation(#[from] ChunkError),
    /// Attempted an operation with incompatible types
    #[error("incompatible types: {}", .0)]
    IncompatibleTypes(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_calculations_run_twice() {
        let mut builder = ChunkBuilder::default();
        builder
            .constant_inst(1.0, 1)
            .constant_inst(2.0, 1)
            .simple_inst(OpCode::Add, 1)
            .constant_inst(6.0, 1)
            .constant_inst(2.0, 1)
            .simple_inst(OpCode::Divide, 1)
            .simple_inst(OpCode::Multiply, 1)
            .return_inst(1);

        let chunk = builder.build().unwrap();
        let ip = chunk.first_instruction_pointer();
        let mut vm = VM {
            stack: vec![],
            stdout: Vec::new(),
            chunk,
            ip,
        };

        vm.interpret().unwrap();

        assert_eq!(vm.stack.len(), 1);
        assert_eq!(vm.stack[0], 9.0.into());

        vm.interpret().unwrap();

        assert_eq!(vm.stack.len(), 2);
        assert_eq!(&vm.stack[..2], &[9.0.into(), 9.0.into()]);
    }

    #[test]
    fn comparison_calculation() {
        let mut builder = ChunkBuilder::default();
        builder
            .constant_inst(5.0, 1)
            .constant_inst(4.0, 1)
            .simple_inst(OpCode::Subtract, 1)
            .constant_inst(3.0, 1)
            .constant_inst(2.0, 1)
            .simple_inst(OpCode::Multiply, 1)
            .simple_inst(OpCode::Greater, 1)
            .simple_inst(OpCode::Nil, 1)
            .simple_inst(OpCode::Not, 1)
            .simple_inst(OpCode::Equal, 1)
            .simple_inst(OpCode::Not, 1)
            .return_inst(1);

        let chunk = builder.build().unwrap();
        let ip = chunk.first_instruction_pointer();
        let mut vm = VM {
            stack: vec![],
            stdout: Vec::new(),
            chunk,
            ip,
        };

        vm.interpret().unwrap();

        assert_eq!(vm.stack.len(), 1);
        assert_eq!(vm.stack[0], true.into());
    }
}
