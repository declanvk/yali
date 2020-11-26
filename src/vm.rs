//! Virtual machine implementation

mod chunk;
mod op;

pub use chunk::{Chunk, ChunkBuilder, ChunkError, ChunkIter};
pub use op::{Instruction, OpCode, TryFromByteError};
use std::io::Write;

/// The value type of the virtual machine
pub type Value = f64;

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

        unsafe { self.interpret_unchecked() }
    }

    /// Execute the current `Chunk` to completion.
    ///
    /// # Safety
    ///
    /// If the `VM` is constructed with a malformed `Chunk` then this function
    /// runs a very real danger of performing out-of-bounds memory accesses
    /// and otherwise dangerous operations.
    pub unsafe fn interpret_unchecked(&mut self) -> Result<(), RuntimeError> {
        macro_rules! binary_op {
            ($op:tt) => {{
                let rhs = self.stack.pop().unwrap();
                let lhs = self.stack.pop().unwrap();

                self.stack.push(lhs $op rhs);
            }}
        }

        loop {
            let (new_ip, inst) = unsafe { chunk::decode_instruction_at_unchecked(self.ip) };
            self.ip = new_ip;

            match inst.op {
                OpCode::Constant => self
                    .stack
                    .push(self.chunk.constants[inst.arguments[0] as usize]),
                OpCode::Add => binary_op!(+),
                OpCode::Subtract => binary_op!(-),
                OpCode::Multiply => binary_op!(*),
                OpCode::Divide => binary_op!(/),
                OpCode::Negate => {
                    let v = self.stack.pop().unwrap();
                    self.stack.push(-v);
                },
                OpCode::Return => {
                    // Just in case `interpret` is called again.
                    self.ip = self.chunk.first_instruction_pointer();
                    return Ok(());
                },
            }
        }
    }
}

/// Errors that can occur during the execution of bytecode.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum RuntimeError {
    /// Error that occured while validating a chunk before execution
    #[error("Validation error: {}", .0)]
    Validation(#[from] ChunkError),
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
            .arithmetic_inst(OpCode::Add, 1)
            .constant_inst(6.0, 1)
            .constant_inst(2.0, 1)
            .arithmetic_inst(OpCode::Divide, 1)
            .arithmetic_inst(OpCode::Multiply, 1)
            .return_inst(1);

        let chunk = builder.build().unwrap();
        let ip = chunk.first_instruction_pointer();
        let mut vm = VM {
            stack: vec![],
            stdout: Vec::new(),
            chunk,
            ip,
        };

        unsafe { vm.interpret_unchecked() }.unwrap();

        assert_eq!(vm.stack.len(), 1);
        assert_eq!(vm.stack[0], 9.0);

        unsafe { vm.interpret_unchecked() }.unwrap();

        assert_eq!(vm.stack.len(), 2);
        assert_eq!(&vm.stack[..2], &[9.0, 9.0]);
    }
}
