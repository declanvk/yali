//! Virtual machine implementation

mod chunk;
mod op;
mod value;

pub use chunk::{Chunk, ChunkBuilder, ChunkError, ChunkIter};
pub use op::{Instruction, OpCode, TryFromByteError};
use std::io::Write;
pub use value::{ConcreteObject, Heap, Object, ObjectBase, ObjectType, StringObject, Value};

/// The virtual machine that executions `Instructions`
///
/// # Safety
///
/// Due to `Value::Object`s pointing to `Heap` allocated data, it is
/// required that no `Value::Object` is used/read after a call to `VM::clear`.
/// The backing memory will have been deallocated, and a use-after-free will
/// result.
///
/// This is mostly relevant to reads from the the `stack` field, as that is the
/// only current method for `Value`s to escape. Technically this would make use
/// of the `clear` function unsafe, but chosen not to mark it as such
/// because of the unclear boundaries of the safety requirements.
pub struct VM<W: Write> {
    /// The stack of `Value`s.
    pub stack: Vec<Value>,
    /// The currently executing chunk
    pub chunk: Chunk,
    /// The standard out buffer, used to print things to screen
    pub stdout: W,
    /// Instruction pointer
    pub ip: *const u8,
    /// The heap memory region, containing `Value`s separate from the stack.
    pub heap: Heap,
}

impl<W: Write> VM<W> {
    /// Create a new `VM` with the given output and code `Chunk`.
    pub fn new(stdout: W, chunk: Chunk, heap: Heap) -> Self {
        let ip = chunk.first_instruction_pointer();
        VM {
            ip,
            chunk,
            stdout,
            heap,
            stack: Vec::new(),
        }
    }

    /// Reset all the state of the `VM`, deallocating some excess memory.
    ///
    /// # Safety
    ///
    /// See the safety documentation on the `VM` struct.
    pub fn clear(&mut self) {
        self.stack.clear();
        // # Safety
        //
        // 1. `heap.clear`
        //   - by calling `stack.clear` prior to this, all the live copies of `Object`s
        //     will have been removed from circulation
        unsafe { self.heap.clear() };
    }

    /// Safely execute the current `Chunk` to completion.
    pub fn interpret(&mut self) -> Result<(), RuntimeError> {
        // The `validate_instructions` will never return an empty list of errors
        self.chunk.validate_instructions().map_err(|errs| errs[0])?;

        let result = unsafe { self.interpret_inner_unchecked() };

        if result.is_err() {
            // Reset the VM on error
            self.clear();
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
                    (Value::Object(a), Value::Object(b)) => {
                        match (a.read::<StringObject>(), b.read::<StringObject>()) {
                            (Some(a), Some(b)) => {
                                let mut res = String::from(&*a.value);
                                res.push_str(&*b.value);

                                self.heap.allocate_string(res).into()
                            },
                            _ => {
                                return Err(RuntimeError::IncompatibleTypes(format!(
                                    "cannot perform '+' on arguments of {:?} and {:?}",
                                    a, b
                                )));
                            }
                        }
                    };
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
        let mut vm = VM::new(Vec::new(), chunk, Heap::new());

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
        let mut vm = VM::new(Vec::new(), chunk, Heap::new());

        vm.interpret().unwrap();

        assert_eq!(vm.stack.len(), 1);
        assert_eq!(vm.stack[0], true.into());
    }

    #[test]
    fn concatenation_calculation() {
        let mut heap = Heap::new();
        let mut builder = ChunkBuilder::default();

        let s1 = heap.allocate_string("a");
        let s2 = heap.allocate_string("b");
        let s3 = heap.allocate_string("c");

        builder
            .constant_inst(s1, 1)
            .constant_inst(s2, 1)
            .simple_inst(OpCode::Add, 1)
            .constant_inst(s3, 1)
            .simple_inst(OpCode::Add, 1)
            .return_inst(1);
        let chunk = builder.build().unwrap();

        let mut vm = VM::new(Vec::new(), chunk, heap);

        vm.interpret().unwrap();

        assert_eq!(vm.stack.len(), 1);
        assert_eq!(
            vm.stack[0]
                .unwrap_object()
                .read::<StringObject>()
                .unwrap()
                .value
                .as_ref(),
            "abc"
        );
    }
}
