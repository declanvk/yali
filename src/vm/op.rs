use std::{convert::TryFrom, fmt};

/// An `Instruction` is the basic unit of execution in the lox virtual machine.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Instruction<'d> {
    /// The type of instruction
    pub op: OpCode,
    /// Extra data that is necessary to execute the instruction
    pub arguments: &'d [u8],
}

/// Virtual machine instruction type
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum OpCode {
    /// Load a constant onto the stack
    Constant = 0,
    /// Push a constant `Value::Nil` onto the stack.
    Nil,
    /// Push a constant `Value::Bool(true)` onto the stack.
    True,
    /// Push a constant `Value::Bool(false)` onto the stack.
    False,
    /// Compare two `Value`s and return `true` if they are equal.
    Equal,
    /// Compare two `Value::Number` and return `true` if the lhs is greater.
    Greater,
    /// Compare two `Value::Number` and return `true` if the lhs is less.
    Less,
    /// Add two `Value`s
    Add,
    /// Subtract two `Value`s
    Subtract,
    /// Multiple two `Value`s
    Multiply,
    /// Divide two `Value`s
    Divide,
    /// Boolean inverse
    Not,
    /// Numeric inverse
    Negate,
    /// Pop the top value off the stack and write it to stdout
    Print,
    /// Pop the top value off the stack and do nothing with it
    Pop,
    /// Return from the current function
    Return,
    /// Read a global variable's value
    GetGlobal,
    /// Define a global variable with an initial value
    DefineGlobal,
    /// Write a new value to an existing global variable
    SetGlobal,
    /// Read a local variable
    GetLocal,
    /// Set a local variable
    SetLocal,
}

const OP_CODE_LOOKUP: &[OpCode] = &[
    OpCode::Constant,
    OpCode::Nil,
    OpCode::True,
    OpCode::False,
    OpCode::Equal,
    OpCode::Greater,
    OpCode::Less,
    OpCode::Add,
    OpCode::Subtract,
    OpCode::Multiply,
    OpCode::Divide,
    OpCode::Not,
    OpCode::Negate,
    OpCode::Print,
    OpCode::Pop,
    OpCode::Return,
    OpCode::GetGlobal,
    OpCode::DefineGlobal,
    OpCode::SetGlobal,
    OpCode::GetLocal,
    OpCode::SetLocal,
];

impl OpCode {
    /// Returns the number of bytes of extra information needed to execute the
    /// instruction.
    pub fn arguments_size(&self) -> usize {
        match self {
            OpCode::Constant
            | OpCode::DefineGlobal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::GetLocal
            | OpCode::SetLocal => 1,
            OpCode::Return
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Negate
            | OpCode::Not
            | OpCode::True
            | OpCode::False
            | OpCode::Nil
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Print
            | OpCode::Pop
            | OpCode::Less => 0,
        }
    }

    /// Attempt to onvert from a byte representation to the `OpCode` enum.
    ///
    /// # Errors
    /// This function will return an error if the given byte is not a valid
    /// `OpCode`.
    pub fn try_from_byte(byte: u8) -> Result<Self, TryFromByteError> {
        if (byte as usize) >= OP_CODE_LOOKUP.len() {
            Err(TryFromByteError(byte))
        } else {
            Ok(unsafe { Self::try_from_byte_unchecked(byte) })
        }
    }

    /// Convert from a byte representation to the `OpCode` enum.
    ///
    /// # Safety
    /// This byte must represent a valid `OpCode`.
    #[inline]
    pub unsafe fn try_from_byte_unchecked(byte: u8) -> Self {
        *OP_CODE_LOOKUP.get_unchecked(byte as usize)
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            OpCode::Return => "OP_RETURN",
            OpCode::Constant => "OP_CONSTANT",
            OpCode::Add => "OP_ADD",
            OpCode::Subtract => "OP_SUBTRACT",
            OpCode::Multiply => "OP_MULTIPLY",
            OpCode::Divide => "OP_DIVIDE",
            OpCode::Negate => "OP_NEGATE",
            OpCode::Not => "OP_NOT",
            OpCode::True => "OP_TRUE",
            OpCode::False => "OP_FALSE",
            OpCode::Nil => "OP_NIL",
            OpCode::Equal => "OP_EQUAL",
            OpCode::Greater => "OP_GREATER",
            OpCode::Less => "OP_LESS",
            OpCode::Pop => "OP_POP",
            OpCode::Print => "OP_PRINT",
            OpCode::GetGlobal => "OP_GET_GLOBAL",
            OpCode::DefineGlobal => "OP_DEFINE_GLOBAL",
            OpCode::SetGlobal => "OP_SET_GLOBAL",
            OpCode::GetLocal => "OP_GET_LOCAL",
            OpCode::SetLocal => "OP_SET_LOCAL",
        };

        f.pad(s)
    }
}

impl fmt::LowerHex for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <u8 as fmt::LowerHex>::fmt(&(*self as u8), f)
    }
}

impl fmt::UpperHex for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <u8 as fmt::UpperHex>::fmt(&(*self as u8), f)
    }
}

impl fmt::Octal for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <u8 as fmt::Octal>::fmt(&(*self as u8), f)
    }
}

impl fmt::Binary for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <u8 as fmt::Binary>::fmt(&(*self as u8), f)
    }
}

impl TryFrom<u8> for OpCode {
    type Error = TryFromByteError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::try_from_byte(value)
    }
}

impl From<OpCode> for u8 {
    fn from(src: OpCode) -> Self {
        src as u8
    }
}

#[derive(Debug, Copy, Clone, PartialEq, thiserror::Error)]
#[error("Failed to convert unknown byte [{:#X}] to opcode.", .0)]
/// The error type returned when conversion from a byte to an opcode fails.
pub struct TryFromByteError(u8);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_opcode_from_byte() {
        for val in 0..=255u8 {
            match OpCode::try_from(val) {
                Ok(op) => assert_eq!(op as u8, val, "{} does not round trip!", op),
                _ => {},
            }
        }
    }
}
