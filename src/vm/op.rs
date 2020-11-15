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
    /// Add two `Value`s
    Add,
    /// Subtract two `Value`s
    Subtract,
    /// Multiple two `Value`s
    Multiply,
    /// Divide two `Value`s
    Divide,
    /// Negate a `Value`
    Negate,
    /// Return from the current function
    Return,
}

const OP_CODE_LOOKUP: &[OpCode] = &[
    OpCode::Constant,
    OpCode::Add,
    OpCode::Subtract,
    OpCode::Multiply,
    OpCode::Divide,
    OpCode::Negate,
    OpCode::Return,
];

impl OpCode {
    /// Return true if the `OpCode` represents an arithmetic operation (add,
    /// subtract, multiply, divide, negate).
    pub fn is_arithmetic(&self) -> bool {
        match self {
            OpCode::Constant => false,
            OpCode::Add => true,
            OpCode::Subtract => true,
            OpCode::Multiply => true,
            OpCode::Divide => true,
            OpCode::Negate => true,
            OpCode::Return => false,
        }
    }

    /// Returns the number of bytes of extra information needed to execute the
    /// instruction.
    pub fn arguments_size(&self) -> usize {
        match self {
            OpCode::Constant => 1,
            OpCode::Return => 0,
            OpCode::Add => 0,
            OpCode::Subtract => 0,
            OpCode::Multiply => 0,
            OpCode::Divide => 0,
            OpCode::Negate => 0,
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

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
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
