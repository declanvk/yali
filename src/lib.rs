#![deny(missing_docs)]

//! The `lox` language.

pub mod analysis;
pub mod ast;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod span;

mod util;
