#![deny(missing_docs)]

//! The `lox` language.

pub mod ast;
pub mod context;
pub mod parser;
pub mod scanner;
pub mod span;

mod util;
