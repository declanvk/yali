#![deny(missing_docs, clippy::missing_safety_doc)]
#![allow(unused_unsafe)]

//! The `lox` language.

pub mod analysis;
pub mod ast;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod vm;

mod util;
