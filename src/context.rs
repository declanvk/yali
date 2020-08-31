//! Global state and helpers for the `lox` implementation.

use std::{error, fmt, io};

/// The global state for a run of the `lox` system.
#[derive(Debug)]
pub struct Context {
    errors: Vec<ErrorReport>,
}

impl Default for Context {
    fn default() -> Self {
        Context { errors: Vec::new() }
    }
}

impl Context {
    /// Return `true` if there were errors reported.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Remove all existing error reports
    pub fn clear_errors(&mut self) {
        self.errors.clear()
    }

    /// Output errors to the given `Write` sink.
    pub fn write_errors(&self, out: &mut impl io::Write) -> Result<(), Box<dyn error::Error>> {
        for err in &self.errors {
            writeln!(out, "{}", err)?
        }

        Ok(())
    }

    /// Report an error on the given line.
    pub fn error(&mut self, line: u32, message: impl Into<String>) {
        self.report(line, "", message)
    }

    /// Report an error with a line, condition, and message.
    pub fn report(&mut self, line: u32, condition: impl Into<String>, message: impl Into<String>) {
        self.errors
            .push(ErrorReport::report(line, condition, message))
    }

    /// Directly add a new `ErrorReport` to the stack
    pub fn report_direct(&mut self, report: ErrorReport) {
        self.errors.push(report);
    }
}

/// An instance of an error
#[derive(Debug)]
pub struct ErrorReport {
    line: u32,
    condition: String,
    message: String,
}

impl ErrorReport {
    /// Create an error on the given line.
    pub fn error(line: u32, message: impl Into<String>) -> Self {
        Self::report(line, "", message)
    }

    /// Create a report with the given line, condition, and message.
    pub fn report(line: u32, condition: impl Into<String>, message: impl Into<String>) -> Self {
        ErrorReport {
            line,
            condition: condition.into(),
            message: message.into(),
        }
    }
}

impl error::Error for ErrorReport {}

impl fmt::Display for ErrorReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[Line {}] Error{}: {}",
            self.line, self.condition, self.message
        )
    }
}
