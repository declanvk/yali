#![warn(missing_docs)]
//! Test utilities

use ansi_term::Colour::{Green, Red};
pub use anyhow;
use backtrace::Backtrace;
pub use filecheck;
pub use globwalk;
pub use num_cpus;
use std::{
    any::Any,
    env,
    fmt::Display,
    io,
    ops::Deref,
    panic::{catch_unwind, UnwindSafe},
    path::PathBuf,
};
pub use threadpool;
pub use tracing_subscriber;

/// Ascending from the current directory, find the workspace root of this
/// project
pub fn get_workspace_root() -> anyhow::Result<PathBuf> {
    let metadata = cargo_metadata::MetadataCommand::new().exec()?;

    Ok(metadata.workspace_root)
}

/// Take an `Any` trait object and attempt to cast it to some form of string.
pub fn get_panic_message(panic: &(dyn Any + Send)) -> Option<&str> {
    panic
        // Try to convert it to a String, then turn that into a str
        .downcast_ref::<String>()
        .map(String::as_str)
        // If that fails, try to turn it into a &'static str
        .or_else(|| panic.downcast_ref::<&'static str>().map(Deref::deref))
}

/// Print a list of `TestOutput`s and a summary.
pub fn display_test_outputs(
    writer: &mut dyn io::Write,
    outputs: impl IntoIterator<Item = TestOutput>,
) -> io::Result<()> {
    const FAILED_STR: &str = "FAILED";
    const PASSED_STR: &str = "ok";
    let trace_is_enabled = env::var_os("RUST_BACKTRACE").is_some();

    let mut passed = 0;
    let mut failed = 0;
    let mut max_name_len = 0;

    let mut outputs: Vec<_> = outputs
        .into_iter()
        .inspect(|output| {
            max_name_len = core::cmp::max(max_name_len, output.name.len());
        })
        .collect();
    outputs.sort_unstable_by(|a, b| a.name.cmp(&b.name));

    writeln!(writer, "\nrunning {} tests", outputs.len())?;

    for output in outputs {
        let spacer: String = core::iter::repeat(' ')
            .take(max_name_len - output.name.len())
            .collect();
        let test_result = match output.result {
            TestResult::Ok => {
                passed += 1;
                Green.paint(PASSED_STR)
            },
            TestResult::Error { .. } => {
                failed += 1;
                Red.paint(FAILED_STR)
            },
        };

        writeln!(writer, "{}{} ... {}", output.name, spacer, test_result)?;

        match output.result {
            TestResult::Error { reason, trace } => {
                writeln!(writer, "{}", reason)?;

                if let Some(mut trace) = trace {
                    if trace_is_enabled {
                        writeln!(writer, "========== {} backtrace ==========", output.name)?;

                        trace.resolve();

                        writeln!(writer, "{:?}", trace)?;
                    }
                }
            },
            _ => {},
        }
    }

    let overall_result = if failed > 0 {
        Red.paint(FAILED_STR)
    } else {
        Green.paint(PASSED_STR)
    };

    //test result: ok. 0 passed; 0 failed;
    writeln!(
        writer,
        "\ntest result: {}. {} passed; {} failed;\n",
        overall_result, passed, failed
    )
}

/// The result of a named test execution
#[derive(Debug, Clone)]
pub struct TestOutput {
    /// The name of the test instance
    pub name: String,
    /// The outcome of the test instance
    pub result: TestResult,
}

/// The outcome of a test.
#[derive(Debug, Clone)]
pub enum TestResult {
    /// The test completed successfully
    Ok,
    /// The test failed with a specific reason
    Error {
        /// The reason the test failed
        reason: String,
        /// An optional backtrace from the point of failure
        trace: Option<Backtrace>,
    },
}

/// A family of tests
pub trait Test {
    /// The arguments, if any, to the tests
    type Args;

    /// Run the test and return the result.
    fn execute(&self, args: Self::Args) -> TestResult;
}

impl<A, E> Test for fn(A) -> Result<(), E>
where
    A: UnwindSafe,
    E: Display,
{
    type Args = A;

    fn execute(&self, args: Self::Args) -> TestResult {
        let res = catch_unwind(|| (*self)(args));

        match res {
            Ok(Ok(())) => TestResult::Ok,
            Ok(Err(e)) => TestResult::Error {
                reason: e.to_string(),
                trace: None,
            },
            Err(e) => TestResult::Error {
                reason: get_panic_message(&e)
                    .map(String::from)
                    .unwrap_or_else(|| "Panic!".into()),
                trace: Some(Backtrace::new_unresolved()),
            },
        }
    }
}

impl<A, B, E> Test for fn(A, B) -> Result<(), E>
where
    A: UnwindSafe,
    B: UnwindSafe,
    E: Display,
{
    type Args = (A, B);

    fn execute(&self, args: Self::Args) -> TestResult {
        let res = catch_unwind(|| (*self)(args.0, args.1));

        match res {
            Ok(Ok(())) => TestResult::Ok,
            Ok(Err(e)) => TestResult::Error {
                reason: e.to_string(),
                trace: None,
            },
            Err(e) => TestResult::Error {
                reason: get_panic_message(&e)
                    .map(String::from)
                    .unwrap_or_else(|| "Panic!".into()),
                trace: Some(Backtrace::new_unresolved()),
            },
        }
    }
}

impl<A> Test for fn(A) -> ()
where
    A: UnwindSafe,
{
    type Args = A;

    fn execute(&self, args: Self::Args) -> TestResult {
        let res = catch_unwind(|| (*self)(args));

        match res {
            Ok(()) => TestResult::Ok,
            Err(e) => TestResult::Error {
                reason: get_panic_message(&e)
                    .map(String::from)
                    .unwrap_or_else(|| "Panic!".into()),
                trace: Some(Backtrace::new_unresolved()),
            },
        }
    }
}

impl<A, B> Test for fn(A, B) -> ()
where
    A: UnwindSafe,
    B: UnwindSafe,
{
    type Args = (A, B);

    fn execute(&self, args: Self::Args) -> TestResult {
        let res = catch_unwind(|| (*self)(args.0, args.1));

        match res {
            Ok(()) => TestResult::Ok,
            Err(e) => TestResult::Error {
                reason: get_panic_message(&e)
                    .map(String::from)
                    .unwrap_or_else(|| "Panic!".into()),
                trace: Some(Backtrace::new_unresolved()),
            },
        }
    }
}
