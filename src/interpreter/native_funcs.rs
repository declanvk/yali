//! Commonly used functions that the host environment provides to lox programs

use super::{NativeFunction, Value};
use std::time::SystemTime;

/// A native functions which takes no arguments and returns the number of
/// seconds since the UNIX epoch, in seconds
pub fn clock() -> NativeFunction {
    fn clock_function(_: Vec<Value>) -> Value {
        let duration = SystemTime::UNIX_EPOCH
            .elapsed()
            .expect("unable to establish a duration since the epoch");

        // TODO: why isn't there an API to perform this cast easily?
        Value::Number(duration.as_secs() as f64)
    }

    NativeFunction {
        f: clock_function,
        name: "clock",
        arity: 0,
    }
}

/// The default list of native functions
pub const DEFAULTS: &[fn() -> NativeFunction] = &[clock];
