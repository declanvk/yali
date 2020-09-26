use super::{NativeFunction, Value};
use std::time::SystemTime;

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
