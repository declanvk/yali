use argh::FromArgs;
use std::{
    fs,
    io::{self, Write},
    path::Path,
};
use walox::{analysis::AstValidator, interpreter::Interpreter, parser::parse, scanner::Scanner};

#[derive(FromArgs)]
/// Interpret lox code from the AST
struct Args {
    /// script path
    #[argh(positional)]
    file_path: String,
}

fn main() {
    tracing_subscriber::fmt::init();
    let args: Args = argh::from_env();

    if run(&args.file_path) {
        panic!(
            "Encounter errors while running [{}].",
            <String as AsRef<Path>>::as_ref(&args.file_path).display()
        )
    }
}

fn run(file_path: impl AsRef<Path>) -> bool {
    let file_path = file_path.as_ref();
    assert!(
        file_path.exists(),
        "File '{}' does not exist!",
        file_path.display()
    );
    assert!(
        file_path.is_file(),
        "'{}' is not a file!",
        file_path.display()
    );

    let file_contents = fs::read_to_string(file_path).expect("Failed to read file");

    let mut interpreter = Interpreter::new(Box::new(io::stdout()));
    let scanner = Scanner::new(file_contents.as_str());
    let statements = match parse(scanner) {
        Ok(statements) => statements,
        Err(errs) => {
            for e in errs {
                tracing::error!(%e);
            }

            return true;
        },
    };

    // Perform static analysis on the AST to check for misplaced `return`, `this`,
    // etc
    let mut validator = AstValidator::default();
    match validator.validate(&statements) {
        Ok(()) => {},
        Err(err) => {
            tracing::error!(%err);

            return true;
        },
    }

    let result = interpreter.interpret(&statements);

    let _ = interpreter.stdout.flush().expect("failed to flush stdout");

    match result {
        Ok(_) => false,
        Err(e) => {
            tracing::error!(%e);
            true
        },
    }
}
