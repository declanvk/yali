use std::{
    env, fs,
    io::{self, BufRead, Write},
    path::Path,
};
use walox::{ast::interpreter::Interpreter, parser::parse, scanner::Scanner};

fn main() {
    tracing_subscriber::fmt::init();

    let mut args: Vec<_> = env::args().collect();
    // Remove the cli argument that is just the binary's name.
    args.remove(0);

    if args.len() > 2 {
        eprintln!("Usage: lox [script]");
    } else if args.len() == 1 {
        run_file(&args[0])
    } else {
        run_prompt()
    }
}

fn run_file(file_path: impl AsRef<Path>) {
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
    let had_errors = run(&mut interpreter, &file_contents);

    if had_errors {
        panic!("Encounter errors while running [{}].", file_path.display())
    }
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut interpreter = Interpreter::new(Box::new(io::stdout()));

    print!("> ");
    io::stdout().flush().unwrap();

    let mut lines = stdin.lock().lines();
    while let Some(Ok(line)) = lines.next() {
        let _ = run(&mut interpreter, &line);

        print!("> ");
        io::stdout().flush().unwrap();
    }
}

fn run(interpreter: &mut Interpreter, source: &str) -> bool {
    let scanner = Scanner::new(source);
    let statements = match parse(scanner) {
        Ok(statements) => statements,
        Err(errs) => {
            for e in errs {
                tracing::error!(%e);
            }

            return true;
        },
    };

    tracing::debug!(?statements);

    match interpreter.interpret(&statements) {
        Ok(_) => false,
        Err(e) => {
            tracing::error!(%e);
            true
        },
    }
}
