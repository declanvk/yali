use lox_core::{context::Context, scanner::Scanner};
use std::{
    env, fs,
    io::{self, BufRead, Write},
    path::Path,
};

fn main() {
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
    assert!(file_path.exists(), "File '{}' does not exist!");
    assert!(file_path.is_file(), "'{}' is not a file!");

    println!("Reading from file {}", file_path.display());

    let file_contents = fs::read_to_string(file_path).expect("Failed to read file");
    let mut ctx = Context::default();

    run(&mut ctx, &file_contents);

    if ctx.has_errors() {
        ctx.write_errors(&mut io::stdout())
            .expect("Failed to write errors");

        panic!("Encounter errors while running [{}].", file_path.display())
    }
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut ctx = Context::default();

    print!("> ");
    io::stdout().flush().unwrap();

    let mut lines = stdin.lock().lines();
    while let Some(Ok(line)) = lines.next() {
        run(&mut ctx, &line);

        if ctx.has_errors() {
            ctx.write_errors(&mut io::stdout())
                .expect("Failed to write errors");
            ctx.clear_errors()
        }

        print!("\n> ");
        io::stdout().flush().unwrap();
    }
}

fn run(ctx: &mut Context, source: &str) {
    let scanner = Scanner::new(ctx, source);
    let tokens: Vec<_> = scanner.collect();

    for token in tokens {
        println!("{:?}", token);
    }
}
