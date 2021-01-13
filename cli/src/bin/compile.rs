use std::{env, fs, io, path::Path};
use walox::{
    compiler::{self, Compiler},
    scanner::Scanner,
    vm::Heap,
};

fn main() {
    tracing_subscriber::fmt::init();

    let mut args: Vec<_> = env::args().collect();
    // Remove the cli argument that is just the binary's name.
    args.remove(0);

    if args.len() != 1 {
        eprintln!("Usage: ./interpret [script]");
    } else {
        if run(&args[0]) {
            panic!(
                "Encounter errors while running [{}].",
                <String as AsRef<Path>>::as_ref(&args[0]).display()
            )
        }
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
    let scanner = Scanner::new(file_contents.as_str());
    let heap = Heap::new();
    let mut compiler = Compiler::new(scanner, &heap);

    while !compiler.cursor.is_empty() {
        match compiler::declaration(&mut compiler) {
            Ok(()) => {},
            Err(e) => {
                tracing::error!(%e);
                return true;
            },
        }
    }

    let chunk = match compiler.current.build() {
        Ok(c) => c,
        Err(e) => {
            tracing::error!(%e);
            return true;
        },
    };

    let mut stdout = io::stdout();

    let chunk_name = file_path
        .file_stem()
        .expect("unable to extract file stem")
        .to_string_lossy();

    chunk
        .write_disassembled(&mut stdout, Some(chunk_name.as_ref()))
        .expect("unable to output chunk");

    false
}
