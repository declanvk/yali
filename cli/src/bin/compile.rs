use argh::FromArgs;
use std::{fs, io, path::Path};
use walox::{
    compiler::{self, Compiler},
    scanner::Scanner,
    vm::Heap,
};

#[derive(FromArgs)]
/// Compile lox code in a single pass
struct Args {
    /// script path
    #[argh(positional)]
    file_path: String,

    /// output nicely formatted chunks
    #[argh(switch, short = 'd')]
    dump: bool,
}

fn main() {
    tracing_subscriber::fmt::init();
    let args: Args = argh::from_env();

    if run(&args.file_path, args.dump) {
        panic!(
            "Encounter errors while running [{}].",
            <String as AsRef<Path>>::as_ref(&args.file_path).display()
        )
    }
}

fn run(file_path: impl AsRef<Path>, should_dump: bool) -> bool {
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

    let last_line = compiler
        .cursor
        .previous()
        .map(|prev| prev.span.line())
        .unwrap_or(0);
    compiler.current.return_inst(last_line as usize);

    let chunk = match compiler.current.build() {
        Ok(c) => c,
        Err(e) => {
            tracing::error!(%e);
            return true;
        },
    };

    if should_dump {
        let mut stdout = io::stdout();

        let chunk_name = file_path
            .file_stem()
            .expect("unable to extract file stem")
            .to_string_lossy();

        chunk
            .write_disassembled(&mut stdout, Some(chunk_name.as_ref()))
            .expect("unable to output chunk");
    }

    false
}
