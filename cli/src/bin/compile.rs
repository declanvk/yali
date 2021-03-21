use argh::FromArgs;
use compiler::compile;
use std::{fs, io, path::Path};
use walox::{compiler, scanner::Scanner, vm::Heap};

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
    let chunk = match compile(scanner, &heap) {
        Ok(c) => c,
        Err(e) => {
            for err in e {
                tracing::error!(%err);
            }
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
