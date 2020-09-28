use io::Write;
use std::{env, fs, io, path::PathBuf};
use walox::{ast::interpreter::Interpreter, parser::parse, scanner::Scanner};
use walox_test_util::{
    anyhow, filecheck, filecheck::CheckerBuilder, get_workspace_root, num_cpus,
    threadpool::ThreadPool, Test, TestOutput,
};

fn main() -> anyhow::Result<()> {
    let test_data_dir = if let Some(val) = env::var_os("TEST_DATA_DIR") {
        PathBuf::from(val)
    } else {
        let mut workspace_root = get_workspace_root()?;
        workspace_root.push("test_data");
        workspace_root
    };

    assert!(
        test_data_dir.is_dir(),
        "The test data directory should be a directory."
    );

    let pool = ThreadPool::new(num_cpus::get());
    let (tx, rx) = std::sync::mpsc::channel();
    let mut test_count = 0;

    for entry in fs::read_dir(test_data_dir)? {
        let entry = entry?;
        let filetype = entry.file_type()?;
        let path = entry.path();

        if !filetype.is_file() {
            continue;
        }

        test_count += 1;

        let test_name: String = path
            .file_stem()
            .ok_or_else(|| anyhow::anyhow!("File lacks name stem"))?
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Unable to convert file stem to unicode"))?
            .into();
        let file_content = fs::read_to_string(path)?;

        pool.execute({
            let tx = tx.clone();
            move || {
                let result = Test::execute(
                    &(execute_interpreter_filecheck as fn(String) -> Result<(), anyhow::Error>),
                    file_content,
                );

                tx.send(TestOutput {
                    name: test_name,
                    result,
                })
                .expect("Failed to send test result");
            }
        });
    }

    pool.join();

    let outputs: Vec<_> = rx.into_iter().take(test_count).collect();

    walox_test_util::display_test_outputs(&mut io::stdout(), outputs)?;

    Ok(())
}

fn execute_interpreter_filecheck(file_content: String) -> anyhow::Result<()> {
    let stdout_backing = Vec::new();
    let mut checker_builder = CheckerBuilder::new();
    let mut stderr_checker_builder = CheckerBuilder::new();

    // Perform textual analysis of the source code
    let mut scanner = Scanner::new(&file_content);
    let parse_result = parse(scanner.by_ref()).map_err(|errs| {
        errs.into_iter()
            .enumerate()
            .map(|(idx, err)| format!("{}: {}\n", idx, err))
            .collect::<String>()
    });

    for comment in scanner.comments {
        if comment.starts_with("+error") {
            let comment = &comment["+error".len()..];
            let _ = stderr_checker_builder.directive(comment).unwrap();
        } else {
            let _ = checker_builder.directive(comment).unwrap();
        }
    }

    let error_checker = stderr_checker_builder.finish();
    let checker = checker_builder.finish();

    let statements = match parse_result {
        Ok(statements) => statements,
        Err(stderr) => {
            let (did_match, explanation) =
                error_checker.explain(&stderr, filecheck::NO_VARIABLES)?;

            if !did_match {
                return Err(anyhow::anyhow!(explanation));
            } else if error_checker.is_empty() {
                return Err(anyhow::anyhow!(stderr));
            } else {
                return Ok(());
            }
        },
    };

    let mut interpreter = Interpreter::new(stdout_backing);
    let interp_result = interpreter.interpret(&statements);

    let _ = interpreter.stdout.flush().expect("failed to flush stdout");
    let stdout_output = String::from_utf8(interpreter.stdout).unwrap();

    let (is_ok, explanation) = match interp_result {
        Ok(_) => checker.explain(&stdout_output, filecheck::NO_VARIABLES)?,
        Err(err) => {
            let (is_ok, explanation) =
                error_checker.explain(&format!("{}", err), filecheck::NO_VARIABLES)?;

            if is_ok && error_checker.is_empty() {
                return Err(anyhow::anyhow!(err));
            }

            (is_ok, explanation)
        },
    };

    if is_ok {
        Ok(())
    } else {
        Err(anyhow::anyhow!("{}", explanation))
    }
}
