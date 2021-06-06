use io::Write;
use std::io;
use walox::{analysis::AstValidator, interpreter::Interpreter, parser::parse, scanner::Scanner};
use walox_test_util::{
    anyhow, filecheck, filecheck_helpers::create_filecheckers, tracing_subscriber,
};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    walox_test_util::filecheck_helpers::execute_filecheck_tests(
        TEST_SUITE_NAME,
        TEST_DATA_PATTERNS,
        execute_interpreter_filecheck,
    )
}

fn execute_interpreter_filecheck(file_content: String) -> anyhow::Result<()> {
    let stdout_backing = Vec::new();

    // Perform textual analysis of the source code
    let mut scanner = Scanner::new(&file_content);
    let parse_result = parse(scanner.by_ref()).map_err(|errs| {
        errs.into_iter()
            .enumerate()
            .map(|(idx, err)| format!("{}: {}\n", idx, err))
            .collect::<String>()
    });

    let (checker, error_checker) = create_filecheckers(&scanner.comments, TEST_SUITE_NAME);

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

    // Perform static analysis on the AST to check for misplaced `return`, `this`,
    // etc
    let mut validator = AstValidator::default();
    match validator.validate(&statements) {
        Ok(()) => {},
        Err(err) => {
            let (did_match, explanation) =
                error_checker.explain(&err.to_string(), filecheck::NO_VARIABLES)?;

            if !did_match {
                return Err(anyhow::anyhow!(explanation));
            } else if error_checker.is_empty() {
                return Err(anyhow::anyhow!(err));
            } else {
                return Ok(());
            }
        },
    }

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

const TEST_DATA_PATTERNS: &[&str] = &[
    "*.lox",
    "**/*.lox",
    "!benchmark/",
    "!expressions",
    "!limit/",
    "!regression/",
    "!scanning/",
];

const TEST_SUITE_NAME: &str = "interpreter";
