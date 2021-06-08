use io::Write;
use std::io;
use walox::{
    compiler::compile,
    scanner::Scanner,
    vm::{Heap, VM},
};
use walox_test_util::{
    anyhow, filecheck, filecheck_helpers::create_filecheckers, tracing_subscriber,
};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    walox_test_util::filecheck_helpers::execute_filecheck_tests(
        TEST_SUITE_NAME,
        TEST_DATA_PATTERNS,
        execute_vm_filecheck,
    )
}

fn execute_vm_filecheck(file_content: String) -> anyhow::Result<()> {
    let stdout_backing = Vec::new();

    // Perform textual analysis of the source code
    let mut scanner = Scanner::new(&file_content);
    let mut heap = Heap::new();
    let chunk_result = compile(&mut scanner, &heap).map_err(|errs| {
        errs.into_iter()
            .enumerate()
            .map(|(idx, err)| format!("{}: {}\n", idx, err))
            .collect::<String>()
    });

    let (checker, error_checker) = create_filecheckers(&scanner.comments, TEST_SUITE_NAME);

    let chunk = match chunk_result {
        Ok(chunk) => chunk,
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

    let mut vm = VM::new(stdout_backing, chunk, &mut heap);
    let interp_result = vm.interpret();

    let _ = vm.stdout.flush().expect("failed to flush stdout");
    let stdout_output = String::from_utf8(vm.stdout).unwrap();

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
    "!unexpected_character.lox",
    "!assignment/to_this.lox",
    "!benchmark/",
    "!block/empty.lox",
    "!call/",
    "!class/",
    "!closure/",
    "!constructor/",
    "!expressions/",
    "!field/",
    "!for/",
    "!function/",
    "!if/class_in_else.lox",
    "!if/class_in_then.lox",
    "!if/dangling_else.lox",
    "!if/fun_in_else.lox",
    "!if/fun_in_then.lox",
    "!if/var_in_else.lox",
    "!if/var_in_then.lox",
    "!inheritance/",
    "!limit/",
    "!method/",
    "!number/decimal_point_at_eof.lox",
    "!number/leading_dot.lox",
    "!number/trailing_dot.lox",
    "!operator/equals_class.lox",
    "!operator/equals_method.lox",
    "!operator/not.lox",
    "!operator/not_class.lox",
    "!print/missing_argument.lox",
    "!regression/",
    "!return/",
    "!scanning/",
    "!string/unterminated.lox",
    "!super/",
    "!this/",
    "!variable/",
    "!while/",
];

const TEST_SUITE_NAME: &str = "vm";
