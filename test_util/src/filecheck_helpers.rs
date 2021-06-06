//! Test helper methods that are specific to filecheck tests.

use anyhow::Context;
use globwalk::FileType;
use std::{
    env, fs, io,
    path::{Component, Path, PathBuf},
};
use threadpool::ThreadPool;

/// Execute a suite of filecheck tests.
pub fn execute_filecheck_tests(
    test_suite_name: &str,
    test_data_patterns: &[&str],
    execute_single_test: fn(String) -> Result<(), anyhow::Error>,
) -> anyhow::Result<()> {
    tracing::debug!("Starting interpreter filecheck tests.");
    let test_data_dir = if let Some(val) = env::var_os("TEST_DATA_DIR") {
        PathBuf::from(val)
    } else {
        let mut workspace_root = super::get_workspace_root()?;
        workspace_root.push("test_data");
        workspace_root
    };

    tracing::debug!("Checking for test name pattern.");
    let test_name_pattern = env::args()
        .nth(1)
        .map(|reg_src| regex::Regex::new(&reg_src).expect("Passed regex was not valid!"));

    assert!(
        test_data_dir.is_dir(),
        "The test data directory should be a directory."
    );

    tracing::debug!(test_data = %test_data_dir.display(), ?test_name_pattern, "Gathering files from test data directory");

    let pool = ThreadPool::with_name("test-runner".into(), num_cpus::get());
    let (tx, rx) = std::sync::mpsc::channel();
    let mut test_count = 0;

    let test_dispatch_span = tracing::debug_span!("test_dispatch");
    for entry in collect_test_files(&test_data_dir, test_data_patterns)? {
        let _guard = test_dispatch_span.enter();

        let entry = entry.context("error retrieving test file entry")?;
        let path = entry.path();

        let mut test_name_components: Vec<_> = path
            .strip_prefix(&test_data_dir)
            .context("unable to strip test data dir prefix")?
            .components()
            .map(|c| match c {
                Component::Normal(c) => c
                    .to_str()
                    .ok_or_else(|| anyhow::anyhow!("Unable to convert file component to unicode")),
                x => Err(anyhow::anyhow!("Non-normal path component: [{:?}]", x)),
            })
            .collect::<Result<_, _>>()?;
        test_name_components.insert(0, test_suite_name);

        let test_name_prefix: String = test_name_components
            .split_last()
            .map(|(_, cs)| cs.join("::"))
            .unwrap_or_default();

        let test_name_suffix: String = path
            .file_stem()
            .ok_or_else(|| anyhow::anyhow!("File lacks name stem"))?
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Unable to convert file stem to unicode"))?
            .into();

        let mut test_name = String::new();
        if !test_name_prefix.is_empty() {
            test_name.push_str(&test_name_prefix);
            test_name.push_str("::");
        }
        test_name.push_str(&test_name_suffix);

        if let Some(ref test_name_pattern) = test_name_pattern {
            if !test_name_pattern.is_match(&test_name) {
                continue;
            }
        }

        let file_content = fs::read_to_string(path)?;

        tracing::trace!(%test_name, "Dispatching test case for execution");

        pool.execute({
            let tx = tx.clone();
            let test_span =
                tracing::trace_span!("test_execution", id = test_count, name = %test_name);

            move || {
                test_span.in_scope(|| {
                    tracing::trace!("Starting test execution");

                    let result = super::Test::execute(&execute_single_test, file_content);

                    tracing::trace!(?result, "Test case finished. Sending result.");

                    tx.send(super::TestOutput {
                        name: test_name,
                        result,
                    })
                    .expect("Failed to send test result");
                });
            }
        });

        test_count += 1;
    }

    tracing::debug!(parent: &test_dispatch_span, %test_count, "Sent all test cases to threadpool, now waiting for tests to complete.");

    pool.join();

    tracing::debug!(parent: &test_dispatch_span, %test_count, "All tests complete. Receiving results.");

    let outputs: Vec<_> = rx.into_iter().take(test_count).collect();

    super::display_test_outputs(&mut io::stdout(), outputs)?;

    Ok(())
}

fn collect_test_files(
    base_dir: &Path,
    test_data_patterns: &[&str],
) -> Result<
    impl Iterator<Item = Result<globwalk::DirEntry, globwalk::WalkError>>,
    globwalk::GlobError,
> {
    globwalk::GlobWalkerBuilder::from_patterns(base_dir, test_data_patterns)
        .file_type(FileType::FILE)
        .build()
}
