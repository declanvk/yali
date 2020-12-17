#!/bin/bash
set -eu
set -o pipefail

get_latest_test_executable() {
    TEST_DIR="$(cargo metadata --format-version 1 | jq -r .target_directory)/debug/deps"
    executable_name=$1

    latest=""
    for file in $(find "$TEST_DIR" -name "$executable_name-*" | grep -E -e "$executable_name-[^\.]*$"); do
        if [[ $file -nt $latest && -x $file ]]; then
            latest=$file
        fi
    done

    [[ -z "$latest" ]] && echo "File not found" || echo "$latest"
}

execute_valgrind() {
    valgrind --error-exitcode=1 \
        --tool=memcheck \
        --leak-check=full \
        --show-leak-kinds=all \
        $(get_latest_test_executable "$1")
}

execute_valgrind "walox"
execute_valgrind "filecheck_interpreter"
