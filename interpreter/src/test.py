from main import run


def run_on_file(file_path):
    print(f"\nRunning {file_path}...")

    # Read the code from a file
    with open(file_path, 'r') as file:
        code = file.read()

    run(code)


def run_on_file_fails(file_path):
    try:
        run_on_file(file_path)
        assert False, "test should fail"
    except AssertionError as e:
        raise e
    except Exception as e:
        print(f"Error: {e}")
        pass


if __name__ == '__main__':
    # Positive test runs:
    # run_on_file('../test/positive/1_plus.lang')
    # run_on_file('../test/positive/2_func_no_params.lang')
    # run_on_file('../test/positive/3_returned_func_captures.lang')
    # run_on_file('../test/positive/4_partial_functions.lang')
    # run_on_file('../test/positive/5_recursive_func.lang')
    # run_on_file('../test/positive/6_list_at_index.lang')
    # run_on_file('../test/positive/7_list_reduce.lang')
    # run_on_file('../test/positive/8_list_map.lang')
    # run_on_file('../test/positive/9_print_values.lang')

    # Negative test runs:
    run_on_file_fails('../test/negative/1_define_var_after_func.lang')
    run_on_file_fails('../test/negative/2_syntax_error_middle_of_file.lang')
    run_on_file_fails('../test/negative/3_syntax_error_end_of_file.lang')
    run_on_file_fails('../test/negative/4_syntax_error_bracket_mismatch.lang')
