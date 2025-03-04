import os
import subprocess
import filecmp
import re
from docx import Document

TEST_DIR = "Custom-Tests"
language = "java"
report_extention = "md"
report_file = "../W-Report.md"

COMPILATIONS = {
    "c": "make",
    "cpp": "make",
    "java": "chmod +x runJava.sh",
    "haskell": "chmod +x runHaskell.sh",
    "python": "chmod +x runPython",
    "javascript": "chmod +x runJS.sh"
}

RUNS = {
    "c": "./main",
    "cpp": "./main",
    "java": "./runJava.sh",
    "haskell": "./main",
    "python": "./runPython.sh",
    "javascript": "./runJS.sh"
}


def find_tests():
    test_cases = []
    for file in os.listdir(TEST_DIR):
        if file.endswith(".in"):
            test_name = file[:-3]  # file name without extension
            expected = f"{test_name}.out"
            if expected in os.listdir(TEST_DIR):
                test_cases.append(test_name)
    return test_cases

def compile_code():
    if language in COMPILATIONS:
        COMP = COMPILATIONS.get(language)
        try:
            subprocess.run(COMP, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
        except Exception as error:
            print(f"COMPILATION FAILED: {error}")
            return 1
    else:
        print("This language is not supported. Modify COMPILATIONS and RUNS in run_tests.py.")
        return 1

def run_tests(test):
    input_file_p = os.path.join(TEST_DIR, f"{test}.in")
    output_file_p = os.path.join(TEST_DIR, f"{test}.out")
    actual_output_p = os.path.join(TEST_DIR, f"{test}.act")

    EXECUTABLE = RUNS.get(language)

    try:
        with open(input_file_p, "r") as input_file, open(actual_output_p, "w") as actual_output:
            subprocess.run(EXECUTABLE, shell=True, stdin=input_file, stdout=actual_output, stderr=actual_output, check=False)

        with open(output_file_p, "r") as file1, open(actual_output_p, "r") as file2:
            expected_output = file1.read().strip()
            actual_output = file2.read().strip()

        if expected_output == actual_output:
            return test, expected_output, actual_output, "PASS"
        else:
            return test, expected_output, actual_output, "FAIL"

    except Exception as error:
        print(f"ERROR in subprocess: {error}")
        return test, "ERROR", "ERROR", "FAIL"

def update_md(results):
    if os.path.exists(report_file):
        with open(report_file, "r") as file:
            lines = file.readlines()
    else:
        lines = []

    new_lines = []
    table = False
    for line in lines:
        if line.startswith("| **Test Name** |"):
            table = True
            # add updated table
            new_lines.append("\n| **Test Name** | **Expected Output** | **Actual Output** | **Resut** |\n")
            new_lines.append("|-----------|----------------|--------------|--------|\n")
            for test, expected, actual, result in results:
                new_lines.append(f"| {test} | {expected} | {actual} | {result} |\n")
            continue # skip table header
        elif table and line.strip() == "":
            table = False # end of table
        if not table:
            new_lines.append(line) # keep all the report contents 

    # write back to he report 
    with open(report_file, "w") as file:
        file.writelines(new_lines)

def main():
    test_cases = find_tests()
    if not test_cases:
        print ("ERROR : no test cases found.")
        exit(1)

    compile_code()

    results = [run_tests(test) for test in test_cases]

    update_md(results)


if __name__ == "__main__":
    main()
