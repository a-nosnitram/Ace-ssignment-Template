# Project-Template
Behold: the ultimate tool for CS students when it comes to creating the same directory structure and report template for EVERY. SINGLE. ASSIGNMENT. I'm sure you're tired of that, too. Hopefully. Because I just spent an embarrassingly long amount of time automating this, and, quite frankly, I would have saved so much time just doing it manually for the rest of my academic career. But here you go anyway. 

# Features 

Now, onto the fun stuff. 

### Language Support
This template builder supports the following languages : 
- Java 
- Python 
- C++
- C 
- JavaScript 
- Haskell

The user can also choose not to select any of the above, in which case no language-specific files will be created in the `src/` directory. 

You can build a group or individual project when filling in your project details. 
### Individual Project Setup
```bash
|---- src
|      \_____ main.xx # main, language-specific file
|      \_____ runXX.sh # script or Makefile to run your files
|      \_____ run_tests.py # optional testing file (see Testing Options)
|      \_____ Custom-Tests # optional testing folder (see Testing Options)
|               \_____ test1.in # sample test files 
|               \_____ test1.out
|               \_____ test2.in
|               \_____ test2.out
|---- W0X-Report.md
```
### Group Project Setup
```bash
|---- Code
|      \_____ src
|              \_____ main.xx # main, language-specific file
|              \_____ runXX.sh # script or Makefile to run your files
|              \_____ run_tests.py # optional testing file (see Testing Options)
|              \_____ Custom-Tests # optional testing folder (see Testing Options)
|                       \_____ test1.in # sample test files 
|                       \_____ test1.out
|                       \_____ test2.in
|                       \_____ test2.out
|---- Reports 
|       \____ Individual-Report.md
|       \____ Group-Report.md
```

### Report Formats 
This template supports both .md and .docx report formats.

### Testing Options
The user can choose to select one of the two testing options : 

1. Automatic Testing Table Generation : The testing table in the report will be populated automatically by test names, expected outputs, and actual outputs, extracted from the Custom-Tests directory, which the user can fill with their own `.in` and `.out` files. The testing table can be updated by running `python3 run_tests.py`
2. Custom stacscheck Output : Running `python3 run_tests.py` will print out a stacscheck-style output based on the contents of your Custom-Tests folder.

The user can also skip this step, in which case no `Custom-Tests` directory and `run_tests.py` file will be generated. If the user does not select a language prior to selecting one of the testing options, the script will fail and need to be modified to compile and run the project's language. 

# Installation 
This package is available on PyPI. Just run
```
pip install assignment-template-creator==1.0.5
```
Then, to run it
```
create_template
```
And you're all set ! I hope someone actually uses this.

## Troubleshooting 
The above works seamlessly on Alma Linux (the lab machines), either directly or via SSH. However, some issues may arise when trying to run `python3 run_tests.py` (in case you have selected one of the testing options). You might get the following error : 
```bash
File "path/to/your/assignment/src/run_tests.py", line 5, in <module>
    from docx import Document
ModuleNotFoundError: No module named 'docx'
```
Try installing the `python-docx` package explicitly :
```bash
pip install python-docx
```
If you're still getting the error, it's likely caused by a mismatch between Python versions. In that case, try installing the package in a virtual environment : 
```bash
python3 -m venv venv 
source venv/bin/activate  # Mac/Linux
venv\Scripts\activate      # Windows
pip install python-docx
```
Then redo the [Installation](#installation) part.  

# Credits
Special thanks to **[Andrew](https://github.com/ThatOtherAndrew)** for the idea behind the automatic test table generation and to **[Yehor](https://github.com/YehorBoiar)** and **[Fedor](https://github.com/DrPepper1337)** for suffering through part of the testing process with me.  
