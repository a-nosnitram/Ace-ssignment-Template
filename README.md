# Project-Template
Behold : the ultimate tool for CS students when it comes to creating the same directory structure and report template for EVERY. SINGLE. ASSIGNMENT. I'm sure you're tired of that too. Hopefully. Becaude I just spent an embarrasingly long amout of times automating this, and, quite frankly, I would have saved so much time just doing it manually for the rest of my academic career.But here you go anyway. 

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

The user can aslo choose to not select any of the above, in whichcase no language-specific files are going to be created in the `src/` directory. 

When filling in your project details, you can select to either build a group or individual project. 
### Individual Project Setup
```
|---- src
|      \_____ main.xx # main, language-specific file
|      \_____ runXX.sh # script or Makefile to run your files
|---- W0X-Report.md
```
### Group Project Setup
```
|---- Code
|      \_____ src
|              \_____ main.xx # main, language-specific file
|              \_____ runXX.sh # script or Makefile to run your files
|---- Reports 
|       \____ Individual-Report.md
|       \____ Group-Report.md
```

### Report Formats 
Thsi template supports both .md and .docx repoort formats. 

# Installation 
This package is available on PyPI. Just run
```
pip install assignment-template-creator==1.0.3
```
The, to run it
```
create_template
```
And you're all set ! I hope someone actually uses this. 
