import os
from pathlib import Path
from string import Template
# from colorama import Fore, Back, Style
from datetime import datetime

# report template
# to be modified
REPORT_TEMPLATE = Template("""<div align="center">

# ${module_code} W${week_number} Assignment ${report_type}
## ${title}
### Tutor: ${tutor_name}
### Student: ${matriculation}
### Date: ${date}
</div>

---

## **Introduction**
Provide a brief overview of the topic, objectives, and scope of the assignment.

---

## **Design**
Explain your approach, methodology, and design decisions.

---

## **Testing**

### **Implementation Details**
Describe the development and coding process.

### **Testing Strategies**
Explain how you tested your program.

### **Test Results**
| **Test Description**   | **Expected Result**   | **Actual Result**   |
|------------------------|----------------------|----------------------|
| Test 1:                |                      |                      |
| Test 2:                |                      |                      |
| Test 3:                |                      |                      |


---

## **Conclusion**
Summarise key points, pretend you genuinely enjoyed the process of creating this, and talk about potential imporvements you'll never implemnent.

---

### **References** *(if needed)*
Include citations in academic format.
""")

def create_project():
    # get user input using input()
    project_type = input_loop("project type (Group (g) / Individual (i))", {"g", "i"})
    week_number = input("Enter week number : ")
    title = input("Enter project title : ")
    print("This template creator provides basic project setups for the following languages :")
    print("Java, Pyhton, C, C++, Haskell, and JavaScript")
    print("Feel free to skip that part of the setup by pressing ENTER.")
    language = input_loop("porgramming language", {"java", "python", "c",
                                                   "c++", "haskell",
                                                   "javascript", "other"})

    # report details input
    module_code = input("Enter module code : ")
    tutor_name = input("Enter tutor name : ")
    matriculation = input("enter matriculation number : ")

    # folder name and base path based on input and folder name
    folder_name = f"W{week_number}-Assignment-{title}"
    base_path = Path(folder_name)

    # create folder structure
    if project_type == "i": # individual report setup
        src_path = base_path / "src"
        report_file = base_path / f"W{week_number}-Report.md"

        report_type = ""
    else: # group project setup
        src_path = base_path / "Code" / "src"
        report_path = base_path / "Reports"
        report_path.mkdir(parents=True, exist_ok=True)
        report_type_g = "Group Report"

        group_report_file = report_path / "Group-Report.md"
        individual_report_file = report_path / "Individual-Report.md"
        report_type_i = "Individual Report"

    # programming language specifics
    match language:
        case "java":
            main_file = src_path / "Main.java"
            main_file_content = """public class Main {
    public static void main(String args[]) {
        System.out.println("Good luck with your coursework ;)");
    }
}"""
            script_content = """#!/bin/bash
# this is a scrip to run your java project
# add files to the compile and run sections as needed

# compile your files
javac main.java

# run your files
java main"""
            script_file = src_path / "runJava.sh"
        case "c":
            main_file = src_path / "Main.c"
            main_file_content = """#include <stdio.h>

int main(int argc, char **argv) {
    printf("Good luck with your coursework ;)\n");
    return 0;
}"""
            script_content = """CC = GCC
TARGET = main
FLAGS = -Wall -Wextra
OBJS = main.o

$(TARGET): $(OBJS)
    $(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

main.o: main.c
    $(CC) $(CFLAGS) -c main.c

clean:
    rm -f *.o $(TARGET)"""

            script_file = src_path / "Makefile"
        case "haskell":
            main_file = src_path / "Main.hs"
            main_file_content = """main :: IO ()
main = putStrLn "Good luck with your coursework ;)"
            """
            script_file = src_path / "runHaskell.sh"
            script_content = """#!/bin/bash

# this is a scrip to run your Haskell project
# add files to the compile and run sections as needed

# compile yout files
ghc -o main Main.hs

# run your files
./main
            """
        case "javscript":
            main_file = src_path / "main.js"
            main_file_content = """console.log("Good luck with your coursework ;)");"""
            script_file = src_path / "runJS.sh"
            script_content = """#!/bin/bash
# this is a scrip to run your JavaScript project
# add files as needed

# run your files
node main.js
"""
        case "python": 
            main_file = src_path / "main.py" 
            main_file_content = """if __name__ == "__main__": 
    print("Good luck with your coursework ;)")"""
            script_file = src_path / "run_python.sh"
            script_content = """#!/bin/bash
# this is a scrip to run your JavaScript project
# add files as needed

# run your files
python3 main.py
"""
        case "c++": 
            main_file = src_path / "main.cpp"
            main_file_content = """#include <iosteram>
int main() {
    std::cout << "Good luck with your coursework ;)" << std::endl;
    return 0; 
}
"""
            script_file = src_path / "Makefile"
            script_content - """CXX=g++
TARGET=main
CXXFLAGS = -Wall -Wextra
OBJS = main.o

$(TARGET): $(OBJS)
    $(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

main.o: main.cpp
    $(CXX) $(CXXFLAGS) -c main.cpp

clean:
    rm -f *.o $(TARGET) 

"""

    # create directories using Path.mkdir
    src_path.mkdir(parents=True, exist_ok=True)

    # configure project files
    if language != "other":
        main_file.write_text(main_file_content)
        script_file.write_text(script_content)

    # define report content using substitute
    if project_type == "i":
        report_contents = REPORT_TEMPLATE.substitute(module_code=module_code,week_number=week_number, report_type=report_type, tutor_name=tutor_name, title=title, matriculation=matriculation, date={datetime.today().strftime('%d %m %Y')})
        # write report files
        report_file.write_text(report_contents)
    else:
        report_g_contents = REPORT_TEMPLATE.substitute(module_code=module_code,week_number=week_number,report_type=report_type_g, tutor_name=tutor_name, title=title, matriculation=matriculation, date={datetime.today().strftime('%d %m %Y')})
        group_report_file.write_text(report_g_contents)

        report_i_contents = REPORT_TEMPLATE.substitute(module_code=module_code,week_number=week_number, report_type=report_type_i,tutor_name=tutor_name, title=title, matriculation=matriculation, date={datetime.today().strftime('%d %m %Y')})
        individual_report_file.write_text(report_i_contents)

    print("All done :з project created !")

def input_loop(prompt, target_set):
    inp = input(f"Enter {prompt} : ").strip().lower()
    while inp not in target_set:
        print("Invalid input")
        inp = input(f"Enter {prompt} : ").strip().lower()
    return inp


if __name__ == "__main__":
    create_project()
