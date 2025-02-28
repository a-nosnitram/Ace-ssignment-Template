import os
from pathlib import Path
from string import Template 

# report template 
# to be modified 
REPORT_TEMPLATE = Template("""# ${module_code}-W${week_number}""")

def create_project():
    # get user input using input()
    project_type = input("Project type (Group / Individual) : ").strip().lower()
    week_number = input("Week number : ")
    project_title = input("Enter project title : ")
    language = input("Enter porgramming language : ").strip()

    # report details input 
    module_code = input("Enter module code : ")
    tutor_name = input("Enter tutor name : ")
    matriculation = input("enter matriculation number : ")

    # folder name and base path based on input and folder name 
    folder_name = f"W{week_number}-Assignment-{project_title}"
    base_path = Path(folder_name) 
    
    # create folder structure 
    if project_type == "individual":
        src_path = base_path / "src"
        report_file = base_path / f"W{week_number}-Report.md"
    else:
        src_path = base_path / "Code" / "src"
        report_path = base_path / "Reports" 
        report_path.mkdir(parents=True, exist_ok=True)
        group_report_file = report_path / "Group-Report.md"
        individual_report_file = report_path / "Individual-Report.md"

    # create directories using Path.mkdir 
    src_path.mkdir(parents=True, exist_ok=True)

    # define report content using substitute
    report_contents = REPORT_TEMPLATE.substitute(module_code=module_code, week_number=week_number)

    # write report files 
    report_file.write_text(report_contents)

    print("done.")

if __name__ == "__main__":
    create_project()
