from pathlib import Path
import subprocess
import getpass

# Dynamically get the username and construct the path
username = getpass.getuser()  # Get the current username

# Determine the base directory based on the username
if username == "User":
    basedir = Path("H:")
elif username in ["azzah", "elgha"]:
    basedir = Path("G:")
else:
    basedir = None  # Default case if username doesn't match

# Ensure basedir is set
if basedir is None:
    raise ValueError(f"Base directory not set for user '{username}'.")

# Change working directory
masterpath = basedir / "Shared drives" / "Projects" / "2025" / "Orwell" / "Breadcrumbs" / "10 Quantitative Narrative Testing" / "9 Main survey"

# Define the subdirectories
ipt = masterpath / "2a input"
temp = masterpath / "2b temp"
opt = masterpath / "2c output"
lg = masterpath / "3 log"
fig = masterpath / "4 figures"
tbl = masterpath / "5 tables"

# Use glob to find all .tex files in the specified directory
tex_files = tbl.glob("*.tex")

# Loop through the specified files
for tex_file in tex_files:
    input_file = tex_file  # Full path to the input file
    output_file = tex_file.with_suffix(".docx")  # Replace .tex with .docx

    # Command to convert .tex to .docx using Pandoc
    command = ["pandoc", str(input_file), "-o", str(output_file)]

    try:
        # Run the Pandoc command
        subprocess.run(command, check=True)
        print(f"Converted {input_file.name} to {output_file.name}")
    except subprocess.CalledProcessError as e:
        print(f"Error converting {input_file.name}: {e}")
    except FileNotFoundError:
        print(f"File not found: {input_file}")
