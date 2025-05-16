##################################### Meta ######################################

# Code for rendering R Markdown of Voltaire National Survey's descriptive analysis
# April 24, 2025
# Contributors : Azzah

############################ Code for Rendering Rmd #################################

##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
#install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, kableExtra)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "User") {
  base_dir <- "H:/"
} else if (current_user %in% c("azzah", "USER")) {
  base_dir <- "G:/"
}

# Set directory
master <- file.path(base_dir, 
                    "Shared drives", 
                    "Projects", 
                    "2025", 
                    "Orwell", 
                    "Breadcrumbs", 
                    "10 Quantitative Narrative Testing", 
                    "9 Main survey")
setwd(master)

arc = file.path(getwd(), "1 archive")
ipt = file.path(getwd(),"2a input")
temp = file.path(getwd(),"2b temp")
opt = file.path(getwd(),"2c output")
lg = file.path(getwd(),"3 log")
fig = file.path(getwd(),"4 figures")
tbl = file.path(getwd(),"5 tables")


##### RUN R MARKDOWN #####
code = file.path("C:",
                 "Users",
                 current_user,
                 "Documents",
                 "GitHub",
                 "Orwell")

rmdfile = file.path(code,"6 descriptive (paper).Rmd")

# Render the R Markdown file and save the output in the 'tbl' directory
filenm <- paste0("descriptive-paper-",Sys.Date(),".pdf")
render(
  input = rmdfile,
  output_file = filenm,
  output_dir = tbl
)
