#*******************************************************************************
#  ORWELL NARRATIVE TESTING: DESCRIPTIVE ANALYSIS							   
#*******************************************************************************
##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
#install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, kableExtra, tidyverse)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "elgha") {
  #base_dir <- "G:/" # laptop
  base_dir <- "H:/" # computer
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

##################################### Meta ######################################

# Code for rendering R Markdown of Orwell narrative testing descriptive analysis
# April 24, 2025
# Contributors : Azzah

############################ Code for Rendering Rmd #################################
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

############################ Compiling unadjusted outcome differences #################################
# Store file names
files <- list.files(temp, pattern = "unadjusted")
filelist <- list("binary" = grepv("binary",files),
                 "likert" = grepv("likert",files))

# Read and bind files within each list element
dfs <- map(filelist, function(files) {
  map_dfr(files, ~ read_excel(file.path(temp, .x)))
})

# Change outcome names
rename_outcome <- function(x){

  n <- as.numeric(str_extract(x, "\\d+$"))

  case_when(
    str_detect(x, "^CB05r_cloned") ~ paste0("CB05", letters[pmax(n - 2, 1)]),
    str_detect(x, "^topthree") ~ paste0("CB06", letters[n]),
    str_detect(x, "^agree") ~ paste0("CB07", letters[n]),
    str_detect(x, "^bigrole") ~ paste0("CB11", letters[n]),
    str_detect(x, "^support") ~ paste0("DK", n),
    str_detect(x, "^higheff") ~ paste0("TD", n),

    str_detect(x, "^CB06_Orderr_cloned") ~ paste0("CB06", letters[n]),
    str_detect(x, "^CB07r_cloned") ~ paste0("CB07", letters[n]),
    str_detect(x, "^CB11r_cloned") ~ paste0("CB11", letters[n]),
    str_detect(x, "^QDKr_cloned") ~ paste0("DK", n),
    str_detect(x, "^TD0_cloned") ~ paste0("TD", n),

    TRUE ~ x
  )
}
dfs$binary$outcome <- rename_outcome(dfs$binary$outcome)
dfs$likert$outcome <- rename_outcome(dfs$likert$outcome)