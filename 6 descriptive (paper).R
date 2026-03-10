#*******************************************************************************
#  ORWELL NARRATIVE TESTING: DESCRIPTIVE ANALYSIS							   
#*******************************************************************************
##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
#install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, kableExtra, tidyverse, kableExtra)

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
    str_detect(x, "CB05r_cloned")        ~ paste0("CB05", letters[n]),
    str_detect(x, "topthree")            ~ paste0("CB06", letters[n]),
    str_detect(x, "agree")               ~ paste0("CB07", letters[n]),
    str_detect(x, "bigrole")             ~ paste0("CB11", letters[n]),
    str_detect(x, "support|QDKr_cloned") ~ paste0("DK", n),
    str_detect(x, "higheff|TD0_cloned")  ~ paste0("TD", n),
    str_detect(x, "CB06_Orderr_cloned")  ~ paste0("CB06", letters[n]),
    str_detect(x, "CB07r_cloned")        ~ paste0("CB07", letters[n]),
    str_detect(x, "CB11r_cloned")        ~ paste0("CB11", letters[n]),
    TRUE ~ x
  )
}
dfs$binary$outcome <- rename_outcome(dfs$binary$outcome)
dfs$likert$outcome <- rename_outcome(dfs$likert$outcome)

# Import outcome label
olables <- read_excel(file.path(ipt,"outcome label.xlsx"))

# Merge labels
dfs$binary <- dfs$binary |>
  left_join(olables, by = c("outcome" = "Outcome"))
dfs$likert <- dfs$likert |>
  left_join(olables, by = c("outcome" = "Outcome"))

# Prepare dataframe for table export
prep_table <- function(df){
  
  df |>
    arrange(Family) |>
    select(
      Family,
      Outcome = Label,
      Control_mean = mean_control,
      T1 = T1_mean_difference,
      T2 = T2_mean_difference,
      T3 = T3_mean_difference,
      T5 = T5_mean_difference
    ) |>
    rename(
      "Control mean" = Control_mean,
      "Fix the Distribution" = T1,
      "No Victimization" = T2,
      "Balanced Development" = T3,
      "Equal Opportunity" = T5
    ) |>
    mutate(across(where(is.numeric), ~round(.x, 3)))
}
binary_tab <- prep_table(dfs$binary)
likert_tab <- prep_table(dfs$likert)

# Table rendering function
make_outcome_table <- function(df, caption){
  
  family_sizes <- df |>
    count(Family)
  
  tab_print <- df |>
    select(-Family)
  
  tab <- kableExtra::kbl(
    tab_print,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    caption = caption
  ) |>
    kableExtra::add_header_above(c(
      " " = 2,
      "Difference to control" = 4
    )) |>
    kableExtra::column_spec(1, width = "7cm") |>
    kableExtra::kable_styling(
      latex_options = "repeat_header"
    )
  
  start <- 1
  
  for(i in seq_len(nrow(family_sizes))){
    
    end <- start + family_sizes$n[i] - 1
    
    tab <- tab |>
      kableExtra::pack_rows(
        family_sizes$Family[i],
        start,
        end,
        indent = FALSE
      )
    
    start <- end + 1
  }
  
  tab
}

# Build the tables
binary_table <- make_outcome_table(
  binary_tab,
  "Unadjusted differences of outcomes in binary indicator format"
)

likert_table <- make_outcome_table(
  likert_tab,
  "Unadjusted differences of outcomes in Likert format"
)