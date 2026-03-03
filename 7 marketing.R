##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Clear and load packages 
# install.packages("pacman") # install the package if you haven't 
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse,data.table,readxl,writexl,fastDummies,hdm,kableExtra,
               jmvReadWrite,miceadds,broom,ivreg,sandwich,lmtest,flextable,officer)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "User") {
  base_dir <- "H:/"
} else if (current_user == "elgha") {
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

ipt = file.path(getwd(),"2a input")
temp = file.path(getwd(),"2b temp")
opt = file.path(getwd(),"2c output")
lg = file.path(getwd(),"3 log")
fig = file.path(getwd(),"4 figures")
tbl = file.path(getwd(),"5 tables")

##### DATA PREPARATION #####
# Set data date
date <- "20250304"

# Load data
datnm <- paste0("processed_",date,".csv") 
data <- file.path(ipt,datnm) 
maindata <- fread(data) %>%
  filter(lfCB != 4) %>%
  mutate(lfCB = ifelse(lfCB == 5, 4, lfCB)) %>%
  mutate(across(starts_with("QDKr"),
                ~ as.integer(.x >= 4),
                .names = "supportDK{substr(.col,5,nchar(.col))}"))

# Outcome treatment set
data <- file.path(ipt,"outcome_treatment_set.csv")
outrset <- fread(data)

# Create a named list where each element is the vector of treatments for that outcome
treatment_sets <- lapply(1:nrow(outrset), function(i) {
  
  row <- outrset[i]
  
  # Which columns have value 1?
  trts <- which(as.numeric(row[, -1]) == 1)
  
  return(trts)
})

# Name the list elements by outcome
names(treatment_sets) <- outrset$Outcome

# Define reusable function
run_ttests <- function(data, outcome, treatments, control = 6) {
  
  results <- data.frame()
  
  for (t in treatments) {
    
    subdata <- subset(data, lfCB %in% c(t, control))
    tt <- t.test(as.formula(paste(outcome, "~ lfCB")), data = subdata)
    
    temp <- data.frame(
      outcome = outcome,
      treatment = t,
      control = control,
      mean_treatment = tt$estimate[1],
      mean_control = tt$estimate[2],
      diff = diff(tt$estimate),
      p_value = tt$p.value,
      ci_lower = tt$conf.int[1],
      ci_upper = tt$conf.int[2]
    )
    
    results <- rbind(results, temp)
  }
  
  return(results)
}
  
# Get list of outcomes
outcomes <- outrset$Outcome

# Automated loop for all
all_results <- do.call(rbind,
                       lapply(outcomes, function(out) {
                         run_ttests(
                           data = maindata,
                           outcome = out,
                           treatments = treatment_sets[[out]],
                           control = 6
                         )
                       })
)

# Export result
write_xlsx(all_results,
           file.path(opt,"marketing orwell.xlsx")) 
