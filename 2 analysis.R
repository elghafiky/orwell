##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
# install.packages("pacman") # install the package if you haven't 
pacman::p_load(tidyverse,data.table,broom,stringr,readxl,purrr,furrr,
               jmv,jmvcore,jmvReadWrite,logitr,fastDummies)

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
dataname <- paste0("raw_",date,".csv") 
data <- file.path(ipt,dataname) 
rdf <- fread(data)
pdf <- rdf 

# Prepare variables

# Overall treatment status
pdf$treat <- pdf$lfCB %in% 1:5

# separate treatment indicator
for (i in 1:6) {
  # Create a new variable name, e.g., treat1, treat2, ..., treat6
  new_var <- paste0("treat", i)
  
  # Assign TRUE/FALSE based on the condition lfCB == i
  pdf[, (new_var) := (lfCB == i)]
}

# Define conditions and create 'region' column
pdf <- pdf %>%
  mutate(region = case_when(
    (ID01 >= 1 & ID01 <= 16) | ID01 %in% c(20, 21) ~ 1,
    (ID01 >= 17 & ID01 <= 19) | (ID01 >= 22 & ID01 <= 30) ~ 2,
    TRUE ~ 3
  ))

levels <- unique(pdf$region)
for (rgn in levels) {
  newvar=paste0("region",rgn)
  pdf[,newvar] <- pdf[,"region"] == rgn
}

# Create 'urban', 'male', and 'unmarried' columns
pdf <- pdf %>%
  mutate (
    urban = (ID02 == 1),
    male = (ID03 == 1),
    age = ID04,
    unmarried = (ID05 == 1),
    hhhead_female = (RT01 == 2),
    nosocast = (RT02 == 3),
    hhsize = RT03r1 + RT03r2 + RT03r3
  )

# Create binary columns for education levels
for (edu in 1:5) {
  newvar <- paste0("edu",edu)
  pdf[,newvar] <- pdf[,"ID06"] == edu
}

# Correct message interpretation
variables_ABC_E <- paste0("CB01", c("A", "B", "C", "E"))  # Exclude D for now

pdf <- pdf %>%
  mutate(across(all_of(variables_ABC_E), ~ . == 1, .names = "correct{col}"))

pdf <- pdf %>%
  mutate(correctCB01D = between(CB01D, 2, 3))

pdf <- pdf %>%
  mutate(across(starts_with("correctCB01"), ~ if_else(lfCB == 6, 1, .)))

pdf <- pdf %>%
  rowwise() %>%
  mutate(crt_intrpt_msg = max(c_across(starts_with("correctCB01")), na.rm = TRUE)) %>%
  ungroup()

pdf <- pdf %>%
  # Compute row-wise mean for pagetimestim variables
  rowwise() %>%
  mutate(read_stim_time = mean(c_across(starts_with("pagetimestim")), na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Recode 1 ↔ 2 for selected SBr variables
  mutate(across(all_of(paste0("SBr", c(5, 7, 9, 10, 13))), ~ recode(., `1` = 2, `2` = 1))) %>%
  
  # Compute row-wise sum for SBr variables
  rowwise() %>%
  mutate(sdbi = sum(c_across(starts_with("SBr")), na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Replace outliers in hhsize
  mutate(hhsize = if_else(hhsize > 12, NA_real_, hhsize))

# Prepare the outcome variables
setDT(pdf)  # Convert to data.table
for (i in 1:13) {
  newvar <- paste0("support", i)
  varname <- paste0("QDKr", i)
  pdf[, (newvar) := get(varname) %in% c(4, 5)]
  
  newvar <- paste0("QDKr_cloned",i)
  pdf[,newvar] <- pdf[,..varname]
}

# Function to apply NA replacement rules to specified variable groups
apply_na_rules <- function(dt, var_prefixes) {
  
  # Find all matching columns for given prefixes
  cols <- grep(paste0("^(", paste(var_prefixes, collapse = "|"), ")\\d+$"), names(dt), value = TRUE)
  
  # Define replacement rules as a list of conditions
  rules <- list(
    list(cond = dt$lfCB %in% 3:4, cols = paste0(var_prefixes, 1)),
    list(cond = dt$lfCB %in% c(1, 4, 5), cols = paste0(var_prefixes, c(2, 3, 9, 12, 13))),
    list(cond = dt$lfCB == 5, cols = paste0(var_prefixes, 4)),
    list(cond = dt$lfCB %in% 4:5, cols = paste0(var_prefixes, 5)),
    list(cond = dt$lfCB %in% 2:4, cols = paste0(var_prefixes, c(6, 7, 10))),
    list(cond = dt$lfCB %in% 3:5, cols = paste0(var_prefixes, c(8, 11)))
  )
  
  # Apply rules efficiently
  for (rule in rules) {
    cols_to_modify <- intersect(rule$cols, cols)  # Ensure only existing columns are modified
    if (length(cols_to_modify) > 0) {
      dt[rule$cond, (cols_to_modify) := lapply(.SD, function(x) replace(x, TRUE, NA)), .SDcols = cols_to_modify]
    }
  }
}

# Apply function to "QDKr_cloned" and "support"
apply_na_rules(pdf, c("QDKr_cloned", "support"))

# List of variables to convert
vars_to_convert <- c('region1', 'region2', 'region3', 'urban', 'male', 
                     'edu1', 'edu2', 'edu3', 'edu4', 'edu5', 
                     'hhhead_female', 'nosocast')

# Convert logical to numeric
pdf <- pdf %>%
  mutate(across(all_of(vars_to_convert), as.numeric))

# Agreeing to stimulus
pdf <- pdf %>%
  mutate(agreestim = (lfCB == 6) | 
           (between(CB04, 4, 6) & (between(lfCB, 1, 3) | lfCB == 5)) | 
           (between(CB04, 1, 3) & lfCB == 4))

# Export analysis data to be imported to omv
dataname <- paste0("procs_",date,".csv") 
data <- file.path(temp,dataname) 
fwrite(pdf,data)

dtcsvnm <- paste0("procs_",date,".csv")
datacsv <- file.path(temp,dtcsvnm) 
dtomvnm <- paste0("procs_",date,".omv")
dataomv <- file.path(temp,dtomvnm)

convert_to_omv(
  fleInp = datacsv,
  fleOut = dataomv,
  frcWrt = T
)

# Define potential controls variables
basechar <- c('region1', 
              'region2',
              'region3',
              'urban',
              'male',
              'age',
              'edu1',
              'edu2',
              'edu3',
              'edu4',
              'edu5',
              'hhhead_female',
              'nosocast',
              'hhsize')

cogctrl <- c('pagetimeQDK', 'read_stim_time', 'sdbi')

fullcov <- c(basechar, cogctrl)

##### MANCOVA #####
# Define the function to perform MANCOVA
perform_mancova <- function(treatment, dep_vars, covariates, data) {
  
  # Perform MANCOVA
  result <- mancova(
    data = data,
    deps = unlist(dep_vars),
    factors = unlist(treatment),
    covs = unlist(covariates)
  )
  
  return(result)
}

# List of dependent variables for each treatment group
dependent_vars <- list(
  treat1 = c("QDKr_cloned1", "QDKr_cloned4", "QDKr_cloned5", "QDKr_cloned6",
             "QDKr_cloned7", "QDKr_cloned8", "QDKr_cloned10", "QDKr_cloned11"),
  treat2 = c("QDKr_cloned1", "QDKr_cloned2", "QDKr_cloned3", "QDKr_cloned4",
             "QDKr_cloned5", "QDKr_cloned8", "QDKr_cloned9", "QDKr_cloned11",
             "QDKr_cloned12", "QDKr_cloned13"),
  treat3 = c("QDKr_cloned2", "QDKr_cloned3", "QDKr_cloned4", "QDKr_cloned5",
             "QDKr_cloned9", "QDKr_cloned12", "QDKr_cloned13"),
  treat5 = c("QDKr_cloned1", "QDKr_cloned6", "QDKr_cloned7", "QDKr_cloned10")
)

# Covariates
covariates <- c(cogctrl,
                'region2',
                'region3',
                'urban',
                'male',
                'age',
                'edu3',
                'edu4',
                'edu5',
                'hhhead_female',
                'nosocast',
                'hhsize')

# Initialize a list to store MANCOVA results
mancova_results <- list()

# Iterate over each treatment group and perform MANCOVA
for (treatment in names(dependent_vars)) {
  dep_vars <- dependent_vars[[treatment]]
  result <- perform_mancova(treatment, dep_vars, covariates, filter(pdf,crt_intrpt_msg==1))
  mancova_results[[treatment]] <- result
}

##### CONJOINT #####
## Prepare chosen profile data 
# Subset profile and pairing data
profpairw <- pdf %>% dplyr::select(uuid,starts_with("Profile"),starts_with("PairingID"))

# Reshape the data from wide to long format
profpairl <- melt(
  profpairw,
  id.vars = "uuid",
  measure.vars = patterns(
    profile = "^Profile_\\d+$",
    pairing_id = "^PairingID_\\d+$"
  ),
  variable.name = "task",
  na.rm = TRUE
)

# Import pairing data
filenm = file.path(ipt,"profile_pairings.xlsx")
pairings <- read_excel(filenm)

# Merge profile and pairing with pairing data
cjdfw <- left_join(profpairl,pairings,by=join_by(pairing_id==Pairing_ID),relationship="many-to-one") %>%
  rename(profile_chosen=profile)

# Melt the data on Profile_A and Profile_B
cjdfl <- melt(
  cjdfw,
  id.vars = c("uuid", "task", "profile_chosen", "pairing_id"),
  measure.vars = c("Profile_A", "Profile_B"),
  variable.name = "alt",
  value.name = "profile_id"
)

# Step 1: Convert 'alt' to numeric
cjdfl[, alt := fifelse(alt == "Profile_A", 1L, 
                       fifelse(alt == "Profile_B", 2L, NA_integer_))]

# Step 2: Create 'alt_chosen' column
cjdfl[, alt_chosen := as.integer(profile_chosen == profile_id)]

# Drop profile_chosen and pairing_id
cjdfl <- cjdfl %>% dplyr::select(!(c(profile_chosen,pairing_id)))

## Prepare profile attributes dta
# Create profile data frame
filenm <- file.path(ipt,"sampled_profiles.xlsx")
profiles <- read_excel(filenm) %>% 
  rename(econ=improvement_of_economic_conditions,
         rights=rights_of_others,
         env=environmental_preservation,
         participation=citizen_participation) %>%
  mutate(econ=case_when(
    econ=="The income of the wealthy has increased the most, while the income of others has only increased modestly." ~ "1",
    econ=="The income of the poor or near-poor has increased the most, while the income of those in the middle and upper classes has only increased modestly." ~ "2",
    econ=="All layers of society have experienced a modest increase in income, but no one has become poorer." ~ "3"
  ),
  rights=case_when(
    rights=="To build something that will benefit many people in the future, no group should be displaced or lose their livelihood, even if there is adequate compensation." ~ "1",
    rights=="To build something that will benefit many people in the future, residents should not only receive adequate compensation but also financial benefits derived from what is built." ~ "2",
    rights=="To build something that will benefit many people in the future, it is reasonable if some residents are forced to be displaced or lose their livelihood, as long as there is adequate compensation." ~ "3"
  ),
  env=case_when(
    env=="Environmental destruction must not occur at all, even if it is to build something that will benefit many people in the future." ~ "1",
    env=="Environmental destruction, to a certain extent, can be accepted, as long as it is to build something that will benefit many people in the future." ~ "2"
  ),
  participation=case_when(
    participation=="Residents feel comfortable and free to actively provide input, ask questions, or express complaints to the government, so that development programs can proceed more carefully." ~ "1",
    participation=="After voting in elections, residents trust and give the government freedom to carry out development programs, so that these programs can proceed more smoothly and quickly." ~ "2"
  )) %>%
  mutate(across(everything(),as.numeric))

## Merge chosen profile data with profile attributes data
cjdfm <- full_join(cjdfl,profiles,by=join_by(profile_id==profile_number),relationship="many-to-one")

## Prepare data for logitr: dummy-code all attribute levels (except one per factor)
data_logitr <- cjdfm %>%
  mutate(econ = factor(econ), rights = factor(rights),
         env = factor(env), participation = factor(participation),
         respID = as.integer(factor(uuid)), task=as.numeric(task)) %>%
  mutate(rights = fct_relevel(rights, "3", "1", "2")) %>% # Reorder the levels of 'rights' so that "3" is the first level
  dummy_cols(select_columns=c("econ","rights","env","participation"),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE) %>% # Dummify data
  mutate(obsID = (respID - 1) * 10 + task) %>% # Create a unique observation ID for each choice task
  arrange(respID, obsID, alt)

## Split data into a list by respondent for individual estimation
data_logitr_nested <- data_logitr %>% group_by(respID) %>% nest()

### Multinomial logit
## Pooled MNL
mnl_pooled <- logitr(data = data_logitr,
                     outcome = "alt_chosen",
                     obsID = "obsID",
                     panelID = "respID",
                     pars = c("econ_2","econ_3",
                               "rights_1","rights_2",
                               "env_2","participation_2"),
                     robust = T)

## Individual MNL
# Fitting individual model
plan(multisession)  # or multicore for parallel on Unix
data_mnl <- data_logitr_nested %>% 
  mutate(model = future_map(data, ~ logitr(
    data    = .x,
    outcome = "alt_chosen", 
    obsID = "obsID",
    pars = c("econ_2","econ_3",
             "rights_1","rights_2",
             "env_2","participation_2"),
    robust = T
  )))
 
### Mixed logit 
## Pooled mixed logit model
mixed_model <- logitr(data = data_logitr, 
                      outcome = "alt_chosen", 
                      obsID = "obsID",
                      panelID = "respID",
                      pars = c("econ_2","econ_3",
                               "rights_1","rights_2",
                               "env_2","participation_2"),
                      randPars = c(econ_2="n", econ_3="n",
                                   rights_1="n", rights_2="n",
                                   env_2="n",participation_2="n"),
                      numMultiStarts = 5, 
                      drawType = "sobol",
                      numDraws = 100,
                      robust = T)
summary(mixed_model)

## Individual mixed logit model
# Fitting individual model
plan(multisession)  # or multicore for parallel on Unix
data_mxl <- data_logitr_nested %>% 
  mutate(model = future_map(data, ~ logitr(
    data    = .x,
    outcome = "alt_chosen", 
    obsID = "obsID",
    pars = c("econ_2","econ_3",
             "rights_1","rights_2",
             "env_2","participation_2"),
    randPars = c(econ_2="n", econ_3="n",
                 rights_1="n", rights_2="n",
                 env_2="n",participation_2="n"),
    numMultiStarts = 5, 
    numDraws = 100,
    drawType = "sobol",
    numCores = 1,
    robust = T
  )))