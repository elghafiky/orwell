##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Clear and load packages 
# install.packages("pacman") # install the package if you haven't 
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse,data.table,readxl,fastDummies,hdm,jmvReadWrite)

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
dataname <- paste0("processed_",date,".csv") 
data <- file.path(temp,dataname) 
anldf <- fread(data)

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

basecogctrl <- c('read_stim_time', 'sdbi', 'agreestim')

basecovlist <- c(basechar, basecogctrl)

##### CONJOINT #####
## Prepare chosen profile data 
# Subset profile and pairing data
profpairw <- anldf %>% dplyr::select(record,
                                   starts_with("Profile"),
                                   starts_with("PairingID"))

# Reshape the data from wide to long format
profpairl <- data.table::melt(
  profpairw,
  id.vars = "record",
  measure.vars = patterns(
    profile = "^Profile_\\d+$",
    pairing_id = "^PairingID_\\d+$"
  ),
  variable.name = "task"
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
  id.vars = c("record", "task", "profile_chosen", "pairing_id"),
  measure.vars = c("Profile_A", "Profile_B"),
  variable.name = "alt",
  value.name = "profile_id"
)

# Step 1: Convert 'alt' to numeric
cjdfl[, alt := fifelse(alt == "Profile_A", 1L, 
                       fifelse(alt == "Profile_B", 2L, NA_integer_))]

# Step 2: Create 'alt_chosen' column
cjdfl[, alt_chosen := as.integer(profile_chosen == profile_id)]

## Prepare profile attributes data
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

## Prepare data for estimation: dummy-code all attribute levels (except one per factor)
cjdff <- cjdfm %>%
  mutate(econ = factor(econ), rights = factor(rights),
         env = factor(env), participation = factor(participation),
         task=as.numeric(task)) %>%
  mutate(rights = fct_relevel(rights, "3", "1", "2"), # Reorder the levels of 'rights' so that "3" is the first level
         participation = fct_relevel(participation, "2", "1")) %>% 
  dummy_cols(select_columns=c("econ","rights","env","participation"),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE) %>% # Dummify data
  left_join(dplyr::select(anldf,
                          record,
                          all_of(basecovlist),
                          ConjointOverall_Time,
                          starts_with("treat")),
            by="record",relationship="many-to-one") # Obtain treatment status and covariates

## PD Lasso estimation

##### EXPORT DATA TO JAMOVI #####
# Export analysis data to be imported to omv
dtcsvnm <- paste0("processed_",date,".csv")
datacsv <- file.path(temp,dtcsvnm) 
dtomvnm <- paste0("procs_",date,".omv")
dataomv <- file.path(temp,dtomvnm)

convert_to_omv(
  fleInp = datacsv,
  fleOut = dataomv,
  frcWrt = T
)