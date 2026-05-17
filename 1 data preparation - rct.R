##### GB RCT (Orwell 2) — DATA PREPARATION #####
# Goal: Clean raw data and create analysis variables
# Author: Elena Novia
# Date: May 2026

graphics.off(); rm(list=ls()); cat("\14")
pacman::p_load(tidyverse, readxl, writexl)

# --- 1. SETUP DIRECTORIES ---
base_dir <- "G:/"
master <- file.path(base_dir, "Shared drives", "Projects", "2026", "Orwell 2", 
                    "Breadcrumbs", "4. Green Barometer", "10 Data RCT")
setwd(master)

input  <- file.path(master, "1a input")
output <- file.path(master, "1c output")
if (!dir.exists(output)) dir.create(output, recursive = TRUE)

# --- 2. LOAD RAW DATA ---
raw <- read_excel(
  file.path(input, "ID25278 Pranata - Raw Data CE 11052026.xlsx"),
  sheet = "Eksperimen"
)

# --- 3. RENAME TREATMENT VARIABLE ---
df <- raw %>%
  rename(treatment = `Video yang diputar`)

# --- 4. CREATE TREATMENT DUMMIES ---
df <- df %>%
  filter(treatment %in% c(1, 2, 3)) %>%
  mutate(
    T1       = ifelse(treatment == 2, 1, 0),
    T2       = ifelse(treatment == 3, 1, 0),
    Pooled_T = ifelse(treatment %in% c(2, 3), 1, 0)
  )

# --- 5. CREATE COVARIATES ---
df <- df %>%
  mutate(
    # Demographics
    age_18_40 = ifelse(`ID11 Usia` <= 40, 1, 0),
    male            = ifelse(`ID10 Jenis kelamin respondent` == 1, 1, 0),
    hsgrad          = ifelse(`ID14 Tingkat pendidikan` >= 4, 1, 0),
    working         = ifelse(`ID15 Kegiatan utama sehari-hari` == 1, 1, 0),
    studying        = ifelse(`ID15 Kegiatan utama sehari-hari` == 2, 1, 0),
    housekeeping    = ifelse(`ID15 Kegiatan utama sehari-hari` == 3, 1, 0),
    jobseeking      = ifelse(`ID15 Kegiatan utama sehari-hari` == 4, 1, 0),
    unemployed      = ifelse(`ID15 Kegiatan utama sehari-hari` == 5, 1, 0),
    retired         = ifelse(`ID15 Kegiatan utama sehari-hari` == 6, 1, 0),
    low_expenditure = ifelse(`RM02 HH Expenditure` < median(`RM02 HH Expenditure`, na.rm = TRUE), 1, 0),
    
    # Psychological baseline
    climate_aware           = ifelse(`KEB01 Apakah Anda pernah mendengar istilah perubahan iklim?` == 1, 1, 0),
    energy_transition_aware = ifelse(`KEB04_3 Transisi energi` == 1, 1, 0),
    
    # Area
    urban = ifelse(`ID04 Urban / rural` == 1, 1, 0),
    
    # Region (WIB = codes 1-10 except Bali/NTT/NTB/Kalimantan Timur)
    region_wib  = ifelse(`ID05 Provinsi` %in% c(1,2,3,4,5,6,7,8,9,10,18), 1, 0),
    region_wita = ifelse(`ID05 Provinsi` %in% c(11,12,13,14,15,16,19,20), 1, 0),
    region_wit  = ifelse(`ID05 Provinsi` %in% c(17,21), 1, 0)
  )



# --- 6. SAVE CLEAN DATASET ---
saveRDS(df, file.path(output, "gb_rct_clean.rds"))
write_xlsx(df, file.path(output, "gb_rct_clean.xlsx"))