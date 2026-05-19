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
  file.path(input, "ID25278 Pranata - Full Raw Data 15052026.xlsx"),
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
    # Demographics: Age
    age_18_40 = ifelse(`ID11 Usia` <= 40, 1, 0),
    
    # Demographics: Gender (1 = Male)
    male      = ifelse(`ID10 Jenis kelamin respondent` == 1, 1, 0),
    
    # Demographics: Education (High School Graduate or above)
    # Catatan: Pastikan di kuesioner Anda code >= 4 artinya minimal tamat SMA
    hsgrad    = ifelse(`ID14 Tingkat pendidikan` >= 4, 1, 0),
    
    # Demographics: Employment Status Dummies
    working      = ifelse(`ID15 Kegiatan utama sehari-hari` == 1, 1, 0),
    studying     = ifelse(`ID15 Kegiatan utama sehari-hari` == 2, 1, 0),
    housekeeping = ifelse(`ID15 Kegiatan utama sehari-hari` == 3, 1, 0),
    jobseeking   = ifelse(`ID15 Kegiatan utama sehari-hari` == 4, 1, 0),
    unemployed   = ifelse(`ID15 Kegiatan utama sehari-hari` == 5, 1, 0),
    retired      = ifelse(`ID15 Kegiatan utama sehari-hari` == 6, 1, 0),
    
    # Demographics: Household Expenditure Below Median (Sesuai PAP: pakai data Rumah Tangga total)
    low_expenditure = ifelse(`RM02 HH Expenditure` < median(`RM02 HH Expenditure`, na.rm = TRUE), 1, 0),
    
    # Psychological Baseline
    climate_aware           = ifelse(`KEB01 Apakah Anda pernah mendengar istilah perubahan iklim?` == 1, 1, 0),
    energy_transition_aware = ifelse(`KEB04_3 Transisi energi` == 1, 1, 0),
    
    # Area Type
    urban = ifelse(`ID04 Urban / rural` == 1, 1, 0),
    
    # Region Dummies
    region_wib  = ifelse(`ID05 Provinsi` %in% c(1,2,3,4,5,6,7,8,9,10,18), 1, 0),
    region_wita = ifelse(`ID05 Provinsi` %in% c(11,12,13,14,15,16,19,20), 1, 0),
    region_wit  = ifelse(`ID05 Provinsi` %in% c(17,21), 1, 0)
  )

# --- 6. RENAME & TRANSFORM OUTCOMES ---

df <- df %>%
  mutate(
    # EK01: Willingness to Donate (1 = Yes, 0 = No)
    EK01 = ifelse(`EK01 Apakah Anda bersedia menyumbangkan kompensasi survei yang Anda terima kepada New Energy Nexus?` == 1, 1, 0),
    
    # EK02: High Donator Dummy (Nilai >= 3 artinya donasi Rp15.000 ke atas)
    # Sesuai PAP: Jika dari awal EK01 tidak mau donasi (0), otomatis bernilai 0
    EK02 = ifelse(EK01 == 1 & `EK02 Berapa besar nominal yang bersedia Anda donasikan untuk New Energy Nexums?` >= 3, 1, 0),
    
    # EK02a & EK02b: Bersihkan ke biner 0 dan 1 (0 jika dari awal tidak mau donasi)
    EK02a = ifelse(EK01 == 1 & `EK02a Apakah Anda mau mencantumkan nama Anda sebagai salah satu donatur?` == 1, 1, 0),
    EK02b = ifelse(EK01 == 1 & `EK02b Apakah Anda bersedia dihubungi kembali?` == 1, 1, 0),
    
    # EK03: Willingness to Sign Petition (1 = Yes, 0 = No)
    EK03 = ifelse(`EK03 Apakan Anda bersedia menandatangani petisi tersebut?` == 1, 1, 0),
    
    # EK04: Petition Disclosure (Sesuai PAP: Bernilai NA_real_ jika EK03 adalah 0)
    EK04 = ifelse(EK03 == 1, ifelse(`EK04 Apakah Anda mencantumkan identitas Anda pada petisi tersebut?` == 1, 1, 0), NA_real_),
    
    # EK05: Information Seeking (1 = Yes, 0 = No)
    EK05 = ifelse(`EK05 Apakah Anda bersedia mendapatkan informasi lebih lanjut mengenai transisi energi setelah mengikuti survei ini?` == 1, 1, 0)
  ) %>%
  # HAPUS kolom teks asli yang panjang agar tidak duplikat dan mengotori dataset .rds kita
  select(
    -`EK01 Apakah Anda bersedia menyumbangkan kompensasi survei yang Anda terima kepada New Energy Nexus?`,
    -`EK02 Berapa besar nominal yang bersedia Anda donasikan untuk New Energy Nexums?`,
    -`EK02a Apakah Anda mau mencantumkan nama Anda sebagai salah satu donatur?`,
    -`EK02b Apakah Anda bersedia dihubungi kembali?`,
    -`EK03 Apakan Anda bersedia menandatangani petisi tersebut?`,
    -`EK04 Apakah Anda mencantumkan identitas Anda pada petisi tersebut?`,
    -`EK05 Apakah Anda bersedia mendapatkan informasi lebih lanjut mengenai transisi energi setelah mengikuti survei ini?`
  )

# --- 7. SAVE CLEAN DATASET ---
saveRDS(df, file.path(output, "gb_rct_clean_new.rds"))
write_xlsx(df, file.path(output, "gb_rct_clean_new.xlsx"))