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

# --- 6. RENAME & TRANSFORM OUTCOMES (STRICTLY COMPLIANT WITH PAP) ---
# Langkah A
df <- df %>%
  rename(
    raw_EK01 = `EK01 Apakah Anda bersedia menyumbangkan kompensasi survei yang Anda terima kepada New Energy Nexus?`,
    raw_EK02 = `EK02 Berapa besar nominal yang bersedia Anda donasikan untuk New Energy Nexus?`,
    raw_EK03 = `EK03 Apakan Anda bersedia menandatangani petisi tersebut?`,
    raw_EK05 = `EK05 Apakah Anda bersedia mendapatkan informasi lebih lanjut mengenai transisi energi setelah mengikuti survei ini?`,
    raw_EK02a = `EK02a Apakah Anda mau mencantumkan nama Anda sebagai salah satu donatur?`,
    raw_EK02b = `EK02b Apakah Anda bersedia dihubungi kembali untuk dikirimkan bukti donasi?`,
    raw_EK04  = `EK04 Apakah Anda mencantumkan identitas Anda pada petisi tersebut?`
  )

# Langkah B: Eksekusi Transformasi 
df <- df %>%
  mutate(
    # EK01: Willingness to Donate (1 = Yes, 0 = No) -> dari raw 1 (willing) dan 2 (not willing)
    EK01 = ifelse(raw_EK01 == 1, 1, 0),
    
    # EK02: High Donator Dummy (1 if >= Rp15.000, yang artinya pilihan 3, 4, 5, atau 6)
    # Jika dari awal tidak mau donasi (raw_EK01 == 2), otomatis diberi nilai 0
    EK02 = ifelse(raw_EK01 == 1 & raw_EK02 >= 3, 1, 0),
    
    # EK02a & EK02b: Tetap kita bersihkan ke 0/1 untuk kelengkapan data (0 jika tidak berdonasi)
    EK02a = ifelse(raw_EK01 == 1 & raw_EK02a == 1, 1, 0),
    EK02b = ifelse(raw_EK01 == 1 & raw_EK02b == 1, 1, 0),
    
    # EK03: Willingness to Sign Petition (1 = Yes, 0 = No)
    EK03 = ifelse(raw_EK03 == 1, 1, 0),
    
    # EK04: Petition Disclosure Dummy
    # PAP: 1 jika mau disclose, 0 jika anonim, NYATAKAN NA jika tidak mau tanda tangan petisi
    EK04 = ifelse(raw_EK03 == 1, ifelse(raw_EK04 == 1, 1, 0), NA_real_),
    
    # EK05: Information Seeking (1 = Yes, 0 = No)
    EK05 = ifelse(raw_EK05 == 1, 1, 0)
  ) %>%
  # Hapus kolom pembantu raw agar data bersih
  select(-starts_with("raw_"))

# --- 7. SAVE CLEAN DATASET ---
saveRDS(df, file.path(output, "gb_rct_clean.rds"))
write_xlsx(df, file.path(output, "gb_rct_clean.xlsx"))