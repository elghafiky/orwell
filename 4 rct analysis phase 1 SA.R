##### ORWELL 2 — GREEN BAROMETER - PHASE 2 SA OUTCOMES ####
# Goal: ITT effects on secondary attitudinal (SA) outcomes
# Coverage: KEA01 (fossil fuel attribution), KEA03 (transition urgency), PP01 (policy support),
#           PP02/PP02r (energy policy selection & ranking)
# Runs on: Full sample + Attentive respondents only
# Last updated: per Fiky + Armand alignment
#
# EN NOTES (pending confirmation tomorrow):
# - KEA01: values 8 (neither fossil nor renewable, shown on card) and 9 (tidak tahu, spontaneous)
#   recoded to NA — cannot be placed on fossil-renewable continuum
# - KEA03: value 7 (tidak tahu, spontaneous) recoded to 0 — pending confirmation
# - PP01: no special recodes needed, clean 1-7 scale
# - PP02/PP02r: "Membuka banyak lapangan pekerjaan" (n=1 selection, not in PP02r 1-23 range)
#   excluded from domain framework — confirmed not a genuine pre-specified option
# - PP03/PP03r: "Membuka lapangan kerja lebih banyak lagi" and "Memajukan kota" (n=1 each,
#   not in PP03r 1-5 range) excluded — revised regression plan specifies 5 domains, not 6
#   (drops "other"). The single PP03r value of 9 (n=1) is out of the 1-5 domain range
#   and is treated as not-ranked for domain scoring purposes.

graphics.off(); rm(list=ls()); cat("\014");
pacman::p_load(tidyverse, data.table, estimatr, broom, glmnet, purrr, writexl, car, sandwich)

# --- 1. SETUP DIRECTORIES ---
base_dir <- "G:/"
master <- file.path(base_dir, "Shared drives", "Projects", "2026", "Orwell 2",
                    "Breadcrumbs", "4. Green Barometer", "10 Data RCT")

setwd(master)
opt = file.path(getwd(), "1c output")
tbl = file.path(getwd(), "4 tables")
fig = file.path(master, "3 figures")
if (!dir.exists(tbl)) dir.create(tbl, recursive = TRUE)

# --- 2. LOAD RAW DATA ---
clean_data <- readRDS(file.path(opt, "gb_rct_clean_new.rds"))

# --- 2.3 PREP ATTENTION CHECK VARIABLE ---
clean_data <- clean_data %>%
  rename(attentive = `IA01a Jawaban Anda`) %>%
  mutate(attentive = ifelse(attentive == 1, 1, 0))


# =============================================================================
# --- 2.5 VARIABLE CONSTRUCTION: KEA01 — FOSSIL FUEL ATTRIBUTION ---
# =============================================================================
clean_data <- clean_data %>%
  rename(
    KEA01_1 = `KEA01_1 Mana yang lebih berpengaruh dalam menyebabkan PERUBAHAN IKLIM?`,
    KEA01_2 = `KEA01_2 Mana yang lebih berpengaruh dalam menyebabkan POLUSI?`,
    KEA01_3 = `KEA01_3 Mana yang lebih berpengaruh dalam menyebabkan KELANGKAAN ENERGI?`,
    KEA01_4 = `KEA01_4 Mana yang lebih berpengaruh dalam menyebabkan BENCANA ALAM?`,
    KEA01_5 = `KEA01_5 Mana yang lebih berpengaruh dalam menyebabkan KERUSAKAN LINGKUNGAN?`,
    KEA01_6 = `KEA01_6 Mana yang lebih berpengaruh dalam menyebabkan WABAH PENYAKIT?`,
    KEA01_7 = `KEA01_7 Mana yang lebih berpengaruh dalam menyebabkan KRISIS EKONOMI?`,
    KEA01_8 = `KEA01_8 Mana yang lebih berpengaruh dalam menyebabkan KETERGANTUNGAN PADA NEGARA ASING?`
  ) %>%
  mutate(across(
    KEA01_1:KEA01_8,
    ~ ifelse(.x %in% c(8, 9), NA, 8 - .x)
    # EN Notes: 8 (neither fossil nor renewable, shown on card) and 9 (tidak tahu)
    # recoded to NA — pending confirmation tomorrow
  ))

cat("KEA01 recode check (should be 1-7 only):\n")
print(summary(clean_data$KEA01_1))


# =============================================================================
# --- 2.6 VARIABLE CONSTRUCTION: KEA03 — TRANSITION URGENCY ---
# =============================================================================
clean_data <- clean_data %>%
  mutate(transition_urgency = case_when(
    `KEA03 Menurut Anda, kapan Indonesia perlu mempercepat transisi energi? (SA)` == 1 ~ 1,
    TRUE ~ 0
    # EN Notes: 7=Tidak tahu is mutated to 0 — pending confirmation tomorrow
  ))

cat("\nKEA03 recode check:\n")
print(table(clean_data$transition_urgency, useNA = "always"))


# =============================================================================
# --- 2.7 VARIABLE CONSTRUCTION: PP01 — POLICY SUPPORT ---
# =============================================================================
clean_data <- clean_data %>%
  rename(
    PP01_1 = `PP01_1 Secara umum pemerintah pusat mampu mengelola transisi energi dengan baik tanpa perlu terlalu banyak pemborosan dan korupsi`,
    PP01_2 = `PP01_2 Subsidi BBM sebaiknya dikurangi secara bertahap dan dananya dialihkan untuk mempercepat pemanfaatan energi terbarukan oleh seluruh lapisan masyarakan Indonesia`,
    PP01_3 = `PP01_3 Pemerintah perlu memprioritaskan pembangunan pembangkit energi terbarukan di dalam negeri, meskipun tarif lisrik mungkin sedikit lebih tinggi`,
    PP01_4 = `PP01_4 Saya percaya pemerintah dapat melindungi rumah tangga miskin dari dampak kenaikan harga listrik atau BBM yang mungkin terjadi karena transisi energi misalnya dengan bantuan sosial`
  )

cat("\nPP01 recode check (should all be 1-7):\n")
print(summary(clean_data[, c("PP01_1", "PP01_2", "PP01_3", "PP01_4")]))


# =============================================================================
# --- 2.8 VARIABLE CONSTRUCTION: PP02 / PP02r — ENERGY POLICY SELECTION & RANKING ---
# =============================================================================
# Domain mapping per Kartu Bantu 24 (23 items -> 6 domains)
# PP02r_1..5 store item numbers 1-23 corresponding to this exact order/grouping.
# "Membuka banyak lapangan pekerjaan" (n=1), "Tidak perlu", "Tidak tahu"
# are NOT part of the 23-item domain framework — excluded entirely.

# --- Rename PP02r columns ---
clean_data <- clean_data %>%
  rename(
    PP02r_1 = `PP02r_1 Rank 1`,
    PP02r_2 = `PP02r_2 Rank 2`,
    PP02r_3 = `PP02r_3 Rank 3`,
    PP02r_4 = `PP02r_4 Rank 4`,
    PP02r_5 = `PP02r_5 Rank 5`
  )

# --- Domain column mapping for PP02 (selection dummies) ---
pp02_prefix <- "PP02 5 hal yang seharusnya menjadi prioritas kebijakan pemerintah dalam hal energi? - "

epol_domain_items <- list(
  subsidies      = c(
    "Menurunkan subsidi BBM dan LPG sehingga harganya naik",
    "Menghapuskan subsidi BBM dan LPG sehingga harganya naik",
    "Mempertahankan subsidi demi menjaga harga BBM dan LPG tetap terjangkau"
  ),
  plants         = c(
    "Membangun pembangkit listrik dengan energi terbarukan secara massal",
    "Mempercepat penutupan pembangkit listrik tenaga uap"
  ),
  assistance     = c(
    "Bantuan tunai ke pekerja sektor industri batubara dan minyak bumi yang kehilangan pekerjaan karena transisi energi",
    "Memberikan bantuan sosial ke lebih banyak orang",
    "Meningkatkan nilai atau jumlah bansos untuk setiap penerimanya",
    "Bantuan tunai ke semua orang setelah harga BBM dan LPG",
    "Bantuan tunai ke orang miskin setelah harga BBM dan LPG"
  ),
  mining         = c(
    "Mengurangi penambangan batu bara",
    "Mengurangi penambangan minyak bumi",
    "Menghentikan penambangan batu bara",
    "Menghentikan penambangan minyak bumi"
  ),
  incentives     = c(
    "Insentif (subsidi atau keringanan pajak) pembuatan bangunan hemat energi",
    "Insentif (subsidi atau keringanan pajak) pembelian kendaraan listrik (mobil atau motor listrik)",
    "Insentif (subsidi atau keringanan pajak) pemasangan panel surya di rumah",
    "Orang yang memasang panel surya di rumah bisa menjual kelebihan daya listriknya ke PLN",
    "Pemerintah membayar perusahaan untuk beralih ke teknologi dan energi ramah lingkungan"
  ),
  transportation = c(
    "Pembangunan sistem transportasi publik massal (seperti bus dan kereta)",
    "Kenaikan pajak kendaraan berbahan bakar bensin",
    "Pembatasan penggunaan kendaraan pribadi berbahan bakar bensin",
    "Pembangunan Stasiun Pengisian Kendaraan Listrik Umum (SPKLU) secara massal"
  )
)

epol_domains <- names(epol_domain_items)  # subsidies, plants, assistance, mining, incentives, transportation

cat("\nPP02 domain item counts (should sum to 23):\n")
print(sapply(epol_domain_items, length))


# --- Build epol_*_selection (count of items selected per domain) ---
for (dom in epol_domains) {
  cols <- paste0(pp02_prefix, epol_domain_items[[dom]])
  clean_data[[paste0("epol_", dom, "_selection")]] <-
    rowSums(clean_data[, cols], na.rm = TRUE)
}

cat("\nepol_*_selection summary:\n")
print(summary(clean_data[, paste0("epol_", epol_domains, "_selection")]))


# --- Build epol_*_ranking (domain-average rank score, 0-5) ---
# Item numbers 1-23 follow the SAME order as epol_domain_items above
# (3 subsidies + 2 plants + 5 assistance + 4 mining + 5 incentives + 4 transportation = 23)
item_domain_vec <- rep(epol_domains, times = sapply(epol_domain_items, length))
# item_domain_vec[k] = domain of item number k (1-23)

build_epol_ranking <- function(data) {
  rank_cols <- c("PP02r_1", "PP02r_2", "PP02r_3", "PP02r_4", "PP02r_5")
  
  # Matrix of item scores per respondent (n x 23): 0 by default
  item_scores <- matrix(0, nrow = nrow(data), ncol = 23)
  
  for (r in 1:5) {
    item_num <- data[[rank_cols[r]]]
    valid <- !is.na(item_num) & item_num >= 1 & item_num <= 23
    # score = 6 - rank position (rank1 -> 5, rank2 -> 4, ..., rank5 -> 1)
    score <- 6 - r
    for (i in which(valid)) {
      item_scores[i, item_num[i]] <- score
    }
  }
  
  for (dom in epol_domains) {
    dom_items <- which(item_domain_vec == dom)
    data[[paste0("epol_", dom, "_ranking")]] <- rowMeans(item_scores[, dom_items, drop = FALSE])
  }
  
  return(data)
}

clean_data <- build_epol_ranking(clean_data)

cat("\nepol_*_ranking summary:\n")
print(summary(clean_data[, paste0("epol_", epol_domains, "_ranking")]))

# =============================================================================
# --- 2.9 VARIABLE CONSTRUCTION: PP03 / PP03r — DEVELOPMENT POLICY SELECTION & RANKING ---
# =============================================================================
# 5 domains, each domain = exactly 1 PP03 item (1:1 mapping, unlike PP02's multi-item domains)
# devpol_*_selection: raw 0/1 dummy (already binary, just renamed)
# devpol_*_ranking: scale 0-3, score = 4 - rank_position (rank1->3, rank2->2, rank3->1)
# Items "Membuka lapangan kerja lebih banyak lagi" and "Memajukan kota" (n=1 each) excluded —
# revised regression plan specifies 5 domains (drops "other" from PAP's original 6)

clean_data <- clean_data %>%
  rename(
    PP03r_1 = `PP03r_1 Rank 1`,
    PP03r_2 = `PP03r_2 Rank 2`,
    PP03r_3 = `PP03r_3 Rank 3`
  )

pp03_prefix <- "PP03 Kebijakan pembangunan mana yang akan anda sarankan? - "

# Domains in PP03r item-number order (1-5)
devpol_domains <- c("infrastructure", "forest", "energy", "assistance", "human")

devpol_item_labels <- c(
  infrastructure = "Membangun pabrik dan infrastruktur penting",
  forest         = "Pemulihan hutan dan lahan",
  energy         = "Peralihan ke energi terbarukan dan pengurangan energi fosil",
  assistance     = "Meningkatkan pemberian bantuan sosial",
  human          = "Meningkatkan kualitas sumber daya manusia"
)

# --- devpol_*_selection: raw 0/1 dummy per domain ---
for (dom in devpol_domains) {
  col <- paste0(pp03_prefix, devpol_item_labels[dom])
  clean_data[[paste0("devpol_", dom, "_selection")]] <- clean_data[[col]]
}

cat("\ndevpol_*_selection summary:\n")
print(summary(clean_data[, paste0("devpol_", devpol_domains, "_selection")]))

# --- devpol_*_ranking: scale 0-3, 1:1 item-domain mapping ---
build_devpol_ranking <- function(data) {
  rank_cols <- c("PP03r_1", "PP03r_2", "PP03r_3")
  item_scores <- matrix(0, nrow = nrow(data), ncol = 5)  # items 1-5 only
  
  for (r in 1:3) {
    item_num <- data[[rank_cols[r]]]
    valid <- !is.na(item_num) & item_num >= 1 & item_num <= 5
    score <- 4 - r  # rank1 -> 3, rank2 -> 2, rank3 -> 1
    for (i in which(valid)) {
      item_scores[i, item_num[i]] <- score
    }
  }
  
  for (k in seq_along(devpol_domains)) {
    data[[paste0("devpol_", devpol_domains[k], "_ranking")]] <- item_scores[, k]
  }
  
  return(data)
}

clean_data <- build_devpol_ranking(clean_data)

cat("\ndevpol_*_ranking summary:\n")
print(summary(clean_data[, paste0("devpol_", devpol_domains, "_ranking")]))


# =============================================================================
# --- 3. INDEX CONSTRUCTION FUNCTIONS (sample-dependent, control-group z-scores) ---
# =============================================================================

build_ff_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)
  kea01_vars   <- paste0("KEA01_", 1:8)
  
  for (var in kea01_vars) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0("ff_attribution", gsub("KEA01_", "", var), "_Z")]] <-
      (raw_vector - c_mean) / c_sd
  }
  
  z_cols <- paste0("ff_attribution", 1:8, "_Z")
  data <- data %>%
    mutate(ff_attribution_index = rowMeans(select(., all_of(z_cols)), na.rm = TRUE))
  return(data)
}

build_policy_support_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)
  pp01_vars    <- paste0("PP01_", 1:4)
  
  for (var in pp01_vars) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0("policy_support", gsub("PP01_", "", var), "_Z")]] <-
      (raw_vector - c_mean) / c_sd
  }
  
  z_cols <- paste0("policy_support", 1:4, "_Z")
  data <- data %>%
    mutate(policy_support_index = rowMeans(select(., all_of(z_cols)), na.rm = FALSE))
  return(data)
}


# --- 4. DEFINE MODEL FUNCTIONS ---
model_1 <- function(data, outcome, treatment_vars) {
  formula <- as.formula(paste(outcome, "~", paste(treatment_vars, collapse = "+")))
  result  <- lm_robust(formula, data = data, se_type = "HC2")
  return(tidy(result))
}

select_lasso_vars <- function(data, outcome, treatment_vars, covariates) {
  dsub <- data %>%
    select(all_of(c(outcome, treatment_vars, covariates))) %>%
    drop_na() %>%
    as.data.frame()
  
  y <- as.numeric(dsub[[outcome]])
  X <- model.matrix(as.formula(paste("~", paste(covariates, collapse = "+"), "-1")), data = dsub)
  
  fit_y <- cv.glmnet(X, y, alpha = 1)
  sel_y <- rownames(coef(fit_y, s = "lambda.1se"))[as.vector(coef(fit_y, s = "lambda.1se") != 0)]
  
  sel_d <- c()
  for (t_var in treatment_vars) {
    d        <- as.numeric(dsub[[t_var]])
    fit_d    <- cv.glmnet(X, d, alpha = 1)
    sel_temp <- rownames(coef(fit_d, s = "lambda.1se"))[as.vector(coef(fit_d, s = "lambda.1se") != 0)]
    sel_d    <- c(sel_d, sel_temp)
  }
  
  selected <- setdiff(union(sel_y, unique(sel_d)), "(Intercept)")
  return(selected)
}

model_2 <- function(data, outcome, treatment_vars, covariates) {
  selected_vars <- select_lasso_vars(data, outcome, treatment_vars, covariates)
  all_vars      <- c(treatment_vars, selected_vars)
  formula       <- as.formula(paste(outcome, "~", paste(all_vars, collapse = "+")))
  result        <- lm_robust(formula, data = data, se_type = "HC2")
  return(tidy(result))
}


# --- 5. DEFINE COVARIATES & OUTCOMES ---
demo_vars <- c("age_18_40", "male", "hsgrad",
               "working", "studying", "housekeeping",
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware",
               "energy_transition_aware",
               "urban",
               "region_wib", "region_wita", "region_wit")

# Components only — indexes excluded from Anderson, run separately
ff_components     <- paste0("ff_attribution", 1:8, "_Z")
policy_components <- paste0("policy_support", 1:4, "_Z")

# PP02/PP02r — no index, all 6 domains are components within their own family
epol_selection_components <- paste0("epol_", epol_domains, "_selection")
epol_ranking_components    <- paste0("epol_", epol_domains, "_ranking")

# PP03/PP03r — no index, all 5 domains are components within their own family
devpol_selection_components <- paste0("devpol_", devpol_domains, "_selection")
devpol_ranking_components    <- paste0("devpol_", devpol_domains, "_ranking")


# All outcomes including indexes (for regression)
ff_all       <- c(ff_components, "ff_attribution_index")
urgency_all  <- c("transition_urgency")
policy_all   <- c(policy_components, "policy_support_index")
epol_sel_all <- epol_selection_components
epol_rank_all <- epol_ranking_components
devpol_sel_all  <- devpol_selection_components
devpol_rank_all <- devpol_ranking_components

all_outcomes <- c(ff_all, urgency_all, policy_all,
                  epol_sel_all, epol_rank_all,
                  devpol_sel_all, devpol_rank_all)

# =============================================================================
# --- 6. MAIN ANALYSIS FUNCTION ---
# =============================================================================
run_itt <- function(data, sample_label) {
  
  cat("\nRunning ITT for sample:", sample_label, "| N =", nrow(data), "\n")
  
  # Build sample-dependent indexes (control-group z-scores)
  data <- build_ff_index(data)
  data <- build_policy_support_index(data)
  # epol_* already constructed globally on clean_data (no control-group dependency)
  
  res_pooled_m1 <- map_dfr(all_outcomes, function(y) {
    model_1(data, y, "Pooled_T") %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
  })
  
  res_joint_m1 <- map_dfr(all_outcomes, function(y) {
    res_tidy <- model_1(data, y, c("T1", "T2")) %>%
      mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 1")
    
    fit <- lm(as.formula(paste(y, "~ T1 + T2")), data = data)
    vcv <- vcovHC(fit, type = "HC2")
    ht  <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
    
    se_diff  <- sqrt(vcv["T1","T1"] + vcv["T2","T2"] - 2*vcv["T1","T2"])
    est_diff <- coef(fit)["T1"] - coef(fit)["T2"]
    
    t1_t2_row <- tibble(
      term      = "T1_vs_T2",
      estimate  = est_diff,
      std.error = se_diff,
      statistic = ht[2, "F"],
      p.value   = ht[2, "Pr(>F)"],
      conf.low  = est_diff - 1.96 * se_diff,
      conf.high = est_diff + 1.96 * se_diff,
      df        = ht[2, "Df"],
      outcome   = y, comparison = "T1 vs T2", model = "Model 1"
    )
    bind_rows(res_tidy, t1_t2_row)
  })
  
  res_pooled_m2 <- map_dfr(all_outcomes, function(y) {
    model_2(data, y, "Pooled_T", demo_vars) %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 2")
  })
  
  res_joint_m2 <- map_dfr(all_outcomes, function(y) {
    selected_vars <- select_lasso_vars(data, y, c("T1", "T2"), demo_vars)
    all_vars      <- c("T1", "T2", selected_vars)
    formula_str   <- paste(y, "~", paste(all_vars, collapse = "+"))
    
    res_tidy <- lm_robust(as.formula(formula_str), data = data, se_type = "HC2") %>%
      tidy() %>%
      mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 2")
    
    fit <- lm(as.formula(formula_str), data = data)
    vcv <- vcovHC(fit, type = "HC2")
    ht  <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
    
    se_diff  <- sqrt(vcv["T1","T1"] + vcv["T2","T2"] - 2*vcv["T1","T2"])
    est_diff <- coef(fit)["T1"] - coef(fit)["T2"]
    
    t1_t2_row <- tibble(
      term      = "T1_vs_T2",
      estimate  = est_diff,
      std.error = se_diff,
      statistic = ht[2, "F"],
      p.value   = ht[2, "Pr(>F)"],
      conf.low  = est_diff - 1.96 * se_diff,
      conf.high = est_diff + 1.96 * se_diff,
      df        = ht[2, "Df"],
      outcome   = y, comparison = "T1 vs T2", model = "Model 2"
    )
    bind_rows(res_tidy, t1_t2_row)
  })
  
  bind_rows(res_pooled_m1, res_pooled_m2, res_joint_m1, res_joint_m2) %>%
    filter(term %in% c("Pooled_T", "T1", "T2", "T1_vs_T2")) %>%
    mutate(sample = sample_label)
}


# --- 7. RUN ON BOTH SAMPLES ---
df_full      <- clean_data
df_attentive <- clean_data %>% filter(attentive == 1)

results_full      <- run_itt(df_full,      "Full sample")
results_attentive <- run_itt(df_attentive, "Attentive only")

all_results <- bind_rows(results_full, results_attentive)


# =============================================================================
# --- 8. ORIGINAL ANDERSON SHARPENED Q-VALUES (BKY 2006) ---
# Exactly as written by Anderson — no modifications
# =============================================================================
anderson_qvalue <- function(pval) {
  valid_idx <- !is.na(pval)
  p_valid   <- pval[valid_idx]
  
  if (length(p_valid) <= 1) return(pval)
  
  totalpvals <- length(p_valid)
  rank_val   <- rank(p_valid)
  
  qval       <- 1
  bky06_qval <- rep(1, totalpvals)
  
  while (qval > 0) {
    qval_adj       <- qval / (1 + qval)
    fdr_temp1      <- qval_adj * rank_val / totalpvals
    reject_temp1   <- ifelse(fdr_temp1 >= p_valid, 1, 0)
    reject_rank1   <- reject_temp1 * rank_val
    total_rejected1 <- max(reject_rank1, na.rm = TRUE)
    
    qval_2st       <- qval_adj * (totalpvals / (totalpvals - total_rejected1))
    fdr_temp2      <- qval_2st * rank_val / totalpvals
    reject_temp2   <- ifelse(fdr_temp2 >= p_valid, 1, 0)
    reject_rank2   <- reject_temp2 * rank_val
    total_rejected2 <- max(reject_rank2, na.rm = TRUE)
    
    bky06_qval[rank_val <= total_rejected2] <- qval
    
    qval <- qval - 0.001
  }
  
  out <- rep(NA_real_, length(pval))
  out[valid_idx] <- bky06_qval
  return(out)
}


# =============================================================================
# --- 9. APPLY Q-VALUES PER FAMILY ---
# Per Armand: each SA outcome family corrected separately
# Grouped by regression equation x model x sample
# Indexes and T1 vs T2: unadjusted p-values only — NOT passed to Anderson
# =============================================================================

apply_anderson <- function(data, component_outcomes, family_label) {
  
  # Equation 1 (Pooled): components only
  eq1 <- data %>%
    filter(outcome %in% component_outcomes, term == "Pooled_T") %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()
  
  # Equation 2 (T1, T2 vs C): components only
  eq2 <- data %>%
    filter(outcome %in% component_outcomes, term %in% c("T1", "T2")) %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()
  
  # T1 vs T2: unadjusted — not passed to Anderson
  t1t2 <- data %>%
    filter(outcome %in% component_outcomes, term == "T1_vs_T2") %>%
    mutate(family = family_label, q_value = p.value)
  
  bind_rows(eq1, eq2, t1t2)
}

# Apply Anderson per family (components only)
results_ff       <- apply_anderson(all_results, ff_components,            "Fossil fuel attribution")
results_urgency  <- apply_anderson(all_results, "transition_urgency",     "Transition urgency")
results_policy   <- apply_anderson(all_results, policy_components,        "Policy support")
results_epol_sel <- apply_anderson(all_results, epol_selection_components, "Energy policy selection")
results_epol_rnk <- apply_anderson(all_results, epol_ranking_components,   "Energy policy ranking")
results_devpol_sel <- apply_anderson(all_results, devpol_selection_components, "Development policy selection")
results_devpol_rnk <- apply_anderson(all_results, devpol_ranking_components,   "Development policy ranking")

# Indexes: unadjusted p-values only — never passed to Anderson
results_indexes <- all_results %>%
  filter(outcome %in% c("ff_attribution_index", "policy_support_index")) %>%
  mutate(
    family  = case_when(
      outcome == "ff_attribution_index" ~ "Fossil fuel attribution index",
      outcome == "policy_support_index" ~ "Policy support index"
    ),
    q_value = p.value
  )

all_results <- bind_rows(
  results_ff, results_urgency, results_policy,
  results_epol_sel, results_epol_rnk, 
  results_devpol_sel, results_devpol_rnk,
  results_indexes
)


# --- 10. SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))


# --- 11. EXPORT ---
write_xlsx(all_results, file.path(tbl, "gb_rct_phase1_sa_results_61426.xlsx"))
View(all_results)


# --- 12. PLOT FUNCTION ---
plot_outcome_results <- function(data, outcome_var, sample_label, title_text) {
  
  plot_data <- data %>%
    filter(outcome == outcome_var, sample == sample_label) %>%
    mutate(
      plot_label = case_when(
        term == "Pooled_T"  ~ "Pooled vs Control",
        term == "T1"        ~ "T1 (State) vs Control",
        term == "T2"        ~ "T2 (Self) vs Control",
        term == "T1_vs_T2"  ~ "T1 vs T2 (Equality Test)"
      ),
      plot_label = factor(plot_label, levels = c(
        "T1 vs T2 (Equality Test)",
        "T2 (Self) vs Control",
        "T1 (State) vs Control",
        "Pooled vs Control"
      ))
    )
  
  ggplot(plot_data, aes(x = estimate, y = plot_label, color = model, group = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.8) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(0.4), height = 0.15, linewidth = 0.8
    ) +
    geom_point(position = position_dodge(0.4), size = 3.5) +
    geom_text(
      aes(label = stars, x = ifelse(is.na(conf.high), estimate, conf.high)),
      position = position_dodge(0.4),
      hjust = -0.3, vjust = 0.3, color = "black", size = 5, show.legend = FALSE
    ) +
    labs(
      title    = title_text,
      subtitle = paste(sample_label, "| Outcome:", outcome_var),
      x        = "Effect Size (OLS Coefficient Estimate with 95% CIs)",
      y        = NULL,
      color    = "Specification",
      caption  = paste0(
        "* q < 0.10, ** q < 0.05, *** q < 0.01\n",
        "Anderson (2008) sharpened q-values applied within each SA outcome family, ",
        "grouped by equation x model x sample.\n",
        "Indexes and T1 vs T2 reported with unadjusted p-values.\n",
        "Model 1: treatment dummies only. Model 2: double LASSO covariate-adjusted."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position      = "top",
      legend.justification = "left",
      panel.grid.minor     = element_blank(),
      plot.title           = element_text(face = "bold", size = 15),
      plot.caption         = element_text(hjust = 0, color = "gray30", size = 9, lineheight = 1.2),
      axis.text.y          = element_text(face = "bold", color = "black")
    ) +
    scale_color_manual(values = c("Model 1" = "#1f77b4", "Model 2" = "#ff7f0e"))
}


# --- 13. OUTCOME TITLES & EXPORT PLOTS ---
outcome_titles <- c(
  # KEA01: Fossil fuel attribution components
  "ff_attribution1_Z"    = "Fossil Fuel Attribution: Climate Change (KEA01_1)",
  "ff_attribution2_Z"    = "Fossil Fuel Attribution: Pollution (KEA01_2)",
  "ff_attribution3_Z"    = "Fossil Fuel Attribution: Energy Scarcity (KEA01_3)",
  "ff_attribution4_Z"    = "Fossil Fuel Attribution: Natural Disasters (KEA01_4)",
  "ff_attribution5_Z"    = "Fossil Fuel Attribution: Environmental Damage (KEA01_5)",
  "ff_attribution6_Z"    = "Fossil Fuel Attribution: Disease Outbreaks (KEA01_6)",
  "ff_attribution7_Z"    = "Fossil Fuel Attribution: Economic Crisis (KEA01_7)",
  "ff_attribution8_Z"    = "Fossil Fuel Attribution: Foreign Dependency (KEA01_8)",
  "ff_attribution_index" = "Fossil Fuel Attribution Index (KEA01, all items)",
  # KEA03: Transition urgency
  "transition_urgency"   = "Transition Urgency: Right Now vs Later (KEA03)",
  # PP01: Policy support components
  "policy_support1_Z"    = "Policy Support: Government Capacity (PP01_1)",
  "policy_support2_Z"    = "Policy Support: Fuel Subsidy Reduction (PP01_2)",
  "policy_support3_Z"    = "Policy Support: Renewable Energy Priority (PP01_3)",
  "policy_support4_Z"    = "Policy Support: Social Protection Trust (PP01_4)",
  "policy_support_index" = "Policy Support Index (PP01, all items)",
  # PP02: Energy policy selection (count per domain, 0-5)
  "epol_subsidies_selection"      = "Energy Policy Selection: Fuel Subsidies (PP02)",
  "epol_plants_selection"         = "Energy Policy Selection: Power Plants (PP02)",
  "epol_assistance_selection"     = "Energy Policy Selection: Social Assistance (PP02)",
  "epol_mining_selection"         = "Energy Policy Selection: Mining (PP02)",
  "epol_incentives_selection"     = "Energy Policy Selection: Green Incentives (PP02)",
  "epol_transportation_selection" = "Energy Policy Selection: Transportation (PP02)",
  # PP02r: Energy policy ranking (domain-average score, 0-5)
  "epol_subsidies_ranking"      = "Energy Policy Ranking: Fuel Subsidies (PP02r)",
  "epol_plants_ranking"         = "Energy Policy Ranking: Power Plants (PP02r)",
  "epol_assistance_ranking"     = "Energy Policy Ranking: Social Assistance (PP02r)",
  "epol_mining_ranking"         = "Energy Policy Ranking: Mining (PP02r)",
  "epol_incentives_ranking"     = "Energy Policy Ranking: Green Incentives (PP02r)",
  "epol_transportation_ranking" = "Energy Policy Ranking: Transportation (PP02r)",
  # PP03: Development policy selection (binary dummy per domain)
  "devpol_infrastructure_selection" = "Development Policy Selection: Infrastructure (PP03)",
  "devpol_forest_selection"          = "Development Policy Selection: Forest & Land (PP03)",
  "devpol_energy_selection"          = "Development Policy Selection: Energy Transition (PP03)",
  "devpol_assistance_selection"      = "Development Policy Selection: Social Assistance (PP03)",
  "devpol_human_selection"           = "Development Policy Selection: Human Capital (PP03)",
  # PP03r: Development policy ranking (score 0-3 per domain)
  "devpol_infrastructure_ranking" = "Development Policy Ranking: Infrastructure (PP03r)",
  "devpol_forest_ranking"          = "Development Policy Ranking: Forest & Land (PP03r)",
  "devpol_energy_ranking"          = "Development Policy Ranking: Energy Transition (PP03r)",
  "devpol_assistance_ranking"      = "Development Policy Ranking: Social Assistance (PP03r)",
  "devpol_human_ranking"           = "Development Policy Ranking: Human Capital (PP03r)"
)

sample_labels <- c("Full sample", "Attentive only")

for (s in sample_labels) {
  for (out in names(outcome_titles)) {
    p <- plot_outcome_results(all_results, out, s, outcome_titles[out])
    clean_s   <- gsub(" ", "_", tolower(s))
    clean_out <- gsub("_Z", "", out)
    ggsave(
      file.path(fig, paste0("plot_sa_", clean_out, "_", clean_s, ".png")),
      plot = p, width = 9, height = 5.5, dpi = 300
    )
  }
}

### THE END ###
