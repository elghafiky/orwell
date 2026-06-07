##### ORWELL 2 — GREEN BAROMETER - PHASE 2 SA OUTCOMES ####
# Goal: ITT effects on secondary attitudinal (SA) outcomes
# Coverage: KEA01 (fossil fuel attribution), KEA03 (transition urgency), PP01 (policy support)
# Runs on: Full sample + Attentive respondents only
# Last updated: per Fiky + Armand alignment
#
# EN NOTES (pending confirmation tomorrow):
# - KEA01: values 8 (neither fossil nor renewable, shown on card) and 9 (tidak tahu, spontaneous)
#   recoded to NA — cannot be placed on fossil-renewable continuum
# - KEA03: value 7 (tidak tahu, spontaneous) recoded to 0 — pending confirmation
# - PP01: no special recodes needed, clean 1-7 scale

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
# Scale: 1 = fully fossil fuel responsible, 7 = fully renewable responsible
# Reverse code: 8 - x so higher = more fossil fuel attribution
# EN Notes: 8 (neither, shown on card) and 9 (tidak tahu, spontaneous) → NA
#   pending confirmation tomorrow

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
  ))

cat("KEA01 recode check (should be 1-7 only):\n")
print(summary(clean_data$KEA01_1))


# =============================================================================
# --- 2.6 VARIABLE CONSTRUCTION: KEA03 — TRANSITION URGENCY ---
# =============================================================================
# Binary: 1 = sekarang juga (right now), 0 = everything else
# EN Notes: 7 (tidak tahu, spontaneous) recoded to 0 — pending confirmation tomorrow

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
# 4-item Likert scale 1-7, higher = more pro-transition support
# No reverse coding needed — all items go in same direction
# Z-scoring and index construction in build_policy_support_index()

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
# --- 3. INDEX CONSTRUCTION FUNCTIONS ---
# =============================================================================

# KEA01: Fossil fuel attribution index
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
  # na.rm = TRUE: some items may be NA (8/9 recoded), index uses available items

  return(data)
}

# PP01: Policy support index
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


# --- 5. DEFINE COVARIATES ---
demo_vars <- c("age_18_40", "male", "hsgrad",
               "working", "studying", "housekeeping",
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware",
               "energy_transition_aware",
               "urban",
               "region_wib", "region_wita", "region_wit")

# Define outcomes per family
# Components = Anderson corrected, indexes = unadjusted
ff_components       <- paste0("ff_attribution", 1:8, "_Z")
policy_components   <- paste0("policy_support", 1:4, "_Z")

ff_all         <- c(ff_components, "ff_attribution_index")
urgency_all    <- c("transition_urgency")
policy_all     <- c(policy_components, "policy_support_index")


# =============================================================================
# --- 6. MAIN ANALYSIS FUNCTION ---
# =============================================================================
run_itt <- function(data, sample_label) {

  cat("\nRunning ITT for sample:", sample_label, "| N =", nrow(data), "\n")

  # Build indexes within this sample
  data <- build_ff_index(data)
  data <- build_policy_support_index(data)

  # Run all SA outcomes
  all_outcomes <- c(ff_all, urgency_all, policy_all)

  # 6A. Model 1 — Pooled vs Control
  res_pooled_m1 <- map_dfr(all_outcomes, function(y) {
    model_1(data, y, "Pooled_T") %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
  })

  # 6B. Model 1 — Joint T1/T2 vs Control + T1 vs T2
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

  # 6C. Model 2 — Pooled vs Control
  res_pooled_m2 <- map_dfr(all_outcomes, function(y) {
    model_2(data, y, "Pooled_T", demo_vars) %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 2")
  })

  # 6D. Model 2 — Joint T1/T2 vs Control + T1 vs T2
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
# --- 8. ANDERSON SHARPENED Q-VALUES (BKY 2006) ---
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
    qval_adj        <- qval / (1 + qval)
    fdr_temp1       <- qval_adj * rank_val / totalpvals
    reject_temp1    <- ifelse(fdr_temp1 >= p_valid, 1, 0)
    total_rejected1 <- max(reject_temp1 * rank_val, na.rm = TRUE)

    if (total_rejected1 == totalpvals) { qval <- qval - 0.001; next }

    qval_2st        <- qval_adj * (totalpvals / (totalpvals - total_rejected1))
    fdr_temp2       <- qval_2st * rank_val / totalpvals
    reject_temp2    <- ifelse(fdr_temp2 >= p_valid, 1, 0)
    total_rejected2 <- max(reject_temp2 * rank_val, na.rm = TRUE)

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
# Indexes and T1 vs T2: unadjusted p-values only
# =============================================================================

apply_anderson <- function(data, component_outcomes, family_label) {

  # Equation 1 (Pooled)
  eq1 <- data %>%
    filter(outcome %in% component_outcomes, term == "Pooled_T") %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()

  # Equation 2 (T1, T2 vs C)
  eq2 <- data %>%
    filter(outcome %in% component_outcomes, term %in% c("T1", "T2")) %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()

  # T1 vs T2: unadjusted
  t1t2 <- data %>%
    filter(outcome %in% component_outcomes, term == "T1_vs_T2") %>%
    mutate(family = family_label, q_value = p.value)

  bind_rows(eq1, eq2, t1t2)
}

# Apply per family
results_ff <- apply_anderson(all_results, ff_components, "Fossil fuel attribution")
results_urgency <- apply_anderson(all_results, "transition_urgency", "Transition urgency")
results_policy <- apply_anderson(all_results, policy_components, "Policy support")

# Indexes: unadjusted
results_indexes <- all_results %>%
  filter(outcome %in% c("ff_attribution_index", "policy_support_index")) %>%
  mutate(
    family  = case_when(
      outcome == "ff_attribution_index"  ~ "Fossil fuel attribution index",
      outcome == "policy_support_index"  ~ "Policy support index"
    ),
    q_value = p.value
  )

all_results <- bind_rows(results_ff, results_urgency, results_policy, results_indexes)


# --- 10. SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))


# --- 11. EXPORT ---
write_xlsx(all_results, file.path(tbl, "gb_rct_phase2_sa_results.xlsx"))
View(all_results)

### THE END ###
