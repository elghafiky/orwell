##### ORWELL 2 — GREEN BAROMETER - PHASE 2 IM OUTCOMES ####
# Goal: ITT effects on intermediate mechanism (IM) outcomes
# Coverage: KEA02 (comprehension score), KEA07 (moral emotions),
#           KEA08 (politicized identity), KEA09/KEA10 (collective efficacy)
# Runs on: Full sample + Attentive respondents only
# Last updated: per Fiky + Armand alignment (each IM family corrected separately, same as SA)
#
# EN NOTES (pending confirmation tomorrow):
# - KEA02: comprehension_correct_only interpreted as "selected >=1 correct AND 0 incorrect
#   items (including Tidak Tahu)" — NOT "selected all 6 correct items", since the latter
#   would be near-impossible given individual selection rates of 11-48%. Correct/incorrect
#   classification confirmed via existing KE20 attributes chart (n=2033, matches our N=2022).
# - KEA08: option 8 ("tidak keberatan dan tidak bangga") recoded to NA, same logic as KEA01.
#   This is ~10-11% of sample (213-218 respondents) — larger than KEA01's ~3%, flag for
#   power implications.
# - KEA07: option 8 ("tidak merasakan keduanya") and 9 (tidak tahu) -> NA for all 5 items,
#   same logic. KEA07_2 (dilindungi/dikorbankan) is reverse-coded due to flipped label order.
# - Collective efficacy: only colleff_index run as outcome (unadjusted) — no separate
#   KEA09/KEA10 component regressions, since regression plan names no components for this
#   family (unlike all other index families, which name both components and index).

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
# --- 2.5 VARIABLE CONSTRUCTION: KEA02 — ENERGY TRANSITION COMPREHENSION SCORE ---
# =============================================================================
# 11 items total: 6 correct + 5 incorrect (incl. "Tidak Tahu")
# Classification confirmed via KE20 attributes chart (correct/incorrect attributes of
# energy transition, n=2033 ~ matches our N=2022)
#
# comprehension_any         = 1 if >=1 correct item selected
# comprehension_score       = z-score of count of correct items selected (0-6)
# comprehension_correct_only = 1 if (>=1 correct selected) AND (0 incorrect selected,
#                               including Tidak Tahu) — see EN Notes above

kea02_prefix <- "KEA02 Mana dari pernyataan di kartu bantu tersebut yang menurut Anda tepat untuk menjelaskan transisi energi? - "

kea02_correct_items <- c(
  "Upaya untuk mengurangi emisi karbon dan polusi",
  "Upaya untuk mengatasi krisis energi",
  "Upaya untuk mengurangi polusi",
  "Upaya untuk menangani perubahan iklim",
  "Peralihan dari penggunaan energi fosil ke energi terbarukan",
  "Lebih banyak menggunakan tenaga matahari dan lebih sedikit menggunakan batu bara untuk menciptakan listrik"
)

kea02_incorrect_items <- c(
  "Melakukan penghematan listrik di rumah masing-masing",
  "Peralihan penggunaan teknologi sederhana ke teknologi modern",
  "Peralihan dari penggunaan energi terbarukan ke energi fosil",
  "Lebih banyak menggunakan batu bara dan lebih sedikit menggunakan tenaga matahari untuk menciptakan listrik",
  "Tidak Tahu"
)

correct_cols   <- paste0(kea02_prefix, kea02_correct_items)
incorrect_cols <- paste0(kea02_prefix, kea02_incorrect_items)

cat("KEA02 column check — correct items found:", sum(correct_cols %in% names(clean_data)), "/ 6\n")
cat("KEA02 column check — incorrect items found:", sum(incorrect_cols %in% names(clean_data)), "/ 5\n")

clean_data <- clean_data %>%
  mutate(
    kea02_n_correct   = rowSums(across(all_of(correct_cols)),   na.rm = TRUE),
    kea02_n_incorrect = rowSums(across(all_of(incorrect_cols)), na.rm = TRUE),
    comprehension_any          = ifelse(kea02_n_correct >= 1, 1, 0),
    comprehension_correct_only = ifelse(kea02_n_correct >= 1 & kea02_n_incorrect == 0, 1, 0)
  )

cat("\nkea02_n_correct distribution:\n")
print(table(clean_data$kea02_n_correct))

cat("\ncomprehension_any distribution:\n")
print(table(clean_data$comprehension_any))

cat("\ncomprehension_correct_only distribution:\n")
print(table(clean_data$comprehension_correct_only))


# build_comprehension_score(): z-score kea02_n_correct using control group mean/SD
build_comprehension_score <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)
  c_mean <- mean(control_data$kea02_n_correct, na.rm = TRUE)
  c_sd   <- sd(control_data$kea02_n_correct,   na.rm = TRUE)
  data$comprehension_score <- (data$kea02_n_correct - c_mean) / c_sd
  return(data)
}

### KEA02 SECTION COMPLETE — comprehension_any, comprehension_score (built per-sample),
### comprehension_correct_only all ready. 3 outcomes, one family "Comprehension score",
### no index to exclude. Next: KEA07, KEA08, KEA09/KEA10. ###


# =============================================================================
# --- 2.6 VARIABLE CONSTRUCTION: KEA08 — POLITICIZED IDENTITY ---
# =============================================================================
# Scale: 1 = sangat keberatan (very reluctant) ... 7 = sangat bangga (very proud)
# 4 = seimbang antara keberatan dan bangga (true bipolar midpoint)
# 8 = tidak keberatan dan tidak bangga (neither — outside continuum) -> NA
# 9 = tidak tahu/tidak yakin (spontaneous) -> NA
# Same logic as KEA01. No reverse coding needed (1=negative -> 7=positive already
# matches "higher = more pro-transition identity").
# EN Notes: option 8 here is ~10-11% of sample (larger than KEA01's ~3%) — flag for
# Fiky/Armand re: power implications.

clean_data <- clean_data %>%
  rename(
    KEA08_1 = `KEA08_1 Mendukung percepatan transisi energi di Indonesia`,
    KEA08_2 = `KEA08_2 Mendesak pemerintah agar lebih serius mempercepat transisi energi`
  ) %>%
  mutate(across(
    KEA08_1:KEA08_2,
    ~ ifelse(.x %in% c(8, 9), NA, .x)
  ))

cat("\nKEA08 recode check (should be 1-7 only):\n")
print(summary(clean_data[, c("KEA08_1", "KEA08_2")]))


# build_politicized_id_index(): z-score each item using control group mean/SD, average
build_politicized_id_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)
  kea08_vars   <- c("KEA08_1", "KEA08_2")

  for (var in kea08_vars) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0("politicized_id", gsub("KEA08_", "", var), "_Z")]] <-
      (raw_vector - c_mean) / c_sd
  }

  z_cols <- paste0("politicized_id", 1:2, "_Z")
  data <- data %>%
    mutate(politicized_id_index = rowMeans(select(., all_of(z_cols)), na.rm = TRUE))
  return(data)
}

### KEA08 SECTION COMPLETE — politicized_id1_Z, politicized_id2_Z (components),
### politicized_id_index (excluded from Anderson). Next: KEA07, KEA09/KEA10. ###


# =============================================================================
# --- 2.7 VARIABLE CONSTRUCTION: KEA07 — MORAL EMOTIONS ---
# =============================================================================
# All 5 items: 8 = "tidak merasakan keduanya" (neither, outside continuum) -> NA
#              9 = "tidak tahu/tidak yakin" -> NA
# Direction: KEA07_1,3,4,5 already 1=negative -> 7=positive (matches PAP "higher=more
#            positive emotional response"), no reverse needed.
# KEA07_2 is REVERSED in raw data: 1=sangat dilindungi (protected, POSITIVE) ->
#            7=sangat dikorbankan (sacrificed, NEGATIVE) — opposite of the other 4 items.
#            Reverse coded (8-x, after 8/9->NA) so higher=positive consistently.
#
# Mapping to regression plan names:
#   KEA07_1 (semena-mena/adil)  -> moral_emo_fairness
#   KEA07_2 (dilindungi/dikorbankan, REVERSED) -> moral_emo_protection
#   KEA07_3 (marah/tenang)      -> moral_emo_calm
#   KEA07_4 (sedih/senang)      -> moral_emo_happy
#   KEA07_5 (kecewa/bersyukur)  -> moral_emo_grateful

clean_data <- clean_data %>%
  rename(
    KEA07_1 = `KEA07_1 Diperlakukan semena-mena Vs Diperlakukan adil`,
    KEA07_2 = `KEA07_2 Dilindungi Vs Dikorbankan`,
    KEA07_3 = `KEA07_3 Marah Vs Tenang`,
    KEA07_4 = `KEA07_4 Sedih Vs Senang`,
    KEA07_5 = `KEA07_5 Kecewa Vs Bersyukur`
  ) %>%
  mutate(
    across(KEA07_1:KEA07_5, ~ ifelse(.x %in% c(8, 9), NA, .x)),
    KEA07_2 = ifelse(is.na(KEA07_2), NA, 8 - KEA07_2)  # reverse code (flipped direction)
  )

cat("\nKEA07 recode check (should all be 1-7 only):\n")
print(summary(clean_data[, paste0("KEA07_", 1:5)]))


# build_moral_emo_index(): z-score each item using control group mean/SD, average
moral_emo_labels <- c(
  KEA07_1 = "fairness",
  KEA07_2 = "protection",
  KEA07_3 = "calm",
  KEA07_4 = "happy",
  KEA07_5 = "grateful"
)

build_moral_emo_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)

  for (var in names(moral_emo_labels)) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0("moral_emo_", moral_emo_labels[var], "_Z")]] <-
      (raw_vector - c_mean) / c_sd
  }

  z_cols <- paste0("moral_emo_", moral_emo_labels, "_Z")
  data <- data %>%
    mutate(moral_emo_index = rowMeans(select(., all_of(z_cols)), na.rm = TRUE))
  return(data)
}

### KEA07 SECTION COMPLETE — moral_emo_fairness_Z, _protection_Z, _calm_Z, _happy_Z,
### _grateful_Z (components), moral_emo_index (excluded from Anderson).
### Next: KEA09/KEA10 (collective efficacy). ###


# =============================================================================
# --- 2.8 VARIABLE CONSTRUCTION: KEA09 / KEA10 — COLLECTIVE EFFICACY ---
# =============================================================================
# KEA09: 8-pt scale "nothing at all" -> "very excessive" (perceived community effort)
# KEA10: 7-pt scale "very not confident" -> "very confident" (confidence in collective action)
# Both clean, no out-of-range values, no recode needed. Higher = higher collective efficacy
# for both — no reverse coding needed.
#
# EN Notes: regression plan only names "colleff_index" (no individual component names,
# unlike moral_emo/politicized_id which have per-item names). Proposing colleff_effort
# (KEA09) and colleff_confidence (KEA10) based on PAP description — pending confirmation.

clean_data <- clean_data %>%
  rename(
    KEA09 = `KEA09 Seberapa besar usaha yang warga di lingkungan Anda sudah lakukan untuk mendesak pemerintah agar mempercepat transisi energi?`,
    KEA10 = `KEA10 Seberapa yakin Anda bahwa warga di lingkungan Anda mampu bersama-sama  mendesak pemerintah agar mempercepat transisi energi?`
  )

cat("\nKEA09/KEA10 check (KEA09: 1-8, KEA10: 1-7):\n")
print(summary(clean_data[, c("KEA09", "KEA10")]))


# build_colleff_index(): z-score each item using control group mean/SD, average
build_colleff_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)

  for (var in c("KEA09", "KEA10")) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0("colleff_", ifelse(var == "KEA09", "effort", "confidence"), "_Z")]] <-
      (raw_vector - c_mean) / c_sd
  }

  data <- data %>%
    mutate(colleff_index = rowMeans(select(., colleff_effort_Z, colleff_confidence_Z), na.rm = TRUE))
  return(data)
}

### KEA09/KEA10 SECTION COMPLETE — colleff_effort_Z, colleff_confidence_Z (components),
### colleff_index (excluded from Anderson). All 4 IM families constructed. ###


# =============================================================================
# --- 3. DEFINE MODEL FUNCTIONS ---
# =============================================================================
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


# =============================================================================
# --- 4. DEFINE COVARIATES & OUTCOMES ---
# =============================================================================
demo_vars <- c("age_18_40", "male", "hsgrad",
               "working", "studying", "housekeeping",
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware",
               "energy_transition_aware",
               "urban",
               "region_wib", "region_wita", "region_wit")

# KEA02: 3 components, no index (none marked "do not include in MHT")
comprehension_components <- c("comprehension_any", "comprehension_score", "comprehension_correct_only")

# KEA07: 5 components + index (excluded)
moral_emo_components <- paste0("moral_emo_", moral_emo_labels, "_Z")

# KEA08: 2 components + index (excluded)
politicized_components <- paste0("politicized_id", 1:2, "_Z")

# KEA09/KEA10: colleff_index is the ONLY outcome (no separate components — see EN Notes)

all_outcomes <- c(
  comprehension_components,
  moral_emo_components, "moral_emo_index",
  politicized_components, "politicized_id_index",
  "colleff_index"
)


# =============================================================================
# --- 5. MAIN ANALYSIS FUNCTION ---
# =============================================================================
run_itt <- function(data, sample_label) {

  cat("\nRunning ITT for sample:", sample_label, "| N =", nrow(data), "\n")

  # Build sample-dependent indexes (control-group z-scores)
  data <- build_comprehension_score(data)
  data <- build_moral_emo_index(data)
  data <- build_politicized_id_index(data)
  data <- build_colleff_index(data)

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


# --- 6. RUN ON BOTH SAMPLES ---
df_full      <- clean_data
df_attentive <- clean_data %>% filter(attentive == 1)

results_full      <- run_itt(df_full,      "Full sample")
results_attentive <- run_itt(df_attentive, "Attentive only")

all_results <- bind_rows(results_full, results_attentive)


# =============================================================================
# --- 7. ORIGINAL ANDERSON SHARPENED Q-VALUES (BKY 2006) ---
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
# --- 8. APPLY Q-VALUES PER FAMILY ---
# Per Armand: each IM outcome family corrected separately (same as SA)
# Grouped by regression equation x model x sample
# Indexes and T1 vs T2: unadjusted p-values only — NOT passed to Anderson
# =============================================================================

apply_anderson <- function(data, component_outcomes, family_label) {

  eq1 <- data %>%
    filter(outcome %in% component_outcomes, term == "Pooled_T") %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()

  eq2 <- data %>%
    filter(outcome %in% component_outcomes, term %in% c("T1", "T2")) %>%
    mutate(family = family_label) %>%
    group_by(model, sample) %>%
    mutate(q_value = anderson_qvalue(p.value)) %>%
    ungroup()

  t1t2 <- data %>%
    filter(outcome %in% component_outcomes, term == "T1_vs_T2") %>%
    mutate(family = family_label, q_value = p.value)

  bind_rows(eq1, eq2, t1t2)
}

# Comprehension score: 3 components, no excluded index
results_comprehension <- apply_anderson(all_results, comprehension_components, "Comprehension score")

# Moral emotions, Politicized identity: components + excluded index
results_moral_emo    <- apply_anderson(all_results, moral_emo_components,    "Moral emotions")
results_politicized  <- apply_anderson(all_results, politicized_components,  "Politicized identity")
# Collective efficacy: no components, colleff_index goes straight to results_indexes below

results_indexes <- all_results %>%
  filter(outcome %in% c("moral_emo_index", "politicized_id_index", "colleff_index")) %>%
  mutate(
    family  = case_when(
      outcome == "moral_emo_index"      ~ "Moral emotions index",
      outcome == "politicized_id_index" ~ "Politicized identity index",
      outcome == "colleff_index"         ~ "Collective efficacy index (only outcome for this family)"
    ),
    q_value = p.value
  )

all_results <- bind_rows(
  results_comprehension, results_moral_emo, results_politicized,
  results_indexes
)


# --- 9. SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))


# --- 10. EXPORT ---
write_xlsx(all_results, file.path(tbl, "gb_rct_phase1_im_results.xlsx"))
View(all_results)


# --- 11. PLOT FUNCTION ---
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
        "Anderson (2008) sharpened q-values applied within each IM outcome family, ",
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


# --- 12. OUTCOME TITLES & EXPORT PLOTS ---
outcome_titles <- c(
  # KEA02: Comprehension score
  "comprehension_any"          = "Comprehension: Selected >=1 Correct Statement (KEA02)",
  "comprehension_score"        = "Comprehension: Z-Score of Correct Answers (KEA02)",
  "comprehension_correct_only" = "Comprehension: Correct Answers Only, No Wrong (KEA02)",
  # KEA07: Moral emotions components
  "moral_emo_fairness_Z"    = "Moral Emotions: Fairness (Arbitrary vs Fair) (KEA07_1)",
  "moral_emo_protection_Z"  = "Moral Emotions: Protection (Sacrificed vs Protected) (KEA07_2)",
  "moral_emo_calm_Z"        = "Moral Emotions: Calm (Angry vs Calm) (KEA07_3)",
  "moral_emo_happy_Z"       = "Moral Emotions: Happiness (Sad vs Happy) (KEA07_4)",
  "moral_emo_grateful_Z"    = "Moral Emotions: Gratitude (Disappointed vs Grateful) (KEA07_5)",
  "moral_emo_index"         = "Moral Emotions Index (KEA07, all items)",
  # KEA08: Politicized identity components
  "politicized_id1_Z"    = "Politicized Identity: Support Accelerating Transition (KEA08_1)",
  "politicized_id2_Z"    = "Politicized Identity: Urge Government to Act Faster (KEA08_2)",
  "politicized_id_index" = "Politicized Identity Index (KEA08, all items)",
  # KEA09/KEA10: Collective efficacy — index only (no separate components)
  "colleff_index"        = "Collective Efficacy Index (KEA09-10)"
)

sample_labels <- c("Full sample", "Attentive only")

for (s in sample_labels) {
  for (out in names(outcome_titles)) {
    p <- plot_outcome_results(all_results, out, s, outcome_titles[out])
    clean_s   <- gsub(" ", "_", tolower(s))
    clean_out <- gsub("_Z", "", out)
    ggsave(
      file.path(fig, paste0("plot_im_", clean_out, "_", clean_s, ".png")),
      plot = p, width = 9, height = 5.5, dpi = 300
    )
  }
}

### THE END — ALL IM OUTCOMES (KEA02, KEA07, KEA08, KEA09/KEA10) ###
