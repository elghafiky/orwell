##### ORWELL 2 — GREEN BAROMETER - PHASE 1 MODEL 1 & 2 ITT ####
# Goal: Main treatment effect (ITT, OLS) 

############## JOINT REGRESSION

graphics.off(); rm(list=ls()); cat("\014");
pacman::p_load(tidyverse, data.table, estimatr, broom, glmnet, purrr, writexl, car, sandwich)

# --- 1. SETUP DIRECTORIES ---
base_dir <- "G:/"
master <- file.path(base_dir, "Shared drives", "Projects", "2026", "Orwell 2", 
                    "Breadcrumbs", "4. Green Barometer", "10 Data RCT")

setwd(master)
opt = file.path(getwd(), "1c output")
tbl = file.path(getwd(), "4 tables")
if (!dir.exists(tbl)) dir.create(tbl, recursive = TRUE)

# --- 2. LOAD RAW DATA & RENAME VARIABLES ---
clean_data <- readRDS(file.path(opt, "gb_rct_clean.rds"))

# Merapikan nama kolom yang terlalu panjang
clean_data <- clean_data %>%
  rename(
    EK02a = `EK02a Apakah Anda mau mencantumkan nama Anda sebagai salah satu donatur?`,
    EK02b = `EK02b Apakah Anda bersedia dihubungi kembali untuk dikirimkan bukti donasi?`,
    EK04  = `EK04 Apakah Anda mencantumkan identitas Anda pada petisi tersebut?`
  )

# --- 2.5 BUILD ANDERSON SUMMARY INDEX ---
# Goal: Standardize using Control Group mean/SD and average components

# 1. Isolate the control group to get the baseline mean and SD
control_data <- clean_data %>% filter(Pooled_T == 0)

# 2. Define the components of the Behavioral Engagement Index
index_vars <- c("EK01", "EK02", "EK02a", "EK02b", "EK03", "EK04", "EK05")

# 3. Create Z-scores for each variable using Control Mean & SD
for (var in index_vars) {
  # Calculate control mean and sd for this specific variable
  c_mean <- mean(control_data[[var]], na.rm = TRUE)
  c_sd <- sd(control_data[[var]], na.rm = TRUE)
  
  # Create a new column with the Z-score for the entire dataset
  z_col_name <- paste0(var, "_Z")
  clean_data[[z_col_name]] <- (clean_data[[var]] - c_mean) / c_sd
}

# 4. Average the Z-scores to create the final index
z_cols <- paste0(index_vars, "_Z")
clean_data <- clean_data %>%
  mutate(Behavioral_Index = rowMeans(select(., all_of(z_cols)), na.rm = TRUE))


# --- 3. DEFINE FUNCTIONS & OUTCOMES ---
model_1 <- function(clean_data, outcome, treatment_vars) {
  formula <- as.formula(paste(outcome, "~", paste(treatment_vars, collapse = "+")))
  result <- lm_robust(formula, data = clean_data, se_type = "HC2")
  return(tidy(result))
}

outcomes <- c("EK01", "EK02", "EK02a", "EK02b", "EK03", "EK04", "EK05", "Behavioral_Index")


# --- 4A. RUN MODEL 1 - POOLED T VS CONTROL ---
results_pooled_m1 <- map_dfr(outcomes, function(y) {
  model_1(clean_data, y, "Pooled_T") %>%
    mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
})

# --- 4B. RUN MODEL 1 - JOINED T VS CONTROL & T1 VS T2 ---
results_joint_m1 <- map_dfr(outcomes, function(y) {
  
  # 1. Run standard joint model
  res_tidy <- model_1(clean_data, y, c("T1", "T2")) %>%
    mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 1")
  
  # 2. Run T1 vs T2 linear hypothesis using base lm for car::linearHypothesis
  fit <- lm(as.formula(paste(y, "~ T1 + T2")), data = clean_data)
  vcv <- vcovHC(fit, type = "HC2")
  ht <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
  
 
  # Hitung SE dan 95% CI yang presisi untuk selisih T1 - T2
  vcv <- vcovHC(fit, type = "HC2")
  se_diff <- sqrt(vcv["T1", "T1"] + vcv["T2", "T2"] - 2 * vcv["T1", "T2"])
  est_diff <- coef(fit)["T1"] - coef(fit)["T2"]
  
  # 3. Create a row for the T1 vs T2 test
  t1_t2_row <- tibble(
    term = "T1_vs_T2",
    estimate = est_diff,
    std.error = se_diff, 
    statistic = ht[2, "F"], 
    p.value = ht[2, "Pr(>F)"],
    conf.low = est_diff - 1.96 * se_diff, 
    conf.high = est_diff + 1.96 * se_diff, 
    df = ht[2, "Df"],
    outcome = y, comparison = "T1 vs T2", model = "Model 1" 
  )
  
  
  bind_rows(res_tidy, t1_t2_row)
})


# --- 5. DEFINE COVARIATES ---
demo_vars <- c("age_18_40", "male", "hsgrad", 
               "working", "studying", "housekeeping", 
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware", 
               "energy_transition_aware", "urban",
               "region_wib", "region_wita", "region_wit")


# --- 6. LASSO VARIABLE SELECTION (UPDATED) ---
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
    d <- as.numeric(dsub[[t_var]])
    fit_d <- cv.glmnet(X, d, alpha = 1)
    sel_temp <- rownames(coef(fit_d, s = "lambda.1se"))[as.vector(coef(fit_d, s = "lambda.1se") != 0)]
    sel_d <- c(sel_d, sel_temp)
  }
  
  selected <- setdiff(union(sel_y, unique(sel_d)), "(Intercept)")
  return(selected)
}

model_2 <- function(clean_data, outcome, treatment_vars, covariates) {
  
  selected_vars <- select_lasso_vars(clean_data, outcome, treatment_vars, covariates)
  all_vars <- c(treatment_vars, selected_vars)
  formula <- as.formula(paste(outcome, "~", paste(all_vars, collapse = "+")))
  result <- lm_robust(formula, data = clean_data, se_type = "HC2")
  return(tidy(result))
}


# --- 7. RUN MODEL 2 - POOLED T VS CONTROL & JOINED T VS CONTROL ---
results_pooled_m2 <- map_dfr(outcomes, function(y) {
  model_2(clean_data, y, "Pooled_T", demo_vars) %>%
    mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 2")
})

results_joint_m2 <- map_dfr(outcomes, function(y) {
  
  # 1. Pilih variabel kontrol via LASSO menggunakan kedua treatment sekaligus
  selected_vars <- select_lasso_vars(clean_data, y, c("T1", "T2"), demo_vars)
  all_vars <- c("T1", "T2", selected_vars)
  formula_str <- paste(y, "~", paste(all_vars, collapse = "+"))
  
  # 2. Jalankan regresi utama Model 2 (Joint)
  res_tidy <- lm_robust(as.formula(formula_str), data = clean_data, se_type = "HC2") %>%
    tidy() %>%
    mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 2")
  
  # 3. Hitung uji hipotesis T1 = T2 menggunakan basic lm untuk car::linearHypothesis
  fit <- lm(as.formula(formula_str), data = clean_data)
  vcv <- vcovHC(fit, type = "HC2")
  ht <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
  
  # 4. Hitung SE dan 95% CI yang presisi untuk selisih T1 - T2 (HC2 Standard Error)
  se_diff <- sqrt(vcv["T1", "T1"] + vcv["T2", "T2"] - 2 * vcv["T1", "T2"])
  est_diff <- coef(fit)["T1"] - coef(fit)["T2"]
  
  # 5. Buat baris data komplit dengan label "Model 2" 
  t1_t2_row <- tibble(
    term = "T1_vs_T2",
    estimate = est_diff,
    std.error = se_diff, 
    statistic = ht[2, "F"], 
    p.value = ht[2, "Pr(>F)"],
    conf.low = est_diff - 1.96 * se_diff, 
    conf.high = est_diff + 1.96 * se_diff, 
    df = ht[2, "Df"],
    outcome = y, comparison = "T1 vs T2", model = "Model 2" 
  )
  
  bind_rows(res_tidy, t1_t2_row)
})

# --- 8. COMBINE ALL RESULTS ---
all_results <- bind_rows(
  results_pooled_m1,
  results_pooled_m2,
  results_joint_m1,
  results_joint_m2
) %>%
  filter(term %in% c("Pooled_T", "T1", "T2", "T1_vs_T2"))


# --- 9. OFFICIAL ANDERSON SHARPENED Q-VALUES (BKY 2006) ---
anderson_qvalue <- function(pval) {
  valid_idx <- !is.na(pval)
  p_valid <- pval[valid_idx]
  
  if (length(p_valid) <= 1) return(pval)
  
  totalpvals <- length(p_valid)
  rank_val <- rank(p_valid)
  
  qval <- 1
  bky06_qval <- rep(1, totalpvals)
  
  while (qval > 0) {
    qval_adj <- qval / (1 + qval)
    fdr_temp1 <- qval_adj * rank_val / totalpvals
    reject_temp1 <- ifelse(fdr_temp1 >= p_valid, 1, 0)
    reject_rank1 <- reject_temp1 * rank_val
    total_rejected1 <- max(reject_rank1, na.rm = TRUE)
    
    qval_2st <- qval_adj * (totalpvals / (totalpvals - total_rejected1))
    fdr_temp2 <- qval_2st * rank_val / totalpvals
    reject_temp2 <- ifelse(fdr_temp2 >= p_valid, 1, 0)
    reject_rank2 <- reject_temp2 * rank_val
    total_rejected2 <- max(reject_rank2, na.rm = TRUE)
    
    bky06_qval[rank_val <= total_rejected2] <- qval
    
    qval <- qval - 0.001
  }
  
  out <- rep(NA_real_, length(pval))
  out[valid_idx] <- bky06_qval
  return(out)
}


# --- 10. APPLY Q-VALUES WITHIN FAMILIES ---
all_results <- all_results %>%
  mutate(family = case_when(
    outcome %in% c("EK01", "EK02", "EK02a", "EK02b") ~ "Donation amount",
    outcome %in% c("EK03", "EK04")                   ~ "Petition signing",
    outcome == "EK05"                                ~ "Information seeking",
    outcome == "Behavioral_Index"                    ~ "Behavioral engagement index",
    TRUE ~ "Other"
  )) %>%
  group_by(family, term, model) %>%
  mutate(q_value = anderson_qvalue(p.value)) %>%
  ungroup()


# --- 11. ADD SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))

# --- 12. EXPORT FINAL RESULTS ---
write_xlsx(all_results, file.path(tbl, "gb_rct_phase1_results.xlsx"))

View(all_results)


# --- 13. INDIVIDUAL OUTCOME PLOT FUNCTION ---
plot_outcome_results <- function(data, outcome_var, title_text) {
  
  # Filter dan rapikan label visualisasi
  plot_data <- data %>%
    filter(outcome == outcome_var) %>%
    mutate(
      plot_label = case_when(
        term == "Pooled_T" ~ "Pooled vs Control",
        term == "T1"       ~ "T1 (State) vs Control",
        term == "T2"       ~ "T2 (Self) vs Control",
        term == "T1_vs_T2" ~ "T1 vs T2 (Direct Comparison)"
      ),
      plot_label = factor(plot_label, levels = c(
        "T1 vs T2 (Direct Comparison)",
        "T2 (Self) vs Control",
        "T1 (State) vs Control",
        "Pooled vs Control"
      ))
    )
  
  ggplot(plot_data, aes(x = estimate, y = plot_label, color = model, group = model)) +
    # Garis bantu vertikal di titik nol (no effect)
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.8) +
    # Menampilkan 95% Confidence Interval horisontal
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(0.4),
      height = 0.15,
      linewidth = 0.8
    ) +
    # Menampilkan titik koefisien estimate
    geom_point(position = position_dodge(0.4), size = 3.5) +
    # Menampilkan bintang signifikansi Anderson q-value di ujung kanan garis error
    geom_text(
      aes(label = stars, x = ifelse(is.na(conf.high), estimate, conf.high)),
      position = position_dodge(0.4),
      hjust = -0.3,
      vjust = 0.3,
      color = "black",
      size = 5,
      show.legend = FALSE
    ) +
    labs(
      title = title_text,
      subtitle = paste("Outcome Variable Code:", outcome_var),
      x = "Effect Size (OLS Coefficient Estimate with 95% CIs)",
      y = NULL,
      color = "Specification",
      caption = "* q < 0.10, ** q < 0.05, *** q < 0.01\nNote: Significance stars are strictly adjusted using the Anderson (2008) sharpened q-value procedure within family."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.justification = "left",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.caption = element_text(hjust = 0, color = "gray30", size = 9, lineheight = 1.2),
      axis.text.y = element_text(face = "bold", color = "black")
    ) +
    scale_color_manual(values = c("Model 1" = "#1f77b4", "Model 2" = "#ff7f0e"))
}

# --- 14. DICTIONARY TITLE DAN AUTOMATIC EXPORT ---
outcome_titles <- c(
  "EK01"             = "Willingness to Donate",
  "EK02"             = "Donation Amount (Continuous)",
  "EK02a"            = "Willingness to Disclose Name as Donator",
  "EK02b"            = "Willingness to be Re-contacted for Receipt",
  "EK03"             = "Willingness to Sign Energy Transition Petition",
  "EK04"             = "Willingness to Disclose Identity on Petition",
  "EK05"             = "Information Seeking Behavior (Link Clicks)",
  "Behavioral_Index" = "Overall Behavioral Engagement Index (Summary Index)"
)

# Jalankan looping untuk membuat dan menyimpan ke-8 grafik secara otomatis
for (out in names(outcome_titles)) {
  p <- plot_outcome_results(all_results, out, outcome_titles[out])
  
  # Menyimpan ke direktori figur dengan resolusi tinggi (300 DPI) siap cetak/presentasi
  ggsave(file.path(fig, paste0("plot_phase1_", out, ".png")), 
         plot = p, width = 9, height = 5.5, dpi = 300)
}


###THE END##############


##### ORWELL 2 — GREEN BAROMETER - PHASE 1 MODEL 1 ITT ####
# Goal: Main treatment effect (ITT, OLS) 

############## JOINT REGRESSION

##### ORWELL 2 — PHASE 1 JOINT ITT (FINAL) #####
# Goal: Main treatment effect (ITT, OLS) with Y ~ T1 + T2 joint regression
# Equality test: linearHypothesis(fit, "T1 = T2") on same model object
# Equivalent to Stata: reg Y T1 T2, robust + test T1=T2

graphics.off(); rm(list=ls()); cat("\14");
pacman::p_load(tidyverse, data.table, estimatr, broom, glmnet, purrr, writexl, car, sandwich)

# --- 1. SETUP DIRECTORIES ---
base_dir <- "G:/"
master <- file.path(base_dir, "Shared drives", "Projects", "2026", "Orwell 2", 
                    "Breadcrumbs", "4. Green Barometer", "10 Data RCT")

setwd(master)
opt = file.path(getwd(), "1c output")
tbl = file.path(getwd(), "4 tables")
fig <- file.path(master, "3 figures")
if (!dir.exists(tbl)) dir.create(tbl, recursive = TRUE)

# --- 2. LOAD RAW DATA ---
clean_data <- readRDS(file.path(opt, "gb_rct_clean.rds"))

# --- 2.5 BUILD ANDERSON SUMMARY INDEX ---
# Goal: Standardize using Control Group mean/SD and average components

# 1. Isolate the control group to get the baseline mean and SD
control_data <- clean_data %>% filter(Pooled_T == 0)

# 2. Define the components of the Behavioral Engagement Index
# Note: Ensure these are already coded so higher values = pro-environmental
index_vars <- c("EK01", "EK02", "EK03", "EK05")

# 3. Create Z-scores for each variable using Control Mean & SD
for (var in index_vars) {
  # Calculate control mean and sd for this specific variable
  c_mean <- mean(control_data[[var]], na.rm = TRUE)
  c_sd <- sd(control_data[[var]], na.rm = TRUE)
  
  # Create a new column with the Z-score for the entire dataset
  z_col_name <- paste0(var, "_Z")
  clean_data[[z_col_name]] <- (clean_data[[var]] - c_mean) / c_sd
}

# 4. Average the Z-scores to create the final index
z_cols <- paste0(index_vars, "_Z")
clean_data <- clean_data %>%
  mutate(Behavioral_Index = rowMeans(select(., all_of(z_cols)), na.rm = TRUE))

# --- 3. DEFINE FUNCTIONS & OUTCOMES ---
model_1 <- function(clean_data, outcome, treatment_vars) {
  formula <- as.formula(paste(outcome, "~", paste(treatment_vars, collapse = "+")))
  result <- lm_robust(formula, data = clean_data, se_type = "HC2")
  return(tidy(result))
}

outcomes <- c("EK01", "EK02", "EK02b", "EK03", "EK04", "EK05", "Behavioral_Index")

# --- 4A. RUN MODEL 1 - POOLED T VS CONTROL ---
results_pooled_m1 <- map_dfr(outcomes, function(y) {
  model_1(clean_data, y, "Pooled_T") %>%
    mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
})

View(results_pooled_m1)

# --- 4B. RUN MODEL 1 - JOINED T VS CONTROL & T1 VS T2 ---
results_joint_m1 <- map_dfr(outcomes, function(y) {
  
  # 1. Run standard joint model
  res_tidy <- model_1(clean_data, y, c("T1", "T2")) %>%
    mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 1")
  
  # 2. Run T1 vs T2 linear hypothesis using base lm for car::linearHypothesis
  fit <- lm(as.formula(paste(y, "~ T1 + T2")), data = clean_data)
  vcv <- vcovHC(fit, type = "HC2")
  ht <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
  
  # 3. Create a row for the T1 vs T2 test
  t1_t2_row <- tibble(
    term = "T1_vs_T2",
    estimate = coef(fit)["T1"] - coef(fit)["T2"], # Difference in coefficients
    std.error = NA, statistic = NA, 
    p.value = ht[2, "Pr(>F)"],
    conf.low = NA, conf.high = NA, df = NA,
    outcome = y, comparison = "T1 vs T2", model = "Model 1"
  )
  
  # Bind them together
  bind_rows(res_tidy, t1_t2_row)
})

# --- 5. DEFINE COVARIATES ---
demo_vars <- c("age_18_40", "male", "hsgrad", 
               "working", "studying", "housekeeping", 
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware", 
               "energy_transition_aware", "urban",
               "region_wib", "region_wita", "region_wit")

# --- 6. LASSO VARIABLE SELECTION ---
select_lasso_vars <- function(data, outcome, treatment_var, covariates) {
  
  # remove rows with missing values
  dsub <- data %>% 
    select(all_of(c(outcome, treatment_var, covariates))) %>% 
    drop_na() %>% 
    as.data.frame()
  
  # define y (outcome) and d (treatment) and X (covariates matrix)
  y <- as.numeric(dsub[[outcome]])
  d <- as.numeric(dsub[[treatment_var]])
  X <- model.matrix(as.formula(paste("~", paste(covariates, collapse = "+"), "-1")), data = dsub)
  
  # run LASSO twice
  fit_y <- cv.glmnet(X, y, alpha = 1)  # LASSO on outcome
  fit_d <- cv.glmnet(X, d, alpha = 1)  # LASSO on treatment
  
  # get selected variables from both
  sel_y <- rownames(coef(fit_y, s = "lambda.1se"))[as.vector(coef(fit_y, s = "lambda.1se") != 0)]
  sel_d <- rownames(coef(fit_d, s = "lambda.1se"))[as.vector(coef(fit_d, s = "lambda.1se") != 0)]
  
  # take union and remove intercept
  selected <- setdiff(union(sel_y, sel_d), "(Intercept)")
  
  return(selected)
}

model_2 <- function(clean_data, outcome, treatment_vars, covariates) {
  
  # step 1: select covariates via LASSO
  selected_vars <- select_lasso_vars(clean_data, outcome, treatment_vars[1], covariates)
  
  # step 2: build formula with treatment + selected covariates
  all_vars <- c(treatment_vars, selected_vars)
  formula <- as.formula(paste(outcome, "~", paste(all_vars, collapse = "+")))
  
  # step 3: run regression
  result <- lm_robust(formula, data = clean_data, se_type = "HC2")
  
  return(tidy(result))
}

# --- 7. RUN MODEL 2 - POOLED T VS CONTROL ---
results_pooled_m2 <- map_dfr(outcomes, function(y) {
  model_2(clean_data, y, "Pooled_T", demo_vars) %>%
    mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 2")
})

View(results_pooled_m2)

results_joint_m2 <- map_dfr(outcomes, function(y) {
  model_2(clean_data, y, c("T1", "T2"), demo_vars) %>%
    mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 2")
})

View(results_joint_m2)

# --- 8. COMBINE ALL RESULTS ---
all_results <- bind_rows(
  results_pooled_m1,
  results_pooled_m2,
  results_joint_m1,
  results_joint_m2
) %>%
  filter(term %in% c("Pooled_T", "T1", "T2"))


# --- 9. T1 VS T2 TEST ---
t1_vs_t2 <- map_dfr(outcomes, function(y) {
  
  # run the joint regression first
  formula <- as.formula(paste(y, "~ T1 + T2"))
  fit <- lm(formula, data = clean_data)
  vcv <- vcovHC(fit, type = "HC2")
  
  # test if T1 = T2
  ht <- linearHypothesis(fit, "T1 = T2", vcov = vcv)
  
  # extract results
  tibble(
    outcome = y,
    comparison = "T1 vs T2",
    F_stat = ht[2, "F"],
    p_value = ht[2, "Pr(>F)"]
  )
})

View(t1_vs_t2)

# --- 10. ANDERSON Q-VALUES ---
anderson_qvalue <- function(pvals) {
  n <- length(pvals)
  ranked <- rank(pvals, ties.method = "first")
  qvals <- pmin(1, pvals * n / ranked)
  # enforce monotonicity
  qvals <- cummax(qvals[order(ranked)])[order(order(ranked))]
  return(qvals)
}

# --- 11. APPLY Q-VALUES ---
all_results <- all_results %>%
  mutate(family = case_when(
    outcome %in% c("EK01", "EK02") ~ "donation",
    outcome == "EK03"              ~ "petition",
    outcome == "EK05"              ~ "info_seeking"
  )) %>%
  group_by(family, term, comparison, model) %>%
  mutate(q_value = anderson_qvalue(p.value)) %>%
  ungroup()

View(all_results)

# --- 12. EXPORT FINAL RESULTS ---
write_xlsx(
  list(
    "regression results" = all_results,
    "t1 vs t2" = t1_vs_t2
  ),
  file.path(tbl, "gb_rct_phase1_results.xlsx")
)

# --- 13. ADD SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))

# --- 14. PLOT FUNCTION ---
plot_coef <- function(data, outcome_var, title) {
  
  plot_data <- data %>%
    filter(outcome == outcome_var) %>%
    filter(!(term == "Pooled_T" & comparison == "T1 T2 vs Control")) %>%
    filter(!(term %in% c("T1", "T2") & comparison == "Pooled vs Control")) %>%
    mutate(
      label = paste0(term, " ", stars),
      term = factor(term, levels = c("Pooled_T", "T1", "T2"), exclude = NULL)
    )
  
  ggplot(plot_data, aes(x = estimate, y = term, color = model)) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(0.5),
      height = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_text(
      aes(x = conf.high + 0.01, label = stars),
      position = position_dodge(0.5),
      hjust = 0, size = 4
    ) +
    facet_wrap(~comparison, ncol = 1, scales = "free_y") +
    labs(
      title = title,
      x = "Treatment Effect (OLS coefficient)",
      y = "",
      color = "Model",
      caption = "* q < 0.10   ** q < 0.05   *** q < 0.01\nBars show 95% confidence intervals. Stars based on Anderson sharpened q-values."
    ) +
    theme_minimal() +
    scale_y_discrete(drop = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left",
      plot.caption = element_text(hjust = 0, size = 9, color = "gray40")
    )
}


# --- 15. GENERATE FIGURES ---
fig_EK01 <- plot_coef(all_results, "EK01", "Donation Willingness (EK01)")
fig_EK02 <- plot_coef(all_results, "EK02", "Donation Amount (EK02)")
fig_EK03 <- plot_coef(all_results, "EK03", "Petition Signing (EK03)")
fig_EK05 <- plot_coef(all_results, "EK05", "Information Seeking (EK05)")

# save figures
ggsave(file.path(fig, "fig_EK01_3.png"), fig_EK01, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK02_3.png"), fig_EK02, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK03_3.png"), fig_EK03, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK05_3.png"), fig_EK05, width = 10, height = 8, dpi = 300)

# --- SWAYABLE STYLE PLOT ---
library(forcats)

# define significance colors and shapes (same as swayable)
sig_colors <- c("***" = "#d7191c", "**" = "#fdae61", "*" = "#2b83ba", "ns" = "grey70")
sig_shapes <- c("***" = 18, "**" = 17, "*" = 16, "ns" = 4)

# prep plot data
swayable_plot_data <- all_results %>%
  mutate(
    significance = case_when(
      q_value < 0.01 ~ "***",
      q_value < 0.05 ~ "**",
      q_value < 0.10 ~ "*",
      TRUE ~ "ns"
    ),
    outcome = factor(outcome, levels = c("EK01", "EK02", "EK03", "EK05")),
    label = paste(outcome, "|", comparison, "|", term, "|", model)
  ) %>%
  arrange(outcome, term, model) %>%
  group_by(outcome) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  mutate(
    outcome_id = as.numeric(outcome),
    y_position = (outcome_id - 1) * 10 + row_id
  )

sep_positions <- unique(swayable_plot_data$outcome_id * 10)

p_swayable <- ggplot(swayable_plot_data,
                     aes(x = estimate, y = y_position,
                         color = significance, shape = significance)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.7) +
  geom_point(size = 3) +
  geom_hline(yintercept = sep_positions, color = "grey85", linewidth = 0.6) +
  scale_color_manual(values = sig_colors, name = "Anderson q-value") +
  scale_shape_manual(values = sig_shapes, name = "Anderson q-value") +
  scale_y_continuous(
    breaks = swayable_plot_data$y_position,
    labels = swayable_plot_data$label
  ) +
  theme_minimal() +
  labs(
    title    = "GB RCT Phase 1 — Main Treatment Effects",
    subtitle = "Error bars = 95% CI. Color/shape = Anderson sharpened q-value significance.",
    x        = "Treatment Effect Estimate (95% CI)",
    y        = NULL,
    caption  = "* q < 0.10   ** q < 0.05   *** q < 0.01"
  ) +
  theme(
    axis.text.y      = element_text(size = 8),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

calc_height <- (max(swayable_plot_data$y_position) * 0.32) + 2

ggsave(file.path(fig, "fig_swayable_style.png"),
       plot = p_swayable, width = 12, height = calc_height,
       units = "in", limitsize = FALSE)

#########THIS IS THE END##################



