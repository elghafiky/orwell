##### ORWELL 2 — GREEN BAROMETER - PHASE 1 MODEL 1 & 2 ITT ####
# Goal: Main treatment effect (ITT, OLS)
# Runs on: Full sample + Attentive respondents only
# Last updated: Final version per Fiky + Armand alignment

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

# --- 2.5 BUILD BEHAVIORAL ENGAGEMENT INDEX ---
# Built on FULL sample first, standardized using full control group
# Each subsample analysis will rebuild the index using its own control group
build_index <- function(data) {
  control_data <- data %>% filter(Pooled_T == 0)
  index_vars   <- c("EK01", "EK02", "EK03", "EK04", "EK05")
  
  for (var in index_vars) {
    raw_vector     <- data[[var]]
    control_vector <- control_data[[var]]
    
    if (var == "EK04") {
      raw_vector[is.na(raw_vector)]         <- 0
      control_vector[is.na(control_vector)] <- 0
    }
    
    c_mean <- mean(control_vector, na.rm = TRUE)
    c_sd   <- sd(control_vector,   na.rm = TRUE)
    data[[paste0(var, "_Z")]] <- (raw_vector - c_mean) / c_sd
  }
  
  z_cols <- paste0(index_vars, "_Z")
  data <- data %>%
    mutate(Behavioral_Index = rowMeans(select(., all_of(z_cols)), na.rm = FALSE))
  
  return(data)
}


# --- 3. DEFINE MODEL FUNCTIONS ---
# Model 1: treatment dummies only (per Fiky)
model_1 <- function(data, outcome, treatment_vars) {
  formula <- as.formula(paste(outcome, "~", paste(treatment_vars, collapse = "+")))
  result  <- lm_robust(formula, data = data, se_type = "HC2")
  return(tidy(result))
}

# LASSO variable selection (per Belloni et al. 2014)
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

# Model 2: LASSO covariate-adjusted (per Fiky — no forced strata)
model_2 <- function(data, outcome, treatment_vars, covariates) {
  selected_vars <- select_lasso_vars(data, outcome, treatment_vars, covariates)
  all_vars      <- c(treatment_vars, selected_vars)
  formula       <- as.formula(paste(outcome, "~", paste(all_vars, collapse = "+")))
  result        <- lm_robust(formula, data = data, se_type = "HC2")
  return(tidy(result))
}


# --- 4. DEFINE COVARIATES ---
# urban included as LASSO candidate (per Fiky — not forced)
demo_vars <- c("age_18_40", "male", "hsgrad",
               "working", "studying", "housekeeping",
               "jobseeking", "unemployed", "retired",
               "low_expenditure", "climate_aware",
               "energy_transition_aware",
               "urban",
               "region_wib", "region_wita", "region_wit")

outcomes <- c("EK01", "EK02", "EK03", "EK04", "EK05", "Behavioral_Index")


# --- 5. MAIN ANALYSIS FUNCTION ---
# Runs full ITT analysis on any dataframe passed to it
run_itt <- function(data, sample_label) {
  
  cat("\nRunning ITT for sample:", sample_label, "| N =", nrow(data), "\n")
  
  # Build index within this sample's control group
  data <- build_index(data)
  
  # 5A. Model 1 — Pooled vs Control
  res_pooled_m1 <- map_dfr(outcomes, function(y) {
    model_1(data, y, "Pooled_T") %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
  })
  
  # 5B. Model 1 — Joint T1/T2 vs Control + T1 vs T2
  res_joint_m1 <- map_dfr(outcomes, function(y) {
    
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
  
  # 5C. Model 2 — Pooled vs Control
  res_pooled_m2 <- map_dfr(outcomes, function(y) {
    model_2(data, y, "Pooled_T", demo_vars) %>%
      mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 2")
  })
  
  # 5D. Model 2 — Joint T1/T2 vs Control + T1 vs T2
  res_joint_m2 <- map_dfr(outcomes, function(y) {
    
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
  
  # Combine and tag with sample label
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


# --- 7. ANDERSON SHARPENED Q-VALUES (BKY 2006) ---
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


# --- 8. APPLY Q-VALUES ---
# Per Armand: one family (EK01-EK05), grouped by regression equation x model x sample
# BEI and T1 vs T2: unadjusted p-values only

component_outcomes <- c("EK01", "EK02", "EK03", "EK04", "EK05")

# Equation 1 (Pooled): Anderson per model x sample
results_eq1 <- all_results %>%
  filter(outcome %in% component_outcomes, term == "Pooled_T") %>%
  mutate(family = "Primary behavioral") %>%
  group_by(model, sample) %>%
  mutate(q_value = anderson_qvalue(p.value)) %>%
  ungroup()

# Equation 2 (Joint): Anderson per model x sample, T1 and T2 vs C only
results_eq2 <- all_results %>%
  filter(outcome %in% component_outcomes, term %in% c("T1", "T2")) %>%
  mutate(family = "Primary behavioral") %>%
  group_by(model, sample) %>%
  mutate(q_value = anderson_qvalue(p.value)) %>%
  ungroup()

# T1 vs T2: unadjusted
results_t1t2 <- all_results %>%
  filter(outcome %in% component_outcomes, term == "T1_vs_T2") %>%
  mutate(family = "Primary behavioral", q_value = p.value)

# BEI: unadjusted
results_bei <- all_results %>%
  filter(outcome == "Behavioral_Index") %>%
  mutate(family = "Behavioral engagement index", q_value = p.value)

all_results <- bind_rows(results_eq1, results_eq2, results_t1t2, results_bei)


# --- 9. SIGNIFICANCE STARS ---
all_results <- all_results %>%
  mutate(stars = case_when(
    q_value < 0.01 ~ "***",
    q_value < 0.05 ~ "**",
    q_value < 0.10 ~ "*",
    TRUE ~ ""
  ))


# --- 10. EXPORT ---
write_xlsx(all_results, file.path(tbl, "gb_rct_phase1_results_final.xlsx"))
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
        "Anderson (2008) sharpened q-values: one family (EK01-EK05), grouped by equation x model x sample.\n",
        "T1 vs T2 and Behavioral Engagement Index reported with unadjusted p-values.\n",
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


# --- 12. EXPORT PLOTS ---
outcome_titles <- c(
  "EK01"             = "Willingness to Donate",
  "EK02"             = "High Donator Dummy (>= IDR 15000)",
  "EK03"             = "Willingness to Sign Energy Transition Petition",
  "EK04"             = "Willingness to Disclose Identity on Petition",
  "EK05"             = "Information Seeking Behavior (Link Clicks)",
  "Behavioral_Index" = "Overall Behavioral Engagement Index (Summary Index)"
)

sample_labels <- c("Full sample", "Attentive only")

for (s in sample_labels) {
  for (out in names(outcome_titles)) {
    p <- plot_outcome_results(all_results, out, s, outcome_titles[out])
    clean_s <- gsub(" ", "_", tolower(s))
    ggsave(
      file.path(fig, paste0("plot_phase1_", out, "_", clean_s, ".png")),
      plot = p, width = 9, height = 5.5, dpi = 300
    )
  }
}

### THE END ###