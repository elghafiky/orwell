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


#--- 3. DEFINE FUNCTIONS ---
model_1 <- function(clean_data, outcome, treatment_vars) {
  formula <- as.formula(paste(outcome, "~", paste(treatment_vars, collapse = "+")))
  result <- lm_robust(formula, data = clean_data, se_type = "HC2")
  return(tidy(result))
}

outcomes <- c("EK01", "EK02", "EK03", "EK05")

# --- 4A. RUN MODEL 1 - POOLED T VS CONTROL ---
results_pooled_m1 <- map_dfr(outcomes, function(y) {
  model_1(clean_data, y, "Pooled_T") %>%
    mutate(outcome = y, comparison = "Pooled vs Control", model = "Model 1")
})

View(results_pooled_m1)

# --- 4B. RUN MODEL 1 - JOINED T VS CONTROL ---

results_joint_m1 <- map_dfr(outcomes, function(y) {
  model_1(clean_data, y, c("T1", "T2")) %>%
    mutate(outcome = y, comparison = "T1 T2 vs Control", model = "Model 1")
})

View(results_joint_m1)

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
    mutate(
      label = paste0(term, " ", stars),
      term = factor(term, levels = c("Pooled_T", "T1", "T2"))
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
    facet_wrap(~comparison, ncol = 1) +
    labs(
      title = title,
      x = "Treatment Effect (OLS coefficient)",
      y = "",
      color = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# --- 15. GENERATE FIGURES ---
fig_EK01 <- plot_coef(all_results, "EK01", "Donation Willingness (EK01)")
fig_EK02 <- plot_coef(all_results, "EK02", "Donation Amount (EK02)")
fig_EK03 <- plot_coef(all_results, "EK03", "Petition Signing (EK03)")
fig_EK05 <- plot_coef(all_results, "EK05", "Information Seeking (EK05)")

# save figures
ggsave(file.path(fig, "fig_EK01.png"), fig_EK01, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK02.png"), fig_EK02, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK03.png"), fig_EK03, width = 10, height = 8, dpi = 300)
ggsave(file.path(fig, "fig_EK05.png"), fig_EK05, width = 10, height = 8, dpi = 300)

#########THIS IS THE END##################



