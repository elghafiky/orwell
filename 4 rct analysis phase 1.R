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


#########THIS IS THE END##################

# --- 2. DEFINE FUNCTIONS ---

run_pds_joint <- function(df, y_var, demo_vars) {
  dsub <- df %>% select(all_of(c(y_var, "T1", "T2", demo_vars))) %>%
    drop_na() %>% as.data.frame()
  y <- as.numeric(dsub[[y_var]])
  d <- as.numeric(dsub[["T1"]])
  
  if (length(unique(y)) < 2) return(list(selected_union = c(), vars_string = ""))
  
  X     <- model.matrix(as.formula(paste0("~", paste(demo_vars, collapse="+"), "-1")), data=dsub)
  fit_y <- cv.glmnet(X, y, alpha=1)
  fit_d <- cv.glmnet(X, d, alpha=1)
  sel_y <- rownames(coef(fit_y, s="lambda.1se"))[as.vector(coef(fit_y, s="lambda.1se") != 0)]
  sel_d <- rownames(coef(fit_d, s="lambda.1se"))[as.vector(coef(fit_d, s="lambda.1se") != 0)]
  
  sel_union   <- setdiff(union(sel_y, sel_d), "(Intercept)")
  vars_string <- paste(sel_union, collapse = ", ")
  
  return(list(selected_union = sel_union, vars_string = vars_string))
}

run_robustness_joint <- function(df, sample_label, outcomes, demo_vars, cog_controls) {
  map_dfr(outcomes, function(y_v) {
    
    fit_and_extract <- function(dsub, rhs_vars, sel_vars_string = "None (Raw)", sel_k = NA) {
      
      fml     <- as.formula(paste0(y_v, " ~ ", paste(rhs_vars, collapse = " + ")))
      fit_rob <- lm_robust(fml, data = dsub, se_type = "stata")
      fit_lm  <- lm(fml, data = dsub)
      vcv     <- vcovHC(fit_lm, type = "HC1")
      
      ht <- tryCatch(
        linearHypothesis(fit_lm, "T1 = T2", vcov = vcv),
        error = function(e) NULL
      )
      
      coefs <- tidy(fit_rob) %>%
        filter(term %in% c("T1", "T2")) %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        pivot_wider(
          names_from  = term,
          values_from = c(estimate, std.error, p.value, conf.low, conf.high),
          names_glue  = "{.value}_{term}"
        )
      
      coefs %>%
        mutate(
          diff_T2_minus_T1    = estimate_T2 - estimate_T1,
          selected_vars_names = sel_vars_string,
          selected_k          = sel_k,
          F_stat_equality     = if (!is.null(ht)) ht[2, "F"]      else NA_real_,
          p_value_equality    = if (!is.null(ht)) ht[2, "Pr(>F)"] else NA_real_
        )
    }
    
    # --- Model 1: Raw ---
    dsub_m1 <- df %>% select(all_of(c(y_v, "T1", "T2"))) %>% drop_na()
    m1 <- fit_and_extract(dsub_m1, c("T1", "T2"), "None (Raw)", 0) %>%
      mutate(model = "Model 1")
    
    # --- PDS variable selection ---
    pds       <- run_pds_joint(df, y_v, demo_vars)
    sel_union <- pds$selected_union
    
    # --- Model 2: PDS controls ---
    dsub_m2 <- df %>% select(all_of(c(y_v, "T1", "T2", demo_vars))) %>% drop_na()
    rhs_m2  <- c("T1", "T2", sel_union)
    m2 <- fit_and_extract(dsub_m2, rhs_m2, pds$vars_string, length(sel_union)) %>%
      mutate(model = "Model 2")
    
    # --- Model 3: PDS + cog controls ---
    dsub_m3   <- df %>% select(all_of(c(y_v, "T1", "T2", demo_vars, cog_controls))) %>% drop_na()
    m3_vars   <- unique(c("T1", "T2", sel_union, cog_controls))
    check     <- lm(as.formula(paste(y_v, "~ .")), data = dsub_m3)
    final_rhs <- setdiff(m3_vars, names(coef(check)[is.na(coef(check))]))
    m3 <- fit_and_extract(dsub_m3, final_rhs, pds$vars_string, length(sel_union)) %>%
      mutate(model = "Model 3")
    
    bind_rows(m1, m2, m3) %>%
      mutate(outcome = y_v, sample = sample_label)
  })
}

# --- 3. SETTINGS & DATA ---
analysis_dt <- fread(file.path(opt, "20260210_swayable_alltests_analysis.csv"))

targeted_outcomes <- c("PE_score", "poleff_post_per_a_dummy", "poleff_post_per_b_dummy",
                       "poleff_post_per_c_dummy", "CE_score", "poleff_post_col_a_dummy",
                       "poleff_post_col_b_dummy", "poleff_post_col_c_dummy", "poleff_post_ext_dummy")

global_outcomes <- c("govrolea_dummy", "govroleb_dummy",
                     "fs1_agree", "fs2_agree", "fs3_agree", "fs4_agree", "fs5_agree",
                     "fs6_agree", "fs7_agree", "fs8_agree", "FS_score",
                     "poleff_post_gen_dummy",
                     "polid1_dummy", "polid2_dummy", "polid3_dummy", "polid4_dummy", "polid_score",
                     "cta_dummy")

demo_vars <- c("region2", "region3", "area1", "male", "age", "colgrad", "unmarried",
               "occup1", "occup2", "occup3", "occup4", "occup6", "polpart1",
               "polpart2", "polpart3", "polpart4", "polpart5", "readnews",
               "poleff_pre_int_a_high", "poleff_pre_int_b_high", "poleff_pre_gen_high")

cog_controls <- c("attentive", "sscscore", "intendedaudience")

# --- 4. EXECUTION ---

df_joint_top <- analysis_dt %>%
  filter(testnum %in% c(1, 2) & treatment_group %in% c(0, 1, 2)) %>%
  mutate(T1 = ifelse(treatment_group == 1, 1, 0),
         T2 = ifelse(treatment_group == 2, 1, 0))

df_joint_glo <- analysis_dt %>%
  filter(treatment_group %in% c(0, 1, 2)) %>%
  mutate(T1 = ifelse(treatment_group == 1, 1, 0),
         T2 = ifelse(treatment_group == 2, 1, 0))

df_attn_only      <- analysis_dt %>% filter(attentive == 1)
df_joint_top_attn <- df_attn_only %>%
  filter(testnum %in% c(1, 2) & treatment_group %in% c(0, 1, 2)) %>%
  mutate(T1 = ifelse(treatment_group == 1, 1, 0),
         T2 = ifelse(treatment_group == 2, 1, 0))
df_joint_glo_attn <- df_attn_only %>%
  filter(treatment_group %in% c(0, 1, 2)) %>%
  mutate(T1 = ifelse(treatment_group == 1, 1, 0),
         T2 = ifelse(treatment_group == 2, 1, 0))

res_joint_top      <- run_robustness_joint(df_joint_top,      "Transpo: Joint ITT",
                                           targeted_outcomes, demo_vars, cog_controls)
res_joint_glo      <- run_robustness_joint(df_joint_glo,      "Global: Joint ITT",
                                           global_outcomes,   demo_vars, cog_controls)
res_joint_top_attn <- run_robustness_joint(df_joint_top_attn, "ATTN-ONLY: Transpo Joint ITT",
                                           targeted_outcomes, demo_vars, cog_controls)
res_joint_glo_attn <- run_robustness_joint(df_joint_glo_attn, "ATTN-ONLY: Global Joint ITT",
                                           global_outcomes,   demo_vars, cog_controls)

# --- 5. COMBINE, HOLM ADJUST & EXPORT ---
all_results <- bind_rows(res_joint_top, res_joint_glo,
                         res_joint_top_attn, res_joint_glo_attn) %>%
  mutate(family = case_when(
    outcome %in% c("PE_score", "poleff_post_per_a_dummy", "poleff_post_per_b_dummy",
                   "poleff_post_per_c_dummy") ~ "PEREFF",
    outcome %in% c("CE_score", "poleff_post_col_a_dummy", "poleff_post_col_b_dummy",
                   "poleff_post_col_c_dummy") ~ "COLEFF",
    outcome == "poleff_post_ext_dummy"  ~ "EXTEFF",
    outcome == "poleff_post_gen_dummy"  ~ "GENEFF",
    outcome %in% c("fs1_agree", "fs2_agree", "fs3_agree", "fs4_agree", "fs5_agree",
                   "fs6_agree", "fs7_agree", "fs8_agree", "FS_score") ~ "FS",
    outcome %in% c("govrolea_dummy", "govroleb_dummy") ~ "GOVROLE",
    outcome %in% c("polid1_dummy", "polid2_dummy", "polid3_dummy",
                   "polid4_dummy", "polid_score") ~ "POLID",
    outcome == "cta_dummy" ~ "CTA",
    TRUE ~ "Other Global"
  ))

final_table_qualified <- all_results %>%
  group_by(sample, model, family) %>%
  mutate(
    p_holm_T1       = p.adjust(p.value_T1,       method = "holm"),
    p_holm_T2       = p.adjust(p.value_T2,       method = "holm"),
    p_holm_equality = p.adjust(p_value_equality, method = "holm")
  ) %>%
  ungroup() %>%
  mutate(
    sig_T1 = case_when(
      p_holm_T1 < 0.01 ~ "***",
      p_holm_T1 < 0.05 ~ "**",
      p_holm_T1 < 0.10 ~ "*",
      TRUE ~ "ns"
    ),
    sig_T2 = case_when(
      p_holm_T2 < 0.01 ~ "***",
      p_holm_T2 < 0.05 ~ "**",
      p_holm_T2 < 0.10 ~ "*",
      TRUE ~ "ns"
    ),
    sig_equality_holm = case_when(
      p_holm_equality < 0.01 ~ "***",
      p_holm_equality < 0.05 ~ "**",
      p_holm_equality < 0.10 ~ "*",
      TRUE ~ "ns"
    ),
    result_label_T1 = paste0(ifelse(estimate_T1 > 0, "(+)", "(-)"), " ",
                             round(estimate_T1, 3), sig_T1),
    result_label_T2 = paste0(ifelse(estimate_T2 > 0, "(+)", "(-)"), " ",
                             round(estimate_T2, 3), sig_T2)
  )

write_xlsx(final_table_qualified,
           file.path(tbl, "20260320_Joint_ITT_Final.xlsx"))

# --- 6. COEFFICIENT PLOTS ---
library(ggplot2)
library(forcats)

plot_data <- final_table_qualified %>%
  drop_na(estimate_T1, estimate_T2) %>%
  pivot_longer(
    cols = c(estimate_T1, estimate_T2,
             conf.low_T1, conf.high_T1,
             conf.low_T2, conf.high_T2,
             sig_T1, sig_T2),
    names_to  = c(".value", "treatment"),
    names_pattern = "(.+)_(T[12])"
  ) %>%
  rename(significance = sig)

plot_folder <- file.path(getwd(), "3 figures")
if (!dir.exists(plot_folder)) dir.create(plot_folder, recursive = TRUE)

sig_colors <- c("***" = "#d7191c", "**" = "#fdae61", "*" = "#2b83ba", "ns" = "grey70")
sig_shapes <- c("***" = 18, "**" = 17, "*" = 16, "ns" = 4)

unique_samples <- unique(plot_data$sample)
print(unique_samples)

for (s_name in unique_samples) {
  
  sample_df <- plot_data %>%
    filter(sample == s_name) %>%
    mutate(
      model     = factor(model,     levels = c("Model 1", "Model 2", "Model 3")),
      treatment = factor(treatment, levels = c("T1", "T2")),
      outcome   = fct_inorder(outcome)
    ) %>%
    arrange(outcome, treatment, model) %>%
    group_by(outcome) %>%
    mutate(row_id = row_number()) %>%
    ungroup() %>%
    mutate(
      outcome_id = as.numeric(factor(outcome, levels = levels(fct_inorder(outcome)))),
      y_position = (outcome_id - 1) * 8 + row_id
    )
  
  if (nrow(sample_df) == 0) next
  
  sep_positions <- unique(sample_df$outcome_id * 8)
  
  p <- ggplot(sample_df,
              aes(x = estimate, y = y_position,
                  color = significance, shape = significance)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.7) +
    geom_point(size = 3) +
    geom_hline(yintercept = sep_positions, color = "grey85", linewidth = 0.6) +
    scale_color_manual(values = sig_colors, name = "Holm-adjusted\np-value") +
    scale_shape_manual(values = sig_shapes, name = "Holm-adjusted\np-value") +
    scale_y_continuous(
      breaks = sample_df$y_position,
      labels = paste(sample_df$outcome, "-", sample_df$treatment, "-", sample_df$model)
    ) +
    theme_minimal() +
    labs(
      title    = paste("Coefficient Plot:", s_name),
      subtitle = "Error bars = 95% CI (unadjusted). Color/shape = Holm-adjusted significance.",
      x        = "Treatment Effect Estimate (95% CI)",
      y        = NULL
    ) +
    theme(
      axis.text.y      = element_text(size = 8),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
  
  clean_s     <- gsub("[[:punct:][:space:]]", "_", s_name)
  file_name   <- paste0("Joint_ITT_", clean_s, ".png")
  calc_height <- (max(sample_df$y_position) * 0.32) + 2
  
  ggsave(file.path(plot_folder, file_name),
         plot = p, width = 12, height = calc_height,
         units = "in", limitsize = FALSE)
}

