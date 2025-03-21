##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
# install.packages("pacman") # install the package if you haven't 
pacman::p_load(tidyverse,data.table,hdm,estimatr,broom,stringr)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "user") {
  base_dir <- "H:/"
}

# Set directory
master <- file.path(base_dir, 
                    "Shared drives", 
                    "Projects", 
                    "2025", 
                    "Orwell", 
                    "Breadcrumbs", 
                    "10 Quantitative Narrative Testing", 
                    "9 Main survey")
setwd(master)

ipt = file.path(getwd(),"2a input")
temp = file.path(getwd(),"2b temp")
opt = file.path(getwd(),"2c output")
lg = file.path(getwd(),"3 log")
fig = file.path(getwd(),"4 figures")
tbl = file.path(getwd(),"5 tables")

##### DATA PREPARATION #####
# Set data date
date <- "20250304"

# Load data
dataname <- paste0("raw_",date,".csv") 
data <- file.path(ipt,dataname) 
rdf <- fread(data)

# Prepare variables

# Overall treatment status
rdf$treat <- rdf$lfCB %in% 1:5

# Define conditions and create 'region' column
rdf <- rdf %>%
  mutate(region = case_when(
    (ID01 >= 1 & ID01 <= 16) | ID01 %in% c(20, 21) ~ 1,
    (ID01 >= 17 & ID01 <= 19) | (ID01 >= 22 & ID01 <= 30) ~ 2,
    TRUE ~ 3
  ))

levels <- unique(rdf$region)
for (rgn in levels) {
  varname=paste0("region",rgn)
  rdf[,varname] <- rdf[,"region"] == rgn
}

# Create 'urban', 'male', and 'unmarried' columns
rdf <- rdf %>%
  mutate(
    urban = (ID02 == 1),
    male = (ID03 == 1),
    unmarried = (ID05 == 1),
    hhhead_female = (RT01 == 2),
    nosocast = (RT02 == 3),
    hhsize = RT03r1 + RT03r2 + RT03r3
  )

# Create binary columns for education levels
for (edu in 1:5) {
  varname <- paste0("edu",edu)
  rdf[,varname] <- rdf[,"ID06"] == edu
}

# Prepare the outcome variables
for (i in 1:13) {
  newvar <- paste0("suppol", i)
  varname <- paste0("QDKr", i)
  rdf[, (newvar) := get(varname) %in% c(4, 5)]
}
outcomes <- rdf %>% select(starts_with("suppol")) %>% names()

# Define potential controls variables
basechar <- c('region1', 
              'region2',
              'region3',
              'urban',
              'male',
              'ID04',
              'unmarried',
              'edu1',
              'edu2',
              'edu3',
              'edu4',
              'edu5',
              'hhhead_female',
              'nosocast',
              'hhsize')

##### LASSO #####
# Apply the rlassoEffect function to each outcome variable
rlasso_models <- lapply(outcomes, function(var) {
  rlassoEffect(
    x = as.matrix(rdf[, ..basechar]),
    y = rdf[[var]],
    d = rdf[["lfCB"]],
    method = "double selection"
  )
})

# Name the list elements for clarity
names(rlasso_models) <- outcomes

# Extract the selected variables for each outcome
selected_variables <- lapply(rlasso_models, function(model) {
  names(model$selection.index)[model$selection.index]
})

# Name the list elements for clarity
names(selected_variables) <- outcomes

##### OVERALL TREATMENT EFFECT #####
ols_models <- lapply(outcomes, function(var) {
  lm_robust(
    as.formula(paste(var, "~ treat + male + ID04 + factor(ID01)")),
    data = rdf,
    se_type = "HC1"
  )
})

##### TREATMENT EFFECT BY ARM #####
# Run OLS with robust SEs for all outcomes, including male, age, and province dummies
ols_models <- lapply(outcomes, function(var) {
  lm_robust(
    as.formula(paste(var, "~ relevel(factor(lfCB), ref = '6') + male + ID04 + factor(ID01)")),
    data = rdf,
    se_type = "HC1"
  )
})

# Name the list elements
names(ols_models) <- outcomes

##### PLOT #####
# Extract coefficients for all outcomes (keeping only lfCB dummies)
coef_df <- bind_rows(lapply(names(ols_models), function(var) {
  # Get the coefficients and confidence intervals
  tidy_res <- tidy(ols_models[[var]], conf.int = TRUE)
  
  # Print term names for debugging
  print(tidy_res$term)
  
  # Keep only the treatment dummies (lfCB)
  tidy_res <- tidy_res %>%
    filter(grepl("lfCB", term)) %>%  # More general match
    mutate(outcome = var)  # Add outcome name
  
  return(tidy_res)
}))

# Rename lfCB dummy variables for better readability (only "1", "2", etc.)
coef_df <- coef_df %>%
  mutate(term = gsub("relevel\\(factor\\(lfCB\\), ref = \"6\"\\)", "", term),  # Remove full reference text
         term = gsub("[^0-9]", "", term),  # Keep only numeric values
         term = factor(term, levels = as.character(1:5)))  # Convert to factor for ordered plotting

# Define custom labels for the first figure
custom_labels_1 <- c(
  "suppol1" = "Fuel and electricity price hikes accompanied by cash transfers to all, not just the poor",
  "suppol2" = "Larger government budget for the development of a mass, integrated, and environmentally friendly public transportation system",
  "suppol3" = "Greater government budget for the development of green technology and renewable energy",
  "suppol4" = "Residents who were displaced due to infrastructure development projects benefit from the utilization of the infrastructure",
  "suppol5" = "Subsidies for energy-efficient homes and buildings",
  "suppol6" = "Greater value of social assistance"
)

# Split the data into two figures
coef_df_1 <- coef_df %>%
  filter(outcome %in% paste0("suppol", 1:6)) %>%
  mutate(outcome = factor(outcome, levels = names(custom_labels_1), labels = custom_labels_1))

coef_df_2 <- coef_df %>%
  filter(outcome %in% paste0("suppol", 7:13))

# Wrap text for first figure (max 40 characters per line)
coef_df_1 <- coef_df_1 %>%
  mutate(outcome = str_wrap(outcome, width = 40))

# Wrap text for second figure (also wrap labels)
coef_df_2 <- coef_df_2 %>%
  mutate(outcome = str_wrap(outcome, width = 40))

##### PLOTTING #####

# First figure: 6 outcome variables
ggplot(coef_df_1, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ outcome, ncol = 3) +  # 6 panels
  labs(x = "Treatment group", 
       y = "Probability to support relative to control group") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10), axis.text.x = element_text(size = 10))