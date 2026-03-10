#*******************************************************************************
#  ORWELL NARRATIVE TESTING: WESTFALL YOUNG PLOTS + CONJOINT ANALYSIS							   
#*******************************************************************************
##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Clear and load packages 
# install.packages("pacman") # install the package if you haven't 
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse,data.table,readxl,writexl,fastDummies,hdm,kableExtra,knitr,
               jmvReadWrite,miceadds,broom,ivreg,sandwich,lmtest,flextable,officer,
               modelsummary,fixest)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "elgha") {
  #base_dir <- "G:/" # laptop
  base_dir <- "H:/" # computer
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

# Set graphic dimension for upcoming graphs
width = 12.8
height = 6.98
res = 300

# Define the Okabe-Ito color-blind-friendly palette
cb_palette <- c("p < .01" = "#E69F00",  # orange
                "p < .05" = "#56B4E9",  # sky blue
                "p < .1"  = "#009E73",  # bluish green
                "Null"= "#999999")  # grey

##### WESTFALL-YOUNG VISUALIZATIONS #####
##### Write reusable function #####
# Define a reusable plotting function
generate_plot <- function(data, 
                          outnums, 
                          model_labels,
                          model_levels=c("Model 1", "Model 2"),
                          xlab, 
                          ncol) {
  # Define desired order for treatment names and models
  treat_levels <- c(
    "Fix the distribution",
    "No victimization",
    "Balanced development",
    "Equal opportunity"
  )
#  model_levels <- c("Model 1", "Model 2")
  
  # Create wrapped labels for facets
  wrapped_labels <- as_labeller(model_labels, 
                                default = label_wrap_gen(width = 25))
  
  # Build ordered combination (treatment within model)
  ordered_levels <- expand.grid(
    model = model_levels,
    treat = treat_levels
  ) %>%
    transmute(level = paste(treat, model, sep = ":")) %>%
    pull(level)
  
  # Enforce ordering inside the function
  data <- data %>%
    mutate(
      narnm = factor(narnm, levels = treat_levels),
      equation = factor(equation, levels = model_levels),
      treateq = factor(
        paste(narnm, equation, sep = ":"),
        levels = ordered_levels
      )
    )
  
  ggplot(filter(data, outnum %in% outnums), 
         aes(x = coef, y = treateq)) +
    geom_errorbar(
      aes(xmin = lci, xmax = uci, color = sig),
      width = 0.2,
      orientation = "y"
    ) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    scale_color_manual(values = cb_palette) +
    scale_shape_manual(values = c("p < .01" = 8, "p < .05" = 17, "p < .1" = 16, "Null" = 1)) +
    scale_y_discrete(limits = rev) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = xlab,
      y = NULL,
      color = "Significance (Westfall-Young p-values)",
      shape = "Significance (Westfall-Young p-values)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.title.x = element_text(size = 10)
    ) +
    guides(
      color = guide_legend(
        title.position = "left",
        nrow = 1,
        byrow = TRUE
      ),
      shape = guide_legend(
        title.position = "left",
        nrow = 1,
        byrow = TRUE
      )
    ) +
    facet_wrap(~ outnum, ncol = ncol, labeller = labeller(outnum = wrapped_labels))
}

# Write a function to process figures
process_plot_configs <- function(plot_configs) {
  plot_list <- list()
  
  for (config in plot_configs) {
    plot <- generate_plot(
      data = config$data,
      outnums = config$outnums,
      model_labels = config$model_labels,
      model_levels = config$model_levels,
      xlab = config$xlabs,
      ncol = config$ncol
    )
    
    # Store the plot in the list with a unique name
    plot_name <- tools::file_path_sans_ext(config$filename)
    plot_list[[plot_name]] <- plot
    
    # Save the plot to a file
    ggsave(
      filename = file.path(fig, config$filename),
      plot = plot,
      width = width,
      height = height,
      bg = "white"
    )
  }
  return(plot_list)
}

##### Setting up graph labels #####
##### DK #####
# Labels for outcomes 1-7
DK_labels_set1 <- c(
  "1" = "Utilities price hike",
  "2" = "Larger gov. budget for public transport",
  "3" = "Larger gov. budget for env. friendly tech and renewable energies",
  "4" = "Displaced people receives benefit from infra. utilization",
  "5" = "Subsidy for energy-efficient houses and buildings",
  "6" = "Larger soc. asst. amount",
  "7" = "Larger soc. asst. recipient coverage"
)

# Labels for outcomes 8-13
DK_labels_set2 <- c(
  "8" = "Higher vehicle tax",
  "9" = "Industry subsidy to switch to env. friendly tech and energy",
  "10" = "Employment insurance",
  "11" = "Forest clearance for agriculture",
  "12" = "Forest clearance for settlement development",
  "13" = "Forest clearance for infrastructure"
)

# # Labels for outcomes 1 to 4
# DK_labels_set1 <- c(
#   "1" = "Utilities price hike",
#   "2" = "Larger gov. budget for public transport",
#   "3" = "Larger gov. budget for env. friendly tech and renewable energies",
#   "4" = "Displaced people receives benefit from infra. utilization"
# )
# 
# # Labels for outcomes 5 to 8
# DK_labels_set2 <- c(
#   "5" = "Subsidy for energy-efficient houses and buildings",
#   "6" = "Larger soc. asst. amount",
#   "7" = "Larger soc. asst. recipient coverage",
#   "8" = "Higher vehicle tax"
# )
# 
# # Labels for outcomes 9 to 13
# DK_labels_set3 <- c(
#   "9" = "Industry subsidy to switch to env. friendly tech and energy",
#   "10" = "Employment insurance",
#   "11" = "Forest clearance for agriculture",
#   "12" = "Forest clearance for settlement development",
#   "13" = "Forest clearance for infrastructure"
# )

##### CB05 #####
# Set 1 labels
CB05_labels_set1 <- c(
  "3" = "Increase income of all citizens",
  "5" = "Improve human resources quality",
  "6" = "Encourage citizens to make democratic decisions about their life",
  "7" = "Build public infrastructure",
  "8" = "Provide basic services",
  "9" = "Advance lagging-behind regions",
  "10" = "Enable citizens achieve important goals according to their own choice",
  "11" = "Ensure fairness for all",
  "12" = "Ensure next generations have a better life"
)

# # Set 1 labels
# CB05_labels_set1 <- c(
#   "3" = "Increase income of all citizens",
#   "5" = "Improve human resources quality",
#   "6" = "Encourage citizens to make democratic decisions about their life",
#   "7" = "Build public infrastructure",
#   "8" = "Provide basic services"
# )

# Set 2 labels
# CB05_labels_set2 <- c(
#   "9" = "Advance lagging-behind regions",
#   "10" = "Enable citizens achieve important goals according to their own choice",
#   "11" = "Ensure fairness for all",
#   "12" = "Ensure next generations have a better life"
# )

##### CB06 #####
# Set 1 labels
CB06_labels_set1 <- c(
  "1" = "Capital owners",
  "2" = "Powerful people",
  "3" = "Well-connected people",
  "4" = "People with grit in working",
  "5" = "People who's clever at exploiting opportunity",
  "6" = "People who don't give up to conditions",
  "7" = "Poor people",
  "8" = "People living in lagging-behind regions"
)

# # Set 1 labels
# CB06_labels_set1 <- c(
#   "1" = "Capital owners",
#   "2" = "Powerful people",
#   "3" = "Well-connected people",
#   "4" = "People with grit in working"
# )

# Set 2 labels
# CB06_labels_set2 <- c(
#   "5" = "People who's clever at exploiting opportunity",
#   "6" = "People who don't give up to conditions",
#   "7" = "Poor people",
#   "8" = "People living in lagging-behind regions"
# )

##### CB07 #####
# Set 1 labels
CB07_labels_set1 <- c(
  "1" = "We can reduce reliance on env. destructive econ. act. once society's econ. condition is good",
  "2" = "Gov. can relocate locals to dev. infra. provided that replacement housing are available",
  "3" = "Citizens must not accept gov. decisions without questions",
  "4" = "Wealth inequality is normal",
  "5" = "Gov.-made laws and reg. determine people's opportunity for success",
  "6" = "Gov. decisions determine who rules in the economy"
)

# # Set 1 labels
# CB07_labels_set1 <- c(
#   "1" = "We can reduce reliance on env. destructive econ. act. once society's econ. condition is good",
#   "2" = "Gov. can relocate locals to dev. infra. provided that replacement housing are available",
#   "3" = "Citizens must not accept gov. decisions without questions"
# )
# 
# # Set 2 labels
# CB07_labels_set2 <- c(
#   "4" = "Wealth inequality is normal",
#   "5" = "Gov.-made laws and reg. determine people's opportunity for success",
#   "6" = "Gov. decisions determine who rules in the economy"
# )

##### CB08 #####
# Set 1 labels
CB08_labels <- c(
  "1" = "Rulers",
  "2" = "Regulator",
  "3" = "Leaders",
  "4" = "Caretaker",
  "5" = "Referee"
)

##### CB11 #####
# Set 1 labels
CB11_labels <- c(
  "1" = "Laws and regulations",
  "2" = "Human nature",
  "3" = "World events",
  "4" = "Fate"
)

##### TD #####
# Set 1 labels
TD_labels <- c(
  "1" = "How much can we do to create an economy that meets everyone's needs?",
  "2" = "How much can we do to force the government to create an economy that meets everyone's needs?",
  "3" = "How optimistic do you feel that the public can make the government take major steps to improve the economy?",
  "4" = "How much responsibility do we have to ensure that everyone has the opportunity to succeed?",
  "5" = "How much responsibility do we have to ensure that everyone can live a good life?"
)

##### ITT ##### 
# Load data
outcomes <- c("DK","CB05","CB06","CB07","CB08","CB11","TD")
modelvariants <- c("linear","ologit")
samplevariants <- c("uncond","cond")
datavariants <- expand.grid(outcomes,modelvariants,samplevariants) %>% 
  rename(outcome=Var1,estimator=Var2,sample=Var3) %>%
  mutate(datfilenm = paste0(temp,"/",outcome,"_wyoung_",estimator,"_allmodel_",sample,".csv"),
         datnm = paste(outcome,estimator,sample))
data_list <- lapply(datavariants$datfilenm, function(x) {
  if (file.exists(x)) {
    fread(x)
  } else {
    NULL
  }
})
names(data_list) <- datavariants$datnm
data_list <- Filter(Negate(is.null), data_list) # Remove null data list

# Modify equation label
data_list_names <- names(data_list)
data_list <- lapply(names(data_list), function(nm) {

  df <- data_list[[nm]]

  if (grepl("linear", nm)) {

    df$equation <- dplyr::recode(
      df$equation,
      "Model 1" = "OLS",
      "Model 2" = "+PDS Lasso"
    )

  } else if (grepl("ologit", nm)) {

    df$equation <- dplyr::recode(
      df$equation,
      "Model 1" = "O. Logit",
      "Model 2" = "+PDS Lasso"
    )

  }

  df
})
names(data_list) <- data_list_names

##### Linear model #####
# Define model labels
model_levels_linear <- c("OLS", "+PDS Lasso")

#### DK ####
## Create plots
# Set data
uncond_data <- data_list$`DK linear uncond`
cond_data <- data_list$`DK linear cond`

# Set x-axis label
xlabel <- "Probability of supporting policy relative to control"

# List plot configs (new)
plot_configs_DK <- list(
  list(data = uncond_data, 
       outnums = 1:7, 
       model_labels = DK_labels_set1,
       model_levels = model_levels_linear,
       filename = "DK_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 8:13, 
       model_labels = DK_labels_set2, 
       model_levels = model_levels_linear,
       filename = "DK_wyoung_linear_set2_uncond.png", 
       ncol = 3,
       xlabs = xlabel)
)

# # List plot configs (old)
# plot_configs_DK <- list(
#   list(data = uncond_data, 
#        outnums = 1:4, 
#        model_labels = DK_labels_set1, 
#        filename = "DK_wyoung_linear_set1_uncond.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = uncond_data, 
#        outnums = 5:8, 
#        model_labels = DK_labels_set2, 
#        filename = "DK_wyoung_linear_set2_uncond.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = uncond_data, 
#        outnums = 9:13, 
#        model_labels = DK_labels_set3, 
#        filename = "DK_wyoung_linear_set3_uncond.png", 
#        ncol = 5,
#        xlabs = xlabel)
# )

#### CB05 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB05 linear uncond`
cond_data <- data_list$`CB05 linear cond`

# Set x-axis label
xlabel <- "Probability of prioritizing development goal relative to control"

# List plot configs
plot_configs_CB05 <- list(
  list(data = uncond_data, 
       outnums = 3:12, 
       model_labels = CB05_labels_set1, 
       model_levels = model_levels_linear,
       filename = "CB05_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel)
  # ,
  # list(data = uncond_data, 
  #      outnums = 9:12, 
  #      model_labels = CB05_labels_set2, 
  #      filename = "CB05_wyoung_linear_set2_uncond.png", 
  #      ncol = 4,
  #      xlabs = xlabel)
)

#### CB06 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB06 linear uncond`
cond_data <- data_list$`CB06 linear cond`

# Set x-axis label
xlabel <- "Probability of being in top three priority relative to control"

# List plot configs
plot_configs_CB06 <- list(
  list(data = uncond_data, 
       outnums = 1:8, 
       model_labels = CB06_labels_set1, 
       model_levels = model_levels_linear,
       filename = "CB06_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel)
  # ,
  # list(data = uncond_data, 
  #      outnums = 5:8, 
  #      model_labels = CB06_labels_set2, 
  #      filename = "CB06_wyoung_linear_set2_uncond.png", 
  #      ncol = 4,
  #      xlabs = xlabel)
)

#### CB07 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB07 linear uncond`
cond_data <- data_list$`CB07 linear cond`

# Set x-axis label
xlabel <- "Probability of agreeing to statement relative to control"

# List plot configs
plot_configs_CB07 <- list(
  list(data = uncond_data, 
       outnums = 1:6, 
       model_labels = CB07_labels_set1, 
       model_levels = model_levels_linear,
       filename = "CB07_wyoung_linear_set1_uncond.png", 
       ncol = 3,
       xlabs = xlabel)
  # ,
  # list(data = uncond_data, 
  #      outnums = 4:6, 
  #      model_labels = CB07_labels_set2, 
  #      filename = "CB07_wyoung_linear_set2_uncond.png", 
  #      ncol = 3,
  #      xlabs = xlabel)
)

#### CB08 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB08 linear uncond`
cond_data <- data_list$`CB08 linear cond`

# Set x-axis label
xlabel <- "Probability of agreeing to supposed government role relative to control"

# List plot configs
plot_configs_CB08 <- list(
  list(data = uncond_data, 
       outnums = 1:5, 
       model_labels = CB08_labels, 
       model_levels = model_levels_linear,
       filename = "CB08_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### CB11 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB11 linear uncond`
cond_data <- data_list$`CB11 linear cond`

# Set x-axis label
xlabel <- "Probability of believing a large role relative to control"

# List plot configs
plot_configs_CB11 <- list(
  list(data = uncond_data, 
       outnums = 1:4, 
       model_labels = CB11_labels, 
       model_levels = model_levels_linear,
       filename = "CB11_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel)
)

#### TD #### 
## Create plots
# Set data
uncond_data <- data_list$`TD linear uncond`
cond_data <- data_list$`TD linear cond`

# Set x-axis label
xlabel <- "Probability of a higher personal efficacy relative to control"

# List plot configs
plot_configs_TD <- list(
  list(data = uncond_data, 
       outnums = 1:5, 
       model_labels = TD_labels, 
       model_levels = model_levels_linear,
       filename = "TD_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### All plots  #### 
# Combine all plot configs
plot_configs <- list(plot_configs_DK,
                     plot_configs_CB05,
                     plot_configs_CB06,
                     plot_configs_CB07,
                     plot_configs_CB08,
                     plot_configs_CB11,
                     plot_configs_TD)
names(plot_configs) <- outcomes

# Process and save plots
allplots_ITT_lin <- lapply(plot_configs,process_plot_configs)

##### Probability model #####
# Define model labels
model_levels_prob <- c("O. Logit", "+PDS Lasso")

#### DK ####
## Create plots
# Set data
uncond_data <- data_list$`DK ologit uncond`
cond_data <- data_list$`DK ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of higher policy support relative to control"

# List plot configs (new)
plot_configs_DK <- list(
  list(data = uncond_data,
       outnums = 1:7,
       model_labels = DK_labels_set1,
       model_levels = model_levels_prob,
       filename = "DK_wyoung_ologit_set1_uncond.png",
       ncol = 4,
       xlabs = xlabel),
  list(data = uncond_data,
       outnums = 8:13,
       model_labels = DK_labels_set2,
       model_levels = model_levels_prob,
       filename = "DK_wyoung_ologit_set2_uncond.png",
       ncol = 3,
       xlabs = xlabel)
)

# # List plot configs (old)
# plot_configs_DK <- list(
#   list(data = uncond_data, 
#        outnums = 1:4, 
#        model_labels = DK_labels_set1, 
#        filename = "DK_wyoung_ologit_set1_uncond.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = uncond_data, 
#        outnums = 5:8, 
#        model_labels = DK_labels_set2, 
#        filename = "DK_wyoung_ologit_set2_uncond.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = uncond_data, 
#        outnums = 9:13, 
#        model_labels = DK_labels_set3, 
#        filename = "DK_wyoung_ologit_set3_uncond.png", 
#        ncol = 5,
#        xlabs = xlabel)
# )

#### CB06 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB06 ologit uncond`
cond_data <- data_list$`CB06 ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of higher priority relative to control"

# List plot configs
plot_configs_CB06 <- list(
  list(data = uncond_data, 
       outnums = 1:8, 
       model_labels = CB06_labels_set1,
       model_levels = model_levels_prob,
       filename = "CB06_wyoung_ologit_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel)
  # ,
  # list(data = uncond_data, 
  #      outnums = 5:8, 
  #      model_labels = CB06_labels_set2, 
  #      filename = "CB06_wyoung_ologit_set2_uncond.png", 
  #      ncol = 4,
  #      xlabs = xlabel)
)

#### CB07 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB07 ologit uncond`
cond_data <- data_list$`CB07 ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of higher level of agreement relative to control"

# List plot configs
plot_configs_CB07 <- list(
  list(data = uncond_data, 
       outnums = 1:6, 
       model_labels = CB07_labels_set1, 
       model_levels = model_levels_prob,
       filename = "CB07_wyoung_ologit_set1_uncond.png", 
       ncol = 3,
       xlabs = xlabel)
  # ,
  # list(data = uncond_data, 
  #      outnums = 4:6, 
  #      model_labels = CB07_labels_set2, 
  #      filename = "CB07_wyoung_ologit_set2_uncond.png", 
  #      ncol = 3,
  #      xlabs = xlabel)
)

#### CB08 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB08 ologit uncond`
cond_data <- data_list$`CB08 ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of higher level of agreement relative to control"

# List plot configs
plot_configs_CB08 <- list(
  list(data = uncond_data, 
       outnums = 1:5, 
       model_labels = CB08_labels, 
       model_levels = model_levels_prob,
       filename = "CB08_wyoung_ologit_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### CB11 #### 
## Create plots
# Set data
uncond_data <- data_list$`CB11 ologit uncond`
cond_data <- data_list$`CB11 ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of believing a larger role relative to control"

# List plot configs
plot_configs_CB11 <- list(
  list(data = uncond_data, 
       outnums = 1:4, 
       model_labels = CB11_labels, 
       model_levels = model_levels_prob,
       filename = "CB11_wyoung_ologit_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel)
)

#### TD #### 
## Create plots
# Set data
uncond_data <- data_list$`TD ologit uncond`
cond_data <- data_list$`TD ologit cond`

# Set x-axis label
xlabel <- "Odds ratio of a higher personal efficacy relative to control"

# List plot configs
plot_configs_TD <- list(
  list(data = uncond_data, 
       outnums = 1:5, 
       model_labels = TD_labels, 
       model_levels = model_levels_prob,
       filename = "TD_wyoung_ologit_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### All plots  #### 
# Combine all plot configs
plot_configs <- list(plot_configs_DK,
                     plot_configs_CB06,
                     plot_configs_CB07,
                     plot_configs_CB08,
                     plot_configs_CB11,
                     plot_configs_TD)
names(plot_configs) <- c(outcomes[1],outcomes[3:length(outcomes)])

# Process and save plots
allplots_ITT_prob <- lapply(plot_configs,process_plot_configs)

##### TOT #####
# Load data
outcomes <- c("DK","CB05","CB06","CB07","CB08","CB11","TD")
datfilenm <- paste0(temp,"/",outcomes,"_wyoung_ivreg_allmodel.csv")
data_list <- lapply(datfilenm, fread)
names(data_list) <- outcomes

# Define equation label
data_list <- lapply(data_list, function(df) {
  
  if (is.null(df) || !"equation" %in% names(df)) return(df)
  
  df$equation <- dplyr::recode(
    df$equation,
    "Model 1" = "2SLS",
    "Model 2" = "+PDS Lasso"
  )
  
  df
})

# Define model labels
model_levels_2sls <- c("2SLS","+PDS Lasso")

#### DK ####
## Create plots
# Set data
ivdata <- data_list$DK

# Set x-axis label
xlabel <- "Probability of supporting policy relative to control"

# List plot configs (new)
plot_configs_DK <- list(
  list(data = ivdata, 
       outnums = 1:7, 
       model_labels = DK_labels_set1, 
       model_levels = model_levels_2sls,
       filename = "DK_wyoung_ivreg_set1.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 8:13, 
       model_labels = DK_labels_set2, 
       model_levels = model_levels_2sls,
       filename = "DK_wyoung_ivreg_set2.png", 
       ncol = 4,
       xlabs = xlabel)
)

# # List plot configs (old)
# plot_configs_DK <- list(
#   list(data = ivdata, 
#        outnums = 1:4, 
#        model_labels = DK_labels_set1, 
#        filename = "DK_wyoung_ivreg_set1.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = ivdata, 
#        outnums = 5:8, 
#        model_labels = DK_labels_set2, 
#        filename = "DK_wyoung_ivreg_set2.png", 
#        ncol = 4,
#        xlabs = xlabel),
#   list(data = ivdata, 
#        outnums = 9:13, 
#        model_labels = DK_labels_set3, 
#        filename = "DK_wyoung_ivreg_set3.png", 
#        ncol = 5,
#        xlabs = xlabel)
# )

#### CB05 #### 
## Create plots
# Set data
ivdata <- data_list$CB05

# Set x-axis label
xlabel <- "Probability of prioritizing development goal relative to control"

# List plot configs
plot_configs_CB05 <- list(
  list(data = ivdata, 
       outnums = 3:12, 
       model_labels = CB05_labels_set1, 
       model_levels = model_levels_2sls,
       filename = "CB05_wyoung_ivreg_set1.png", 
       ncol = 5,
       xlabs = xlabel)
  # ,
  # list(data = ivdata, 
  #      outnums = 9:12, 
  #      model_labels = CB05_labels_set2, 
  #      filename = "CB05_wyoung_ivreg_set2.png", 
  #      ncol = 4,
  #      xlabs = xlabel)
)

#### CB06 #### 
## Create plots
# Set data
ivdata <- data_list$CB06

# Set x-axis label
xlabel <- "Probability of being in top three priority relative to control"

# List plot configs
plot_configs_CB06 <- list(
  list(data = ivdata, 
       outnums = 1:8, 
       model_labels = CB06_labels_set1, 
       model_levels = model_levels_2sls,
       filename = "CB06_wyoung_ivreg_set1.png", 
       ncol = 4,
       xlabs = xlabel)
  # ,
  # list(data = ivdata, 
  #      outnums = 5:8, 
  #      model_labels = CB06_labels_set2, 
  #      filename = "CB06_wyoung_ivreg_set2.png", 
  #      ncol = 4,
  #      xlabs = xlabel)
)

#### CB07 #### 
## Create plots
# Set data
ivdata <- data_list$CB07

# Set x-axis label
xlabel <- "Probability of agreeing to statement relative to control"

# List plot configs
plot_configs_CB07 <- list(
  list(data = ivdata, 
       outnums = 1:6, 
       model_labels = CB07_labels_set1, 
       model_levels = model_levels_2sls,
       filename = "CB07_wyoung_ivreg_set1.png", 
       ncol = 3,
       xlabs = xlabel)
  # ,
  # list(data = ivdata, 
  #      outnums = 4:6, 
  #      model_labels = CB07_labels_set2, 
  #      filename = "CB07_wyoung_ivreg_set2.png", 
  #      ncol = 3,
  #      xlabs = xlabel)
)

#### CB08 #### 
## Create plots
# Set data
ivdata <- data_list$CB08

# Set x-axis label
xlabel <- "Probability of agreeing to supposed government role relative to control"

# List plot configs
plot_configs_CB08 <- list(
  list(data = ivdata, 
       outnums = 1:5, 
       model_labels = CB08_labels, 
       model_levels = model_levels_2sls,
       filename = "CB08_wyoung_ivreg_set1.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### CB11 #### 
## Create plots
# Set data
ivdata <- data_list$CB11

# Set x-axis label
xlabel <- "Probability of believing a large role relative to control"

# List plot configs
plot_configs_CB11 <- list(
  list(data = ivdata, 
       outnums = 1:4, 
       model_labels = CB11_labels, 
       model_levels = model_levels_2sls,
       filename = "CB11_wyoung_ivreg_set1.png", 
       ncol = 4,
       xlabs = xlabel)
)

#### TD #### 
## Create plots
# Set data
ivdata <- data_list$TD

# Set x-axis label
xlabel <- "Probability of a higher personal efficacy relative to control"

# List plot configs
plot_configs_TD <- list(
  list(data = ivdata, 
       outnums = 1:5, 
       model_labels = TD_labels, 
       model_levels = model_levels_2sls,
       filename = "TD_wyoung_ivreg_set1.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### All plots  #### 
# Combine all plot configs
plot_configs <- list(plot_configs_DK,
                     plot_configs_CB05,
                     plot_configs_CB06,
                     plot_configs_CB07,
                     plot_configs_CB08,
                     plot_configs_CB11,
                     plot_configs_TD)
names(plot_configs) <- outcomes

# Process and save plots
allplots_TOT <- lapply(plot_configs,process_plot_configs)

##### CONJOINT #####
## Data preparation for conjoint
# Set data date
date <- "20250304"

# Load data
datnm <- paste0("processed_",date,".csv") 
data <- file.path(ipt,datnm) 
maindata <- fread(data)

## Prepare chosen profile data 
# Subset profile and pairing data
profpairw <- maindata %>% dplyr::select(record,
                                   starts_with("Profile"),
                                   starts_with("PairingID"))

# Reshape the data from wide to long format
profpairl <- data.table::melt(
  profpairw,
  id.vars = "record",
  measure.vars = patterns(
    profile = "^Profile_\\d+$",
    pairing_id = "^PairingID_\\d+$"
  ),
  variable.name = "task"
)

# Import pairing data
filenm = file.path(ipt,"profile_pairings.xlsx")
pairings <- read_excel(filenm)

# Merge profile and pairing with pairing data
cjdfw <- left_join(profpairl,pairings,by=join_by(pairing_id==Pairing_ID),relationship="many-to-one") %>%
  rename(profile_chosen=profile)

# Melt the data on Profile_A and Profile_B
cjdfl <- melt(
  cjdfw,
  id.vars = c("record", "task", "profile_chosen", "pairing_id"),
  measure.vars = c("Profile_A", "Profile_B"),
  variable.name = "alt",
  value.name = "profile_id"
)

# Step 1: Convert 'alt' to numeric
cjdfl[, alt := fifelse(alt == "Profile_A", 1L, 
                       fifelse(alt == "Profile_B", 2L, NA_integer_))]

# Step 2: Create 'alt_chosen' column
cjdfl[, alt_chosen := as.integer(profile_chosen == profile_id)]

## Prepare profile attributes data
# Create profile data frame
filenm <- file.path(ipt,"sampled_profiles.xlsx")
profiles <- read_excel(filenm) %>% 
  rename(econ=improvement_of_economic_conditions,
         rights=rights_of_others,
         env=environmental_preservation,
         participation=citizen_participation) %>%
  mutate(econ=case_when(
    econ=="The income of the wealthy has increased the most, while the income of others has only increased modestly." ~ "1",
    econ=="The income of the poor or near-poor has increased the most, while the income of those in the middle and upper classes has only increased modestly." ~ "2",
    econ=="All layers of society have experienced a modest increase in income, but no one has become poorer." ~ "3"
  ),
  rights=case_when(
    rights=="To build something that will benefit many people in the future, no group should be displaced or lose their livelihood, even if there is adequate compensation." ~ "1",
    rights=="To build something that will benefit many people in the future, residents should not only receive adequate compensation but also financial benefits derived from what is built." ~ "2",
    rights=="To build something that will benefit many people in the future, it is reasonable if some residents are forced to be displaced or lose their livelihood, as long as there is adequate compensation." ~ "3"
  ),
  env=case_when(
    env=="Environmental destruction must not occur at all, even if it is to build something that will benefit many people in the future." ~ "1",
    env=="Environmental destruction, to a certain extent, can be accepted, as long as it is to build something that will benefit many people in the future." ~ "2"
  ),
  participation=case_when(
    participation=="Residents feel comfortable and free to actively provide input, ask questions, or express complaints to the government, so that development programs can proceed more carefully." ~ "1",
    participation=="After voting in elections, residents trust and give the government freedom to carry out development programs, so that these programs can proceed more smoothly and quickly." ~ "2"
  )) %>%
  mutate(across(everything(),as.numeric))

## Merge chosen profile data with profile attributes data
cjdfm <- full_join(cjdfl,profiles,by=join_by(profile_id==profile_number),relationship="many-to-one")

## Prepare data for estimation: dummy-code all attribute levels (except one per factor)
cjdff <- cjdfm %>%
  mutate(econ = factor(econ), rights = factor(rights),
         env = factor(env), participation = factor(participation),
         task=as.numeric(task)) %>%
  mutate(rights = fct_relevel(rights, "3", "1", "2"), # Reorder the levels of 'rights' so that "3" is the first level
         participation = fct_relevel(participation, "2", "1")) %>% 
  dummy_cols(select_columns=c("econ","rights","env","participation"),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE) %>% # Dummify data
  left_join(dplyr::select(maindata,
                          record,
                          crt_intrpt_msg,
                          ConjointOverall_Time,
                          contains("treat"),
                          lfCB),
            by="record",relationship="many-to-one") %>% # Obtain treatment status and covariates
  rename(treat5=treat4,treat4=treat5,complytreat5=complytreat4,complytreat4=complytreat5) %>% # Reverse treat4 and treat5
  filter(lfCB!=4) # Remove original treatment 4 units from data 

# Setup the interaction variables
attrb <- c("econ","rights","env","participation")
attrblvl <- lapply(attrb, function(f) {
  cjdff %>% select(starts_with(f)) %>% names()
})
names(attrblvl) <- attrb
attr_levels <- paste(unlist(attrblvl))
for (treatvar in c("treat","complytreat")) {
  assign(paste0(treatvar,"ments"),
         paste0(treatvar,1:4))
  
  assign(paste0("intrterms_",treatvar),
         c(paste0("econ_2:", treatvar, c(1,4)),
           paste0("econ_3:", treatvar, c(1,4)),
           paste0("rights_1:", treatvar, c(2,3)),
           paste0("rights_2:", treatvar, c(2,3)),
           paste0("env_2:", treatvar, c(2,3)),
           paste0("participation_1:", treatvar, 2))
  )
}

for (treat in c(treatments,complytreatments)) {
  for (levels in attr_levels) {
    cjdff[,paste0(levels,":",treat)] <- cjdff[,..levels] * cjdff[,..treat] 
  }
}

## Prepare two sample sets
cjdf <- list("unconditional" = cjdff, 
             "conditional" = cjdff %>% filter(crt_intrpt_msg==1))

# Write a function to tidy regression results
plotprep <- function(model) {
  model %>%
    summary() %>%
    unclass() %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    rename(
      estimate = Estimate,
      std.error = Std..Error,
      p.unadj  = Pr...t..
    ) %>%
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

# Holm Function With Attribute Grouping
adjust_holm <- function(df, model_type) {
  
  # classify attribute
  df <- df %>%
    mutate(
      attribute = case_when(
        grepl("^econ_", term) ~ "econ",
        grepl("^rights_", term) ~ "rights",
        grepl("^env_", term) ~ "env",
        grepl("^participation_", term) ~ "participation",
        TRUE ~ NA_character_
      ),
      is_interaction = grepl(":", term)
    )
  
  if (model_type == "base") {
    
    # Only adjust main AMCEs (exclude intercept)
    df <- df %>%
      mutate(
        p.adj = ifelse(
          !is.na(attribute),
          ave(p.unadj, attribute, FUN = function(x) p.adjust(x, "holm")),
          p.unadj
        )
      )
    
  }
  
  if (model_type == "interactions") {
    
    # Only adjust interaction terms
    df <- df %>%
      mutate(
        p.adj = ifelse(
          is_interaction & !is.na(attribute),
          ave(p.unadj, attribute, FUN = function(x) p.adjust(x, "holm")),
          p.unadj
        )
      )
    
  }
  
  df %>%
    mutate(
      sig = case_when(
        p.adj < 0.01 ~ "p < .01",
        p.adj < 0.05 ~ "p < .05",
        p.adj < 0.1  ~ "p < .1",
        TRUE         ~ "Null"
      )
    )
}

##### ITT ##### 
# Write a function to conduct cluster regression 
clusterols <- function(data,formula) {
  lm.cluster(data = data, formula = formula, cluster = "record")  
}

##### Estimation (OLS) #####
# Setting up the formula
attrbvarlist <- paste(attr_levels, collapse = " + ")
treatvarlist <- paste(treatments, collapse = " + ")
main_effects <- paste(attrbvarlist,treatvarlist,sep = " + ")
interactions <- paste(intrterms_treat, collapse = " + ")
regressors <- paste(main_effects,interactions, sep = " + ")
formula_base <- paste("alt_chosen ~",attrbvarlist)
formula_intr <- paste("alt_chosen ~",regressors)
formulas <- list(formula_base,formula_intr)
names(formulas) <- c("base","interactions")

# Run the model
wgt__ <- NULL  # Initialize wgt__ in the global environment
infmodels <- lapply(names(cjdf), function(data) {
  lapply(names(formulas), function(formulae) {
    clusterols(cjdf[[data]],formulas[[formulae]])
  })
})

# Name the results
names(infmodels) <- names(cjdf)
for (sampresc in names(cjdf)) {
  names(infmodels[[sampresc]]) <- names(formulas)
}

# Tidy results
modsres <- lapply(names(cjdf), function(sampresc) {
  lapply(names(formulas), function(formulae) {
    
    raw <- plotprep(infmodels[[sampresc]][[formulae]])
    
    adjust_holm(raw, model_type = formulae)
    
  })
})

# Name the tidy results
names(modsres) <- names(infmodels)
for (sampresc in names(infmodels)) {
  names(modsres[[sampresc]]) <- names(formulas)
}

# Combine results
allmodres <- list("unconditional"=bind_rows(mutate(modsres$unconditional$base,
                                                   model="base"),
                                            mutate(modsres$unconditional$interactions,
                                                   model="interactions")),
                  "conditional"=bind_rows(mutate(modsres$conditional$base,
                                                 model="base"),
                                          mutate(modsres$conditional$interactions,
                                                 model="interactions"))
)

# Export results
filenm <- file.path(opt,"conjoint_itt_acmes.xlsx")
write_xlsx(allmodres$unconditional, path = filenm)

##### Create combined plots #####
# Write reusable function for plotting and saving the graph
plotnsave <- function(modres, filepath) {
  
  # -----------------------------
  # 1. Treatment label mapping
  # -----------------------------
  
  treat_labels <- c(
    "treat1" = "Fix the distribution",
    "treat2" = "No victimization",
    "treat3" = "Balanced development",
    "treat4" = "Equal opportunity"
  )
  
  for (pattern in names(treat_labels)) {
    modres$term <- str_replace_all(
      modres$term,
      paste0("\\b", pattern, "\\b"),
      treat_labels[[pattern]]
    )
  }
  
  # -----------------------------
  # 2. Identify main vs interaction
  # -----------------------------
  
  modres <- modres %>%
    mutate(is_interaction = str_detect(term, ":"))
  
  # -----------------------------
  # 3. Split and filter per model
  # -----------------------------
  
  base_df <- modres %>%
    filter(model == "base", !is_interaction) %>%
    mutate(attribute = as.character(attribute))
  
  interaction_df <- modres %>%
    filter(model == "interactions", is_interaction) %>%
    mutate(attribute = as.character(attribute))
  
  # -----------------------------
  # 4. Order BASE terms
  # -----------------------------
  
  base_order <- c(
    "econ_2",
    "econ_3",
    "rights_1",
    "rights_2",
    "env_2",
    "participation_1"
  )
  
  base_df <- base_df %>%
    mutate(term = factor(term, levels = base_order))
  
  # -----------------------------
  # 5. Clean & Order INTERACTIONS
  # -----------------------------
  
  treat_order <- c(
    "Fix the distribution",
    "Equal opportunity",
    "No victimization",
    "Balanced development"
  )
  
  interaction_df <- interaction_df %>%
    mutate(
      attribute_rank = case_when(
        str_detect(term, "^econ_") ~ 1,
        str_detect(term, "^rights_") ~ 2,
        str_detect(term, "^env_") ~ 3,
        str_detect(term, "^participation_") ~ 4
      ),
      treat_rank = sapply(term, function(x) {
        idx <- which(sapply(treat_order, function(t)
          str_detect(x, fixed(t))
        ))
        if (length(idx) == 0) NA_integer_ else idx[1]
      }),
      level_rank = case_when(
        str_detect(term, "econ_2") ~ 1,
        str_detect(term, "econ_3") ~ 2,
        str_detect(term, "rights_1") ~ 1,
        str_detect(term, "rights_2") ~ 2,
        TRUE ~ 1
      ),
      order_val = attribute_rank * 100 + treat_rank * 10 + level_rank,
      term_clean = str_replace(term, ":", " × ")
    ) %>%
    arrange(order_val) %>%
    mutate(term_clean = factor(term_clean, levels = rev(unique(term_clean))))
  
  # -----------------------------
  # 6. Combine
  # -----------------------------
  
  base_df <- base_df %>%
    mutate(
      term_clean = factor(term, levels = rev(base_order))
    )
  
  plot_df <- bind_rows(base_df, interaction_df)
  
  # -----------------------------
  # 7. Plot (VERTICAL stacking)
  # -----------------------------
  
  plot <- ggplot(plot_df, aes(y = term_clean, x = estimate)) +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high, color = sig),
      width = 0.2,
      orientation = "y"
    ) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_color_manual(values = cb_palette) +
    scale_shape_manual(
      values = c(
        "p < .01" = 8,
        "p < .05" = 17,
        "p < .1"  = 16,
        "Null"    = 1
      )
    ) +
    facet_wrap(~ model, ncol = 1, scales = "free_y",
               labeller = labeller(model = model_labels)) +
    labs(
      y = NULL,
      x = "Probability of choosing a development scenario relative to base option",
      color = "Significance (Holm p-values)",
      shape = "Significance (Holm p-values)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.title.x = element_text(size = 10)
    )
  
  ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    bg = "white"
  )
  
  return(plot)
}

# Remove intercept from the final data to be plotted
plotdata <- allmodres$unconditional %>%
  filter(
    (model == "base" & !grepl(":", term) & term != "(Intercept)") |
      (model == "interactions" & grepl(":", term))
  )

# Define a named vector for the new facet labels
model_labels <- c(
  "base" = "Base",
  "interactions" = "Interactions"
)

# Plot
fignm <- file.path(fig,"conjoint_itt.png")
plotcjitt <- plotnsave(plotdata, filepath = fignm)

##### Calculate attribute relative importance #####
# Calculation
relimp <- allmodres$unconditional %>%
  filter(!grepl("treat",term)) %>%
  
  # 1. identify attribute from term name
  mutate(attribute = case_when(
    str_detect(term, "^econ_")         ~ "econ",
    str_detect(term, "^rights_")       ~ "rights",
    str_detect(term, "^env_")          ~ "env",
    str_detect(term, "^participation_") ~ "participation",
    TRUE                               ~ NA_character_
  )) %>%
  filter(!is.na(attribute)) %>%
  
  # 2. compute range per attribute per model (include 0 for the base level)
  group_by(model, attribute) %>%
  summarise(
    range = max(estimate, 0) - min(estimate, 0),
    .groups = "drop"
  ) %>%
  
  # 3. convert to relative importance
  group_by(model) %>%
  mutate(
    rel_imp = 100 * range / sum(range)
  ) %>%
  ungroup()

# Export results
filenm <- file.path(opt,"conjoint_itt_relimp.xlsx")
write_xlsx(relimp, path = filenm)

##### TOT #####
# Write reusable function to estimate IV with cluster SE
IVcse <- function(formula, data, cluster_var) {
  # Fit the IV regression model
  iv_model <- ivreg(formula = formula, data = data)
  
  # Compute clustered standard errors
  cluster_formula <- as.formula(paste0("~", cluster_var))
  cluster_se <- vcovCL(iv_model, cluster = cluster_formula)
  
  # Obtain coefficient test results with clustered SEs
  coeftest(iv_model, vcov = cluster_se)
}

# Write a function to prepare results for plotting
plotprep <- function(model, keyterm) {
  
  model %>%
    unclass() %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "term") %>%
    filter(term == keyterm) %>%
    rename(
      p.unadj = Pr...t..,
      estimate = Estimate,
      std.error = Std..Error
    ) %>%
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      
      # identify attribute family
      attribute = dplyr::case_when(
        stringr::str_detect(term, "^econ_") ~ "Economic",
        stringr::str_detect(term, "^rights_") ~ "Rights",
        stringr::str_detect(term, "^env_") ~ "Environment",
        stringr::str_detect(term, "^participation_") ~ "Participation"
      )
    )
}

#### Estimation (IVREG) ####
# Build the structural equation
attrbvarlist <- paste(attr_levels, collapse = " + ")
treatvarlist <- paste(complytreatments, collapse = " + ")
main_effects <- paste(attrbvarlist,treatvarlist,sep = " + ")
main_interactions <- paste(intrterms_complytreat, collapse = " + ")
main_regressors <- paste(main_effects,main_interactions, sep = " + ")
str_formula <- paste("alt_chosen ~",main_regressors)

# Create the interaction list of the reduced form equation
interactions_list <- lapply(seq_along(intrterms_treat), function(i) {
  c(intrterms_treat[i], intrterms_complytreat[-i])
})
redf_interactions <- lapply(seq_along(interactions_list), function(i) {
  paste(interactions_list[[i]], collapse = " + ")
})
redf_regressors <- lapply(seq_along(redf_interactions), function(i) {
  paste(main_effects, redf_interactions[[i]], sep = " + ")
})

# Build the complete equation list
formulas <- lapply(seq_along(redf_interactions), function(i) {
  paste(str_formula,redf_regressors[[i]], sep = " | ")
})
names(formulas) <- intrterms_complytreat

# Estimate IV
infmodels <- lapply(formulas, IVcse, data = cjdff,
                    cluster_var = "record")

# Prepare results for plotting
modres <- lapply(names(infmodels), function(key) {
  plotprep(infmodels[[key]], keyterm = key)
}) %>% 
  bind_rows() %>%
  group_by(attribute) %>%
  mutate(p.adj = p.adjust(p.unadj, method = "holm")) %>%
  ungroup() %>%
  mutate(sig = case_when(
    p.adj < 0.01 ~ "p < .01",
    p.adj < 0.05 ~ "p < .05",
    p.adj < 0.1  ~ "p < .1",
    TRUE ~ "Null"
  ))

##### Create combined plots #####
plot_iv_interactions <- function(modres, filepath) {
  
  # -----------------------------
  # 1. Treatment label mapping
  # -----------------------------
  
  treat_labels <- c(
    "complytreat1" = "Fix the distribution",
    "complytreat2" = "No victimization",
    "complytreat3" = "Balanced development",
    "complytreat4" = "Equal opportunity"
  )
  
  for (pattern in names(treat_labels)) {
    modres$term <- stringr::str_replace_all(
      modres$term,
      paste0("\\b", pattern, "\\b"),
      treat_labels[[pattern]]
    )
  }
  
  # Clean interaction symbol
  modres <- modres %>%
    mutate(term_clean = stringr::str_replace(term, ":", " × "))
  
  # -----------------------------
  # 2. Ordering variables
  # -----------------------------
  
  modres <- modres %>%
    mutate(
      attribute_rank = case_when(
        attribute == "Economic" ~ 1,
        attribute == "Rights" ~ 2,
        attribute == "Environment" ~ 3,
        attribute == "Participation" ~ 4
      ),
      
      level_rank = case_when(
        stringr::str_detect(term, "econ_2") ~ 1,
        stringr::str_detect(term, "econ_3") ~ 2,
        stringr::str_detect(term, "rights_1") ~ 1,
        stringr::str_detect(term, "rights_2") ~ 2,
        TRUE ~ 1
      ),
      
      treat_rank = case_when(
        stringr::str_detect(term, "Fix the distribution") ~ 1,
        stringr::str_detect(term, "No victimization") ~ 2,
        stringr::str_detect(term, "Balanced development") ~ 3,
        stringr::str_detect(term, "Equal opportunity") ~ 4
      ),
      
      order_val = attribute_rank * 100 +
        treat_rank * 10 +
        level_rank
    ) %>%
    arrange(order_val) %>%
    mutate(term_clean = factor(term_clean,
                               levels = rev(unique(term_clean))))
  
  # -----------------------------
  # 3. Plot
  # -----------------------------
  
  plot <- ggplot(modres, aes(y = term_clean, x = estimate)) +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high, color = sig),
      width = 0.2,
      orientation = "y"
    ) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = "red") +
    scale_color_manual(values = cb_palette) +
    scale_shape_manual(
      values = c(
        "p < .01" = 8,
        "p < .05" = 17,
        "p < .1"  = 16,
        "Null"    = 1
      )
    ) +
    labs(
      y = NULL,
      x = "Probability of choosing a development scenario relative to base option",
      color = "Significance (Holm-adjusted)",
      shape = "Significance (Holm-adjusted)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.title.x = element_text(size = 10)
    )
  
  ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    bg = "white"
  )
  
  return(plot)
}

# Plot
fignm <- file.path(fig,paste0("conjoint_tot.png"))
plotcjtot <- plot_iv_interactions(modres, filepath = fignm)

##### COVARIATE BALANCE #####
## Data preparation for covariate balance
# Set data date
date <- "20250304"

# Load data
datnm <- paste0("processed_",date,".csv") 
data <- file.path(ipt,datnm) 
maindata <- fread(data)

# Define potential controls variables
basechar <- c('region1', 
              'region2',
              'region3',
              'urban',
              'male',
              'age',
              'edu1',
              'edu2',
              'edu3',
              'edu4',
              'edu5',
              'hhhead_female',
              'nosocast',
              'hhsize',
              'sdbi')

# Setup data
cbdata <- maindata %>% 
  filter(lfCB!=4) %>%
  rename(treat5=treat4,treat4=treat5)

# Setup formula
treatments <- paste0("treat",1:4) %>% paste(collapse=" + ")
formulas <- lapply(basechar, function(i) {
  paste(i,"~",treatments)
})
names(formulas) <- basechar 

# Run OLS
covbal_models <- lapply(formulas, function(f) {
  lm(f, data = cbdata)
})
covbal_models <- covbal_models[names(covbal_models) != "edu1"]

# Rename variables for table display
names(covbal_models) <- c(
  "West","Central","East","Urban","Male","Age",
  "Primary","JHS","SHS","Tertiary",
  "HH head female","No soc. asst.","Household size","SDBI"
)

coef_map <- c(
  "treat1" = "Fix the distribution",
  "treat2" = "No victimization",
  "treat3" = "Balanced development",
  "treat4" = "Equal opportunity"
)

# Create a custom tidy method
tidy_holm <- function(model){
  
  s <- broom::tidy(model)
  
  s$p_holm <- p.adjust(s$p.value, method = "holm")
  
  s$stars <- dplyr::case_when(
    s$p_holm < 0.01 ~ "***",
    s$p_holm < 0.05 ~ "**",
    s$p_holm < 0.10 ~ "*",
    TRUE ~ ""
  )
  
  s$estimate <- sprintf("%.3f%s", s$estimate, s$stars)
  
  s
}

# Final table export
modelsummary(
  covbal_models,
  coef_map = coef_map,
  statistic = "({std.error})",
  vcov = "HC1",
  gof_omit = ".*",
  tidy = tidy_holm,
  
  title = "Conventional balance testing results",
  notes = "Statistical significance based on Holm p-values: *** p < 0.01; ** p < 0.05; * p < 0.10. Standard errors in parentheses.",
  
  output = "latex"
)

##### EXPORT DATA TO JAMOVI #####
# Export analysis data to be imported to omv
dtcsvnm <- paste0("processed_",date,".csv")
datacsv <- file.path(temp,dtcsvnm)
dtomvnm <- paste0("procs_",date,".omv")
dataomv <- file.path(temp,dtomvnm)

convert_to_omv(
  fleInp = datacsv,
  fleOut = dataomv,
  frcWrt = T
)