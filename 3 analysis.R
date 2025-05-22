##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Clear and load packages 
# install.packages("pacman") # install the package if you haven't 
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse,data.table,readxl,fastDummies,hdm,jmvReadWrite,miceadds,broom,ivreg,sandwich,lmtest)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "User") {
  base_dir <- "H:/"
} else if (current_user == "elgha") {
  base_dir <- "G:/"
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
cb_palette <- c("***" = "#E69F00",  # orange
                "**"  = "#56B4E9",  # sky blue
                "*"   = "#009E73",  # bluish green
                "Null"= "#999999")  # grey

##### WESTFALL-YOUNG VISUALIZATIONS #####
##### Write reusable function #####
# Define a reusable plotting function
generate_plot <- function(data, outnums, model_labels, xlab, ncol) {
  wrapped_labels <- as_labeller(model_labels, default = label_wrap_gen(width = 25))
  
  ggplot(filter(data, outnum %in% outnums), 
         aes(x = coef, y = treateq)) +
    geom_errorbarh(aes(xmin = lci, xmax = uci, color = sig), height = 0.2) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    scale_color_manual(values = cb_palette) +
    scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
    scale_y_discrete(limits = rev) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      x = xlab,
      y = NULL,
      color = "Statistical significance based on Westfall-Young p-values",
      shape = "Statistical significance based on Westfall-Young p-values"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    guides(
      color = guide_legend(title.position = "top"),
      shape = guide_legend(title.position = "top")
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
#### DK 
# Labels for models 1 to 4
DK_labels_set1 <- c(
  "1" = "Utilities price hike",
  "2" = "Larger gov. budget for public transport",
  "3" = "Larger gov. budget for env. friendly tech and renewable energies",
  "4" = "Displaced people receives benefit from infra. utilization"
)

# Labels for models 5 to 8
DK_labels_set2 <- c(
  "5" = "Subsidy for energy-efficient houses and buildings",
  "6" = "Larger soc. asst. amount",
  "7" = "Larger soc. asst. recipient coverage",
  "8" = "Higher vehicle tax"
)

# Labels for models 9 to 13
DK_labels_set3 <- c(
  "9" = "Industry subsidy to switch to env. friendly tech and energy",
  "10" = "Employment insurance",
  "11" = "Forest clearance for agriculture",
  "12" = "Forest clearance for settlement development",
  "13" = "Forest clearance for infrastructure"
)

#### CB05
# Set 1 labels
CB05_labels_set1 <- c(
  "3" = "Increase income of all citizens",
  "5" = "Improve human resources quality",
  "6" = "Encourage citizens to make democratic decisions about their life",
  "7" = "Build public infrastructure",
  "8" = "Provide basic services"
)

# Set 2 labels
CB05_labels_set2 <- c(
  "9" = "Advance lagging-behind regions",
  "10" = "Enable citizens achieve important goals according to their own choice",
  "11" = "Ensure fairness for all",
  "12" = "Ensure next generations have a better life"
)

#### CB06 
# Set 1 labels
CB06_labels_set1 <- c(
  "1" = "Capital owners",
  "2" = "Powerful people",
  "3" = "Well-connected people",
  "4" = "People with grit in working"
)

# Set 2 labels
CB06_labels_set2 <- c(
  "5" = "People who's clever at exploiting opportunity",
  "6" = "People who don't give up to conditions",
  "7" = "Poor people",
  "8" = "People living in lagging-behind regions"
)

#### CB07
# Set 1 labels
CB07_labels_set1 <- c(
  "1" = "We can reduce reliance on env. destructive econ. act. once society's econ. condition is good",
  "2" = "Gov. can relocate locals to dev. infra.",
  "3" = "Citizens must not accept gov. decisions without questions"
)

# Set 2 labels
CB07_labels_set2 <- c(
  "4" = "Wealth inequality is normal",
  "5" = "Gov.-made laws and reg. determine people's opportunity for success",
  "6" = "Gov. decisions determine who rules in the economy"
)

#### CB08
# Set 1 labels
CB08_labels <- c(
  "1" = "Rulers",
  "2" = "Regulator",
  "3" = "Leaders",
  "4" = "Caretaker",
  "5" = "Referee"
)

#### CB11
# Set 1 labels
CB11_labels <- c(
  "1" = "Laws and regulations",
  "2" = "Human nature",
  "3" = "World events",
  "4" = "Fate"
)

#### TD 
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
data_list <- lapply(datavariants$datfilenm, fread)
names(data_list) <- datavariants$datnm

##### Linear model #####
#### DK ####
## Create plots
# Set data
uncond_data <- data_list$`DK linear uncond`
cond_data <- data_list$`DK linear cond`

# Set x-axis label
xlabel <- "Probability of supporting policy relative to control"

# List plot configs
plot_configs_DK <- list(
  list(data = uncond_data, 
       outnums = 1:4, 
       model_labels = DK_labels_set1, 
       filename = "DK_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 5:8, 
       model_labels = DK_labels_set2, 
       filename = "DK_wyoung_linear_set2_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 9:13, 
       model_labels = DK_labels_set3, 
       filename = "DK_wyoung_linear_set3_uncond.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:4, 
       model_labels = DK_labels_set1, 
       filename = "DK_wyoung_linear_set1_cond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 5:8, 
       model_labels = DK_labels_set2, 
       filename = "DK_wyoung_linear_set2_cond.png", 
       ncol = 4),
  list(data = cond_data, 
       outnums = 9:13, 
       model_labels = DK_labels_set3, 
       filename = "DK_wyoung_linear_set3_cond.png", 
       ncol = 5,
       xlabs = xlabel)
)

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
       outnums = 3:8, 
       model_labels = CB05_labels_set1, 
       filename = "CB05_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 9:12, 
       model_labels = CB05_labels_set2, 
       filename = "CB05_wyoung_linear_set2_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 3:8, 
       model_labels = CB05_labels_set1, 
       filename = "CB05_wyoung_linear_set1_cond.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 9:12, 
       model_labels = CB05_labels_set2, 
       filename = "CB05_wyoung_linear_set2_cond.png", 
       ncol = 4,
       xlabs = xlabel)
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
       outnums = 1:4, 
       model_labels = CB06_labels_set1, 
       filename = "CB06_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 5:8, 
       model_labels = CB06_labels_set2, 
       filename = "CB06_wyoung_linear_set2_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:4, 
       model_labels = CB06_labels_set1, 
       filename = "CB06_wyoung_linear_set1_cond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 5:8, 
       model_labels = CB06_labels_set2, 
       filename = "CB06_wyoung_linear_set2_cond.png", 
       ncol = 4,
       xlabs = xlabel)
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
       outnums = 1:3, 
       model_labels = CB07_labels_set1, 
       filename = "CB07_wyoung_linear_set1_uncond.png", 
       ncol = 3,
       xlabs = xlabel),
  list(data = uncond_data, 
       outnums = 4:6, 
       model_labels = CB07_labels_set2, 
       filename = "CB07_wyoung_linear_set2_uncond.png", 
       ncol = 3,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:3, 
       model_labels = CB07_labels_set1, 
       filename = "CB07_wyoung_linear_set1_cond.png", 
       ncol = 3,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 4:6, 
       model_labels = CB07_labels_set2, 
       filename = "CB07_wyoung_linear_set2_cond.png", 
       ncol = 3,
       xlabs = xlabel)
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
       filename = "CB08_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:5, 
       model_labels = CB08_labels, 
       filename = "CB08_wyoung_linear_set1_cond.png", 
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
       filename = "CB11_wyoung_linear_set1_uncond.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:4, 
       model_labels = CB11_labels, 
       filename = "CB11_wyoung_linear_set1_cond.png", 
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
       filename = "TD_wyoung_linear_set1_uncond.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = cond_data, 
       outnums = 1:5, 
       model_labels = TD_labels, 
       filename = "TD_wyoung_linear_set1_cond.png", 
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
allplots_ITT <- lapply(plot_configs,process_plot_configs)

##### Probability model #####

##### TOT #####
# Load data
outcomes <- c("DK","CB05","CB06","CB07","CB08","CB11","TD")
datfilenm <- paste0(temp,"/",outcomes,"_wyoung_ivreg_allmodel.csv")
data_list <- lapply(datfilenm, fread)
names(data_list) <- outcomes

#### DK ####
## Create plots
# Set data
ivdata <- data_list$DK

# Set x-axis label
xlabel <- "Probability of supporting policy relative to control"

# List plot configs
plot_configs_DK <- list(
  list(data = ivdata, 
       outnums = 1:4, 
       model_labels = DK_labels_set1, 
       filename = "DK_wyoung_ivreg_set1.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 5:8, 
       model_labels = DK_labels_set2, 
       filename = "DK_wyoung_ivreg_set2.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 9:13, 
       model_labels = DK_labels_set3, 
       filename = "DK_wyoung_ivreg_set3.png", 
       ncol = 5,
       xlabs = xlabel)
)

#### CB05 #### 
## Create plots
# Set data
ivdata <- data_list$CB05

# Set x-axis label
xlabel <- "Probability of prioritizing development goal relative to control"

# List plot configs
plot_configs_CB05 <- list(
  list(data = ivdata, 
       outnums = 3:8, 
       model_labels = CB05_labels_set1, 
       filename = "CB05_wyoung_ivreg_set1.png", 
       ncol = 5,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 9:12, 
       model_labels = CB05_labels_set2, 
       filename = "CB05_wyoung_ivreg_set2.png", 
       ncol = 4,
       xlabs = xlabel)
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
       outnums = 1:4, 
       model_labels = CB06_labels_set1, 
       filename = "CB06_wyoung_ivreg_set1.png", 
       ncol = 4,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 5:8, 
       model_labels = CB06_labels_set2, 
       filename = "CB06_wyoung_ivreg_set2.png", 
       ncol = 4,
       xlabs = xlabel)
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
       outnums = 1:3, 
       model_labels = CB07_labels_set1, 
       filename = "CB07_wyoung_ivreg_set1.png", 
       ncol = 3,
       xlabs = xlabel),
  list(data = ivdata, 
       outnums = 4:6, 
       model_labels = CB07_labels_set2, 
       filename = "CB07_wyoung_ivreg_set2.png", 
       ncol = 3,
       xlabs = xlabel)
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
data <- file.path(temp,datnm) 
anldf <- fread(data)

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
              'hhsize')

basecogctrl <- c('read_stim_time', 'sdbi', 'agreestim', 'crt_intrpt_msg')

basecovlist <- c(basechar, basecogctrl)

## Prepare chosen profile data 
# Subset profile and pairing data
profpairw <- anldf %>% dplyr::select(record,
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
  left_join(dplyr::select(anldf,
                          record,
                          all_of(basecovlist),
                          ConjointOverall_Time,
                          contains("treat"),
                          lfCB),
            by="record",relationship="many-to-one") %>% # Obtain treatment status and covariates
  rename(treat5=treat4,treat4=treat5) # Reverse treat4 and treat5

# Setup the interaction variables
attrb <- c("econ","rights","env","participation")
attrblvl <- lapply(attrb, function(f) {
  cjdff %>% select(starts_with(f)) %>% names()
})
names(attrblvl) <- attrb
attr_levels <- paste(unlist(attrblvl))
for (treatvar in c("treat","complytreat")) {
  assign(paste0(treatvar,"ments"),
         paste0(treatvar,1:5))
  
  assign(paste0("intrterms_",treatvar),
         c(paste0("econ_2:", treatvar, c(1,5)),
           paste0("econ_3:", treatvar, c(1,5)),
           paste0("rights_1:", treatvar, c(2,3)),
           paste0("rights_2:", treatvar, c(2,3)),
           paste0("env_2:", treatvar, c(2,3)),
           paste0("participation_1:", treatvar, 4))
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

# Write a function to prepare results for plotting
plotprep <- function(model) {
  model %>%
    summary() %>% 
    unclass() %>% 
    data.frame() %>% 
    rownames_to_column(var = "term") %>%
    filter(!term %in% c("(Intercept)",
                        paste0("treat", 1:5),
                        paste0("complytreat", 1:5),
                        basechar,
                        basecogctrl,
                        "ConjointOverall_Time")) %>%
    rename(p.unadj = Pr...t..,
           estimate = Estimate,
           std.error = Std..Error) %>%
    mutate(p.adj = p.adjust(p.unadj, method ="holm"),
           conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error,
           term = factor(term, levels=term)) %>%
    mutate(sig   = case_when(
      p.adj < 0.01 ~ "***",
      p.adj < 0.05  ~ "**",
      p.adj < 0.1  ~ "*",
      TRUE          ~ "Null"
    )) 
}

# Write a function to extract selected variables from PDS Lasso
obtain_selecvar <- function(modres) {
  # Extract the selection matrix
  selectionmat <- coef(modres, selection.matrix = TRUE, include.targets = FALSE)$selection.matrix
  
  # Identify variables selected in at least one auxiliary regression
  selected <- rownames(selectionmat)[apply(selectionmat[, -ncol(selectionmat)], 1, function(row) any(row == "x"))]  
  
  # Remove "sum" element
  selected[!selected %in% "sum"]
}


# Write reusable function for plotting and saving the graph
plotnsave <- function(modres, filepath) {
  plot <- ggplot(data = modres, aes(x = estimate, y = term)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    scale_color_manual(values = cb_palette) +
    scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
    scale_y_discrete(limits = rev) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~ model, ncol = 3, labeller = labeller(model = model_labels)) +
    labs(
      x = "Probability of choosing an economic scenario relative to base option",
      y = NULL,
      color = "Statistical significance based on Holm p-values",
      shape = "Statistical significance based on Holm p-values"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    guides(
      color = guide_legend(title.position = "top"),
      shape = guide_legend(title.position = "top")
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

##### ITT ##### 
# Write a function to conduct cluster regression 
clusterols <- function(data,formula) {
  lm.cluster(data = data, formula = formula, cluster = "record")  
}

##### Model 1 estimation (OLS) #####
# Setting up the formula
attrbvarlist <- paste(attr_levels, collapse = " + ")
treatvarlist <- paste(treatments, collapse = " + ")
main_effects <- paste(attrbvarlist,treatvarlist,sep = " + ")
interactions <- paste(intrterms_treat, collapse = " + ")
regressors1 <- paste(main_effects,interactions, sep = " + ")
formula1 <- paste("alt_chosen ~",regressors1)

# Run the model
wgt__ <- NULL  # Initialize wgt__ in the global environment
infmodel1 <- lapply(cjdf, clusterols, formula1)

##### Model 2 covariates selection (PDS LASSO) #####
# Setup the matrices
cjdf_nacl <- lapply(cjdf,na.omit)

# Set inference variables
infervars <- c(attr_levels,treatments,intrterms_treat)  

# Loop through conditional and unconditional data
lassomod2 <- lapply(names(cjdf_nacl), function(sample) {
  # Prepare variables
  regressors <- select(cjdf_nacl[[sample]],all_of(c(attr_levels,
                                                    treatments,
                                                    intrterms_treat,
                                                    basechar)))
  outcome <- cjdf_nacl[[sample]]$alt_chosen
  
  # Run Lasso
  rlassoEffects(x = as.matrix(regressors),
                y = outcome,
                index = infervars,
                method = "double selection")
})
names(lassomod2) <- names(cjdf_nacl)

##### Model 3 covariates selection (PDS LASSO) #####
# Loop through conditional and unconditional data
lassomod3 <- lapply(names(cjdf_nacl), function(sample) {
  # Prepare variables
  regressors <- select(cjdf_nacl[[sample]],all_of(c(attr_levels,
                                                    treatments,
                                                    intrterms_treat,
                                                    basechar,
                                                    basecogctrl,
                                                    "ConjointOverall_Time")))
  outcome <- cjdf_nacl[[sample]]$alt_chosen
  
  # Run Lasso
  rlassoEffects(x = as.matrix(regressors),
                y = outcome,
                index = infervars,
                method = "double selection",
                I3 = c(basecogctrl,"ConjointOverall_Time"))
})
names(lassomod3) <- names(cjdf_nacl)

##### Estimate model 2 and 3 with OLS #####
## Setup formula model 2 and 3
# Extract selected variables from PDS Lasso results
selectedctrls <- lapply(c(lassomod2,lassomod3),obtain_selecvar)
names(selectedctrls)[1:2] <- paste("model 2", names(selectedctrls)[1:2])
names(selectedctrls)[3:4] <- paste("model 3", names(selectedctrls)[3:4])

# Convert character strings to formula objects
formulas <- lapply(names(selectedctrls), function(model) {
  formula_str <- paste(formula1, paste(selectedctrls[[model]], collapse = " + "), sep = " + ")
})
names(formulas) <- names(selectedctrls)

# Run models
infmodels <- mapply(clusterols, cjdf, formulas, SIMPLIFY=F)
names(infmodels) <- names(formulas)

# Prepare results for plotting
modsres <- lapply(c(infmodel1,infmodels),plotprep)

##### Create combined plots #####
# Combine results
allmodres <- list("unconditional"=bind_rows(mutate(modsres$unconditional,model=1),
                                            mutate(modsres$`model 2 unconditional`,model=2),
                                            mutate(modsres$`model 3 unconditional`,model=3)),
                  "conditional"=bind_rows(mutate(modsres$conditional,model=1),
                                          mutate(modsres$`model 2 conditional`,model=2),
                                          mutate(modsres$`model 3 conditional`,model=3)))

# Define a named vector for the new facet labels
model_labels <- c(
  "1" = "Model 1: OLS",
  "2" = "Model 2: PDS Lasso",
  "3" = "Model 3: PDS Lasso"
)

# Plot
plotlist_cjITT <- lapply(names(allmodres), function(model) {
  fignm <- file.path(fig,paste0("conjoint_allmodel_",model,".png"))
  plotnsave(allmodres[[model]], filepath = fignm)
})  
names(plotlist_cjITT) <- names(allmodres)

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
    rownames_to_column(var = "term") %>%
    filter(term == keyterm) %>%
    rename(p.unadj = Pr...t..,
           estimate = Estimate,
           std.error = Std..Error) %>%
    mutate(p.adj = p.adjust(p.unadj, method ="holm"),
           conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error,
           term = factor(term, levels=term)) %>%
    mutate(sig   = case_when(
      p.adj < 0.01 ~ "***",
      p.adj < 0.05  ~ "**",
      p.adj < 0.1  ~ "*",
      TRUE          ~ "Null"
    )) 
}

#### Model 1 estimation (IVREG) ####
# Build the structural equation
attrbvarlist <- paste(attr_levels, collapse = " + ")
treatvarlist <- paste(complytreatments, collapse = " + ")
main_effects <- paste(attrbvarlist,treatvarlist,sep = " + ")
main_interactions <- paste(intrterms_complytreat, collapse = " + ")
main_regressors1 <- paste(main_effects,main_interactions, sep = " + ")
str_formula1 <- paste("alt_chosen ~",main_regressors1)

# Create the interaction list of the reduced form equation
interactions_list1 <- lapply(seq_along(intrterms_treat), function(i) {
  c(intrterms_treat[i], intrterms_complytreat[-i])
})
redf_interactions1 <- lapply(seq_along(interactions_list1), function(i) {
  paste(interactions_list1[[i]], collapse = " + ")
})
redf_regressors1 <- lapply(seq_along(redf_interactions1), function(i) {
  paste(main_effects, redf_interactions1[[i]], sep = " + ")
})

# Build the complete equation list
formulas1 <- lapply(seq_along(redf_interactions1), function(i) {
  paste(str_formula1,redf_regressors1[[i]], sep = " | ")
})
names(formulas1) <- intrterms_complytreat

# Estimate IV
infmodels1 <- lapply(formulas1, IVcse, data = cjdff,
                     cluster_var = "record")

# Prepare results for plotting
modres1 <- lapply(names(infmodels1), function(key) {
  plotprep(infmodels1[[key]], keyterm = key)
}) %>% bind_rows() 

##### Model 2 covariates selection (PDS LASSO) #####
# Setup the matrices
cjdff_nacl <- na.omit(cjdff)

# Set inference variables
infervars <- c(attr_levels,complytreatments,intrterms_complytreat)  

# Prepare variables
regressors <- select(cjdff_nacl,all_of(c(attr_levels,
                                         complytreatments,
                                         intrterms_complytreat,
                                         basechar)))
outcome <- cjdff_nacl$alt_chosen

# Run Lasso
lassomod2 <- rlassoEffects(x = as.matrix(regressors),
              y = outcome,
              index = infervars,
              method = "double selection")

##### Model 3 covariates selection (PDS LASSO) #####
# Prepare variables
regressors <- select(cjdff_nacl,all_of(c(attr_levels,
                                         complytreatments,
                                         intrterms_complytreat,
                                         basechar,
                                         basecogctrl,
                                         "ConjointOverall_Time")))
outcome <- cjdff_nacl$alt_chosen

# Run Lasso
lassomod3 <-  rlassoEffects(x = as.matrix(regressors),
              y = outcome,
              index = infervars,
              method = "double selection",
              I3 = c(basecogctrl,"ConjointOverall_Time"))

##### Estimate model 2 and 3 with IVREG #####
## Setup formula model 2 and 3
# Extract selected variables from PDS Lasso results
selectedctrls <- lapply(list(lassomod2,lassomod3),obtain_selecvar)
names(selectedctrls) <- paste0("lassomod",2:3)

# Create formula
for (equation in c("str_formula","redf_regressors")) {
  assign(equation,
         lapply(names(selectedctrls), function(model) {
           paste(get(paste0(equation,"1")), paste(selectedctrls[[model]], collapse = " + "), sep = " + ")
         }))
}

formulas <- lapply(seq_along(str_formula), function(j) {
  lapply(seq_along(redf_regressors[[j]]), function(i) {
    paste(str_formula[[j]],redf_regressors[[j]][[i]], sep = " | ")
  })
})

# Run models
infmodels <- lapply(seq_along(formulas), function(i) {
  lapply(seq_along(formulas[[i]]), function(j) {
    IVcse(formulas[[i]][[j]], data = cjdff, cluster_var = "record")
  })
})
names(infmodels) <- names(selectedctrls)
for (model in names(infmodels)) {
  names(infmodels[[model]]) <- intrterms_complytreat
}

# Prepare results for plotting
modsres <- lapply(names(infmodels), function(model) {
  lapply(names(infmodels[[model]]), function(key) {
    plotprep(infmodels[[model]][[key]], keyterm = key)
  }) %>% bind_rows()    
})
names(modsres) <- names(infmodels)

##### Create combined plots #####
# Combine results
allmodres <- bind_rows(mutate(modres1,model=1),
                       mutate(modsres$lassomod2,model=2),
                       mutate(modsres$lassomod3,model=3))

# Define a named vector for the new facet labels
model_labels <- c(
  "1" = "Model 1: 2SLS",
  "2" = "Model 2: PDS Lasso + 2SLS",
  "3" = "Model 3: PDS Lasso + 2SLS"
)

# Plot
fignm <- file.path(fig,paste0("conjoint_allmodel_TOT.png"))
plotcjTOT <- plotnsave(allmodres, filepath = fignm)

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