##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Clear and load packages 
# install.packages("pacman") # install the package if you haven't 
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse,data.table,readxl,fastDummies,hdm,jmvReadWrite,miceadds,broom)

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
width = 3840
height = 2094
res = 300

# Define the Okabe-Ito color-blind-friendly palette
cb_palette <- c("***" = "#E69F00",  # orange
                "**"  = "#56B4E9",  # sky blue
                "*"   = "#009E73",  # bluish green
                "Null"= "#999999")  # grey

##### DATA PREPARATION #####
# Set data date
date <- "20250304"

# Load data
dataname <- paste0("processed_",date,".csv") 
data <- file.path(temp,dataname) 
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

basecogctrl <- c('read_stim_time', 'sdbi', 'agreestim')

basecovlist <- c(basechar, basecogctrl)

##### CONJOINT #####
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
                          starts_with("treat")),
            by="record",relationship="many-to-one") # Obtain treatment status and covariates

### Model 1 estimation (OLS) ###
# Setting up the formula
attrb <- c("econ","rights","env","participation")
attrblvl <- lapply(attrb, function(f) {
  cjdff %>% select(starts_with(f)) %>% names()
})
names(attrblvl) <- attrb
attr_levels <- paste(unlist(attrblvl))
treatments <- paste0("treat", 1:5)
attrbvarlist <- paste(attr_levels, collapse = " + ")
treatvarlist <- paste(treatments, collapse = " + ")
main_effects <- paste(attrbvarlist,treatvarlist,sep = " + ")
intrterms <- c(
  paste0("econ_2:treat", c(1,5)),
  paste0("econ_3:treat", c(1,5)),
  paste0("rights_1:treat", c(2,3)),
  paste0("rights_2:treat", c(2,3)),
  paste0("env_2:treat", c(2,3)),
  paste0("participation_1:treat", 4)
)
interactions <- paste(intrterms, collapse = " + ")
regressors1 <- paste(main_effects,interactions, sep = " + ")
formula1 <- paste("alt_chosen ~ ",regressors1)

# Run the model
infmodel1 <- lm.cluster(formula1, data=cjdff, cluster="record") 

# Prepare results for plotting
mod1res <- infmodel1 %>% 
  summary() %>% 
  unclass() %>% 
  data.frame() %>% 
  rownames_to_column(var = "term") %>%
  filter(!term %in% c("(Intercept)",paste0("treat", 1:5),basechar)) %>%
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

# Create the plot
# Open a PNG device
fignm = file.path(fig,"conjoint_model1.png")
png(fignm, width = width, height = height, res = res)

# Generate the plot
ggplot(mod1res, aes(x = estimate, y = term)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
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

# Close the device to save the file
dev.off()

### Model 2 estimation (PD LASSO) ###
# Setup the variables
for (treat in treatments) {
  for (levels in attr_levels) {
    cjdff[,paste0(levels,":",treat)] <- cjdff[,..levels] * cjdff[,..treat] 
  }
}

# Setup the matrices
cjdff_naclean <- na.omit(cjdff)
regressors2 <- select(cjdff_naclean,all_of(c(attr_levels,treatments,intrterms,basechar)))
outcome <- cjdff_naclean$alt_chosen
infervars <- c(attr_levels,treatments,intrterms)

# Run Lasso
lassomodel2 <- rlassoEffects(x = as.matrix(regressors2),
                             y = outcome,
                             index = infervars,
                             method = "double selection")

# Extract the selection matrix
selectionmat_lassomod2 <- coef(lassomodel2, selection.matrix = TRUE, include.targets = FALSE)$selection.matrix

# Identify variables selected in at least one auxiliary regression
selectedctrls_lassomod2 <- rownames(selectionmat_lassomod2)[apply(selectionmat_lassomod2[, -ncol(selectionmat_lassomod2)], 1, function(row) any(row == "x"))]

# Run OLS with cluster SE and selected covariates
# Setup formula
formula2 <- paste(formula1, paste(selectedctrls_lassomod2,collapse = " + "), sep = " + ")

# Run model
infmodel2 <- lm.cluster(formula2, data=cjdff, cluster="record") 

# Prepare results for plotting
mod2res <- infmodel2 %>% 
  summary() %>% 
  unclass() %>% 
  data.frame() %>% 
  rownames_to_column(var = "term") %>%
  filter(!term %in% c("(Intercept)",paste0("treat", 1:5),basechar)) %>%
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

# Create the plot
# Open a PNG device
fignm = file.path(fig,"conjoint_model2.png")
png(fignm, width = width, height = height, res = res)

# Generate the plot
ggplot(mod2res, aes(x = estimate, y = term)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
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

# Close the device to save the file
dev.off()

### Model 3 estimation (PD LASSO) ###
# Setup the matrices
regressors3 <- select(cjdff_naclean,all_of(c(attr_levels,
                                             treatments,
                                             intrterms,
                                             basechar,
                                             basecogctrl,
                                             "ConjointOverall_Time")))

# Run Lasso
lassomodel3 <- rlassoEffects(x = as.matrix(regressors3),
                             y = outcome,
                             index = infervars,
                             method = "double selection",
                             I3 = c(basecogctrl,"ConjointOverall_Time"))

# Extract the selection matrix
selectionmat_lassomod3 <- coef(lassomodel3, selection.matrix = TRUE, include.targets = FALSE)$selection.matrix

# Identify variables selected in at least one auxiliary regression
selectedctrls_lassomod3 <- rownames(selectionmat_lassomod3)[apply(selectionmat_lassomod3[, -ncol(selectionmat_lassomod3)], 1, function(row) any(row == "x"))]

# Run OLS with cluster SE and selected covariates
# Setup formula
formula3 <- paste(formula1, paste(selectedctrls_lassomod3,collapse = " + "), sep = " + ")

# Run model
infmodel3 <- lm.cluster(formula3, data=cjdff, cluster="record") 

# Prepare results for plotting
mod3res <- infmodel3 %>% 
  summary() %>% 
  unclass() %>% 
  data.frame() %>% 
  rownames_to_column(var = "term") %>%
  filter(!term %in% c("(Intercept)",paste0("treat", 1:5),basechar,basecogctrl,"ConjointOverall_Time")) %>%
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

# Create the plot
# Open a PNG device
fignm = file.path(fig,"conjoint_model3.png")
png(fignm, width = width, height = height, res = res)

# Generate the plot
ggplot(mod3res, aes(x = estimate, y = term)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
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

# Close the device to save the file
dev.off()


##3 Combined plot ###
# Combine results
allmodres <- bind_rows(mutate(mod1res,model=1), mutate(mod2res,model=2), mutate(mod3res,model=3))

# Define a named vector for the new facet labels
model_labels <- c(
  "1" = "Model 1: OLS",
  "2" = "Model 2: PDS Lasso",
  "3" = "Model 3: PDS Lasso"
)

# Open a PNG device
fignm = file.path(fig,"conjoint_allmodel.png")
png(fignm, width = width, height = height, res = res)

# Use facet_wrap
# ggplot(allmodres, aes(x = estimate, y = term)) +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
#   geom_point(aes(color = sig, shape = sig), size = 3) +
#   scale_color_manual(values = cb_palette) +
#   scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
#   scale_y_discrete(limits = rev) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#   facet_wrap(~ model, ncol = 3, scales = "free_y", labeller = labeller(model = model_labels)) +
#   labs(
#     x = "Probability of choosing an economic scenario relative to base option",
#     y = NULL,
#     color = "Statistical significance based on Holm p-values",
#     shape = "Statistical significance based on Holm p-values"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   guides(
#     color = guide_legend(title.position = "top"),
#     shape = guide_legend(title.position = "top")
#   )

# Use facet_grid
ggplot(allmodres, aes(x = estimate, y = term)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = c("***" = 8, "**" = 17, "*" = 16, "Null" = 1)) +
  scale_y_discrete(limits = rev) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(rows = vars(term), cols = vars(model), scales = "free_y", labeller = labeller(model = model_labels)) +
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

# Close the device to save the file
dev.off()

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