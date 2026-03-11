#*******************************************************************************
#  ORWELL NARRATIVE TESTING: DESCRIPTIVE ANALYSIS							   
#*******************************************************************************
##### PRELIMINARIES #####
# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(rmarkdown, knitr, kableExtra, tidyverse, 
               kableExtra, readxl, data.table)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "elgha") {
  base_dir <- "G:/" # laptop
  #base_dir <- "H:/" # computer
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

############################ Code for Rendering Rmd #################################

# Code for rendering R Markdown of Orwell narrative testing descriptive analysis
# April 24, 2025
# Contributors : Azzah

# Run R Markdown
code = file.path("C:",
                 "Users",
                 current_user,
                 "Documents",
                 "GitHub",
                 "Orwell")

rmdfile = file.path(code,"3 descriptive.Rmd")

# Render the R Markdown file and save the output in the 'tbl' directory
filenm <- paste0("descriptive-paper-",Sys.Date(),".pdf")
render(
  input = rmdfile,
  output_file = filenm,
  output_dir = tbl
)

############################ Compiling unadjusted outcome differences #################################
# Store file names
files <- list.files(temp, pattern = "unadjusted")
filelist <- list("binary" = grepv("binary",files),
                 "likert" = grepv("likert",files))

# Read and bind files within each list element
dfs <- map(filelist, function(files) {
  map_dfr(files, ~ read_excel(file.path(temp, .x)))
})

# Change outcome names
rename_outcome <- function(x){
  
  n <- as.numeric(str_extract(x, "\\d+$"))
  
  case_when(
    str_detect(x, "CB05r_cloned")        ~ paste0("CB05", letters[n]),
    str_detect(x, "topthree")            ~ paste0("CB06", letters[n]),
    str_detect(x, "agree")               ~ paste0("CB07", letters[n]),
    str_detect(x, "bigrole")             ~ paste0("CB11", letters[n]),
    str_detect(x, "support|QDKr_cloned") ~ paste0("DK", n),
    str_detect(x, "higheff|TD0_cloned")  ~ paste0("TD", n),
    str_detect(x, "CB06_Orderr_cloned")  ~ paste0("CB06", letters[n]),
    str_detect(x, "CB07r_cloned")        ~ paste0("CB07", letters[n]),
    str_detect(x, "CB11r_cloned")        ~ paste0("CB11", letters[n]),
    TRUE ~ x
  )
}
dfs$binary$outcome <- rename_outcome(dfs$binary$outcome)
dfs$likert$outcome <- rename_outcome(dfs$likert$outcome)

# Import outcome label
olables <- read_excel(file.path(ipt,"outcome label.xlsx"))

# Merge labels
dfs$binary <- dfs$binary |>
  left_join(olables, by = c("outcome" = "Outcome"))
dfs$likert <- dfs$likert |>
  left_join(olables, by = c("outcome" = "Outcome"))

# Prepare dataframe for table export
prep_table <- function(df){
  
  df |>
    mutate(
      Family = factor(
        Family,
        levels = c(
          "Development priority",
          "Whom should the government prioritize?",
          "Opinions about the economy",
          "Factors influencing the economy",
          "Policy support",
          "Personal efficacy"
        )
      )
    ) |>
    arrange(Family) |>
    select(
      Family,
      Outcome = Label,
      Control_mean = mean_control,
      T1 = T1_mean_difference,
      T2 = T2_mean_difference,
      T3 = T3_mean_difference,
      T5 = T5_mean_difference
    ) |>
    rename(
      "Control mean" = Control_mean,
      "Fix the Distribution" = T1,
      "No Victimization" = T2,
      "Balanced Development" = T3,
      "Equal Opportunity" = T5
    ) |>
    mutate(across(where(is.numeric), ~round(.x, 3)))
}
binary_tab <- prep_table(dfs$binary)
likert_tab <- prep_table(dfs$likert)

# Table rendering function
make_outcome_table <- function(df, caption){
  
  family_sizes <- df |>
    dplyr::count(Family)
  
  tab_print <- df |>
    dplyr::select(-Family)
  
  tab <- kableExtra::kbl(
    tab_print,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    caption = caption
  ) |>
    kableExtra::add_header_above(c(
      " " = 2,
      "Difference to control" = 4
    )) |>
    kableExtra::column_spec(1, width = "8cm") |>
    kableExtra::kable_styling(
      latex_options = "repeat_header"
    )
  
  start <- 1
  
  for(i in seq_len(nrow(family_sizes))){
    
    end <- start + family_sizes$n[i] - 1
    
    tab <- tab |>
      kableExtra::pack_rows(
        family_sizes$Family[i],
        start,
        end,
        indent = FALSE,
        latex_gap_space = "0.6em"
      )
    
    start <- end + 1
  }
  
  tab
}

# Build the tables
binary_table <- make_outcome_table(
  binary_tab,
  "Unadjusted differences of outcomes in binary indicator format"
)

likert_table <- make_outcome_table(
  likert_tab,
  "Unadjusted differences of outcomes in Likert format"
)

# Export tables
kableExtra::save_kable(
  binary_table,
  file.path(tbl,"unadj_diff_binary_outcomes.tex")
)

kableExtra::save_kable(
  likert_table,
  file.path(tbl,"unadj_diff_likert_outcomes.tex")
)

############################ Correct stimulus interpretation #################################
# Load data
date <- "20250304" # Set data date
datnm <- paste0("processed_",date,".csv") 
data <- file.path(ipt,datnm) 
maindata <- fread(data)

# Calculate correct interpretation rate by treatment arms
crt_ipt_msg_rate <- maindata %>%
  filter(!lfCB %in% c(4,6)) %>% # Remove non-development stimulus
  group_by(lfCB) %>%
  summarise(crt_intrpt_msg = mean(crt_intrpt_msg, na.rm = TRUE)) %>%
  mutate(word_count=case_when(
    lfCB==1 ~ 75,
    lfCB==2 ~ 98,
    lfCB==3 ~ 89,
    lfCB==5 ~ 130
  ))

# Prepare plot
labels <- c(
  "1" = "Fix the distribution",
  "2" = "No victimization",
  "3" = "Balanced development",
  "5" = "Equal opportunity"
)

df <- crt_ipt_msg_rate %>%
  arrange(word_count) %>%
  mutate(
    lfCB = factor(lfCB, levels = lfCB),
    lfCB_lab = labels[as.character(lfCB)]
  )

# scaling factor for the secondary axis
scale_factor <- max(df$word_count) / max(df$crt_intrpt_msg)

# Build the plot
p <- ggplot(df, aes(x = reorder(lfCB_lab, word_count))) +
  
  geom_col(
    aes(y = word_count, fill = "Word count"),
    width = 0.6
  ) +
  
  geom_line(
    aes(y = crt_intrpt_msg * scale_factor,
        color = "Correct interpretation (%)",
        group = 1),
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = crt_intrpt_msg * scale_factor,
        color = "Correct interpretation (%)"),
    size = 3
  ) +
  
  scale_y_continuous(
    name = "Word count",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Correct interpretation (%)")
  ) +
  
  scale_fill_manual(
    values = c("Word count" = "#999999"),
    breaks = c("Word count")
  ) +
  
  scale_color_manual(
    values = c("Correct interpretation (%)" = "#E69F00"),
    breaks = c("Correct interpretation (%)")
  ) +
  
  labs(x = NULL, fill = NULL, color = NULL) +
  
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

ggsave(
  filename = file.path(fig, "comprehension.png"),
  plot = p,
  width = 12.8,
  height = 6.98,
  dpi = 300
)