#*******************************************************************************
#  ORWELL NARRATIVE TESTING: PRESENTATION FIGURES SPLIT BY TREATMENT ARM
#-------------------------------------------------------------------------------
#  Purpose:
#    Recreate the CB05 (Development priority) and DK (Policy support)
#    Westfall-Young figures for presentation, but reorganized so that each
#    figure shows ONE treatment arm and only the outcomes that are relevant
#    for that arm (per Table C.1 of the paper / pre-analysis plan).
#
#    The script produces 4 figures per outcome family (one per treatment arm),
#    8 figures total. Each y-axis row is a model variant (OLS, +PDS Lasso),
#    and each panel column (facet) is a relevant outcome for that arm.
#
#  Notes on integration:
#    - This script is designed to be slotted into the existing 4b main
#      analysis.R pipeline. It mirrors the path conventions there (`temp`,
#      `fig`, the cb_palette, the equation recoding, the label vectors).
#    - When integrated, the PRELIMINARIES, paths, palette, label-vector, and
#      data-loading blocks below are redundant with the parent script and
#      can be deleted; only the PLOT FUNCTION and GENERATE & SAVE blocks
#      need to be retained.
#*******************************************************************************

##### PRELIMINARIES #####
# Clear console and environment
graphics.off(); rm(list=ls()); cat("\14");

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_unload(p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse, data.table)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "elgha") {
  # base_dir <- "G:/" # laptop
  base_dir <- "H:/" # computer
}

# Set directory (mirrors 4b main analysis.R)
master <- file.path(base_dir,
                    "Shared drives",
                    "Projects",
                    "2025",
                    "Orwell",
                    "Breadcrumbs",
                    "10 Quantitative Narrative Testing",
                    "9 Main survey")
setwd(master)

ipt  = file.path(getwd(), "2a input")
temp = file.path(getwd(), "2b temp")
opt  = file.path(getwd(), "2c output")
lg   = file.path(getwd(), "3 log")
fig  = file.path(getwd(), "4 figures")
tbl  = file.path(getwd(), "5 tables")

# Okabe-Ito color-blind-friendly palette (same as 4b main analysis.R)
cb_palette <- c("p < .01" = "#E69F00",  # orange
                "p < .05" = "#56B4E9",  # sky blue
                "p < .1"  = "#009E73",  # bluish green
                "Null"    = "#999999")  # grey

##### LOAD & RECODE DATA #####
# Read the linear ITT uncond CSVs and recode equation labels to match the
# convention used in the paper figures (OLS / +PDS Lasso).
load_and_recode <- function(outcome) {
  fn <- file.path(temp, paste0(outcome, "_wyoung_linear_allmodel_uncond.csv"))
  df <- fread(fn)
  df$equation <- dplyr::recode(df$equation,
                               "Model 1" = "OLS",
                               "Model 2" = "+PDS Lasso")
  df
}

data_CB05 <- load_and_recode("CB05")
data_DK   <- load_and_recode("DK")

##### OUTCOME LABELS #####
# Reused verbatim from 4b main analysis.R, combined into single vectors
# (rather than set1/set2) since this script splits by treatment, not outcome.

# CB05 (Development priority)
CB05_labels <- c(
  "3"  = "Increase income of all citizens",
  "5"  = "Improve human resources quality",
  "6"  = "Encourage citizens to make democratic decisions about their life",
  "7"  = "Build public infrastructure",
  "8"  = "Provide basic services",
  "9"  = "Advance lagging-behind regions",
  "10" = "Enable citizens achieve important goals according to their own choice",
  "11" = "Ensure fairness for all",
  "12" = "Ensure next generations have a better life"
)

# DK (Policy support)
DK_labels <- c(
  "1"  = "Utilities price hike",
  "2"  = "Larger gov. budget for public transport",
  "3"  = "Larger gov. budget for env. friendly tech and renewable energies",
  "4"  = "Displaced people receives benefit from infra. utilization",
  "5"  = "Subsidy for energy-efficient houses and buildings",
  "6"  = "Larger soc. asst. amount",
  "7"  = "Larger soc. asst. recipient coverage",
  "8"  = "Higher vehicle tax",
  "9"  = "Industry subsidy to switch to env. friendly tech and energy",
  "10" = "Employment insurance",
  "11" = "Forest clearance for agriculture",
  "12" = "Forest clearance for settlement development",
  "13" = "Forest clearance for infrastructure"
)

##### ADAPTIVE LAYOUT HELPER #####
# Pick a panel grid + figure dimensions based on # of relevant outcomes.
# Goal: keep panels reasonably large for slide-deck readability, and prefer
# at most 2 rows so the figure fits a 16:9 slide.
compute_layout <- function(n_outs) {
  ncol <- dplyr::case_when(
    n_outs <= 3 ~ as.integer(n_outs),
    n_outs == 4 ~ 2L,
    n_outs <= 6 ~ 3L,
    n_outs <= 8 ~ 4L,
    TRUE        ~ 5L                # 9+ outcomes: 5 cols, 2 rows
  )
  nrow <- as.integer(ceiling(n_outs / ncol))

  # Per-panel dimensions (inches). Tuned so text remains legible on slides.
  panel_w  <- 3.4
  panel_h  <- 2.6
  pad_w    <- 1.0   # axes + side margins
  pad_h    <- 1.8   # title + legend + axis + margins

  list(
    ncol   = ncol,
    nrow   = nrow,
    width  = ncol * panel_w + pad_w,
    height = nrow * panel_h + pad_h
  )
}

##### PLOT FUNCTION (BY-TREATMENT) #####
# For one treatment arm, plot only the outcomes that are present in the data
# for that arm (which by construction are the pre-specified relevant ones).
# y-axis  = equation (OLS top, +PDS Lasso below).
# facets  = outcome (only the relevant ones for the treatment).
# title   = treatment arm name.
generate_treatment_plot <- function(data,
                                    treatment_name,
                                    outcome_labels,
                                    xlab) {

  # Filter to the requested treatment arm
  d <- data %>% filter(narnm == treatment_name)

  if (nrow(d) == 0) {
    stop(sprintf("No rows found for treatment '%s'. Check narnm values.",
                 treatment_name))
  }

  # Identify relevant outcomes (those that survived the PAP relevance filter)
  outnums_present <- sort(unique(d$outnum))
  n_outs          <- length(outnums_present)

  # Subset and order the outcome labels to match what's present
  labels_present  <- outcome_labels[as.character(outnums_present)]
  if (any(is.na(labels_present))) {
    missing <- outnums_present[is.na(labels_present)]
    stop(sprintf("Missing label(s) for outnum(s): %s",
                 paste(missing, collapse = ", ")))
  }

  # Layout for facets
  lyt <- compute_layout(n_outs)

  # Factor ordering
  model_levels <- c("OLS", "+PDS Lasso")
  d <- d %>%
    mutate(
      equation = factor(equation, levels = model_levels),
      outnum   = factor(outnum, levels = outnums_present),
      sig      = factor(sig, levels = c("p < .01", "p < .05", "p < .1", "Null"))
    )

  # Wrapped panel labels
  wrapped_labels <- as_labeller(labels_present,
                                default = label_wrap_gen(width = 25))

  ggplot(d, aes(x = coef, y = equation)) +
    geom_errorbar(
      aes(xmin = lci, xmax = uci, color = sig),
      width = 0.2,
      orientation = "y"
    ) +
    geom_point(aes(color = sig, shape = sig), size = 3) +
    scale_color_manual(values = cb_palette, drop = FALSE) +
    scale_shape_manual(values = c("p < .01" = 8,
                                  "p < .05" = 17,
                                  "p < .1"  = 16,
                                  "Null"    = 1),
                       drop = FALSE) +
    scale_y_discrete(limits = rev) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = treatment_name,
      x     = xlab,
      y     = NULL,
      color = "Significance (Westfall-Young p-values)",
      shape = "Significance (Westfall-Young p-values)"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      legend.position  = "top",
      legend.direction = "horizontal",
      axis.title.x     = element_text(size = 10),
      strip.text       = element_text(size = 10)
    ) +
    guides(
      color = guide_legend(title.position = "left", nrow = 1, byrow = TRUE),
      shape = guide_legend(title.position = "left", nrow = 1, byrow = TRUE)
    ) +
    facet_wrap(~ outnum,
               ncol     = lyt$ncol,
               labeller = labeller(outnum = wrapped_labels))
}

##### GENERATE & SAVE #####
# Treatment arms and short filename slugs
treatments <- c(
  "Fix the distribution" = "fixdist",
  "No victimization"     = "novict",
  "Balanced development" = "balanced",
  "Equal opportunity"    = "equalopp"
)

# Per-outcome configuration
outcome_configs <- list(
  CB05 = list(
    data   = data_CB05,
    labels = CB05_labels,
    xlab   = "Probability of prioritizing development goal relative to control"
  ),
  DK = list(
    data   = data_DK,
    labels = DK_labels,
    xlab   = "Probability of supporting policy relative to control"
  )
)

# Loop over outcome families and treatments
plots_by_treat <- list()

for (oc in names(outcome_configs)) {

  cfg <- outcome_configs[[oc]]
  plots_by_treat[[oc]] <- list()

  for (i in seq_along(treatments)) {

    treat_name <- names(treatments)[i]
    treat_slug <- treatments[[i]]

    # Build the plot
    p <- generate_treatment_plot(
      data           = cfg$data,
      treatment_name = treat_name,
      outcome_labels = cfg$labels,
      xlab           = cfg$xlab
    )

    plots_by_treat[[oc]][[treat_name]] <- p

    # Recompute layout (so width/height match the in-plot ncol)
    n_outs <- length(unique(cfg$data$outnum[cfg$data$narnm == treat_name]))
    lyt    <- compute_layout(n_outs)

    # Save figure
    fname <- paste0(oc, "_wyoung_linear_uncond_T", i, "_", treat_slug, ".png")
    ggsave(
      filename = file.path(fig, fname),
      plot     = p,
      width    = lyt$width,
      height   = lyt$height,
      bg       = "white"
    )

    message(sprintf("Saved: %s  (%dx%d panels, %.1f\" x %.1f\")",
                    fname, lyt$ncol, lyt$nrow, lyt$width, lyt$height))
  }
}

# `plots_by_treat[[outcome]][[treatment_name]]` holds each ggplot object
# in case further tweaking is needed in an interactive session.
