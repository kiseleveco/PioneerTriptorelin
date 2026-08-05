# ==============================================================================
# PioneerTriptorelin — Visualisation Script
#
# Reads analysis outputs and produces publication-ready plots:
#   1. Combination regimen distribution (bar chart)
#   2. Adverse event incidence rates with 95% CI (forest-style plot)
#   3. Kaplan-Meier cumulative incidence curves (one per outcome)
#   4. 3-year cumulative incidence point estimates with 95% CI (dot-range plot)
#
# Plots are saved to <outputFolder>/plots/ as PNG files (300 dpi).
# ==============================================================================
rm (list = ls())
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(gt)
library(DescTools)
library(stringr)

# ------------------------------------------------------------------------------
# 0. Paths — adjust outputFolder if running at a different site
# ------------------------------------------------------------------------------
outputFolder <- "~/output/MarketScan_triptorelin_v2"
resultFolder   <- "~/output/MarketScan_triptorelin_v2/plots"
if (!dir.exists(resultFolder)) dir.create(resultFolder, recursive = TRUE)

# ------------------------------------------------------------------------------
# Helper: strip the Atlas cohort name prefix e.g. "[PIONEER_Triptorelin] Foo"
# ------------------------------------------------------------------------------
.cleanName <- function(x) {
  sub("^\\[PIONEER_Triptorelin\\]\\s*", "", x)
}


# helper function for the number of digits
round_based_on_digits <- function(x) {
  sapply(x, function(val) {
    x_str <- as.character(val)
    digits_before_decimal <- nchar(strsplit(x_str, "\\.")[[1]][1])

    if (digits_before_decimal == 1) {
      return(format(round(val, 1), nsmall = 1))
    } else if (digits_before_decimal == 2) {
      return(round(val))
    } else {
      return(val)
    }
  } )
}


# Consistent ggplot2 theme
theme_pioneer <- function() {
  theme_bw(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey92", colour = NA),
      legend.position   = "bottom",
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(colour = "grey40", size = 10)
    )
}

# Outcome colour palette (consistent across all plots)
outcome_colours <- c(
  "Acute Cardiac Event"          = "#E63946",
  "Thromboembolism"              = "#F4A261",
  "Diabetes"                     = "#2A9D8F",
  "Fractures"                    = "#457B9D"
)

# Match a cleaned name to the nearest palette key (partial match on first word)
.outcomeColour <- function(names, palette = outcome_colours) {
  cols <- vapply(names, function(nm) {
    hit <- names(palette)[sapply(names(palette), function(k) grepl(k, nm, fixed = TRUE))]
    if (length(hit)) palette[[hit[1]]] else "grey60"
  }, character(1))
  cols
}

# ==============================================================================
# 1. Combination regimen distribution
# ==============================================================================
combFile <- file.path(outputFolder, "utilisationAnalysis", "combinationRegimens.csv")

if (file.exists(combFile)) {
  comb <- read_csv(combFile, col_types = cols(), show_col_types = FALSE)
  comb <- comb %>%
    mutate(
      label = .cleanName(regimenName),
      label = sub("mPC treated ", "", label),
      label = factor(label, levels = label[order(pctOfTarget)]),
      prop_ci = BinomCI(x = cohortEntries, n = sum(cohortEntries), method = "wilson")
    ) %>%
    mutate (prop = paste0(
      round_based_on_digits(prop_ci[, 1]*100),
      " (",
      round_based_on_digits(prop_ci[, 2]*100),
      ", ",
      round_based_on_digits(prop_ci[, 3]*100),
      ")"
    )
    )

  comb %>%
    mutate (regimenName =
             str_replace (regimenName,  "\\[PIONEER_Triptorelin\\]\\s* mPC treated", "")) %>%
    select (regimenName, cohortEntries, prop) %>%
  gt() %>%
    tab_header (
      title =  "Cohort conuts",
      subtitle = "MarketScan"
    ) %>%
    cols_label(
      regimenName = "Treatment",
      cohortEntries = "n",
      prop = "Proportion and 95% CI (Wilson)") %>%
    gtsave (file.path(resultFolder, "Cohort counts.docx"))
}

# ==============================================================================
# 2. Adverse event incidence rates (forest-style plot)
# ==============================================================================
irFile <- file.path(outputFolder, "utilisationAnalysis", "incidenceRates.csv")

if (file.exists(irFile)) {
  ir <- read_csv(irFile, col_types = cols(), show_col_types = FALSE)
  ir <- ir %>%
    mutate(
      outcomeName = .cleanName(outcomeName),
      typeLabel   = ifelse(type == "episodic", "Episodic (all events)", "Chronic (first event)"),
      outcomeName = factor(outcomeName, levels = rev(unique(outcomeName)))
    )

  ir %>%
    select (outcomeName, type, incidenceRatePer1000Py:ci95Ub) %>%
    mutate (res = paste0(incidenceRatePer1000Py, " (", ci95Lb, ", ", ci95Ub, ")"),
            type = recode_values (type,
                                  "episodic" ~ "Episodic (all events)",
                                  "chronic" ~ "Chronic (first event)"
                                  )
            ) %>%
    select(- c(contains("95"), incidenceRatePer1000Py)) %>%
    gt() %>%
    tab_header (
      title =  "Adverse event incidence rates",
      subtitle = "MarketScan"
    ) %>%
    cols_label(
      outcomeName = "Outcome",
      type = "Analysis type",
      res = "Incidence rate and 95% CI (per 1,000 person-years)") %>%
    gtsave (file.path(outputFolder, "Incidence rates.docx"))
  message("Saved: Incidence rates.docx")
} else {
  message("incidenceRates.csv not found — skipping.")
}

# ==============================================================================
# 3. Kaplan-Meier cumulative incidence curves
# ==============================================================================
## Data preparation
kmFile <- file.path(outputFolder, "safetyAnalysis", "kmTable.csv")

if (file.exists(kmFile)) {
  km <- read_csv(kmFile, col_types = cols(), show_col_types = FALSE)
  km <- km %>%
    group_by (databaseId, outcome) %>%
    arrange(databaseId, outcome, time) %>%
    mutate(
      outcome  = .cleanName(outcome),
      time_yrs = time / 365.25,
      cum_inc  = 1 - surv,
      ci_lo    = 1 - ci_upper,   # note: flip bounds for cumulative incidence
      ci_hi    = 1 - ci_lower,
      cum_events = cumsum(n_event)
    ) %>%
    mutate (
      cum_events = case_when (cum_events <5 ~ "<5",
                              T ~ as.character(cum_events))
    ) %>%
    ungroup()

  # Anchor each curve at time 0
  anchors <- km %>%
    distinct(outcome, databaseId) %>%
    mutate(time_yrs = 0, cum_inc = 0, ci_lo = 0, ci_hi = 0)

  km_plot <- bind_rows(anchors, km) %>% arrange(outcome, time_yrs)

  outcomes_present <- unique(km_plot$outcome)
  col_map <- setNames(
    .outcomeColour(outcomes_present),
    outcomes_present
  )
}

label.min <- -1.5

for (outcomes.id in (unique (km_plot$outcome))) {
  df.temp <- km_plot %>%
    filter (outcome == outcomes.id) %>%
    select(-time) %>%
    rename (time = time_yrs)

  ggplot (df.temp, aes(x = time, y = cum_inc*100, ymin = ci_lo*100, ymax = ci_hi*100)) +
    geom_line() +
    geom_ribbon(alpha=0.3)  +
    # scale_colour_manual(color = cols[[i]]) +
    # scale_fill_manual(color = cols[[i]]) +
    labs (x = "Years after initiation of Triptorelin",
          y = paste0 ("Cumulative incidence (%)\n", df.temp$outcome[1])) +
    scale_x_continuous(breaks = seq(0, 5, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1*100, by = .1*100)) +
    coord_cartesian(ylim = c(-0.015*100, 1*100),
                    xlim = c(-2,5)) +
    theme_pioneer() +
    geom_text(x= -1.2, color = "black", y= label.min, label= "Number at risk (cum. events)") +
    geom_text(x = 0, color = "black",y = label.min, label = paste0(df.temp %>% filter(time > 0) %>% slice (1) %>% pull(n_risk), " (",
                                                                   df.temp %>%filter(time > 0) %>% slice (1) %>% pull(cum_events), ")")) +
    geom_text(x = 1, color = "black",y = label.min, label = paste0(df.temp %>% filter(time <= 1) %>% slice_tail() %>% pull(n_risk), " (",
                                                                   df.temp %>% filter(time <= 1) %>% slice_tail()  %>% pull(cum_events), ")"))+
    geom_text(x = 2, color = "black",y = label.min, label = paste0(df.temp %>% filter(time <= 2) %>% slice_tail()  %>% pull(n_risk), " (",
                                                                   df.temp %>% filter(time <= 2) %>% slice_tail()  %>% pull(cum_events), ")")) +
    geom_text(x = 3, color = "black",y = label.min, label = paste0(df.temp %>% filter(time <= 3) %>% slice_tail()  %>% pull(n_risk), " (",
                                                                   df.temp %>% filter(time <= 3) %>% slice_tail()  %>% pull(cum_events), ")")) +
    geom_text(x = 4, color = "black",y = label.min, label = paste0(df.temp %>% filter(time <= 4) %>% slice_tail()  %>% pull(n_risk), " (",
                                                                   df.temp %>% filter(time <= 4) %>% slice_tail()  %>% pull(cum_events), ")"))+
    geom_text(x = 5, color = "black",y = label.min, label = paste0(df.temp %>% filter(time <= 5) %>% slice_tail()  %>% pull(n_risk), " (",
                                                                   df.temp %>% filter(time <= 5) %>% slice_tail()  %>% pull(cum_events), ")"))


  ggsave(file.path(resultFolder, paste0("km_", outcomes.id, ".png")),
         width = 10, height = 6, dpi = 300)
}


## KM at 3 years
km_plot %>%
  group_by (outcome) %>%
  filter (time_yrs <= 3) %>%
  slice_tail() %>%
  mutate (res = paste0(round_based_on_digits(cum_inc*100) ," (", round_based_on_digits(ci_lo*100), ", ", round_based_on_digits(ci_hi*100), ")")) %>%
  select(outcome, res) %>%
  gt() %>%
  tab_header (
    title =  "",
    subtitle = "MarketScan"
  ) %>%
  cols_label(
    res = "Survival probability (%) at 3 years") %>%
  gtsave (file.path(resultFolder, "Cumulative incidence at three year.docx"))
