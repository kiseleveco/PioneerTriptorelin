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

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 0. Paths — adjust outputFolder if running at a different site
# ------------------------------------------------------------------------------
outputFolder <- "/Users/andreikiselev/Documents/Research_Questions/RQ3-tirptorelin/MarketScan_triptorelin"
plotFolder   <- file.path(outputFolder, "plots")
if (!dir.exists(plotFolder)) dir.create(plotFolder, recursive = TRUE)

# ------------------------------------------------------------------------------
# Helper: strip the Atlas cohort name prefix e.g. "[PIONEER_Triptorelin] Foo"
# ------------------------------------------------------------------------------
.cleanName <- function(x) {
  sub("^\\[PIONEER_Triptorelin\\]\\s*", "", x)
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
      label = factor(label, levels = label[order(pctOfTarget)])
    )

  p_comb <- ggplot(comb, aes(x = label, y = pctOfTarget, fill = label)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = paste0(pctOfTarget, "%\n(n = ", cohortSubjects, ")")),
              hjust = -0.08, size = 3.5) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, max(comb$pctOfTarget) * 1.35),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Combination treatment regimens at triptorelin initiation",
      subtitle = "Percentage of triptorelin cohort (n = total subjects)",
      x        = NULL,
      y        = "% of triptorelin cohort"
    ) +
    theme_pioneer()

  ggsave(file.path(plotFolder, "combinationRegimens.png"),
         p_comb, width = 8, height = 4.5, dpi = 300)
  message("Saved: combinationRegimens.png")
} else {
  message("combinationRegimens.csv not found — skipping.")
}

# ==============================================================================
# 2. Adverse event incidence rates (forest-style plot)
# ==============================================================================
irFile <- file.path(outputFolder, "safetyAnalysis", "incidenceRates.csv")

if (file.exists(irFile)) {
  ir <- read_csv(irFile, col_types = cols(), show_col_types = FALSE)
  ir <- ir %>%
    mutate(
      outcomeName = .cleanName(outcomeName),
      typeLabel   = ifelse(type == "episodic", "Episodic (all events)", "Chronic (first event)"),
      outcomeName = factor(outcomeName, levels = rev(unique(outcomeName)))
    )

  p_ir <- ggplot(ir, aes(x = incidenceRatePer1000Py, y = outcomeName,
                          colour = outcomeName, shape = typeLabel)) +
    geom_errorbarh(aes(xmin = ci95Lb, xmax = ci95Ub),
                   height = 0.25, linewidth = 0.8) +
    geom_point(size = 4) +
    scale_colour_manual(values = .outcomeColour(levels(ir$outcomeName)),
                        guide = "none") +
    scale_shape_manual(
      values = c("Episodic (all events)" = 16, "Chronic (first event)" = 17),
      name   = "Analysis type"
    ) +
    labs(
      title    = "Adverse event incidence rates",
      subtitle = "Per 1,000 person-years with 95% Poisson CI  |  MarketScan",
      x        = "Incidence rate per 1,000 person-years",
      y        = NULL
    ) +
    theme_pioneer() +
    theme(legend.position = "right")

  ggsave(file.path(plotFolder, "incidenceRates.png"),
         p_ir, width = 9, height = 4, dpi = 300)
  message("Saved: incidenceRates.png")
} else {
  message("incidenceRates.csv not found — skipping.")
}

# ==============================================================================
# 3. Kaplan-Meier cumulative incidence curves
# ==============================================================================
kmFile <- file.path(outputFolder, "safetyAnalysis", "kmTable.csv")

if (file.exists(kmFile)) {
  km <- read_csv(kmFile, col_types = cols(), show_col_types = FALSE)
  km <- km %>%
    filter(!is.na(surv)) %>%
    mutate(
      outcome  = .cleanName(outcome),
      time_yrs = time / 365.25,
      cum_inc  = 1 - surv,
      ci_lo    = 1 - ci_upper,   # note: flip bounds for cumulative incidence
      ci_hi    = 1 - ci_lower
    )

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

  p_km <- ggplot(km_plot, aes(x = time_yrs, y = cum_inc,
                               colour = outcome, fill = outcome)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, colour = NA) +
    geom_step(linewidth = 0.9) +
    geom_vline(xintercept = 3, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    scale_colour_manual(values = col_map, name = NULL) +
    scale_fill_manual(values = col_map, guide = "none") +
    scale_x_continuous(breaks = 0:ceiling(max(km_plot$time_yrs, na.rm = TRUE)),
                       labels = function(x) paste0(x, "y")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, NA)) +
    labs(
      title    = "Cumulative incidence of adverse events",
      subtitle = "Kaplan-Meier estimates with 95% CI  |  MarketScan",
      x        = "Time from triptorelin initiation",
      y        = "Cumulative incidence"
    ) +
    theme_pioneer()

  ggsave(file.path(plotFolder, "kmCurves_all.png"),
         p_km, width = 10, height = 6, dpi = 300)
  message("Saved: kmCurves_all.png")

  # Individual plot per outcome
  for (oc in outcomes_present) {
    km_oc <- km_plot %>% filter(outcome == oc)
    p_oc <- ggplot(km_oc, aes(x = time_yrs, y = cum_inc)) +
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15,
                  fill = col_map[[oc]], colour = NA) +
      geom_step(colour = col_map[[oc]], linewidth = 1) +
      geom_vline(xintercept = 3, linetype = "dashed", colour = "grey50") +
      scale_x_continuous(breaks = 0:ceiling(max(km_oc$time_yrs, na.rm = TRUE)),
                         labels = function(x) paste0(x, "y")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, NA)) +
      labs(
        title    = oc,
        subtitle = "Kaplan-Meier cumulative incidence with 95% CI  |  MarketScan",
        x        = "Time from triptorelin initiation",
        y        = "Cumulative incidence"
      ) +
      theme_pioneer()

    safe_name <- gsub("[^A-Za-z0-9_]", "_", oc)
    ggsave(file.path(plotFolder, paste0("km_", safe_name, ".png")),
           p_oc, width = 7, height = 5, dpi = 300)
    message("Saved: km_", safe_name, ".png")
  }
} else {
  message("kmTable.csv not found — skipping.")
}

# ==============================================================================
# 4. 3-year cumulative incidence point estimates
# ==============================================================================
km3File <- file.path(outputFolder, "safetyAnalysis", "km3YearEstimates.csv")

if (file.exists(km3File)) {
  km3 <- read_csv(km3File, col_types = cols(), show_col_types = FALSE)
  km3 <- km3 %>%
    mutate(
      outcome  = .cleanName(outcome),
      cum_inc  = cumulative_incidence_3yr,
      ci_lo    = 1 - ci_upper_3yr,
      ci_hi    = 1 - ci_lower_3yr,
      outcome  = factor(outcome, levels = rev(unique(outcome)))
    )

  p_km3 <- ggplot(km3, aes(x = cum_inc, y = outcome, colour = outcome)) +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                   height = 0.25, linewidth = 0.9) +
    geom_point(size = 4) +
    scale_colour_manual(values = .outcomeColour(levels(km3$outcome)),
                        guide = "none") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "3-year cumulative incidence of adverse events",
      subtitle = "Kaplan-Meier point estimates with 95% CI  |  MarketScan",
      x        = "3-year cumulative incidence",
      y        = NULL
    ) +
    theme_pioneer()

  ggsave(file.path(plotFolder, "km3YearEstimates.png"),
         p_km3, width = 8, height = 4, dpi = 300)
  message("Saved: km3YearEstimates.png")
} else {
  message("km3YearEstimates.csv not found — skipping.")
}

message("\nAll plots saved to: ", plotFolder)
