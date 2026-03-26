# =====================================================================
# ICRAG-2 FDC-HTA: 02 — DSA + THRESHOLD ANALYSIS
# =====================================================================
# One-way DSA: 12 params x 4 scenarios x 2 bounds = 96 engine runs
# Threshold analysis: HR sweep (THRESH_STEPS values x 4 scenarios)
# All parallelised via mclapply.
# Requires: report/outputs/base_case.rds
# Saves:    report/outputs/dsa.rds
#
# Usage:  Rscript report/02_dsa.R   (from project root)
# =====================================================================

cat("\n===============================================\n")
cat("  ICRAG-2 FDC-HTA: DSA + Threshold Analysis\n")
cat("===============================================\n\n")

source("report/00_config.R")

# Wait for base case
f <- "report/outputs/base_case.rds"
if (!file.exists(f)) {
  cat("Waiting for base case results...\n")
  elapsed <- 0
  while (!file.exists(f) && elapsed < 600) { Sys.sleep(5); elapsed <- elapsed + 5 }
  if (!file.exists(f)) stop("Timed out waiting for base_case.rds")
}
base_data <- readRDS(f)

t_start <- Sys.time()

# =====================================================================
# HELPER: Get DSA bounds for a parameter in a scenario
# =====================================================================
get_dsa_bounds <- function(scen, param) {
  if (param %in% c("hr_MI", "hr_Stroke", "hr_HF", "hr_SCD")) {
    lo <- scenario_evidence[[scen]][[paste0(param, "_lo")]]
    hi <- scenario_evidence[[scen]][[paste0(param, "_hi")]]
  } else if (param %in% c("p_MI", "p_Stroke", "p_HF", "p_SCD")) {
    bv <- switch(param, p_MI = BASE_P_MI, p_Stroke = BASE_P_STROKE,
                 p_HF = BASE_P_HF, p_SCD = BASE_P_SCD)
    lo <- bv * 0.8; hi <- bv * 1.2
  } else if (param %in% c("c_fdc_daily", "c_soc_daily")) {
    fdc_base  <- fdc_lookup[[scen]]$price
    soc_daily <- sum(sapply(fdc_lookup[[scen]]$soc_drugs, function(d) drug_prices$govt[[d]]))
    bv <- if (param == "c_fdc_daily") fdc_base else soc_daily
    lo <- bv * 0.8; hi <- bv * 1.2
  } else if (param %in% c("u_stable", "u_hf")) {
    bv <- if (param == "u_stable") 0.814 else 0.55
    lo <- max(0, bv * 0.8); hi <- min(1, bv * 1.2)
  } else {
    lo <- NA; hi <- NA
  }
  list(lo = lo, hi = hi)
}

# =====================================================================
# PART 1: ONE-WAY DSA (parallelised)
# =====================================================================
cat("Part 1: One-way DSA\n")

dsa_results   <- list()
tornado_plots <- list()

for (scen in scenario_names) {
  cat(sprintf("  DSA: %s ", scenario_short[[scen]]))

  # Base ICER from base case
  so <- base_data$strategy_outputs[[scen]]
  base_icer <- so$govt_icer

  # Build job list: each job = (param, bound_value)
  jobs <- list()
  for (param in params_to_vary) {
    bounds <- get_dsa_bounds(scen, param)
    jobs[[length(jobs) + 1]] <- list(param = param, value = bounds$lo, side = "lo")
    jobs[[length(jobs) + 1]] <- list(param = param, value = bounds$hi, side = "hi")
  }

  # Run all perturbations in parallel
  icers <- parallel::mclapply(jobs, function(j) {
    run_dsa_single(scen, j$param, j$value, n_pat = DSA_N, horizon = HORIZON, seed = SEED)
  }, mc.cores = N_CORES)

  # Reassemble into tornado data
  tornado_data <- data.frame(
    Parameter = character(), Param_Label = character(),
    Low_ICER = numeric(), High_ICER = numeric(),
    Base_ICER = numeric(), Range = numeric(),
    stringsAsFactors = FALSE
  )

  for (p_idx in seq_along(params_to_vary)) {
    param   <- params_to_vary[p_idx]
    icer_lo <- icers[[(p_idx - 1) * 2 + 1]]
    icer_hi <- icers[[(p_idx - 1) * 2 + 2]]
    rng     <- if (!is.na(icer_lo) && !is.na(icer_hi)) abs(icer_hi - icer_lo) else 0
    tornado_data <- rbind(tornado_data, data.frame(
      Parameter   = param,
      Param_Label = param_labels[[param]],
      Low_ICER    = ifelse(is.na(icer_lo), base_icer, icer_lo),
      High_ICER   = ifelse(is.na(icer_hi), base_icer, icer_hi),
      Base_ICER   = base_icer,
      Range       = rng,
      stringsAsFactors = FALSE
    ))
  }

  dsa_results[[scen]] <- tornado_data

  # Pre-build tornado plot
  df_sorted <- tornado_data %>%
    arrange(desc(Range)) %>% head(10) %>%
    mutate(Param_Label = factor(Param_Label, levels = rev(Param_Label)))

  tornado_plots[[scen]] <- ggplot(df_sorted) +
    geom_segment(aes(x = Param_Label, xend = Param_Label,
                     y = Low_ICER, yend = High_ICER),
                 linewidth = 5, color = "#0072B2", alpha = 0.7) +
    geom_point(aes(x = Param_Label, y = Low_ICER), size = 2, color = "#D55E00") +
    geom_point(aes(x = Param_Label, y = High_ICER), size = 2, color = "#D55E00") +
    geom_hline(yintercept = base_icer, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = scenario_labels[[scen]], x = NULL, y = "ICER (Rs per QALY gained)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12),
          panel.grid.major.y = element_blank())

  cat("done.\n")
}

# =====================================================================
# PART 2: THRESHOLD ANALYSIS (HR sweep)
# =====================================================================
cat("\nPart 2: Threshold Analysis\n")

threshold_results <- list()

for (scen in scenario_names) {
  cat(sprintf("  Threshold: %s ", scenario_short[[scen]]))

  # Identify the most influential HR from tornado
  td <- dsa_results[[scen]]
  hr_params <- td %>% filter(grepl("^hr_", Parameter)) %>% arrange(desc(Range))
  top_hr <- hr_params$Parameter[1]  # e.g. "hr_MI"

  hr_lo <- scenario_evidence[[scen]][[paste0(top_hr, "_lo")]]
  hr_hi <- scenario_evidence[[scen]][[paste0(top_hr, "_hi")]]
  hr_seq <- seq(hr_lo, hr_hi, length.out = THRESH_STEPS)

  # Run sweep in parallel
  sweep_icers <- parallel::mclapply(hr_seq, function(h) {
    run_dsa_single(scen, top_hr, h, n_pat = THRESH_N, horizon = HORIZON, seed = SEED)
  }, mc.cores = N_CORES)

  sweep_df <- data.frame(
    HR_value = hr_seq,
    ICER     = unlist(sweep_icers),
    stringsAsFactors = FALSE
  )

  # Find threshold crossing (where ICER crosses WTP)
  threshold_hr <- NA_real_
  for (i in 2:nrow(sweep_df)) {
    if (!is.na(sweep_df$ICER[i-1]) && !is.na(sweep_df$ICER[i])) {
      if ((sweep_df$ICER[i-1] - WTP_THRESHOLD) * (sweep_df$ICER[i] - WTP_THRESHOLD) < 0) {
        # Linear interpolation
        x1 <- sweep_df$HR_value[i-1]; x2 <- sweep_df$HR_value[i]
        y1 <- sweep_df$ICER[i-1];     y2 <- sweep_df$ICER[i]
        threshold_hr <- x1 + (WTP_THRESHOLD - y1) * (x2 - x1) / (y2 - y1)
        break
      }
    }
  }

  # Pre-build threshold plot
  thresh_plot <- ggplot(sweep_df, aes(x = HR_value, y = ICER)) +
    geom_line(color = "#0072B2", linewidth = 1.2) +
    geom_hline(yintercept = WTP_THRESHOLD, linetype = "dashed", color = "red", linewidth = 0.8) +
    {if (!is.na(threshold_hr))
      geom_vline(xintercept = threshold_hr, linetype = "dotted", color = "darkgreen", linewidth = 0.8)} +
    {if (!is.na(threshold_hr))
      annotate("label", x = threshold_hr, y = WTP_THRESHOLD * 1.1,
               label = sprintf("Threshold HR = %.3f", threshold_hr),
               fill = "white", size = 3.5)} +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste0(scenario_labels[[scen]], ": Threshold Analysis"),
         subtitle = paste("Swept parameter:", param_labels[[top_hr]]),
         x = paste("Hazard Ratio:", param_labels[[top_hr]]),
         y = "ICER (Rs per QALY gained)",
         caption = "Red dashed = WTP threshold; green dotted = threshold HR") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Point estimate HR for this parameter
  point_hr <- scenario_evidence[[scen]][[top_hr]]

  threshold_results[[scen]] <- list(
    swept_param    = top_hr,
    swept_label    = param_labels[[top_hr]],
    sweep_df       = sweep_df,
    threshold_hr   = threshold_hr,
    point_estimate = point_hr,
    margin         = if (!is.na(threshold_hr)) threshold_hr - point_hr else NA_real_,
    plot           = thresh_plot
  )

  cat(sprintf("swept %s, threshold=%.3f, point=%.2f\n",
              top_hr, ifelse(is.na(threshold_hr), -1, threshold_hr), point_hr))
}

# =====================================================================
# DSA combined table
# =====================================================================
dsa_combined <- do.call(rbind, lapply(scenario_names, function(scen) {
  df <- dsa_results[[scen]]
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df$Scenario <- scenario_labels[[scen]]
  df %>% arrange(desc(Range)) %>% head(5) %>%
    select(Scenario, Param_Label, Low_ICER, High_ICER, Base_ICER, Range)
}))

# =====================================================================
# Save
# =====================================================================
saveRDS(
  list(
    dsa_results       = dsa_results,
    tornado_plots     = tornado_plots,
    dsa_combined      = dsa_combined,
    threshold_results = threshold_results,
    timestamp         = Sys.time()
  ),
  "report/outputs/dsa.rds"
)

elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
cat(sprintf("\n✓ DSA + Threshold complete. Saved to report/outputs/dsa.rds [%.1f min]\n", elapsed))
