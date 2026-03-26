# =====================================================================
# ICRAG-2 FDC-HTA: 03 — PSA + EVPI
# =====================================================================
# PSA: 5,000 iterations x 4 scenarios (N=1,000 per iter), parallelised
# EVPI: computed from PSA output (no additional engine runs)
# Saves: report/outputs/psa.rds
#
# Usage:  Rscript report/03_psa.R   (from project root)
# =====================================================================

cat("\n===============================================\n")
cat("  ICRAG-2 FDC-HTA: PSA + Value of Information\n")
cat("===============================================\n\n")

source("report/00_config.R")

t_start <- Sys.time()

# =====================================================================
# PSA: Per-scenario, parallelised across iterations
# =====================================================================
psa_results <- list()

for (scen in scenario_names) {
  cat(sprintf("PSA: %s [%s iters, %d cores] ",
              scenario_short[[scen]], format(PSA_ITERATIONS, big.mark=","), N_CORES))

  scen_obj <- scenario_evidence[[scen]]

  # Run all iterations in parallel
  raw_iters <- parallel::mclapply(1:PSA_ITERATIONS, function(iter) {
    set.seed(SEED + iter)

    # Sample HRs (log-normal, tier-aware SE)
    se_log <- if (scen_obj$tier == 1) 0.10 else 0.30
    hr_mi_s     <- exp(rnorm(1, log(scen_obj$hr_MI),     se_log))
    hr_stroke_s <- exp(rnorm(1, log(scen_obj$hr_Stroke),  se_log))
    hr_hf_s     <- exp(rnorm(1, log(scen_obj$hr_HF),      se_log))
    hr_scd_s    <- exp(rnorm(1, log(scen_obj$hr_SCD),     se_log))

    # Clamp to evidence bounds
    hr_mi_s     <- pmax(scen_obj$hr_MI_lo,     pmin(scen_obj$hr_MI_hi,     hr_mi_s))
    hr_stroke_s <- pmax(scen_obj$hr_Stroke_lo, pmin(scen_obj$hr_Stroke_hi, hr_stroke_s))
    hr_hf_s     <- pmax(scen_obj$hr_HF_lo,     pmin(scen_obj$hr_HF_hi,     hr_hf_s))
    hr_scd_s    <- pmax(scen_obj$hr_SCD_lo,    pmin(scen_obj$hr_SCD_hi,    hr_scd_s))

    # Sample transition probabilities
    p_mi_s     <- pmax(0.001, rnorm(1, BASE_P_MI,     BASE_P_MI * 0.2))
    p_stroke_s <- pmax(0.001, rnorm(1, BASE_P_STROKE, BASE_P_STROKE * 0.2))
    p_hf_s     <- pmax(0.001, rnorm(1, BASE_P_HF,     BASE_P_HF * 0.2))
    p_scd_s    <- pmax(0.001, rnorm(1, BASE_P_SCD,    BASE_P_SCD * 0.2))

    tp_s <- build_transition_matrices(
      p_mi_s, p_stroke_s, p_hf_s, p_scd_s, BASE_P_BG_MORT,
      BASE_CF_MI, BASE_CF_STROKE, BASE_P_HF_DEATH,
      hr_MI = hr_mi_s, hr_Stroke = hr_stroke_s,
      hr_HF = hr_hf_s, hr_SCD = hr_scd_s
    )

    costs_s <- build_costs_list(scen, "govt", state_costs)

    res_s <- tryCatch(
      run_microsim_engine(tp_s, costs_s, utils_list,
                          n_pat = PSA_N_PER_ITER, horizon = HORIZON,
                          seed = SEED + iter),
      error = function(e) NULL
    )

    if (is.null(res_s) || is.null(res_s$FDC) || is.null(res_s$SoC)) return(NULL)

    dC   <- res_s$FDC$avg_cost - res_s$SoC$avg_cost
    dE   <- res_s$FDC$avg_qaly - res_s$SoC$avg_qaly
    icer <- safe_icer(dC, dE)

    # NMB for both arms (needed for EVPI)
    nmb_fdc <- (res_s$FDC$avg_qaly * WTP_THRESHOLD) - res_s$FDC$avg_cost
    nmb_soc <- (res_s$SoC$avg_qaly * WTP_THRESHOLD) - res_s$SoC$avg_cost

    data.frame(
      Iteration  = iter,
      Delta_Cost = dC,
      Delta_QALY = dE,
      ICER       = icer,
      NMB_FDC    = nmb_fdc,
      NMB_SoC    = nmb_soc,
      CE_at_WTP  = (dE * WTP_THRESHOLD) - dC > 0,
      stringsAsFactors = FALSE
    )
  }, mc.cores = N_CORES)

  # Combine (remove NULLs)
  psa_iterations_data <- do.call(rbind, Filter(Negate(is.null), raw_iters))
  psa_results[[scen]] <- psa_iterations_data

  n_ok <- nrow(psa_iterations_data)
  p_ce <- sum(psa_iterations_data$CE_at_WTP, na.rm = TRUE) / n_ok
  cat(sprintf(" %d/%d ok, P(CE)=%.1f%%\n", n_ok, PSA_ITERATIONS, p_ce * 100))
}

# =====================================================================
# EVPI COMPUTATION (from PSA output, no engine runs)
# =====================================================================
cat("\nComputing EVPI...\n")

evpi_results <- list()

for (scen in scenario_names) {
  psa_d <- psa_results[[scen]]
  if (is.null(psa_d) || nrow(psa_d) == 0) next

  # Per-iteration: max NMB under perfect information
  max_nmb_per_iter <- pmax(psa_d$NMB_FDC, psa_d$NMB_SoC)

  # Expected NMB under current information (pick strategy with higher mean)
  mean_nmb_fdc <- mean(psa_d$NMB_FDC, na.rm = TRUE)
  mean_nmb_soc <- mean(psa_d$NMB_SoC, na.rm = TRUE)
  current_best_nmb <- max(mean_nmb_fdc, mean_nmb_soc)

  # EVPI per patient
  evpi_per_patient <- mean(max_nmb_per_iter, na.rm = TRUE) - current_best_nmb

  # Population EVPI
  pop_evpi <- evpi_per_patient * ELIGIBLE_POP_ANNUAL * EVPI_DECISION_HORIZON

  evpi_results[[scen]] <- list(
    evpi_per_patient    = evpi_per_patient,
    pop_evpi            = pop_evpi,
    pop_evpi_crore      = pop_evpi / 1e7,  # Rs crore
    mean_nmb_fdc        = mean_nmb_fdc,
    mean_nmb_soc        = mean_nmb_soc,
    optimal_strategy    = if (mean_nmb_fdc >= mean_nmb_soc) "FDC" else "SoC",
    decision_confidence = max(mean(psa_d$CE_at_WTP, na.rm = TRUE),
                              1 - mean(psa_d$CE_at_WTP, na.rm = TRUE))
  )

  cat(sprintf("  %-40s EVPI/patient = Rs %s, Pop EVPI = Rs %s crore\n",
              scenario_short[[scen]],
              format(round(evpi_per_patient, 0), big.mark = ","),
              format(round(pop_evpi / 1e7, 1), big.mark = ",")))
}

# =====================================================================
# CEAC data (all scenarios overlaid)
# =====================================================================
wtp_range <- seq(0, 750000, by = 25000)
ceac_data <- data.frame(WTP = numeric(), P_CE = numeric(),
                        Scenario = character(), stringsAsFactors = FALSE)

for (scen in scenario_names) {
  psa_d <- psa_results[[scen]]
  if (is.null(psa_d) || nrow(psa_d) == 0) next
  for (wtp in wtp_range) {
    nmb  <- (psa_d$Delta_QALY * wtp) - psa_d$Delta_Cost
    p_ce <- sum(nmb > 0, na.rm = TRUE) / nrow(psa_d)
    ceac_data <- rbind(ceac_data, data.frame(
      WTP = wtp, P_CE = p_ce,
      Scenario = scenario_labels[[scen]],
      stringsAsFactors = FALSE
    ))
  }
}

# =====================================================================
# PSA summary statistics
# =====================================================================
psa_summary_list <- list()

for (scen in scenario_names) {
  psa_d <- psa_results[[scen]]
  if (is.null(psa_d) || nrow(psa_d) == 0) next

  psa_summary_list[[scen]] <- data.frame(
    Scenario   = scenario_evidence[[scen]]$label,
    Tier       = scenario_evidence[[scen]]$tier,
    Mean_ICER  = mean(psa_d$ICER, na.rm = TRUE),
    ICER_2p5   = quantile(psa_d$ICER, 0.025, na.rm = TRUE),
    ICER_97p5  = quantile(psa_d$ICER, 0.975, na.rm = TRUE),
    P_CE       = sum(psa_d$CE_at_WTP, na.rm = TRUE) / nrow(psa_d),
    P_Dominant = sum(psa_d$Delta_QALY > 0 & psa_d$Delta_Cost < 0, na.rm = TRUE) / nrow(psa_d),
    stringsAsFactors = FALSE
  )
}

psa_summary_df <- do.call(rbind, psa_summary_list)
rownames(psa_summary_df) <- NULL

# =====================================================================
# CE plane data (all scenarios)
# =====================================================================
ce_plane_data <- do.call(rbind, lapply(scenario_names, function(scen) {
  psa_d <- psa_results[[scen]]
  if (is.null(psa_d) || nrow(psa_d) == 0) return(NULL)
  psa_d$Scenario <- scenario_labels[[scen]]
  psa_d
}))

# =====================================================================
# Save
# =====================================================================
saveRDS(
  list(
    psa_results    = psa_results,
    evpi_results   = evpi_results,
    ceac_data      = ceac_data,
    psa_summary_df = psa_summary_df,
    ce_plane_data  = ce_plane_data,
    timestamp      = Sys.time()
  ),
  "report/outputs/psa.rds"
)

elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
total_iters <- sum(sapply(psa_results, nrow))
cat(sprintf("\n✓ PSA + EVPI complete. %s iterations saved to report/outputs/psa.rds [%.1f min]\n",
            format(total_iters, big.mark = ","), elapsed))
