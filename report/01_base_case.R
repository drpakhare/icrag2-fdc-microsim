# =====================================================================
# ICRAG-2 FDC-HTA: 01 — BASE CASE ANALYSIS
# =====================================================================
# 4 scenarios x 4 pricing = 16 simulations (N=10,000 each)
# Builds per-strategy survival & trace data for chapter-based report.
# Saves: report/outputs/base_case.rds
#
# Usage:  Rscript report/01_base_case.R   (from project root)
# =====================================================================

cat("\n===============================================\n")
cat("  ICRAG-2 FDC-HTA: Base Case Analysis\n")
cat("===============================================\n\n")

source("report/00_config.R")

t_start <- Sys.time()

# =====================================================================
# Run all 16 combinations (parallelised across scenario-pricing pairs)
# =====================================================================
combos <- expand.grid(scen = scenario_names, pricing = pricing_names,
                      stringsAsFactors = FALSE)

cat(sprintf("Running %d scenario-pricing combinations (N=%s, %d-yr)...\n",
            nrow(combos), format(N_PAT, big.mark=","), HORIZON))

raw_results <- parallel::mclapply(1:nrow(combos), function(idx) {
  scen    <- combos$scen[idx]
  pricing <- combos$pricing[idx]

  res <- tryCatch(
    run_scenario(scen, pricing, n_pat = N_PAT, horizon = HORIZON, seed = SEED),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)

  dC   <- res$FDC$avg_cost - res$SoC$avg_cost
  dE   <- res$FDC$avg_qaly - res$SoC$avg_qaly
  icer <- safe_icer(dC, dE)
  nmb  <- (dE * WTP_THRESHOLD) - dC

  list(
    scenario     = scenario_evidence[[scen]]$label,
    scenario_key = scen,
    pricing      = pricing_labels[which(pricing_names == pricing)],
    pricing_key  = pricing,
    fdc_cost     = res$FDC$avg_cost,
    soc_cost     = res$SoC$avg_cost,
    delta_cost   = dC,
    fdc_qaly     = res$FDC$avg_qaly,
    soc_qaly     = res$SoC$avg_qaly,
    delta_qaly   = dE,
    icer         = icer,
    nmb          = nmb,
    res_obj      = res
  )
}, mc.cores = N_CORES)

# Remove NULLs
results_list <- Filter(Negate(is.null), raw_results)
cat(sprintf("  %d/%d runs succeeded.\n", length(results_list), nrow(combos)))

# =====================================================================
# Results data frame
# =====================================================================
results_df <- do.call(rbind, lapply(results_list, function(x) {
  data.frame(
    Scenario   = x$scenario,
    Pricing    = x$pricing,
    FDC_Cost   = x$fdc_cost,
    SoC_Cost   = x$soc_cost,
    Delta_Cost = x$delta_cost,
    FDC_QALY   = x$fdc_qaly,
    SoC_QALY   = x$soc_qaly,
    Delta_QALY = x$delta_qaly,
    ICER       = x$icer,
    NMB        = x$nmb,
    stringsAsFactors = FALSE
  )
}))

# =====================================================================
# Per-strategy outputs for chapter-based report
# =====================================================================
strategy_outputs <- list()

for (scen in scenario_names) {
  # Find Govt RC result for this scenario
  idx <- which(sapply(results_list, function(x)
    x$scenario_key == scen && x$pricing_key == "govt"))

  if (length(idx) == 0) next
  r <- results_list[[idx[1]]]

  # Survival & trace
  st <- build_surv_trace(r$res_obj, HORIZON)

  # Base case table (all 4 pricing perspectives)
  scen_rows <- results_df %>% filter(Scenario == scenario_evidence[[scen]]$label)
  base_tbl <- scen_rows %>%
    select(Pricing, Delta_Cost, Delta_QALY, ICER, NMB) %>%
    mutate(
      Delta_Cost = format(round(Delta_Cost, 0), big.mark = ","),
      Delta_QALY = round(Delta_QALY, 4),
      ICER       = ifelse(is.na(ICER), "Dominated", format(round(ICER, 0), big.mark = ",")),
      NMB        = format(round(NMB, 0), big.mark = ",")
    )

  strategy_outputs[[scen]] <- list(
    label      = scenario_evidence[[scen]]$label,
    tier       = scenario_evidence[[scen]]$tier,
    base_tbl   = base_tbl,
    surv_df    = st$surv_df,
    trace_long = st$trace_long,
    govt_icer  = r$icer,
    govt_nmb   = r$nmb,
    govt_dC    = r$delta_cost,
    govt_dE    = r$delta_qaly
  )
  cat(sprintf("  %-40s Govt ICER = %s\n",
              scenario_evidence[[scen]]$label,
              format(round(r$icer, 0), big.mark = ",")))
}

# =====================================================================
# 4x4 ICER matrix
# =====================================================================
scen_labels_ordered <- sapply(scenario_names, function(s) scenario_evidence[[s]]$label)
icer_matrix <- matrix(NA, nrow = length(scenario_names), ncol = length(pricing_names))
rownames(icer_matrix) <- scen_labels_ordered
colnames(icer_matrix) <- pricing_labels

for (i in 1:nrow(results_df)) {
  ri <- match(results_df$Scenario[i], rownames(icer_matrix))
  ci <- match(results_df$Pricing[i], colnames(icer_matrix))
  if (!is.na(ri) && !is.na(ci)) icer_matrix[ri, ci] <- results_df$ICER[i]
}

icer_display <- icer_matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Scenario") %>%
  mutate(across(-Scenario, ~ifelse(is.na(.), "\u2014", format(round(., 0), big.mark = ","))))

# =====================================================================
# Save
# =====================================================================
saveRDS(
  list(
    results_df       = results_df,
    results_list     = results_list,
    strategy_outputs = strategy_outputs,
    icer_display     = icer_display,
    timestamp        = Sys.time()
  ),
  "report/outputs/base_case.rds"
)

elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
cat(sprintf("\n✓ Base case complete. Saved to report/outputs/base_case.rds [%.1f min]\n", elapsed))
