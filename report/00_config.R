# =====================================================================
# ICRAG-2 FDC-HTA: SHARED CONFIGURATION
# =====================================================================
# Single source of truth for all parameters, data structures, helpers.
# Sourced by 01_base_case.R, 02_dsa.R, 03_psa.R, and the QMD report.
#
# Usage:  source("report/00_config.R")   # from project root
#         source("00_config.R")           # from report/ directory
# =====================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(parallel)

# Source the microsimulation engine (handle both cwd locations)
engine_path <- if (file.exists("engine_v2.R")) "engine_v2.R" else "../engine_v2.R"
source(engine_path)

# =====================================================================
# PARALLEL COMPUTING
# =====================================================================
N_CORES <- max(1, parallel::detectCores() - 1)
cat(sprintf("  Parallel cores available: %d\n", N_CORES))

# =====================================================================
# GLOBAL CONSTANTS
# =====================================================================
N_PAT          <- 10000   # Patients per base-case run
HORIZON        <- 30      # Lifetime horizon (years)
PSA_ITERATIONS <- 5000    # PSA draws
PSA_N_PER_ITER <- 1000    # Patients per PSA iteration
DSA_N          <- 1000    # Patients per DSA perturbation run
THRESH_N       <- 1000    # Patients per threshold analysis run
THRESH_STEPS   <- 20      # HR sweep granularity per parameter
DISC_RATE      <- 0.03    # Annual discount rate (costs & utilities)
WTP_THRESHOLD  <- 236000  # Rs 2,36,000 = 1x India GDP/capita FY24-25
SEED           <- 42

# Population EVPI parameters
ELIGIBLE_POP_ANNUAL <- 2500000  # ~2.5M post-ACS patients/year in India
EVPI_DECISION_HORIZON <- 5     # Years over which the decision is relevant

# =====================================================================
# SCENARIO EVIDENCE: 4 FDC Strategies with Tier Classification
# =====================================================================
scenario_evidence <- list(
  secure = list(
    tier = 1,
    label = "SECURE type Rational FDC",
    hr_MI = 0.70, hr_Stroke = 0.65, hr_HF = 0.80, hr_SCD = 0.80,
    hr_MI_lo = 0.55, hr_MI_hi = 0.90,
    hr_Stroke_lo = 0.50, hr_Stroke_hi = 0.85,
    hr_HF_lo = 0.60, hr_HF_hi = 1.00,
    hr_SCD_lo = 0.60, hr_SCD_hi = 1.00,
    source = "SECURE Trial (Castellano et al., NEJM 2022)",
    note = "Direct RCT: FDC vs same 3 drugs as separate pills, post-MI, N=2,499"
  ),
  dapt_atorva = list(
    tier = 2,
    label = "Triple DAPT + Atorvastatin FDC",
    hr_MI = 0.95, hr_Stroke = 0.95, hr_HF = 0.97, hr_SCD = 0.97,
    hr_MI_lo = 0.80, hr_MI_hi = 1.05,
    hr_Stroke_lo = 0.80, hr_Stroke_hi = 1.05,
    hr_HF_lo = 0.85, hr_HF_hi = 1.05,
    hr_SCD_lo = 0.85, hr_SCD_hi = 1.05,
    source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
    note = "No direct MACE trial. HR from adherence-outcome modelling."
  ),
  dapt_rosuva = list(
    tier = 2,
    label = "Triple DAPT + Rosuvastatin FDC",
    hr_MI = 0.95, hr_Stroke = 0.95, hr_HF = 0.97, hr_SCD = 0.97,
    hr_MI_lo = 0.80, hr_MI_hi = 1.05,
    hr_Stroke_lo = 0.80, hr_Stroke_hi = 1.05,
    hr_HF_lo = 0.85, hr_HF_hi = 1.05,
    hr_SCD_lo = 0.85, hr_SCD_hi = 1.05,
    source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
    note = "Same adherence-derived HRs as DAPT+Atorva."
  ),
  dapt_only = list(
    tier = 2,
    label = "DAPT only FDC (ASA + Clopidogrel)",
    hr_MI = 0.96, hr_Stroke = 0.96, hr_HF = 0.98, hr_SCD = 0.98,
    hr_MI_lo = 0.82, hr_MI_hi = 1.05,
    hr_Stroke_lo = 0.82, hr_Stroke_hi = 1.05,
    hr_HF_lo = 0.88, hr_HF_hi = 1.05,
    hr_SCD_lo = 0.88, hr_SCD_hi = 1.05,
    source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
    note = "2-drug FDC: smaller adherence gain (~8-10%). Conservative estimate."
  )
)

# =====================================================================
# DRUG PRICES: Four Pricing Perspectives (Rs per tablet, daily)
# =====================================================================
drug_prices <- list(
  govt = list(asp=0.19, clopi=0.59, atorva=0.218, rosuva=0.6685, telmi=0.3636,
              ramipril=0.396, losartan=0.345, amlod=0.1112, cilni=1.01, hctz=0.1215),
  ja   = list(asp=0.33, clopi=1.50, atorva=0.88, rosuva=1.20, telmi=0.80,
              ramipril=1.26, losartan=0.75, amlod=0.25, cilni=2.50, hctz=0.20),
  hs   = list(asp=0.264, clopi=1.20, atorva=0.704, rosuva=0.96, telmi=0.64,
              ramipril=1.008, losartan=0.60, amlod=0.20, cilni=2.00, hctz=0.16),
  private = list(asp=0.38, clopi=7.114, atorva=5.31, rosuva=26.53, telmi=7.22,
                 ramipril=3.27, losartan=15.30, amlod=2.667, cilni=16.594, hctz=1.857)
)

# =====================================================================
# FDC LOOKUP: Price + SoC Composition per Scenario
# =====================================================================
fdc_lookup <- list(
  secure     = list(price=5.73, low=1.25, high=7.50,
                    soc_drugs=c("asp","atorva","ramipril"),
                    label="ASA 75 + Atorvastatin 20 + Ramipril 5"),
  dapt_atorva = list(price=6.90, low=3.20, high=10.40,
                     soc_drugs=c("asp","clopi","atorva"),
                     label="ASA 75 + Clopidogrel 75 + Atorvastatin 20"),
  dapt_rosuva = list(price=14.90, low=8.00, high=26.53,
                     soc_drugs=c("asp","clopi","rosuva"),
                     label="ASA 75 + Clopidogrel 75 + Rosuvastatin 10"),
  dapt_only  = list(price=1.72, low=0.78, high=7.50,
                    soc_drugs=c("asp","clopi"),
                    label="ASA 75 + Clopidogrel 75")
)

# =====================================================================
# HEALTH STATE COSTS (Rs per year, Indian data sources)
# =====================================================================
# Background medication cost: Rs 12/day other meds (matches Shiny default)
BG_ANNUAL <- 12.0 * 365.25  # Rs 4,383/year

state_costs <- list(
  maint_stable   = 7475  + BG_ANNUAL,  # Routine GP + tests + background meds
  maint_mi       = 20077 + BG_ANNUAL,  # Post-MI maintenance + background meds
  maint_stroke   = 18000 + BG_ANNUAL,  # Post-stroke maintenance + background meds
  maint_hf       = 11000 + BG_ANNUAL,  # HF maintenance + background meds
  acute_mi       = 72521 * 0.77 + 217876 * 0.23,  # Weighted PCI(77%)/CABG(23%)
  acute_stroke   = 214013              # Acute stroke ICU + rehab
)

# =====================================================================
# UTILITY WEIGHTS (EQ-5D-5L, Indian norm-based)
# =====================================================================
utils_list <- list(
  u_stable       = 0.814,   # General population 50-60y (DEVINE Study)
  u_mi           = 0.82,    # Chronic post-MI
  u_acute_mi     = 0.814 - 0.15,  # u_stable - disutility (matches Shiny: 0.664)
  u_acute_stroke = 0.814 - 0.25,  # u_stable - disutility (matches Shiny: 0.564)
  u_stroke       = 0.65,    # Chronic post-stroke
  u_hf           = 0.55,    # Heart failure
  use_decay      = TRUE,
  decay_rate     = 0.001
)

# =====================================================================
# SCENARIO & PRICING VECTORS
# =====================================================================
scenario_names  <- c("secure", "dapt_atorva", "dapt_rosuva", "dapt_only")
pricing_names   <- c("govt", "ja", "private")
pricing_labels  <- c("Govt Rate Contract", "Jan Aushadhi", "Private Branded")

# Short labels for chapter headings (with tier)
scenario_labels <- c(
  secure      = "SECURE type Rational FDC (Tier 1)",
  dapt_atorva = "Triple DAPT + Atorvastatin FDC (Tier 2)",
  dapt_rosuva = "Triple DAPT + Rosuvastatin FDC (Tier 2)",
  dapt_only   = "DAPT only FDC (Tier 2)"
)

# Chapter-style short names (no tier suffix)
scenario_short <- c(
  secure      = "SECURE type Rational FDC",
  dapt_atorva = "Triple DAPT + Atorvastatin FDC",
  dapt_rosuva = "Triple DAPT + Rosuvastatin FDC",
  dapt_only   = "DAPT only FDC"
)

# =====================================================================
# BASE CLINICAL PARAMETERS (India-specific)
# =====================================================================
BASE_P_MI       <- 0.0265
BASE_P_MI_YR2   <- 0.012    # Year 2+ MI rate (time-varying, matches Shiny default)
BASE_P_STROKE   <- 0.011
BASE_P_HF       <- 0.023
BASE_P_SCD      <- 0.008
BASE_P_BG_MORT  <- 0.009
BASE_CF_MI      <- 0.08
BASE_CF_STROKE  <- 0.20
BASE_P_HF_DEATH <- 0.181

# =====================================================================
# DSA PARAMETER DEFINITIONS (used by 02_dsa.R and threshold analysis)
# =====================================================================
params_to_vary <- c("p_MI", "p_Stroke", "p_HF", "p_SCD",
                    "hr_MI", "hr_Stroke", "hr_HF", "hr_SCD",
                    "c_fdc_daily", "c_soc_daily", "u_stable", "u_hf")

param_labels <- c(
  p_MI        = "MI incidence (annual)",
  p_Stroke    = "Stroke incidence (annual)",
  p_HF        = "HF incidence (annual)",
  p_SCD       = "SCD incidence (annual)",
  hr_MI       = "HR: MI (FDC vs SoC)",
  hr_Stroke   = "HR: Stroke (FDC vs SoC)",
  hr_HF       = "HR: HF (FDC vs SoC)",
  hr_SCD      = "HR: SCD (FDC vs SoC)",
  c_fdc_daily = "FDC daily cost (Rs)",
  c_soc_daily = "SoC daily cost (Rs)",
  u_stable    = "Utility: Stable state",
  u_hf        = "Utility: Heart failure"
)

# =====================================================================
# HELPER: Build Transition Matrices (8-state Markov)
# =====================================================================
build_transition_matrices <- function(p_MI, p_Stroke, p_HF, p_SCD, p_BG_Mort,
                                       cf_MI, cf_Stroke, p_HF_Death,
                                       hr_MI, hr_Stroke, hr_HF, hr_SCD) {

  states <- c("Stable", "Acute_MI", "Acute_Stroke", "Chronic_MI",
              "Chronic_Stroke", "HF", "Death_CV", "Death_NonCV")

  # Matrix construction matches Shiny transition_module.R build_matrix() exactly
  mat_soc <- matrix(0, nrow=8, ncol=8, dimnames=list(states, states))

  mat_soc["Stable", "Acute_MI"]      <- p_MI
  mat_soc["Stable", "Acute_Stroke"]  <- p_Stroke
  mat_soc["Stable", "HF"]            <- p_HF
  mat_soc["Stable", "Death_CV"]      <- p_SCD
  mat_soc["Stable", "Death_NonCV"]   <- p_BG_Mort
  mat_soc["Stable", "Stable"]        <- 1 - sum(mat_soc["Stable", ])

  mat_soc["Acute_MI", "Death_CV"]    <- cf_MI
  mat_soc["Acute_MI", "Chronic_MI"]  <- 1 - cf_MI

  mat_soc["Acute_Stroke", "Death_CV"]       <- cf_Stroke
  mat_soc["Acute_Stroke", "Chronic_Stroke"] <- 1 - cf_Stroke

  mat_soc["Chronic_MI", "Acute_MI"]     <- p_MI * 1.5
  mat_soc["Chronic_MI", "Acute_Stroke"] <- p_Stroke
  mat_soc["Chronic_MI", "HF"]           <- p_HF * 2.0
  mat_soc["Chronic_MI", "Death_CV"]     <- p_SCD * 1.3
  mat_soc["Chronic_MI", "Death_NonCV"]  <- p_BG_Mort
  mat_soc["Chronic_MI", "Chronic_MI"]   <- 1 - sum(mat_soc["Chronic_MI", ])

  mat_soc["Chronic_Stroke", "Acute_MI"]       <- p_MI * 1.2
  mat_soc["Chronic_Stroke", "Acute_Stroke"]   <- p_Stroke * 1.5
  mat_soc["Chronic_Stroke", "HF"]             <- p_HF * 1.3
  mat_soc["Chronic_Stroke", "Death_CV"]       <- p_SCD * 1.5
  mat_soc["Chronic_Stroke", "Death_NonCV"]    <- p_BG_Mort
  mat_soc["Chronic_Stroke", "Chronic_Stroke"] <- 1 - sum(mat_soc["Chronic_Stroke", ])

  mat_soc["HF", "Acute_MI"]     <- p_MI * 0.5
  mat_soc["HF", "Acute_Stroke"] <- p_Stroke * 0.75
  mat_soc["HF", "Death_CV"]     <- p_HF_Death
  mat_soc["HF", "Death_NonCV"]  <- p_BG_Mort
  mat_soc["HF", "HF"]           <- 1 - sum(mat_soc["HF", ])

  mat_soc["Death_CV", "Death_CV"]       <- 1
  mat_soc["Death_NonCV", "Death_NonCV"] <- 1

  # FDC matrix: built from scratch with HR-adjusted probabilities (matches Shiny)
  fdc_mi <- p_MI * hr_MI
  fdc_st <- p_Stroke * hr_Stroke
  fdc_hf <- p_HF * hr_HF
  fdc_scd <- p_SCD * hr_SCD

  mat_fdc <- matrix(0, nrow=8, ncol=8, dimnames=list(states, states))

  mat_fdc["Stable", "Acute_MI"]      <- fdc_mi
  mat_fdc["Stable", "Acute_Stroke"]  <- fdc_st
  mat_fdc["Stable", "HF"]            <- fdc_hf
  mat_fdc["Stable", "Death_CV"]      <- fdc_scd
  mat_fdc["Stable", "Death_NonCV"]   <- p_BG_Mort
  mat_fdc["Stable", "Stable"]        <- 1 - sum(mat_fdc["Stable", ])

  mat_fdc["Acute_MI", "Death_CV"]    <- cf_MI
  mat_fdc["Acute_MI", "Chronic_MI"]  <- 1 - cf_MI

  mat_fdc["Acute_Stroke", "Death_CV"]       <- cf_Stroke
  mat_fdc["Acute_Stroke", "Chronic_Stroke"] <- 1 - cf_Stroke

  mat_fdc["Chronic_MI", "Acute_MI"]     <- fdc_mi * 1.5
  mat_fdc["Chronic_MI", "Acute_Stroke"] <- fdc_st
  mat_fdc["Chronic_MI", "HF"]           <- fdc_hf * 2.0
  mat_fdc["Chronic_MI", "Death_CV"]     <- fdc_scd * 1.3
  mat_fdc["Chronic_MI", "Death_NonCV"]  <- p_BG_Mort
  mat_fdc["Chronic_MI", "Chronic_MI"]   <- 1 - sum(mat_fdc["Chronic_MI", ])

  mat_fdc["Chronic_Stroke", "Acute_MI"]       <- fdc_mi * 1.2
  mat_fdc["Chronic_Stroke", "Acute_Stroke"]   <- fdc_st * 1.5
  mat_fdc["Chronic_Stroke", "HF"]             <- fdc_hf * 1.3
  mat_fdc["Chronic_Stroke", "Death_CV"]       <- fdc_scd * 1.5
  mat_fdc["Chronic_Stroke", "Death_NonCV"]    <- p_BG_Mort
  mat_fdc["Chronic_Stroke", "Chronic_Stroke"] <- 1 - sum(mat_fdc["Chronic_Stroke", ])

  mat_fdc["HF", "Acute_MI"]     <- fdc_mi * 0.5
  mat_fdc["HF", "Acute_Stroke"] <- fdc_st * 0.75
  mat_fdc["HF", "Death_CV"]     <- p_HF_Death
  mat_fdc["HF", "Death_NonCV"]  <- p_BG_Mort
  mat_fdc["HF", "HF"]           <- 1 - sum(mat_fdc["HF", ])

  mat_fdc["Death_CV", "Death_CV"]       <- 1
  mat_fdc["Death_NonCV", "Death_NonCV"] <- 1

  list(
    soc = mat_soc, fdc = mat_fdc,
    p_MI = p_MI, p_MI_yr2 = BASE_P_MI_YR2,
    use_time_varying_mi = TRUE,
    p_Stroke = p_Stroke, p_HF = p_HF, p_SCD = p_SCD,
    p_BG_Mort = p_BG_Mort, cf_MI = cf_MI, cf_Stroke = cf_Stroke,
    p_HF_Death = p_HF_Death,
    hr_MI = hr_MI, hr_Stroke = hr_Stroke, hr_HF = hr_HF, hr_SCD = hr_SCD
  )
}

# =====================================================================
# HELPER: Build Costs List (returns list — engine uses $)
# =====================================================================
build_costs_list <- function(scenario_key, pricing_key, state_costs) {
  prices   <- drug_prices[[pricing_key]]
  fdc_info <- fdc_lookup[[scenario_key]]

  # Perspective-specific FDC prices (actual formulary/RC rates only)
  # DAPT+Atorva: actual Jan Aushadhi price = Rs 4.125 (in JA national formulary)
  fdc_daily <- fdc_info$price  # default
  if (scenario_key == "dapt_atorva" && pricing_key == "ja") {
    fdc_daily <- 4.125
  }
  fdc_annual <- fdc_daily * 365.25
  soc_annual <- sum(sapply(fdc_info$soc_drugs, function(d) prices[[d]] * 365.25))

  list(
    fdc_annual   = fdc_annual,
    soc_annual   = soc_annual,
    maint_stable = state_costs$maint_stable,
    maint_mi     = state_costs$maint_mi,
    maint_stroke = state_costs$maint_stroke,
    maint_hf     = state_costs$maint_hf,
    acute_mi     = state_costs$acute_mi,
    acute_stroke = state_costs$acute_stroke
  )
}

# =====================================================================
# HELPER: Run Single Scenario
# =====================================================================
run_scenario <- function(scenario_key, pricing_key,
                         n_pat = N_PAT, horizon = HORIZON, seed = SEED) {
  scen <- scenario_evidence[[scenario_key]]
  tp_list <- build_transition_matrices(
    BASE_P_MI, BASE_P_STROKE, BASE_P_HF, BASE_P_SCD, BASE_P_BG_MORT,
    BASE_CF_MI, BASE_CF_STROKE, BASE_P_HF_DEATH,
    hr_MI = scen$hr_MI, hr_Stroke = scen$hr_Stroke,
    hr_HF = scen$hr_HF, hr_SCD = scen$hr_SCD
  )
  costs_list <- build_costs_list(scenario_key, pricing_key, state_costs)
  run_microsim_engine(tp_list, costs_list, utils_list,
                      n_pat = n_pat, horizon = horizon, seed = seed)
}

# =====================================================================
# HELPER: DSA single-parameter perturbation run
# =====================================================================
run_dsa_single <- function(scenario_key, param_name, param_value,
                           n_pat = DSA_N, horizon = HORIZON, seed = SEED) {
  p_MI <- BASE_P_MI; p_Stroke <- BASE_P_STROKE; p_HF <- BASE_P_HF; p_SCD <- BASE_P_SCD
  p_BG_Mort <- BASE_P_BG_MORT; cf_MI <- BASE_CF_MI; cf_Stroke <- BASE_CF_STROKE
  p_HF_Death <- BASE_P_HF_DEATH

  scen <- scenario_evidence[[scenario_key]]
  hr_MI <- scen$hr_MI; hr_Stroke <- scen$hr_Stroke
  hr_HF <- scen$hr_HF; hr_SCD <- scen$hr_SCD

  switch(param_name,
    "p_MI"       = { p_MI       <- param_value },
    "p_Stroke"   = { p_Stroke   <- param_value },
    "p_HF"       = { p_HF       <- param_value },
    "p_SCD"      = { p_SCD      <- param_value },
    "hr_MI"      = { hr_MI      <- param_value },
    "hr_Stroke"  = { hr_Stroke  <- param_value },
    "hr_HF"      = { hr_HF      <- param_value },
    "hr_SCD"     = { hr_SCD     <- param_value }
  )

  tp_list <- build_transition_matrices(
    p_MI, p_Stroke, p_HF, p_SCD, p_BG_Mort,
    cf_MI, cf_Stroke, p_HF_Death,
    hr_MI, hr_Stroke, hr_HF, hr_SCD
  )

  costs_vec <- build_costs_list(scenario_key, "govt", state_costs)
  if (param_name == "c_fdc_daily") costs_vec$fdc_annual <- param_value * 365.25
  if (param_name == "c_soc_daily") costs_vec$soc_annual <- param_value * 365.25

  u_list <- utils_list
  if (param_name == "u_stable") u_list$u_stable <- param_value
  if (param_name == "u_hf")    u_list$u_hf     <- param_value

  disc_c <- if (param_name == "disc_cost") param_value else DISC_RATE
  disc_u <- if (param_name == "disc_util") param_value else DISC_RATE

  res <- tryCatch(
    run_microsim_engine(tp_list, costs_vec, u_list,
                        n_pat = n_pat, horizon = horizon,
                        disc_cost = disc_c, disc_util = disc_u, seed = seed),
    error = function(e) NULL
  )

  if (is.null(res) || is.null(res$FDC) || is.null(res$SoC)) return(NA_real_)
  dC <- res$FDC$avg_cost - res$SoC$avg_cost
  dE <- res$FDC$avg_qaly - res$SoC$avg_qaly
  safe_icer(dC, dE)
}

# =====================================================================
# HELPER: Compute ICER safely
# =====================================================================
safe_icer <- function(dC, dE) {
  if (length(dC) == 0 || length(dE) == 0 || is.na(dC) || is.na(dE)) return(NA_real_)
  if (abs(dE) < 0.0001) return(NA_real_)
  dC / dE
}

# =====================================================================
# HELPER: Build survival & trace data from engine results
# =====================================================================
build_surv_trace <- function(res_obj, horizon) {
  trace_soc <- res_obj$SoC$trace
  trace_fdc <- res_obj$FDC$trace

  alive_cols <- !colnames(trace_soc) %in% c("Death_CV", "Death_NonCV")
  surv_df <- data.frame(
    Year = 0:horizon,
    SoC  = rowSums(trace_soc[, alive_cols]),
    FDC  = rowSums(trace_fdc[, alive_cols])
  ) %>% pivot_longer(cols = c(SoC, FDC), names_to = "Strategy", values_to = "Survival")

  trace_long <- trace_fdc %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Year") %>%
    mutate(Year = as.numeric(Year) - 1) %>%
    pivot_longer(cols = -Year, names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("Stable", "Acute_MI", "Acute_Stroke",
                                             "Chronic_MI", "Chronic_Stroke", "HF",
                                             "Death_CV", "Death_NonCV")))

  list(surv_df = surv_df, trace_long = trace_long)
}

# Create outputs directory (handle both cwd locations)
outputs_dir <- if (dir.exists("report")) "report/outputs" else "outputs"
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)

# =====================================================================
# CONVENIENCE ALIASES (used by QMD report)
# =====================================================================
CURRENCY_YEAR <- "FY2024-25"

cat("✓ 00_config.R loaded.\n")
