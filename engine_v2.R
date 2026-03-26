# engine_v2.R — Upgraded microsimulation engine
# Features: heterogeneous patients, age-dependent mortality, adherence decay,
#           recurrent event tracking, Indian EQ-5D norms, vectorized inner loop

# Indian SRS life table — background mortality by age-sex (annual probability)
srs_life_table <- data.frame(
  age_lower = c(30, 40, 50, 60, 70, 80),
  age_upper = c(39, 49, 59, 69, 79, 99),
  male   = c(0.0022, 0.0044, 0.0098, 0.0220, 0.0510, 0.1200),
  female = c(0.0018, 0.0032, 0.0068, 0.0175, 0.0440, 0.1000)
)

get_bg_mortality <- function(age, is_male) {
  idx <- which(srs_life_table$age_lower <= age & srs_life_table$age_upper >= age)
  if (length(idx) == 0) idx <- nrow(srs_life_table)
  if (is_male) srs_life_table$male[idx] else srs_life_table$female[idx]
}

# Indian EQ-5D-5L norms (DEVINE Study / J Global Health 2023)
eq5d_norms_v2 <- data.frame(
  age_lower = c(0, 20, 30, 40, 50, 60, 70),
  age_upper = c(19, 29, 39, 49, 59, 69, 99),
  male   = c(0.936, 0.920, 0.882, 0.833, 0.814, 0.780, 0.643),
  female = c(0.922, 0.908, 0.858, 0.831, 0.766, 0.669, 0.488)
)

get_eq5d_utility <- function(age, is_male) {
  idx <- which(eq5d_norms_v2$age_lower <= age & eq5d_norms_v2$age_upper >= age)
  if (length(idx) == 0) idx <- nrow(eq5d_norms_v2)
  if (is_male) eq5d_norms_v2$male[idx] else eq5d_norms_v2$female[idx]
}

# Adherence decay (exponential, fitted from UMPIRE/FOCUS/TIPS-3)
adherence_fdc <- function(t_years) {
  t_months <- t_years * 12
  0.877 * exp(-0.00692 * t_months)
}

adherence_soc <- function(t_years) {
  t_months <- t_years * 12
  0.887 * exp(-0.02455 * t_months)
}

# Effective HR adjusted for adherence decay
effective_hr <- function(base_hr, t_years) {
  adh <- adherence_fdc(t_years)
  adh_0 <- 0.877  # initial adherence
  rel_retained <- adh / adh_0
  1 - rel_retained * (1 - base_hr)
}


# === MAIN ENGINE V2 ===
run_microsim_engine_v2 <- function(tp_list, costs_list, utils_list, n_pat = 1000,
                                    horizon = 30, disc_cost = 0.03, disc_util = 0.03,
                                    seed = 123, patient_profiles = NULL,
                                    use_heterogeneity = TRUE,
                                    use_adherence_decay = TRUE,
                                    use_age_mortality = TRUE,
                                    use_age_utilities = TRUE) {
  set.seed(seed)

  if (is.null(tp_list$soc) | is.null(tp_list$fdc)) return(NULL)

  states <- colnames(tp_list$soc)
  n_states <- length(states)
  results <- list()

  # Generate patient cohort if not provided
  if (is.null(patient_profiles) || !use_heterogeneity) {
    # Fallback: homogeneous cohort (backward compatible)
    patient_profiles <- data.frame(
      id = 1:n_pat,
      age = rep(58, n_pat),
      is_male = rep(TRUE, n_pat),
      rr_mi = rep(1, n_pat),
      rr_stroke = rep(1, n_pat),
      base_utility = rep(utils_list$u_stable, n_pat),
      has_dm = rep(FALSE, n_pat),
      has_htn = rep(FALSE, n_pat),
      has_smoke = rep(FALSE, n_pat)
    )
  }

  # Ensure cohort size matches
  if (nrow(patient_profiles) != n_pat) {
    patient_profiles <- patient_profiles[sample(nrow(patient_profiles), n_pat, replace = TRUE), ]
    patient_profiles$id <- 1:n_pat
  }

  # State utility lookup (disease multiplier relative to stable)
  disease_util_multiplier <- function(state_name, utils) {
    switch(state_name,
           "Stable"         = utils$u_stable,
           "Acute_MI"       = utils$u_acute_mi,
           "Acute_Stroke"   = utils$u_acute_stroke,
           "Chronic_MI"     = utils$u_mi,
           "Chronic_Stroke" = utils$u_stroke,
           "HF"             = utils$u_hf,
           0)
  }

  # Cost lookup per state
  state_cost <- function(state_name, costs, is_fdc) {
    maint <- switch(state_name,
                    "Stable"         = costs$maint_stable,
                    "Acute_MI"       = costs$acute_mi,
                    "Acute_Stroke"   = costs$acute_stroke,
                    "Chronic_MI"     = costs$maint_mi,
                    "Chronic_Stroke" = costs$maint_stroke,
                    "HF"             = costs$maint_hf,
                    0)
    drug <- if (is_fdc) costs$fdc_annual else costs$soc_annual
    maint + drug
  }

  for (strat in c("SoC", "FDC")) {
    is_fdc <- (strat == "FDC")
    tp_matrix_base <- if (strat == "SoC") tp_list$soc else tp_list$fdc

    total_costs <- 0
    total_qalys <- 0
    trace <- matrix(0, nrow = horizon + 1, ncol = n_states)
    colnames(trace) <- states

    # Event counter for recurrent events
    event_counts <- matrix(0, nrow = n_pat, ncol = 2)  # col 1: MI count, col 2: Stroke count
    colnames(event_counts) <- c("n_mi", "n_stroke")

    # Pre-compute Year 2+ matrix if time-varying MI is enabled
    use_tv_mi <- !is.null(tp_list$use_time_varying_mi) && tp_list$use_time_varying_mi &&
                 !is.null(tp_list$p_MI_yr2)
    tp_matrix_yr2 <- NULL
    if (use_tv_mi) {
      # Rebuild matrix with Year 2+ MI rate (lower rate)
      p_mi_yr2 <- tp_list$p_MI_yr2
      p_mi_yr1 <- tp_list$p_MI
      if (p_mi_yr1 > 0 && p_mi_yr2 < p_mi_yr1) {
        mi_ratio <- p_mi_yr2 / p_mi_yr1
        tp_matrix_yr2 <- tp_matrix_base
        # Scale MI transitions in rows that reference MI
        for (rn in c("Stable", "Chronic_MI", "Chronic_Stroke", "HF")) {
          if (rn %in% rownames(tp_matrix_yr2)) {
            mi_col <- which(colnames(tp_matrix_yr2) == "Acute_MI")
            if (length(mi_col) > 0) {
              old_mi <- tp_matrix_yr2[rn, mi_col]
              new_mi <- old_mi * mi_ratio
              delta <- old_mi - new_mi
              tp_matrix_yr2[rn, mi_col] <- new_mi
              # Add delta back to self-loop
              tp_matrix_yr2[rn, rn] <- tp_matrix_yr2[rn, rn] + delta
            }
          }
        }
      }
    }

    for (i in 1:n_pat) {
      pat <- patient_profiles[i, ]
      state <- "Stable"
      pat_age <- pat$age
      n_mi <- 0
      n_stroke <- 0

      for (t in 0:horizon) {
        # Record state in trace
        trace[t + 1, state] <- trace[t + 1, state] + 1

        # If dead, carry forward and exit
        if (state %in% c("Death_CV", "Death_NonCV")) {
          if (t < horizon) {
            for (f_t in (t + 1):horizon) {
              trace[f_t + 1, state] <- trace[f_t + 1, state] + 1
            }
          }
          break
        }

        current_age <- pat_age + t

        # --- Build per-patient, time-dependent transition matrix ---
        # Use Year 2+ matrix after first cycle if time-varying MI enabled
        if (use_tv_mi && t >= 1 && !is.null(tp_matrix_yr2)) {
          tp_matrix <- tp_matrix_yr2
        } else {
          tp_matrix <- tp_matrix_base
        }

        # 1. Age-dependent background mortality
        if (use_age_mortality) {
          bg_mort <- get_bg_mortality(current_age, pat$is_male)
          old_bg <- tp_list$p_BG_Mort
          if (!is.null(old_bg) && old_bg > 0) {
            bg_ratio <- bg_mort / old_bg
          } else {
            bg_ratio <- 1
          }
          # Adjust rows that have NonCV death
          for (row_name in c("Stable", "Chronic_MI", "Chronic_Stroke", "HF")) {
            if (row_name %in% rownames(tp_matrix)) {
              old_val <- tp_matrix[row_name, "Death_NonCV"]
              new_val <- bg_mort
              delta <- new_val - old_val
              tp_matrix[row_name, "Death_NonCV"] <- new_val
              # Adjust self-loop to keep row summing to 1
              tp_matrix[row_name, row_name] <- tp_matrix[row_name, row_name] - delta
              # Clamp to valid probability
              tp_matrix[row_name, row_name] <- max(0, tp_matrix[row_name, row_name])
              # Re-normalize
              row_sum <- sum(tp_matrix[row_name, ])
              if (row_sum > 0) tp_matrix[row_name, ] <- tp_matrix[row_name, ] / row_sum
            }
          }
        }

        # 2. Patient-specific risk multipliers (heterogeneity)
        if (use_heterogeneity && (pat$rr_mi != 1 || pat$rr_stroke != 1)) {
          # Scale MI and stroke transitions by patient-level HRs for recurrent events
          # Sources: REACH Registry, Recurrent Stroke Meta-analysis (PMID 34153594)
          # These are secondary prevention HRs (NOT primary prevention INTERHEART ORs)
          mi_cols <- grep("MI", colnames(tp_matrix))
          stroke_cols <- grep("Stroke", colnames(tp_matrix))

          for (row_name in c("Stable", "Chronic_MI", "Chronic_Stroke")) {
            if (row_name %in% rownames(tp_matrix)) {
              for (mc in mi_cols) {
                tp_matrix[row_name, mc] <- tp_matrix[row_name, mc] * pat$rr_mi
              }
              for (sc in stroke_cols) {
                tp_matrix[row_name, sc] <- tp_matrix[row_name, sc] * pat$rr_stroke
              }
              # Clamp and re-normalize
              tp_matrix[row_name, ] <- pmax(0, tp_matrix[row_name, ])
              diag_idx <- which(rownames(tp_matrix) == row_name)
              tp_matrix[row_name, diag_idx] <- 0
              total_exit <- sum(tp_matrix[row_name, ])
              if (total_exit >= 1) {
                tp_matrix[row_name, ] <- tp_matrix[row_name, ] * (0.99 / total_exit)
                tp_matrix[row_name, diag_idx] <- 0.01
              } else {
                tp_matrix[row_name, diag_idx] <- 1 - total_exit
              }
            }
          }
        }

        # 3. Adherence decay (FDC only): attenuate HRs over time
        if (is_fdc && use_adherence_decay && t > 0) {
          # The FDC matrix was built with initial HRs. Adjust toward SoC as adherence drops.
          adh_factor <- adherence_fdc(t) / 0.877  # fraction of initial adherence retained
          # Blend: effective_matrix = adh_factor * fdc_matrix + (1 - adh_factor) * soc_matrix
          for (row_name in rownames(tp_matrix)) {
            if (row_name %in% c("Death_CV", "Death_NonCV")) next
            tp_matrix[row_name, ] <- adh_factor * tp_matrix[row_name, ] +
                                      (1 - adh_factor) * tp_list$soc[row_name, ]
          }
        }

        # 4. Recurrent event escalation
        if (n_mi > 0 && state == "Chronic_MI") {
          esc <- min(1 + 0.2 * n_mi, 2.0)  # escalation caps at 2x
          for (mc in grep("MI", colnames(tp_matrix))) {
            tp_matrix["Chronic_MI", mc] <- tp_matrix["Chronic_MI", mc] * esc
          }
          # Re-normalize
          tp_matrix["Chronic_MI", ] <- pmax(0, tp_matrix["Chronic_MI", ])
          diag_idx <- which(rownames(tp_matrix) == "Chronic_MI")
          tp_matrix["Chronic_MI", diag_idx] <- 0
          tot <- sum(tp_matrix["Chronic_MI", ])
          if (tot >= 1) {
            tp_matrix["Chronic_MI", ] <- tp_matrix["Chronic_MI", ] * (0.99 / tot)
            tp_matrix["Chronic_MI", diag_idx] <- 0.01
          } else {
            tp_matrix["Chronic_MI", diag_idx] <- 1 - tot
          }
        }

        # --- State rewards ---
        cost_current <- state_cost(state, costs_list, is_fdc)

        # Utility: use Indian EQ-5D norm adjusted by disease state
        if (use_age_utilities) {
          age_norm <- get_eq5d_utility(current_age, pat$is_male)
          # Disease state utility as proportion of stable baseline
          disease_u <- disease_util_multiplier(state, utils_list)
          u_current <- age_norm * (disease_u / max(utils_list$u_stable, 0.01))
        } else {
          u_current <- disease_util_multiplier(state, utils_list)
          if (utils_list$use_decay) u_current <- u_current * (1 - utils_list$decay_rate)^t
        }

        # --- Transition ---
        probs <- pmax(0, tp_matrix[state, ])
        probs <- probs / sum(probs)
        next_state <- sample(states, size = 1, prob = probs)

        # Track events
        if (next_state == "Acute_MI") n_mi <- n_mi + 1
        if (next_state == "Acute_Stroke") n_stroke <- n_stroke + 1

        # --- Half-cycle correction rewards ---
        cost_next <- state_cost(next_state, costs_list, is_fdc)
        if (use_age_utilities) {
          next_age_norm <- get_eq5d_utility(current_age + 1, pat$is_male)
          next_disease_u <- disease_util_multiplier(next_state, utils_list)
          u_next <- next_age_norm * (next_disease_u / max(utils_list$u_stable, 0.01))
        } else {
          u_next <- disease_util_multiplier(next_state, utils_list)
          if (utils_list$use_decay) u_next <- u_next * (1 - utils_list$decay_rate)^t
        }

        cycle_cost <- (cost_current + cost_next) / 2
        cycle_util <- (u_current + u_next) / 2

        total_costs <- total_costs + (cycle_cost / (1 + disc_cost)^t)
        total_qalys <- total_qalys + (cycle_util / (1 + disc_util)^t)

        state <- next_state
      }

      event_counts[i, "n_mi"] <- n_mi
      event_counts[i, "n_stroke"] <- n_stroke
    }

    trace_norm <- trace / n_pat
    alive_cols <- !colnames(trace_norm) %in% c("Death_CV", "Death_NonCV")

    results[[strat]] <- list(
      avg_cost = total_costs / n_pat,
      avg_qaly = total_qalys / n_pat,
      trace = trace_norm,
      survival = rowSums(trace_norm[, alive_cols]),
      event_counts = event_counts,
      mean_mi_events = mean(event_counts[, "n_mi"]),
      mean_stroke_events = mean(event_counts[, "n_stroke"])
    )
  }

  return(results)
}

# === BACKWARD-COMPATIBLE WRAPPER ===
# This ensures existing DSA/PSA modules continue to work
run_microsim_engine <- function(tp_list, costs_list, utils_list, n_pat = 1000,
                                 horizon = 30, disc_cost = 0.03, disc_util = 0.03,
                                 seed = 123) {
  run_microsim_engine_v2(tp_list, costs_list, utils_list, n_pat, horizon,
                          disc_cost, disc_util, seed,
                          patient_profiles = NULL,
                          use_heterogeneity = FALSE,
                          use_adherence_decay = FALSE,
                          use_age_mortality = FALSE,
                          use_age_utilities = FALSE)
}
