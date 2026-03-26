# engine_v2.R — Vectorized microsimulation engine
# Features: heterogeneous patients, age-dependent mortality, adherence decay,
#           recurrent event tracking, Indian EQ-5D norms
# Performance: Patient loop fully vectorized — all patients processed simultaneously per cycle

# Indian SRS life table — background mortality by age-sex (annual probability)
srs_life_table <- data.frame(
  age_lower = c(30, 40, 50, 60, 70, 80),
  age_upper = c(39, 49, 59, 69, 79, 99),
  male   = c(0.0022, 0.0044, 0.0098, 0.0220, 0.0510, 0.1200),
  female = c(0.0018, 0.0032, 0.0068, 0.0175, 0.0440, 0.1000)
)

# Vectorized background mortality lookup
get_bg_mortality_vec <- function(ages, is_male) {
  # Pre-build lookup: findInterval on age_lower boundaries
  breaks <- c(srs_life_table$age_lower, Inf)
  idx <- findInterval(ages, breaks, rightmost.closed = TRUE)
  idx <- pmax(1, pmin(idx, nrow(srs_life_table)))
  ifelse(is_male, srs_life_table$male[idx], srs_life_table$female[idx])
}

# Scalar version for backward compat
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

# Vectorized EQ-5D lookup
get_eq5d_utility_vec <- function(ages, is_male) {
  breaks <- c(eq5d_norms_v2$age_lower, Inf)
  idx <- findInterval(ages, breaks, rightmost.closed = TRUE)
  idx <- pmax(1, pmin(idx, nrow(eq5d_norms_v2)))
  ifelse(is_male, eq5d_norms_v2$male[idx], eq5d_norms_v2$female[idx])
}

# Scalar version for backward compat
get_eq5d_utility <- function(age, is_male) {
  idx <- which(eq5d_norms_v2$age_lower <= age & eq5d_norms_v2$age_upper >= age)
  if (length(idx) == 0) idx <- nrow(eq5d_norms_v2)
  if (is_male) eq5d_norms_v2$male[idx] else eq5d_norms_v2$female[idx]
}

# Adherence decay (exponential, fitted from UMPIRE/FOCUS/TIPS-3)
adherence_fdc <- function(t_years) {
  0.877 * exp(-0.00692 * t_years * 12)
}
adherence_soc <- function(t_years) {
  0.887 * exp(-0.02455 * t_years * 12)
}

# Effective HR adjusted for adherence decay
effective_hr <- function(base_hr, t_years) {
  adh <- adherence_fdc(t_years)
  rel_retained <- adh / 0.877
  1 - rel_retained * (1 - base_hr)
}


# === MAIN ENGINE V2 (VECTORIZED) ===
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
  state_idx <- setNames(seq_along(states), states)
  results <- list()

  # Generate patient cohort if not provided
  if (is.null(patient_profiles) || !use_heterogeneity) {
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

  if (nrow(patient_profiles) != n_pat) {
    patient_profiles <- patient_profiles[sample(nrow(patient_profiles), n_pat, replace = TRUE), ]
    patient_profiles$id <- 1:n_pat
  }

  # Pre-compute state utility and cost vectors (indexed by state_idx)
  util_by_state <- vapply(states, function(s) {
    switch(s, "Stable" = utils_list$u_stable, "Acute_MI" = utils_list$u_acute_mi,
           "Acute_Stroke" = utils_list$u_acute_stroke, "Chronic_MI" = utils_list$u_mi,
           "Chronic_Stroke" = utils_list$u_stroke, "HF" = utils_list$u_hf, 0)
  }, numeric(1))

  maint_by_state <- vapply(states, function(s) {
    switch(s, "Stable" = costs_list$maint_stable, "Acute_MI" = costs_list$acute_mi,
           "Acute_Stroke" = costs_list$acute_stroke, "Chronic_MI" = costs_list$maint_mi,
           "Chronic_Stroke" = costs_list$maint_stroke, "HF" = costs_list$maint_hf, 0)
  }, numeric(1))

  # Identify column indices for MI and stroke states
  mi_cols <- grep("MI", states)      # Acute_MI, Chronic_MI
  stroke_cols <- grep("Stroke", states)
  acute_mi_idx <- state_idx["Acute_MI"]
  acute_stroke_idx <- state_idx["Acute_Stroke"]
  death_cv_idx <- state_idx["Death_CV"]
  death_noncv_idx <- state_idx["Death_NonCV"]
  dead_indices <- c(death_cv_idx, death_noncv_idx)

  # Non-death rows that need age-mortality adjustment
  living_rows <- c("Stable", "Chronic_MI", "Chronic_Stroke", "HF")
  living_row_idx <- state_idx[living_rows]
  # Rows for heterogeneity adjustment
  het_rows <- c("Stable", "Chronic_MI", "Chronic_Stroke")
  het_row_idx <- state_idx[het_rows]

  # Pre-compute Year 2+ matrix if time-varying MI
  use_tv_mi <- !is.null(tp_list$use_time_varying_mi) && tp_list$use_time_varying_mi &&
               !is.null(tp_list$p_MI_yr2)
  tp_yr2_soc <- tp_list$soc
  tp_yr2_fdc <- tp_list$fdc
  if (use_tv_mi) {
    mi_ratio <- tp_list$p_MI_yr2 / tp_list$p_MI
    if (is.finite(mi_ratio) && mi_ratio < 1) {
      for (mat_name in c("tp_yr2_soc", "tp_yr2_fdc")) {
        mat <- get(mat_name)
        for (rn in living_rows) {
          if (rn %in% rownames(mat)) {
            old_mi <- mat[rn, acute_mi_idx]
            new_mi <- old_mi * mi_ratio
            mat[rn, rn] <- mat[rn, rn] + (old_mi - new_mi)
            mat[rn, acute_mi_idx] <- new_mi
          }
        }
        assign(mat_name, mat)
      }
    }
  }

  # Extract patient vectors for speed
  pat_ages <- patient_profiles$age
  pat_male <- patient_profiles$is_male
  pat_rr_mi <- patient_profiles$rr_mi
  pat_rr_stroke <- patient_profiles$rr_stroke

  # Pre-compute per-patient bg mortality for each year (matrix n_pat x (horizon+1))
  if (use_age_mortality) {
    bg_mort_mat <- matrix(0, nrow = n_pat, ncol = horizon + 1)
    for (t in 0:horizon) {
      bg_mort_mat[, t + 1] <- get_bg_mortality_vec(pat_ages + t, pat_male)
    }
    old_bg <- if (!is.null(tp_list$p_BG_Mort)) tp_list$p_BG_Mort else 0.01
  }

  # Pre-compute per-patient EQ-5D norms for each year
  if (use_age_utilities) {
    eq5d_mat <- matrix(0, nrow = n_pat, ncol = horizon + 1)
    eq5d_mat_next <- matrix(0, nrow = n_pat, ncol = horizon + 1)
    for (t in 0:horizon) {
      eq5d_mat[, t + 1] <- get_eq5d_utility_vec(pat_ages + t, pat_male)
      eq5d_mat_next[, t + 1] <- get_eq5d_utility_vec(pat_ages + t + 1, pat_male)
    }
    u_stable_inv <- 1 / max(utils_list$u_stable, 0.01)
  }

  # Pre-compute discount factors
  disc_cost_factors <- 1 / (1 + disc_cost)^(0:horizon)
  disc_util_factors <- 1 / (1 + disc_util)^(0:horizon)

  # Pre-compute adherence factors per year (for FDC blending)
  if (use_adherence_decay) {
    adh_factors <- adherence_fdc(0:horizon) / 0.877
  }

  for (strat in c("SoC", "FDC")) {
    is_fdc <- (strat == "FDC")
    tp_base <- if (is_fdc) tp_list$fdc else tp_list$soc
    tp_base_yr2 <- if (is_fdc) tp_yr2_fdc else tp_yr2_soc
    tp_soc <- tp_list$soc  # for adherence blending

    drug_annual <- if (is_fdc) costs_list$fdc_annual else costs_list$soc_annual

    # State vector: integer index into states (1-based)
    cur_state <- rep(state_idx["Stable"], n_pat)
    alive <- rep(TRUE, n_pat)

    # Accumulators
    pat_costs <- numeric(n_pat)
    pat_qalys <- numeric(n_pat)
    n_mi_events <- integer(n_pat)
    n_stroke_events <- integer(n_pat)

    trace <- matrix(0, nrow = horizon + 1, ncol = n_states)
    colnames(trace) <- states

    # Pre-draw all random numbers at once (n_pat x horizon)
    rand_mat <- matrix(runif(n_pat * (horizon + 1)), nrow = n_pat, ncol = horizon + 1)

    for (t in 0:horizon) {
      # Record trace
      for (s in 1:n_states) {
        trace[t + 1, s] <- sum(cur_state == s)
      }

      # Skip if no one alive
      n_alive <- sum(alive)
      if (n_alive == 0) {
        # Fill remaining trace with current distribution
        if (t < horizon) {
          for (ft in (t + 1):horizon) {
            trace[ft + 1, ] <- trace[t + 1, ]
          }
        }
        break
      }

      # Get alive patient indices
      alive_idx <- which(alive)

      # Select base transition matrix for this cycle
      if (use_tv_mi && t >= 1) {
        tp_cycle <- tp_base_yr2
      } else {
        tp_cycle <- tp_base
      }

      # Adherence decay: blend FDC matrix toward SoC at matrix level
      # (BEFORE per-patient adjustments so heterogeneity/age-mortality apply uniformly)
      if (is_fdc && use_adherence_decay && t > 0) {
        af <- adh_factors[t + 1]
        tp_soc_cycle <- if (use_tv_mi && t >= 1) tp_yr2_soc else tp_soc
        tp_cycle <- af * tp_cycle + (1 - af) * tp_soc_cycle
      }

      # Build per-patient transition probability matrix (only for alive patients)
      # Each alive patient gets a row of transition probs from their current state
      n_a <- length(alive_idx)
      probs_mat <- matrix(0, nrow = n_a, ncol = n_states)

      # Group alive patients by their current state for efficiency
      states_alive <- cur_state[alive_idx]
      unique_states <- unique(states_alive)

      for (s in unique_states) {
        # Skip dead states
        if (s %in% dead_indices) next

        s_mask <- which(states_alive == s)  # indices within alive_idx
        s_patients <- alive_idx[s_mask]     # global patient indices
        ns <- length(s_patients)

        # Start with base transition row for this state
        base_row <- tp_cycle[s, ]

        # 1. Age-dependent background mortality (per-patient)
        if (use_age_mortality && states[s] %in% living_rows) {
          bg_new <- bg_mort_mat[s_patients, t + 1]
          # Each patient may have different bg mortality
          # Adjust Death_NonCV and self-loop
          delta <- bg_new - base_row[death_noncv_idx]
          # Build per-patient rows
          p_rows <- matrix(rep(base_row, ns), nrow = ns, byrow = TRUE)
          p_rows[, death_noncv_idx] <- bg_new
          p_rows[, s] <- p_rows[, s] - delta
          p_rows[, s] <- pmax(0, p_rows[, s])
          # Normalize rows
          rsums <- rowSums(p_rows)
          p_rows <- p_rows / rsums
        } else {
          p_rows <- matrix(rep(base_row, ns), nrow = ns, byrow = TRUE)
        }

        # 2. Patient-specific risk multipliers (heterogeneity)
        if (use_heterogeneity && states[s] %in% het_rows) {
          rr_mi_s <- pat_rr_mi[s_patients]
          rr_str_s <- pat_rr_stroke[s_patients]
          needs_adj <- (rr_mi_s != 1) | (rr_str_s != 1)
          if (any(needs_adj)) {
            adj_idx <- which(needs_adj)
            for (mc in mi_cols) {
              p_rows[adj_idx, mc] <- p_rows[adj_idx, mc] * rr_mi_s[adj_idx]
            }
            for (sc in stroke_cols) {
              p_rows[adj_idx, sc] <- p_rows[adj_idx, sc] * rr_str_s[adj_idx]
            }
            # Re-normalize adjusted rows
            p_rows[adj_idx, ] <- pmax(p_rows[adj_idx, , drop = FALSE], 0)
            diag_col <- s
            p_rows[adj_idx, diag_col] <- 0
            exit_sums <- rowSums(p_rows[adj_idx, , drop = FALSE])
            too_high <- exit_sums >= 1
            if (any(too_high)) {
              th_idx <- adj_idx[too_high]
              p_rows[th_idx, ] <- p_rows[th_idx, , drop = FALSE] * (0.99 / exit_sums[too_high])
              p_rows[th_idx, diag_col] <- 0.01
            }
            ok_idx <- adj_idx[!too_high]
            if (length(ok_idx) > 0) {
              p_rows[ok_idx, diag_col] <- 1 - rowSums(p_rows[ok_idx, -diag_col, drop = FALSE])
            }
          }
        }

        # 3. Adherence decay — now applied at matrix level before this loop
        #    (see tp_cycle blending above)

        # 4. Recurrent event escalation for Chronic_MI patients
        if (states[s] == "Chronic_MI") {
          mi_counts <- n_mi_events[s_patients]
          has_prev <- mi_counts > 0
          if (any(has_prev)) {
            esc_factors <- pmin(1 + 0.2 * mi_counts[has_prev], 2.0)
            hp_idx <- which(has_prev)
            for (mc in mi_cols) {
              p_rows[hp_idx, mc] <- p_rows[hp_idx, mc] * esc_factors
            }
            p_rows[hp_idx, ] <- pmax(p_rows[hp_idx, , drop = FALSE], 0)
            p_rows[hp_idx, s] <- 0
            exit_s <- rowSums(p_rows[hp_idx, , drop = FALSE])
            too_h <- exit_s >= 1
            if (any(too_h)) {
              th2 <- hp_idx[too_h]
              p_rows[th2, ] <- p_rows[th2, , drop = FALSE] * (0.99 / exit_s[too_h])
              p_rows[th2, s] <- 0.01
            }
            ok2 <- hp_idx[!too_h]
            if (length(ok2) > 0) {
              p_rows[ok2, s] <- 1 - rowSums(p_rows[ok2, -s, drop = FALSE])
            }
          }
        }

        # Final normalize (safety)
        p_rows <- pmax(p_rows, 0)
        rsums <- rowSums(p_rows)
        p_rows <- p_rows / rsums

        probs_mat[s_mask, ] <- p_rows
      }

      # Handle already-dead patients in alive set (shouldn't happen, but safety)
      for (s in intersect(unique_states, dead_indices)) {
        s_mask <- which(states_alive == s)
        probs_mat[s_mask, s] <- 1  # stay dead
      }

      # --- State rewards for current state (vectorized) ---
      cur_util_state <- util_by_state[cur_state[alive_idx]]
      cur_cost_state <- maint_by_state[cur_state[alive_idx]] + drug_annual

      if (use_age_utilities) {
        age_norm <- eq5d_mat[alive_idx, t + 1]
        u_current <- age_norm * cur_util_state * u_stable_inv
      } else {
        u_current <- cur_util_state
        if (utils_list$use_decay) u_current <- u_current * (1 - utils_list$decay_rate)^t
      }

      # --- Transition: vectorized sampling ---
      # Cumulative probabilities for each patient
      cum_probs <- t(apply(probs_mat, 1, cumsum))
      rands <- rand_mat[alive_idx, t + 1]
      # Find next state: first column where cumsum >= rand
      next_state_local <- apply(cum_probs >= rands, 1, function(x) which(x)[1])
      next_state_local[is.na(next_state_local)] <- death_noncv_idx

      # Track events
      new_mi <- next_state_local == acute_mi_idx
      new_stroke <- next_state_local == acute_stroke_idx
      n_mi_events[alive_idx[new_mi]] <- n_mi_events[alive_idx[new_mi]] + 1L
      n_stroke_events[alive_idx[new_stroke]] <- n_stroke_events[alive_idx[new_stroke]] + 1L

      # --- Half-cycle correction rewards for next state ---
      next_util_state <- util_by_state[next_state_local]
      next_cost_state <- maint_by_state[next_state_local] + drug_annual

      if (use_age_utilities) {
        next_age_norm <- eq5d_mat_next[alive_idx, t + 1]
        u_next <- next_age_norm * next_util_state * u_stable_inv
      } else {
        u_next <- next_util_state
        if (utils_list$use_decay) u_next <- u_next * (1 - utils_list$decay_rate)^t
      }

      cycle_cost <- (cur_cost_state + next_cost_state) / 2
      cycle_util <- (u_current + u_next) / 2

      pat_costs[alive_idx] <- pat_costs[alive_idx] + cycle_cost * disc_cost_factors[t + 1]
      pat_qalys[alive_idx] <- pat_qalys[alive_idx] + cycle_util * disc_util_factors[t + 1]

      # Update states
      cur_state[alive_idx] <- next_state_local

      # Update alive status
      alive[alive_idx] <- !(next_state_local %in% dead_indices)
    }

    trace_norm <- trace / n_pat
    alive_cols <- !states %in% c("Death_CV", "Death_NonCV")

    event_counts <- cbind(n_mi = n_mi_events, n_stroke = n_stroke_events)

    results[[strat]] <- list(
      avg_cost = mean(pat_costs),
      avg_qaly = mean(pat_qalys),
      trace = trace_norm,
      survival = rowSums(trace_norm[, alive_cols]),
      event_counts = event_counts,
      mean_mi_events = mean(n_mi_events),
      mean_stroke_events = mean(n_stroke_events)
    )
  }

  return(results)
}

# === DEFAULT PATIENT PROFILE GENERATOR ===
# Matches Shiny app defaults (CREATE Registry demographics, REACH/recurrent HRs)
generate_default_profiles <- function(n, seed = 123) {
  set.seed(seed)
  mean_age <- 58; sd_age <- 12; min_age <- 30; max_age <- 85
  pct_male <- 79
  prev_dm <- 0.31; prev_htn <- 0.43; prev_smoke <- 0.40
  rr_dm_mi <- 1.44;  rr_htn_mi <- 1.21;  rr_smoke_mi <- 1.63
  rr_dm_stroke <- 1.85; rr_htn_stroke <- 1.27; rr_smoke_stroke <- 1.52

  eq5d_norms <- data.frame(
    age_lower = c(0, 20, 30, 40, 50, 60, 70),
    age_upper = c(19, 29, 39, 49, 59, 69, 99),
    male   = c(0.936, 0.920, 0.882, 0.833, 0.814, 0.780, 0.643),
    female = c(0.922, 0.908, 0.858, 0.831, 0.766, 0.669, 0.488)
  )
  get_eq5d <- function(age, is_m) {
    idx <- which(eq5d_norms$age_lower <= age & eq5d_norms$age_upper >= age)
    if (length(idx) == 0) idx <- nrow(eq5d_norms)
    if (is_m) eq5d_norms$male[idx] else eq5d_norms$female[idx]
  }

  ages <- pmin(max_age, pmax(min_age, round(rnorm(n, mean_age, sd_age))))
  is_male <- runif(n) < (pct_male / 100)
  has_dm <- runif(n) < prev_dm
  has_htn <- runif(n) < prev_htn
  has_smoke <- runif(n) < prev_smoke

  rr_mi <- rep(1, n); rr_stroke <- rep(1, n)
  rr_mi[has_dm] <- rr_mi[has_dm] * rr_dm_mi
  rr_mi[has_htn] <- rr_mi[has_htn] * rr_htn_mi
  rr_mi[has_smoke] <- rr_mi[has_smoke] * rr_smoke_mi
  rr_stroke[has_dm] <- rr_stroke[has_dm] * rr_dm_stroke
  rr_stroke[has_htn] <- rr_stroke[has_htn] * rr_htn_stroke
  rr_stroke[has_smoke] <- rr_stroke[has_smoke] * rr_smoke_stroke
  rr_mi <- pmin(rr_mi, 10); rr_stroke <- pmin(rr_stroke, 10)

  base_utility <- mapply(get_eq5d, ages, is_male)

  data.frame(
    id = 1:n, age = ages, is_male = is_male,
    has_dm = has_dm, has_htn = has_htn, has_smoke = has_smoke,
    rr_mi = rr_mi, rr_stroke = rr_stroke, base_utility = base_utility
  )
}

# === BACKWARD-COMPATIBLE WRAPPER ===
# Uses full v2 features by default (matching Shiny app behaviour)
run_microsim_engine <- function(tp_list, costs_list, utils_list, n_pat = 1000,
                                 horizon = 30, disc_cost = 0.03, disc_util = 0.03,
                                 seed = 123, patient_profiles = NULL) {
  if (is.null(patient_profiles)) {
    patient_profiles <- generate_default_profiles(n_pat, seed)
  }
  run_microsim_engine_v2(tp_list, costs_list, utils_list, n_pat, horizon,
                          disc_cost, disc_util, seed,
                          patient_profiles = patient_profiles,
                          use_heterogeneity = TRUE,
                          use_adherence_decay = TRUE,
                          use_age_mortality = TRUE,
                          use_age_utilities = TRUE)
}
