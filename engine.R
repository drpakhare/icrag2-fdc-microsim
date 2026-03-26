# engine.R

run_microsim_engine <- function(tp_list, costs_list, utils_list, n_pat = 1000, 
                                horizon = 30, disc_cost = 0.03, disc_util = 0.03, 
                                seed = 123) {
  set.seed(seed)
  
  if (is.null(tp_list$soc) | is.null(tp_list$fdc)) return(NULL)
  
  states <- colnames(tp_list$soc)
  results <- list()
  
  for (strat in c("SoC", "FDC")) {
    tp_matrix <- if(strat == "SoC") tp_list$soc else tp_list$fdc
    total_costs <- 0; total_qalys <- 0
    trace <- matrix(0, nrow = horizon + 1, ncol = length(states))
    colnames(trace) <- states
    
    for (i in 1:n_pat) {
      state <- "Stable"
      
      for (t in 0:horizon) {
        # 1. Record current state in trace
        trace[t + 1, state] <- trace[t + 1, state] + 1
        
        # 2. If already in death, carry forward to ALL future years and exit
        if (state %in% c("Death_CV", "Death_NonCV")) {
          if (t < horizon) {
            for (f_t in (t + 1):horizon) {
              trace[f_t + 1, state] <- trace[f_t + 1, state] + 1
            }
          }
          break 
        }
        
        # --- Identify State Rewards ---
        maint <- switch(state, 
                        "Stable" = costs_list$maint_stable, 
                        "Acute_MI"     = costs_list$acute_mi,     # Weighted PCI/CABG/Med
                        "Acute_Stroke" = costs_list$acute_stroke, # Stroke Hospitalization
                        "Chronic_MI" = costs_list$maint_mi, 
                        "Chronic_Stroke" = costs_list$maint_stroke, 
                        "HF" = costs_list$maint_hf, 0)
        drug <- if(strat == "FDC") costs_list$fdc_annual else costs_list$soc_annual
        
        u_base <- switch(state, 
                         "Stable" = utils_list$u_stable, 
                         "Acute_MI"     = utils_list$u_acute_mi, 
                         "Acute_Stroke" = utils_list$u_acute_stroke,
                         "Chronic_MI" = utils_list$u_mi, 
                         "Chronic_Stroke" = utils_list$u_stroke, 
                         "HF" = utils_list$u_hf, 0)
        if (utils_list$use_decay) u_base <- u_base * (1 - utils_list$decay_rate)^t
        
        # --- Transition Step ---
        probs <- pmax(0, tp_matrix[state, ])
        probs <- probs / sum(probs)
        next_state <- sample(states, size = 1, prob = probs)
        
        # --- Half-Cycle Correction (HCC) Rewards ---
        maint_next <- switch(next_state, 
                             "Stable" = costs_list$maint_stable, 
                             "Chronic_MI" = costs_list$maint_mi, 
                             "Chronic_Stroke" = costs_list$maint_stroke, 
                             "HF" = costs_list$maint_hf, 0)
        u_next <- switch(next_state, 
                         "Stable" = utils_list$u_stable, 
                         "Chronic_MI" = utils_list$u_mi, 
                         "Chronic_Stroke" = utils_list$u_stroke, 
                         "HF" = utils_list$u_hf, 0)
        if (utils_list$use_decay) u_next <- u_next * (1 - utils_list$decay_rate)^t
        
        cycle_cost <- ( (maint + drug) + (maint_next + drug) ) / 2
        cycle_util <- (u_base + u_next) / 2
        
        total_costs <- total_costs + (cycle_cost / (1 + disc_cost)^t)
        total_qalys <- total_qalys + (cycle_util / (1 + disc_util)^t)
        
        # Update state
        state <- next_state
      }
    }
    
    trace_norm <- trace / n_pat
    results[[strat]] <- list(
      avg_cost = total_costs / n_pat, 
      avg_qaly = total_qalys / n_pat,
      trace = trace_norm, 
      survival = rowSums(trace_norm[, !colnames(trace_norm) %in% c("Death_CV", "Death_NonCV")])
    )
  }
  return(results)
}