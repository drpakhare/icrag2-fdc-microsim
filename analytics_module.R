# analytics_module.R
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)

# --- FUNCTION: ICER Interpretation Box ---
render_icer_box <- function(res, wtp) {
  dC <- res$FDC$avg_cost - res$SoC$avg_cost
  dE <- res$FDC$avg_qaly - res$SoC$avg_qaly
  icer <- dC / dE
  
  if (dC < 0 & dE > 0) {
    infoBox("Result: FDC DOMINATES", "More Effective & Less Expensive", 
            icon = icon("medal"), color = "green", fill = TRUE)
  } else if (icer < wtp) {
    infoBox("Result: COST-EFFECTIVE", paste("ICER (₹", round(icer,0), ") is below Threshold"), 
            icon = icon("check-circle"), color = "olive", fill = TRUE)
  } else {
    infoBox("Result: NOT COST-EFFECTIVE", paste("ICER (₹", round(icer,0), ") exceeds WTP"), 
            icon = icon("circle-xmark"), color = "red", fill = TRUE)
  }
}

# --- FUNCTION: Survival Curves ---
render_survival_plot <- function(res, horizon) {
  plot_ly() %>%
    add_lines(x = 0:horizon, y = res$SoC$survival, name = "Standard of Care", 
              line = list(dash = "dash", color = "grey")) %>%
    add_lines(x = 0:horizon, y = res$FDC$survival, name = "FDC Strategy", 
              line = list(color = "#2c3e50", width = 3)) %>%
    layout(title = "Survival Probability (Model Validation)", 
           yaxis = list(title = "Survival %", range = c(0, 1)), 
           xaxis = list(title = "Years"))
}

# --- FUNCTION: Markov Trace ---
render_trace_plot <- function(res, horizon) {
  df <- as.data.frame(res$FDC$trace) %>%
    mutate(Year = 0:horizon) %>%
    pivot_longer(cols = -Year, names_to = "State", values_to = "Proportion")
  
  p <- ggplot(df, aes(x = Year, y = Proportion, fill = State)) +
    geom_area(alpha = 0.8) +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral") +
    labs(title = "Life Cycle Distribution (FDC Arm)", x = "Year", y = "Proportion")
  
  ggplotly(p)
}

render_economic_summary <- function(res, wtp = 150000) {
  # 1. Immediate exit if data is missing
  if(is.null(res) || is.null(res$SoC) || is.null(res$FDC)) return(NULL)
  
  # 2. Extract CV Mortality using your exact state name: "Death_CV"
  get_mortality <- function(strat_res) {
    trace <- strat_res$trace
    cols <- colnames(trace)
    
    # Matching the exact name from your transition_module.R
    # We use grep as a fallback just in case of minor string variations
    cv_idx <- which(cols == "Death_CV")
    
    if(length(cv_idx) > 0) {
      return(trace[nrow(trace), cv_idx])
    } else {
      # Fallback if names are lost: 1 - sum of alive states
      death_keywords <- c("Death", "Mortality")
      death_cols <- grep(paste(death_keywords, collapse="|"), cols, ignore.case = TRUE)
      alive_cols <- setdiff(1:ncol(trace), death_cols)
      total_survival <- sum(trace[nrow(trace), alive_cols])
      return(1 - total_survival)
    }
  }
  
  m_soc <- get_mortality(res$SoC)
  m_fdc <- get_mortality(res$FDC)
  
  # 3. Standard Economic Values
  cost_soc <- if(is.numeric(res$SoC$avg_cost)) res$SoC$avg_cost else 0
  cost_fdc <- if(is.numeric(res$FDC$avg_cost)) res$FDC$avg_cost else 0
  qaly_soc <- if(is.numeric(res$SoC$avg_qaly)) res$SoC$avg_qaly else 0
  qaly_fdc <- if(is.numeric(res$FDC$avg_qaly)) res$FDC$avg_qaly else 0
  
  dC <- cost_fdc - cost_soc
  dE <- qaly_fdc - qaly_soc
  
  # 4. Calculate Deaths Prevented (Difference in cumulative mortality * 1000)
  # This uses the difference in Death_CV between SoC and FDC
  dM_prevented <- (m_soc - m_fdc) * 1000 
  
  # 5. Build the Summary Table
  col_metric <- c("Total Cost (Avg)", 
                  "Total QALYs (Avg)", 
                  "CV Mortality Rate (%)", 
                  "CV Deaths Prevented per 1000")
  
  col_soc <- c(paste0("₹ ", format(round(cost_soc, 0), big.mark = ",")),
               round(qaly_soc, 3),
               paste0(round(m_soc * 100, 1), "%"),
               "-")
  
  col_fdc <- c(paste0("₹ ", format(round(cost_fdc, 0), big.mark = ",")),
               round(qaly_fdc, 3),
               paste0(round(m_fdc * 100, 1), "%"),
               paste0("<b>", round(dM_prevented, 1), "</b>"))
  
  col_incr <- c(paste0("₹ ", format(round(dC, 0), big.mark = ",")),
                round(dE, 4),
                "-",
                "-")
  
  data.frame(
    Metric = col_metric,
    SoC = col_soc,
    FDC = col_fdc,
    Incremental = col_incr,
    stringsAsFactors = FALSE
  )
}