# modules/psa_module.R

psaUI <- function(id) {
  ns <- NS(id)
  tagList(
    # --- SECTION 1: PSA INPUTS ---
    fluidRow(
      box(title = "1. PSA Parameter Uncertainty (Standard Errors)", width = 12, status = "primary", solidHeader = TRUE,
          column(4, 
                 h4("Sampling Control"),
                 numericInput(ns("psa_n"), "PSA Iterations", 5000, min = 50, step = 50),
                 helpText("Default: 5000 (matches report pipeline)."),
                 actionButton(ns("run_psa"), "Run Probabilistic Analysis", 
                              class = "btn-danger btn-lg", width = "100%")
          ),
          column(4,
                 h4("Clinical & Cost Uncertainty"),
                 numericInput(ns("se_hr"), "SE of Log(HR) — Tier 1", 0.05, step = 0.01),
                 helpText("Auto-widened for Tier 2 scenarios (adherence-derived HRs)."),
                 numericInput(ns("se_c_rel"), "SE of Costs (Relative %)", 0.15, step = 0.05),
                 numericInput(ns("se_tp"), "SE of Transition Probs", 0.005, step = 0.001),
                 helpText("Applied to p_MI, p_Stroke, p_HF_Death, p_BG_Mort")
          ),
          column(4,
                 h4("Utility Uncertainty"),
                 numericInput(ns("se_u"), "SE of State Utilities", 0.02, step = 0.01),
                 helpText("Applied to all 4 chronic state utilities via Beta sampling")
          )
      )
    ),
    
    # --- SECTION 2: SUMMARY TABLE ---
    fluidRow(
      box(title = "2. PSA Economic Results (Mean & 95% CrI)", width = 12, status = "success", 
          tableOutput(ns("psa_summary_table")),
          footer = "CrI: Credible Interval (2.5th and 97.5th percentiles)")
    ),
    
    # --- SECTION 3: CE PLANE (4 Quadrants) ---
    fluidRow(
      box(title = "3. Incremental Cost-Effectiveness (CE) Plane", width = 12, 
          plotlyOutput(ns("ce_plane"), height = "600px"),
          helpText("The crosshairs at (0,0) divide the plane. Bottom-right is the 'Dominant' zone."))
    ),
    
    # --- SECTION 4: CEAC ---
    fluidRow(
      box(title = "4. Cost-Effectiveness Acceptability Curve (CEAC)", width = 12, 
          plotlyOutput(ns("ceac_plot"), height = "500px"),
          helpText("Probability that the FDC strategy is cost-effective compared to SoC at various WTP thresholds."))
    )
  )
}

psaServer <- function(id, tp_base, cost_base, util_base, n_pat, horizon, disc_cost, disc_util) {
  moduleServer(id, function(input, output, session) {
    
    # Probabilistic Sampling Helpers
    sample_beta <- function(m, se) {
      if(m >= 1 | m <= 0) return(m)
      v <- se^2
      alpha <- ((1 - m) / v - 1 / m) * m ^ 2
      beta <- alpha * (1 / m - 1)
      if(alpha <= 0 | beta <= 0) return(m)
      rbeta(1, alpha, beta)
    }
    
    sample_gamma <- function(m, se_rel) {
      if(m <= 0) return(0)
      se <- m * se_rel
      shape <- (m / se)^2
      scale <- (se^2) / m
      rgamma(1, shape = shape, scale = scale)
    }
    
    # Helper: rebuild 8-state transition matrix from sampled parameters
    rebuild_tp_matrix <- function(p_mi, p_st, p_hf, p_scd, p_bg, cf_mi, cf_st, p_hf_d) {
      states <- c("Stable","Acute_MI","Acute_Stroke","Chronic_MI","Chronic_Stroke","HF","Death_CV","Death_NonCV")
      m <- matrix(0, 8, 8, dimnames = list(states, states))
      exit_sum <- p_mi + p_st + p_hf + p_scd + p_bg
      if (exit_sum >= 1) {
        scale <- 0.99 / exit_sum
        p_mi <- p_mi * scale; p_st <- p_st * scale; p_hf <- p_hf * scale
        p_scd <- p_scd * scale; p_bg <- p_bg * scale
      }
      m["Stable","Acute_MI"] <- p_mi; m["Stable","Acute_Stroke"] <- p_st
      m["Stable","HF"] <- p_hf; m["Stable","Death_CV"] <- p_scd
      m["Stable","Death_NonCV"] <- p_bg; m["Stable","Stable"] <- 1 - sum(m["Stable",])
      m["Acute_MI","Death_CV"] <- cf_mi; m["Acute_MI","Chronic_MI"] <- 1 - cf_mi
      m["Acute_Stroke","Death_CV"] <- cf_st; m["Acute_Stroke","Chronic_Stroke"] <- 1 - cf_st
      m["Chronic_MI","Acute_MI"] <- p_mi*1.5; m["Chronic_MI","Acute_Stroke"] <- p_st
      m["Chronic_MI","HF"] <- p_hf*2.0; m["Chronic_MI","Death_CV"] <- p_scd*1.3
      m["Chronic_MI","Death_NonCV"] <- p_bg
      m["Chronic_MI","Chronic_MI"] <- max(0, 1 - sum(m["Chronic_MI",]))
      m["Chronic_Stroke","Acute_MI"] <- p_mi*1.2; m["Chronic_Stroke","Acute_Stroke"] <- p_st*1.5
      m["Chronic_Stroke","HF"] <- p_hf*1.3; m["Chronic_Stroke","Death_CV"] <- p_scd*1.5
      m["Chronic_Stroke","Death_NonCV"] <- p_bg
      m["Chronic_Stroke","Chronic_Stroke"] <- max(0, 1 - sum(m["Chronic_Stroke",]))
      m["HF","Acute_MI"] <- p_mi*0.5; m["HF","Acute_Stroke"] <- p_st*0.75
      m["HF","Death_CV"] <- p_hf_d; m["HF","Death_NonCV"] <- p_bg
      m["HF","HF"] <- max(0, 1 - sum(m["HF",]))
      m["Death_CV","Death_CV"] <- 1; m["Death_NonCV","Death_NonCV"] <- 1
      # Normalize
      for (i in 1:8) { m[i,] <- pmax(0, m[i,]); m[i,] <- m[i,] / sum(m[i,]) }
      m
    }

    psa_res <- eventReactive(input$run_psa, {
      # FIXED SEED FOR REPRODUCIBILITY
      set.seed(42) 
      
      n_iter <- input$psa_n
      sim_data <- data.frame(dC = numeric(n_iter), dE = numeric(n_iter))
      
      withProgress(message = 'Simulating PSA Cloud...', value = 0, {
        for(i in 1:n_iter) {
          # Capture base values
          tp_p <- tp_base()
          cost_p <- cost_base()
          util_p <- util_base()
          
          # 1. SAMPLE PARAMETERS PROBABILISTICALLY

          # Determine evidence tier from cost module for HR uncertainty
          ev_tier <- if (!is.null(cost_p$evidence_tier)) cost_p$evidence_tier else 1
          # Tier 2 (adherence-derived): widen SE by 3x to reflect derivation uncertainty
          se_hr_adj <- if (!is.na(ev_tier) && ev_tier == 2) input$se_hr * 3.0 else input$se_hr

          # Sample HRs: Log-normal (Preserves ratio symmetry)
          tp_p$hr_MI     <- exp(rnorm(1, log(tp_p$hr_MI), se_hr_adj))
          tp_p$hr_Stroke <- exp(rnorm(1, log(tp_p$hr_Stroke), se_hr_adj))
          tp_p$hr_HF     <- exp(rnorm(1, log(tp_p$hr_HF), se_hr_adj))
          tp_p$hr_SCD    <- exp(rnorm(1, log(tp_p$hr_SCD), se_hr_adj))

          # Sample transition probabilities: Beta
          tp_p$p_MI       <- sample_beta(tp_p$p_MI, input$se_tp)
          tp_p$p_Stroke   <- sample_beta(tp_p$p_Stroke, input$se_tp)
          tp_p$p_HF_Death <- sample_beta(tp_p$p_HF_Death, input$se_tp * 3)
          tp_p$p_BG_Mort  <- sample_beta(tp_p$p_BG_Mort, input$se_tp)

          # Rebuild transition matrices with sampled probabilities
          # SoC matrix
          tp_p$soc <- rebuild_tp_matrix(tp_p$p_MI, tp_p$p_Stroke, tp_p$p_HF,
                                         tp_p$p_SCD, tp_p$p_BG_Mort,
                                         tp_p$cf_MI, tp_p$cf_Stroke, tp_p$p_HF_Death)
          # FDC matrix (apply sampled HRs)
          tp_p$fdc <- rebuild_tp_matrix(tp_p$p_MI * tp_p$hr_MI,
                                         tp_p$p_Stroke * tp_p$hr_Stroke,
                                         tp_p$p_HF * tp_p$hr_HF,
                                         tp_p$p_SCD * tp_p$hr_SCD,
                                         tp_p$p_BG_Mort,
                                         tp_p$cf_MI, tp_p$cf_Stroke, tp_p$p_HF_Death)

          # Sample Costs: Gamma (Ensures non-negative costs)
          cost_p$fdc_annual    <- sample_gamma(cost_p$fdc_annual, input$se_c_rel)
          cost_p$soc_annual    <- sample_gamma(cost_p$soc_annual, input$se_c_rel)
          cost_p$acute_mi      <- sample_gamma(cost_p$acute_mi, input$se_c_rel)
          cost_p$acute_stroke  <- sample_gamma(cost_p$acute_stroke, input$se_c_rel)
          cost_p$maint_stable  <- sample_gamma(cost_p$maint_stable, input$se_c_rel)
          cost_p$maint_mi      <- sample_gamma(cost_p$maint_mi, input$se_c_rel)
          cost_p$maint_stroke  <- sample_gamma(cost_p$maint_stroke, input$se_c_rel)
          cost_p$maint_hf      <- sample_gamma(cost_p$maint_hf, input$se_c_rel)

          # Sample Utilities: Beta (Capped at 1.0)
          util_p$u_stable <- sample_beta(util_p$u_stable, input$se_u)
          util_p$u_mi     <- sample_beta(util_p$u_mi, input$se_u)
          util_p$u_stroke <- sample_beta(util_p$u_stroke, input$se_u)
          util_p$u_hf     <- sample_beta(util_p$u_hf, input$se_u)
          
          # 2. RUN ENGINE
          # We pass a unique seed (42 + i) to each cohort run so they are different 
          # but the entire set of runs remains reproducible.
          res <- run_microsim_engine(tp_p, cost_p, util_p, n_pat = 1000,
                                     horizon = horizon, disc_cost = disc_cost(),
                                     disc_util = disc_util(), seed = 42 + i)
          
          sim_data$dC[i] <- res$FDC$avg_cost - res$SoC$avg_cost
          sim_data$dE[i] <- res$FDC$avg_qaly - res$SoC$avg_qaly
          
          incProgress(1/n_iter)
        }
      })
      sim_data
    })
    
    # SUMMARY TABLE
    output$psa_summary_table <- renderTable({
      req(psa_res()); df <- psa_res()
      data.frame(
        Metric = c("Mean Incremental Cost", "Mean Incremental QALY", "95% CrI (Cost)", "95% CrI (QALY)"),
        Value = c(
          round(mean(df$dC), 0), 
          round(mean(df$dE), 3),
          paste(round(quantile(df$dC, 0.025),0), "to", round(quantile(df$dC, 0.975),0)),
          paste(round(quantile(df$dE, 0.025),3), "to", round(quantile(df$dE, 0.975),3))
        )
      )
    })
    
    # CE PLANE: 4-QUADRANTS
    output$ce_plane <- renderPlotly({
      req(psa_res()); df <- psa_res()
      
      # Determine symmetric axis limits to center (0,0)
      max_e <- max(abs(df$dE)) * 1.3
      max_c <- max(abs(df$dC)) * 1.3
      
      # Quadrant counts for annotation
      pct_dominant <- mean(df$dE > 0 & df$dC < 0) * 100
      
      plot_ly(df, x = ~dE, y = ~dC, type = 'scatter', mode = 'markers', 
              marker = list(color = 'rgba(31, 119, 180, 0.5)', size = 6)) %>%
        layout(
          title = "Incremental Cost-Effectiveness Plane",
          xaxis = list(title = "Incremental QALYs (ΔE)", range = c(-max_e, max_e), 
                       zeroline = TRUE, zerolinecolor = "black", zerolinewidth = 2),
          yaxis = list(title = "Incremental Cost (ΔC) in ₹", range = c(-max_c, max_c), 
                       zeroline = TRUE, zerolinecolor = "black", zerolinewidth = 2),
          annotations = list(
            list(x = max_e*0.8, y = max_c*0.8, text = "Costlier / More Effective", showarrow = FALSE, font = list(size = 10)),
            list(x = max_e*0.8, y = -max_c*0.8, text = paste0("<b>DOMINANT (", round(pct_dominant,1), "%)</b>"), 
                 showarrow = FALSE, font = list(color = "green", size = 12)),
            list(x = -max_e*0.8, y = max_c*0.8, text = "<b>DOMINATED</b>", showarrow = FALSE, font = list(color = "red", size = 12)),
            list(x = -max_e*0.8, y = -max_c*0.8, text = "Cheaper / Less Effective", showarrow = FALSE, font = list(size = 10))
          )
        )
    })
    
    # CEAC CALCULATION
    output$ceac_plot <- renderPlotly({
      req(psa_res()); df <- psa_res()
      wtp_thresholds <- seq(0, 750000, length.out = 50)  # Up to ~3x GDP/capita
      
      ceac_df <- data.frame(
        WTP = wtp_thresholds, 
        Prob = sapply(wtp_thresholds, function(w) mean((df$dE * w) - df$dC > 0))
      )
      
      plot_ly(ceac_df, x = ~WTP, y = ~Prob, type = 'scatter', mode = 'lines+markers', 
              line = list(color = "firebrick", width = 3)) %>%
        layout(
          title = "Cost-Effectiveness Acceptability Curve (CEAC)",
          xaxis = list(title = "Willingness to Pay Threshold (₹)"),
          yaxis = list(title = "Probability Cost-Effective", range = c(0, 1.1))
        )
    })
    return(list(
      scatter = reactive({ output$psa_plot }),
      ceac    = reactive({ output$ceac_plot }),
      data_rx = reactive({ psa_res() })
    ))
  })
}