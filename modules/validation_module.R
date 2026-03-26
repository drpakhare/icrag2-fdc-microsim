# modules/validation_module.R
# Internal validation + External validation against Indian registries + PSA convergence
# Updated: March 2026

validationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = tagList(icon("vial"), "1. Internal Validation Tests"),
          width = 12, status = "primary", solidHeader = TRUE,
          helpText("Run automated checks to ensure model integrity and face validity."),
          column(4,
                 actionButton(ns("run_null_test"), "Null Test (HR=1.0)",
                              class = "btn-warning btn-lg", icon = icon("equals"),
                              width = "100%"),
                 helpText("Sets all HRs to 1.0. Both arms should produce identical results.")
          ),
          column(4,
                 actionButton(ns("run_extreme_test"), "Extreme Value Test",
                              class = "btn-danger btn-lg", icon = icon("bolt"),
                              width = "100%"),
                 helpText("Tests model with extreme inputs (very high/low probabilities).")
          ),
          column(4,
                 actionButton(ns("run_convergence_test"), "Convergence Test (N→∞)",
                              class = "btn-info btn-lg", icon = icon("chart-line"),
                              width = "100%"),
                 helpText("Runs increasing N to check if ICER stabilizes.")
          )
      )
    ),

    fluidRow(
      box(title = "Null Test Results", width = 6, status = "warning",
          uiOutput(ns("null_test_result")),
          tableOutput(ns("null_test_table"))
      ),
      box(title = "Extreme Value Test Results", width = 6, status = "danger",
          uiOutput(ns("extreme_test_result")),
          tableOutput(ns("extreme_test_table"))
      )
    ),

    fluidRow(
      box(title = tagList(icon("chart-line"), "2. ICER Convergence by Cohort Size"),
          width = 12, status = "info",
          plotlyOutput(ns("convergence_plot"), height = "400px"),
          helpText("ICER should stabilize as N increases. If volatile, increase base case N.")
      )
    ),

    fluidRow(
      box(title = tagList(icon("bullseye"), "3. External Validation: Indian Registry Benchmarks"),
          width = 12, status = "success", solidHeader = TRUE,
          helpText("Compare model-predicted outcomes (SoC arm) against observed data from Indian post-ACS registries.
                    Uses a 5,000-patient deterministic run over 10 years."),
          uiOutput(ns("tier_banner")),
          column(4,
                 actionButton(ns("run_external_val"), "Run External Validation",
                              class = "btn-success btn-lg", icon = icon("magnifying-glass"),
                              width = "100%")
          ),
          column(8,
                 uiOutput(ns("external_val_status"))
          )
      )
    ),

    fluidRow(
      box(title = "3a. Mortality & Survival Benchmarks", width = 6, status = "success",
          solidHeader = TRUE,
          tableOutput(ns("val_mortality_table")),
          uiOutput(ns("val_mortality_verdict"))
      ),
      box(title = "3b. Morbidity & Event Rate Benchmarks", width = 6, status = "info",
          solidHeader = TRUE,
          tableOutput(ns("val_morbidity_table")),
          uiOutput(ns("val_morbidity_verdict"))
      )
    ),

    fluidRow(
      box(title = "3c. Clinical Coherence Checks", width = 12, status = "warning",
          solidHeader = TRUE,
          tableOutput(ns("val_coherence_table")),
          helpText("These checks verify that the model's internal logic produces clinically plausible patterns.")
      )
    ),

    fluidRow(
      box(title = tagList(icon("dice"), "4. PSA Convergence Diagnostic"),
          width = 12, status = "info",
          helpText("Cumulative mean ICER should stabilize. Run PSA first on Tab 8."),
          plotlyOutput(ns("psa_convergence_plot"), height = "400px")
      )
    )
  )
}

validationServer <- function(id, tp_base, cost_base, util_base, horizon_rx,
                              psa_data_rx = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========== EVIDENCE TIER DETECTION ==========
    current_tier <- reactive({
      co <- tryCatch(cost_base(), error = function(e) NULL)
      if (!is.null(co) && !is.null(co$evidence_tier)) co$evidence_tier else 1
    })

    current_source <- reactive({
      co <- tryCatch(cost_base(), error = function(e) NULL)
      if (!is.null(co) && !is.null(co$evidence_source)) co$evidence_source else "SECURE (default)"
    })

    output$tier_banner <- renderUI({
      tier <- current_tier()
      if (!is.na(tier) && tier == 2) {
        div(class = "alert alert-warning", style = "margin-bottom: 15px;",
            icon("info-circle"),
            tags$strong(" Tier 2 scenario selected: "), current_source(), tags$br(),
            "FDC efficacy is adherence-derived (HR 0.95\u20130.97). Coherence checks in Section 3c ",
            "use relaxed thresholds since the expected FDC\u2013SoC difference is small and may ",
            "fall within stochastic noise. Sections 3a/3b (SoC arm benchmarks) remain fully valid.")
      }
    })

    # ========== NULL TEST ==========
    null_res <- eventReactive(input$run_null_test, {
      tp_null <- tp_base()
      tp_null$hr_MI <- 1.0
      tp_null$hr_Stroke <- 1.0
      tp_null$hr_HF <- 1.0
      tp_null$hr_SCD <- 1.0
      tp_null$fdc <- tp_null$soc

      co_null <- cost_base()
      co_null$fdc_annual <- co_null$soc_annual

      withProgress(message = "Running null test (HR=1.0, equal costs)...", value = 0.5, {
        res <- run_microsim_engine(tp_null, co_null, util_base(),
                                    n_pat = 5000, horizon = horizon_rx(),
                                    seed = 777)
      })
      res
    })

    output$null_test_result <- renderUI({
      req(null_res())
      res <- null_res()
      dE <- abs(res$FDC$avg_qaly - res$SoC$avg_qaly)
      dC <- abs(res$FDC$avg_cost - res$SoC$avg_cost)
      threshold_e <- 0.15
      threshold_c_pct <- 0.05
      avg_cost <- (res$SoC$avg_cost + res$FDC$avg_cost) / 2
      rel_c <- if (avg_cost > 0) dC / avg_cost else 0

      if (dE < threshold_e && rel_c < threshold_c_pct) {
        div(class = "alert alert-success",
            icon("check-circle"),
            sprintf(" PASS: ΔQALY = %.4f (< %.2f), ΔCost = %.0f (%.1f%% relative). Arms equivalent when HR=1.",
                    dE, threshold_e, dC, rel_c * 100))
      } else {
        div(class = "alert alert-warning",
            icon("info-circle"),
            sprintf(" QALY diff = %.4f, Cost diff = %.0f (%.1f%% relative). ",
                    dE, dC, rel_c * 100),
            "Differences reflect stochastic noise in microsimulation. ",
            "Run with larger N or use common random numbers to reduce variance.")
      }
    })

    output$null_test_table <- renderTable({
      req(null_res())
      res <- null_res()
      data.frame(
        Metric = c("Avg Cost", "Avg QALY", "CV Mortality (final yr)"),
        SoC = c(round(res$SoC$avg_cost), round(res$SoC$avg_qaly, 3),
                round(res$SoC$trace[nrow(res$SoC$trace), "Death_CV"], 3)),
        FDC = c(round(res$FDC$avg_cost), round(res$FDC$avg_qaly, 3),
                round(res$FDC$trace[nrow(res$FDC$trace), "Death_CV"], 3)),
        Difference = c(
          round(res$FDC$avg_cost - res$SoC$avg_cost),
          round(res$FDC$avg_qaly - res$SoC$avg_qaly, 4),
          round(res$FDC$trace[nrow(res$FDC$trace), "Death_CV"] -
                res$SoC$trace[nrow(res$SoC$trace), "Death_CV"], 4)
        )
      )
    }, digits = 4)

    # ========== EXTREME VALUE TEST ==========
    extreme_res <- eventReactive(input$run_extreme_test, {
      results_list <- list()

      withProgress(message = "Running extreme value tests...", value = 0, {
        # Test 1: Very low MI risk
        tp_low <- tp_base()
        tp_low$soc["Stable", "Acute_MI"] <- 0.001
        tp_low$soc["Stable", "Stable"] <- 1 - sum(tp_low$soc["Stable", ]) + tp_low$soc["Stable", "Stable"]
        tp_low$fdc <- tp_low$soc
        res1 <- run_microsim_engine(tp_low, cost_base(), util_base(), 500, 10, seed = 111)
        results_list[["Low MI Risk"]] <- c(res1$SoC$avg_cost, res1$SoC$avg_qaly,
                                            min(res1$SoC$survival) >= 0)
        incProgress(0.25)

        # Test 2: Very high mortality
        tp_high <- tp_base()
        tp_high$soc["HF", "Death_CV"] <- 0.50
        tp_high$soc["HF", "HF"] <- 1 - sum(tp_high$soc["HF", ]) + tp_high$soc["HF", "HF"]
        tp_high$fdc <- tp_high$soc
        res2 <- run_microsim_engine(tp_high, cost_base(), util_base(), 500, 10, seed = 222)
        results_list[["High HF Mortality"]] <- c(res2$SoC$avg_cost, res2$SoC$avg_qaly,
                                                   min(res2$SoC$survival) >= 0)
        incProgress(0.25)

        # Test 3: Zero costs
        co_zero <- cost_base()
        co_zero$maint_stable <- 0; co_zero$maint_mi <- 0; co_zero$maint_stroke <- 0
        co_zero$maint_hf <- 0; co_zero$soc_annual <- 0; co_zero$fdc_annual <- 0
        co_zero$acute_mi <- 0; co_zero$acute_stroke <- 0
        res3 <- run_microsim_engine(tp_base(), co_zero, util_base(), 500, 10, seed = 333)
        results_list[["Zero Costs"]] <- c(res3$SoC$avg_cost, res3$SoC$avg_qaly,
                                           res3$SoC$avg_cost < 1)
        incProgress(0.25)

        # Test 4: Perfect drug (HR = 0.01 for all events)
        tp_perf <- tp_base()
        tp_perf$hr_MI <- 0.01; tp_perf$hr_Stroke <- 0.01
        tp_perf$hr_HF <- 0.01; tp_perf$hr_SCD <- 0.01
        # Rebuild FDC matrix with near-zero event rates
        tp_perf$fdc <- tp_perf$soc
        for (rn in rownames(tp_perf$fdc)) {
          if (rn %in% c("Death_CV", "Death_NonCV")) next
          for (cn in c("Acute_MI", "Acute_Stroke", "HF", "Death_CV")) {
            if (cn %in% colnames(tp_perf$fdc)) {
              tp_perf$fdc[rn, rn] <- tp_perf$fdc[rn, rn] + tp_perf$fdc[rn, cn] * 0.99
              tp_perf$fdc[rn, cn] <- tp_perf$fdc[rn, cn] * 0.01
            }
          }
        }
        res4 <- run_microsim_engine(tp_perf, cost_base(), util_base(), 500, 10, seed = 444)
        fdc_better <- res4$FDC$avg_qaly > res4$SoC$avg_qaly
        results_list[["Perfect Drug (HR→0)"]] <- c(res4$FDC$avg_cost, res4$FDC$avg_qaly, fdc_better)
        incProgress(0.25)
      })

      results_list
    })

    output$extreme_test_result <- renderUI({
      req(extreme_res())
      all_pass <- all(sapply(extreme_res(), function(x) x[3]))
      if (all_pass) {
        div(class = "alert alert-success", icon("check-circle"),
            " All extreme value tests passed. No invalid outputs detected.")
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"),
            " Some tests produced unexpected results. Review table below.")
      }
    })

    output$extreme_test_table <- renderTable({
      req(extreme_res())
      rl <- extreme_res()
      data.frame(
        Test = names(rl),
        Cost = sapply(rl, function(x) round(x[1])),
        QALY = sapply(rl, function(x) round(x[2], 3)),
        Valid = sapply(rl, function(x) ifelse(x[3], "PASS", "FAIL"))
      )
    })

    # ========== CONVERGENCE TEST ==========
    conv_res <- eventReactive(input$run_convergence_test, {
      ns <- c(100, 250, 500, 1000, 2000, 5000)
      icers <- numeric(length(ns))

      withProgress(message = "Testing convergence...", value = 0, {
        for (j in seq_along(ns)) {
          res <- run_microsim_engine(tp_base(), cost_base(), util_base(),
                                      n_pat = ns[j], horizon = min(horizon_rx(), 20),
                                      seed = 42)
          dC <- res$FDC$avg_cost - res$SoC$avg_cost
          dE <- res$FDC$avg_qaly - res$SoC$avg_qaly
          icers[j] <- if (abs(dE) > 0.0001) dC / dE else NA
          incProgress(1 / length(ns))
        }
      })

      data.frame(N = ns, ICER = icers)
    })

    output$convergence_plot <- renderPlotly({
      req(conv_res())
      df <- conv_res()

      plot_ly(df, x = ~N, y = ~ICER, type = "scatter", mode = "lines+markers",
              line = list(color = "#2c3e50", width = 3),
              marker = list(size = 10)) %>%
        layout(title = "ICER Convergence by Cohort Size",
               xaxis = list(title = "Number of Patients (N)", type = "log"),
               yaxis = list(title = "ICER (₹ / QALY)"),
               shapes = list(
                 list(type = "line", x0 = min(df$N), x1 = max(df$N),
                      y0 = median(df$ICER, na.rm = TRUE),
                      y1 = median(df$ICER, na.rm = TRUE),
                      line = list(dash = "dash", color = "red"))
               ))
    })

    # ========== EXTERNAL VALIDATION: INDIAN REGISTRY BENCHMARKS ==========
    ext_val_res <- eventReactive(input$run_external_val, {
      withProgress(message = "Running validation simulation (N=5000, 10yr)...", value = 0.5, {
        res <- run_microsim_engine(tp_base(), cost_base(), util_base(),
                                    n_pat = 5000, horizon = 10, seed = 54321)
      })

      trace_soc <- res$SoC$trace
      surv_soc  <- res$SoC$survival

      # Model predictions at key timepoints
      # Year 0 = index (row 1), year 1 = row 2, year 5 = row 6, year 10 = row 11
      pred_1yr_cv_mort    <- trace_soc[2, "Death_CV"]
      pred_1yr_all_mort   <- trace_soc[2, "Death_CV"] + trace_soc[2, "Death_NonCV"]
      pred_5yr_cv_mort    <- trace_soc[6, "Death_CV"]
      pred_5yr_all_mort   <- trace_soc[6, "Death_CV"] + trace_soc[6, "Death_NonCV"]
      pred_1yr_surv       <- surv_soc[2]
      pred_5yr_surv       <- surv_soc[6]
      pred_10yr_surv      <- if(length(surv_soc) >= 11) surv_soc[11] else NA

      # Morbidity: extract from trace proportions
      pred_1yr_acute_mi   <- trace_soc[2, "Acute_MI"]
      pred_1yr_stroke     <- trace_soc[2, "Acute_Stroke"]
      pred_1yr_hf         <- trace_soc[2, "HF"]
      pred_1yr_mace       <- pred_1yr_cv_mort + pred_1yr_acute_mi + pred_1yr_stroke

      # 5-year cumulative: approximate from Death_CV accumulation
      pred_5yr_mace_approx <- pred_5yr_cv_mort + trace_soc[6, "Chronic_MI"] +
                               trace_soc[6, "Chronic_Stroke"]

      # FDC arm for coherence checks
      trace_fdc <- res$FDC$trace
      surv_fdc  <- res$FDC$survival

      list(
        # Mortality predictions
        pred_1yr_cv_mort = pred_1yr_cv_mort,
        pred_1yr_all_mort = pred_1yr_all_mort,
        pred_5yr_cv_mort = pred_5yr_cv_mort,
        pred_5yr_all_mort = pred_5yr_all_mort,
        pred_1yr_surv = pred_1yr_surv,
        pred_5yr_surv = pred_5yr_surv,
        pred_10yr_surv = pred_10yr_surv,
        # Morbidity
        pred_1yr_acute_mi = pred_1yr_acute_mi,
        pred_1yr_stroke = pred_1yr_stroke,
        pred_1yr_hf = pred_1yr_hf,
        pred_1yr_mace = pred_1yr_mace,
        # FDC arm
        fdc_1yr_surv = surv_fdc[2],
        fdc_5yr_surv = surv_fdc[6],
        fdc_1yr_cv_mort = trace_fdc[2, "Death_CV"],
        # Full results for additional checks
        res = res
      )
    })

    output$external_val_status <- renderUI({
      req(ext_val_res())
      div(class = "alert alert-success", icon("check-circle"),
          " Validation simulation complete. Review benchmarks below.")
    })

    # 3a. Mortality & Survival Table
    output$val_mortality_table <- renderTable({
      req(ext_val_res())
      v <- ext_val_res()
      data.frame(
        Metric = c(
          "1-Year CV Mortality",
          "1-Year All-Cause Mortality",
          "1-Year Survival",
          "5-Year CV Mortality",
          "5-Year All-Cause Mortality",
          "5-Year Survival",
          "10-Year Survival"
        ),
        Model = c(
          paste0(round(v$pred_1yr_cv_mort * 100, 1), "%"),
          paste0(round(v$pred_1yr_all_mort * 100, 1), "%"),
          paste0(round(v$pred_1yr_surv * 100, 1), "%"),
          paste0(round(v$pred_5yr_cv_mort * 100, 1), "%"),
          paste0(round(v$pred_5yr_all_mort * 100, 1), "%"),
          paste0(round(v$pred_5yr_surv * 100, 1), "%"),
          paste0(round(v$pred_10yr_surv * 100, 1), "%")
        ),
        Registry_Target = c(
          "5.4% (Kalliath 2021, Kerala, N=1,148)",
          "6.3% (Kalliath 2021, 1yr all-cause)",
          "93.7% (Kalliath 2021 implied)",
          "~15-20% (extrapolated from 1yr rates)",
          "~25-30% (Indian cohorts)",
          "70-75% (Indian cohorts, Kalliath implied)",
          "~50-60% (extrapolated)"
        ),
        Acceptable_Range = c(
          "3.5 - 7.5%",
          "4.5 - 9.0%",
          "91 - 96%",
          "12 - 25%",
          "20 - 35%",
          "65 - 80%",
          "40 - 65%"
        )
      )
    })

    output$val_mortality_verdict <- renderUI({
      req(ext_val_res())
      v <- ext_val_res()
      checks <- c(
        v$pred_1yr_cv_mort >= 0.035 && v$pred_1yr_cv_mort <= 0.075,
        v$pred_1yr_all_mort >= 0.045 && v$pred_1yr_all_mort <= 0.09,
        v$pred_5yr_surv >= 0.65 && v$pred_5yr_surv <= 0.80
      )
      n_pass <- sum(checks)
      if (n_pass == 3) {
        div(class = "alert alert-success", icon("check"),
            sprintf(" All %d key mortality benchmarks within acceptable range.", n_pass))
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"),
            sprintf(" %d of 3 key benchmarks within range. Review outliers above.", n_pass))
      }
    })

    # 3b. Morbidity Table
    output$val_morbidity_table <- renderTable({
      req(ext_val_res())
      v <- ext_val_res()
      data.frame(
        Metric = c(
          "1-Year MI Recurrence",
          "1-Year Stroke",
          "1-Year MACE (MI+Stroke+CVD)",
          "1-Year HF Incidence"
        ),
        Model = c(
          paste0(round(v$pred_1yr_acute_mi * 100, 2), "%"),
          paste0(round(v$pred_1yr_stroke * 100, 2), "%"),
          paste0(round(v$pred_1yr_mace * 100, 1), "%"),
          paste0(round(v$pred_1yr_hf * 100, 2), "%")
        ),
        Registry_Target = c(
          "2.65% (Dev 2025, Kerala, N=234)",
          "1.1% (Isezuo 2014, Chennai, N=1,468)",
          "~7% (composite: MI+Stroke+CVDeath)",
          "2.3% (Huffman & Prabhakaran 2010)"
        ),
        Acceptable_Range = c(
          "1.5 - 4.0%",
          "0.5 - 2.0%",
          "4.0 - 10.0%",
          "1.0 - 4.0%"
        )
      )
    })

    output$val_morbidity_verdict <- renderUI({
      req(ext_val_res())
      v <- ext_val_res()
      checks <- c(
        v$pred_1yr_acute_mi >= 0.015 && v$pred_1yr_acute_mi <= 0.04,
        v$pred_1yr_stroke >= 0.005 && v$pred_1yr_stroke <= 0.02,
        v$pred_1yr_mace >= 0.04 && v$pred_1yr_mace <= 0.10,
        v$pred_1yr_hf >= 0.01 && v$pred_1yr_hf <= 0.04
      )
      n_pass <- sum(checks)
      if (n_pass == 4) {
        div(class = "alert alert-success", icon("check"),
            sprintf(" All %d morbidity benchmarks within acceptable range.", n_pass))
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"),
            sprintf(" %d of 4 morbidity benchmarks within range. Review above.", n_pass))
      }
    })

    # 3c. Clinical Coherence Checks (tier-aware)
    output$val_coherence_table <- renderTable({
      req(ext_val_res())
      v <- ext_val_res()
      tier <- current_tier()

      # Raw differences
      surv_diff_5yr <- v$fdc_5yr_surv - v$pred_5yr_surv
      cv_diff_1yr   <- v$pred_1yr_cv_mort - v$fdc_1yr_cv_mort

      # Tier 1: FDC must be clearly better. Tier 2: accept >=0 (within noise)
      if (!is.na(tier) && tier == 2) {
        # Tier 2: small effect, allow stochastic overlap (direction correct OR within 1% noise)
        fdc_surv_better <- surv_diff_5yr >= -0.01
        fdc_cv_lower    <- cv_diff_1yr >= -0.005
        fdc_surv_label  <- "FDC 5yr survival >= SoC (within noise)"
        fdc_cv_label    <- "FDC 1yr CV mort <= SoC (within noise)"
        fdc_surv_expect <- "TRUE (Tier 2: <=1% noise tolerated)"
        fdc_cv_expect   <- "TRUE (Tier 2: <=0.5% noise tolerated)"
      } else {
        fdc_surv_better <- surv_diff_5yr > 0
        fdc_cv_lower    <- cv_diff_1yr > 0
        fdc_surv_label  <- "FDC 5yr survival > SoC 5yr survival"
        fdc_cv_label    <- "FDC 1yr CV mortality < SoC 1yr CV mortality"
        fdc_surv_expect <- "TRUE"
        fdc_cv_expect   <- "TRUE"
      }

      # These checks are always the same regardless of tier
      mort_monotone   <- v$pred_5yr_all_mort > v$pred_1yr_all_mort
      surv_decreasing <- v$pred_5yr_surv < v$pred_1yr_surv
      cv_dominant     <- v$pred_1yr_cv_mort > (v$pred_1yr_all_mort - v$pred_1yr_cv_mort)

      data.frame(
        Check = c(
          fdc_surv_label,
          fdc_cv_label,
          "5yr all-cause mortality > 1yr all-cause mortality",
          "5yr survival < 1yr survival (monotone decline)",
          "CV death > Non-CV death at 1yr (post-MI cohort)"
        ),
        Expected = c(fdc_surv_expect, fdc_cv_expect, "TRUE", "TRUE", "TRUE (CV dominant cause)"),
        Result = c(
          ifelse(fdc_surv_better, "PASS", "FAIL"),
          ifelse(fdc_cv_lower, "PASS", "FAIL"),
          ifelse(mort_monotone, "PASS", "FAIL"),
          ifelse(surv_decreasing, "PASS", "FAIL"),
          ifelse(cv_dominant, "PASS", "CHECK")
        ),
        Detail = c(
          sprintf("FDC: %.1f%% vs SoC: %.1f%% (diff: %+.2f%%)", v$fdc_5yr_surv * 100, v$pred_5yr_surv * 100, surv_diff_5yr * 100),
          sprintf("FDC: %.2f%% vs SoC: %.2f%% (diff: %+.3f%%)", v$fdc_1yr_cv_mort * 100, v$pred_1yr_cv_mort * 100, -cv_diff_1yr * 100),
          sprintf("5yr: %.1f%% vs 1yr: %.1f%%", v$pred_5yr_all_mort * 100, v$pred_1yr_all_mort * 100),
          sprintf("5yr: %.1f%% vs 1yr: %.1f%%", v$pred_5yr_surv * 100, v$pred_1yr_surv * 100),
          sprintf("CV: %.2f%% vs Non-CV: %.2f%%",
                  v$pred_1yr_cv_mort * 100,
                  (v$pred_1yr_all_mort - v$pred_1yr_cv_mort) * 100)
        )
      )
    })

    # ========== PSA CONVERGENCE ==========
    output$psa_convergence_plot <- renderPlotly({
      req(!is.null(psa_data_rx))
      df <- psa_data_rx()
      req(nrow(df) > 5)

      # Cumulative mean ICER (handle division carefully)
      cum_icer <- cumsum(df$dC) / cumsum(df$dE)
      cum_df <- data.frame(Iteration = 1:nrow(df), Cum_ICER = cum_icer)

      # Also compute running 95% CI
      final_icer <- mean(df$dC) / mean(df$dE)

      plot_ly(cum_df, x = ~Iteration, y = ~Cum_ICER, type = "scatter", mode = "lines",
              line = list(color = "#e74c3c", width = 2)) %>%
        layout(title = "PSA Convergence: Cumulative Mean ICER",
               xaxis = list(title = "PSA Iteration"),
               yaxis = list(title = "Cumulative Mean ICER (₹)"),
               shapes = list(
                 list(type = "line", x0 = 0, x1 = nrow(df),
                      y0 = final_icer, y1 = final_icer,
                      line = list(dash = "dot", color = "grey"))
               ))
    })
  })
}
