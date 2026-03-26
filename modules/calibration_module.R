# modules/calibration_module.R
# Auto-calibration against Indian registry targets (CREATE, Kerala ACS, GRACE)

calibrationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = tagList(icon("bullseye"), "1. Calibration Targets (Indian Registry Data)"),
          width = 12, status = "danger", solidHeader = TRUE,
          helpText("Set observed outcomes from Indian registries. The calibrator will search for
                    transition probabilities that reproduce these targets."),
          column(3,
                 h4("Short-Term (CREATE/Kerala)"),
                 numericInput(ns("target_1yr_cv_mort"), "1-Year CV Mortality (%)", 5.4, min = 1, max = 20, step = 0.5),
                 numericInput(ns("target_30d_mort"), "30-Day All-Cause Mortality (%)", 5.6, min = 1, max = 15, step = 0.5),
                 helpText("Kalliath 2021: CVD death 5.4%; CREATE: 30-day 5.6%")
          ),
          column(3,
                 h4("Medium-Term"),
                 numericInput(ns("target_1yr_mace"), "1-Year MACE Rate (%)", 7.0, min = 3, max = 25, step = 0.5),
                 numericInput(ns("target_1yr_mi_recur"), "1-Year MI Recurrence (%)", 2.65, min = 0.5, max = 15, step = 0.5),
                 helpText("Dev 2025 Kerala: MI 2.65%; MACE ~7% (MI+Stroke+CVDeath)")
          ),
          column(3,
                 h4("Long-Term"),
                 numericInput(ns("target_5yr_survival"), "5-Year Survival (%)", 72.0, min = 50, max = 90, step = 1),
                 helpText("Indian cohorts ~70-75%. Kalliath: 1yr all-cause 6.3%")
          ),
          column(3,
                 h4("Setting"),
                 selectInput(ns("calib_setting"), "Care Setting",
                             choices = c("Urban Tertiary (PCI 55%)" = "urban",
                                         "District Hospital (PCI 15%)" = "district",
                                         "Rural (PCI <1%)" = "rural")),
                 helpText("Uses deterministic cohort trace (instant) + Nelder-Mead optimization. ~2 seconds.")
          )
      )
    ),

    fluidRow(
      box(title = tagList(icon("play"), "2. Run Calibration"),
          width = 12, status = "warning", solidHeader = TRUE,
          column(4,
                 actionButton(ns("run_calib"), "Auto-Calibrate Parameters",
                              class = "btn-danger btn-lg", icon = icon("crosshairs"),
                              width = "100%"),
                 helpText("Grid search over plausible parameter space. Takes 1-3 minutes.")
          ),
          column(8,
                 uiOutput(ns("calib_status"))
          )
      )
    ),

    fluidRow(
      box(title = "3. Calibration Results: Optimal Parameters", width = 6, status = "success",
          solidHeader = TRUE,
          tableOutput(ns("calib_params_table")),
          hr(),
          actionButton(ns("apply_calib"), "Apply Calibrated Values to Model",
                       class = "btn-success btn-lg", icon = icon("check"), width = "100%")
      ),
      box(title = "4. Fit Quality: Model vs Registry", width = 6, status = "info",
          solidHeader = TRUE,
          tableOutput(ns("calib_fit_table")),
          uiOutput(ns("calib_fit_status"))
      )
    ),

    fluidRow(
      box(title = "5. Case Fatality by Setting", width = 12, status = "primary",
          helpText("Blended case fatality depends on your care setting's intervention mix."),
          tableOutput(ns("setting_table"))
      )
    )
  )
}

calibrationServer <- function(id, tp_base, cost_base, util_base) {
  moduleServer(id, function(input, output, session) {

    # Setting-dependent case fatality
    setting_params <- reactive({
      switch(input$calib_setting,
        "urban" = list(
          cf_MI = 0.065,   # Weighted: 55% PCI(4.1%) + 33% lysis(12.3%) + 12% conserv(15%)
          cf_Stroke = 0.12,
          label = "Urban Tertiary (PCI 55%, Lysis 33%, Conservative 12%)"
        ),
        "district" = list(
          cf_MI = 0.115,   # Weighted: 15% PCI + 37% lysis + 48% conservative
          cf_Stroke = 0.15,
          label = "District Hospital (PCI 15%, Lysis 37%, Conservative 48%)"
        ),
        "rural" = list(
          cf_MI = 0.14,    # Mostly conservative + some lysis
          cf_Stroke = 0.18,
          label = "Rural (PCI <1%, Lysis 36%, Conservative 63%)"
        )
      )
    })

    output$setting_table <- renderTable({
      sp <- setting_params()
      data.frame(
        Parameter = c("Setting", "Blended MI Case Fatality", "Blended Stroke Case Fatality"),
        Value = c(sp$label, paste0(round(sp$cf_MI * 100, 1), "%"), paste0(round(sp$cf_Stroke * 100, 1), "%"))
      )
    })

    # ====== FAST CALIBRATION ENGINE (Deterministic Cohort Trace + Nelder-Mead) ======
    # Uses matrix multiplication instead of microsimulation — instant evaluation
    calib_results <- eventReactive(input$run_calib, {
      sp <- setting_params()

      # Targets
      t_1yr_cv_mort <- input$target_1yr_cv_mort / 100
      t_1yr_mace <- input$target_1yr_mace / 100
      t_5yr_surv <- input$target_5yr_survival / 100

      tp_ref <- tp_base()
      states <- colnames(tp_ref$soc)
      n_states <- length(states)

      # Deterministic cohort trace: no stochastic noise, instant evaluation
      run_cohort_trace <- function(p_mi, p_scd, p_hf_d, p_bg) {
        m <- matrix(0, n_states, n_states, dimnames = list(states, states))

        p_st <- tp_ref$p_Stroke
        p_hf <- tp_ref$p_HF

        exit_sum <- p_mi + p_st + p_hf + p_scd + p_bg
        if (exit_sum >= 0.99) return(list(cv_mort_1yr = NA, mace_1yr = NA, surv_5yr = NA))

        m["Stable", "Acute_MI"] <- p_mi
        m["Stable", "Acute_Stroke"] <- p_st
        m["Stable", "HF"] <- p_hf
        m["Stable", "Death_CV"] <- p_scd
        m["Stable", "Death_NonCV"] <- p_bg
        m["Stable", "Stable"] <- 1 - sum(m["Stable",])

        m["Acute_MI", "Death_CV"] <- sp$cf_MI
        m["Acute_MI", "Chronic_MI"] <- 1 - sp$cf_MI

        m["Acute_Stroke", "Death_CV"] <- sp$cf_Stroke
        m["Acute_Stroke", "Chronic_Stroke"] <- 1 - sp$cf_Stroke

        m["Chronic_MI", "Acute_MI"] <- p_mi * 1.5
        m["Chronic_MI", "Acute_Stroke"] <- p_st
        m["Chronic_MI", "HF"] <- p_hf * 2.0
        m["Chronic_MI", "Death_CV"] <- p_scd * 1.3
        m["Chronic_MI", "Death_NonCV"] <- p_bg
        m["Chronic_MI", "Chronic_MI"] <- 1 - sum(m["Chronic_MI",])

        m["Chronic_Stroke", "Acute_MI"] <- p_mi * 1.2
        m["Chronic_Stroke", "Acute_Stroke"] <- p_st * 1.5
        m["Chronic_Stroke", "HF"] <- p_hf * 1.3
        m["Chronic_Stroke", "Death_CV"] <- p_scd * 1.5
        m["Chronic_Stroke", "Death_NonCV"] <- p_bg
        m["Chronic_Stroke", "Chronic_Stroke"] <- 1 - sum(m["Chronic_Stroke",])

        m["HF", "Acute_MI"] <- p_mi * 0.5
        m["HF", "Acute_Stroke"] <- p_st * 0.75
        m["HF", "Death_CV"] <- p_hf_d
        m["HF", "Death_NonCV"] <- p_bg
        m["HF", "HF"] <- 1 - sum(m["HF",])

        m["Death_CV", "Death_CV"] <- 1
        m["Death_NonCV", "Death_NonCV"] <- 1

        # Check for invalid rows
        for (r in 1:n_states) {
          if (any(m[r, ] < -0.001)) return(list(cv_mort_1yr = NA, mace_1yr = NA, surv_5yr = NA))
          m[r, ] <- pmax(0, m[r, ])
          m[r, ] <- m[r, ] / sum(m[r, ])
        }

        # Run deterministic cohort trace via matrix power
        cohort <- rep(0, n_states)
        cohort[1] <- 1  # All start in Stable

        trace <- matrix(0, nrow = 11, ncol = n_states)
        colnames(trace) <- states
        trace[1, ] <- cohort

        for (t in 1:10) {
          cohort <- cohort %*% m
          trace[t + 1, ] <- cohort
        }

        alive_cols <- !states %in% c("Death_CV", "Death_NonCV")
        cv_mort_1yr <- trace[2, "Death_CV"]
        surv_5yr <- sum(trace[6, alive_cols])
        mace_1yr <- cv_mort_1yr + trace[2, "Acute_MI"] + trace[2, "Acute_Stroke"]

        list(cv_mort_1yr = cv_mort_1yr, mace_1yr = min(1, mace_1yr), surv_5yr = surv_5yr)
      }

      # Objective function for optimizer
      objective <- function(par) {
        p_mi <- par[1]; p_scd <- par[2]; p_hf_d <- par[3]; p_bg <- par[4]
        # Bounds check
        if (any(par < 0.001) || any(par > 0.5)) return(1e6)
        if (p_mi + p_scd + tp_ref$p_Stroke + tp_ref$p_HF + p_bg >= 0.95) return(1e6)

        pred <- run_cohort_trace(p_mi, p_scd, p_hf_d, p_bg)
        if (is.na(pred$cv_mort_1yr)) return(1e6)

        # Weighted squared error
        3 * (pred$cv_mort_1yr - t_1yr_cv_mort)^2 +
        2 * (pred$surv_5yr - t_5yr_surv)^2 +
        1 * (pred$mace_1yr - t_1yr_mace)^2
      }

      # Starting values from current model
      start <- c(tp_ref$p_MI, tp_ref$p_SCD, tp_ref$p_HF_Death, tp_ref$p_BG_Mort)

      withProgress(message = 'Optimizing (Nelder-Mead)...', value = 0.3, {
        result <- optim(start, objective, method = "Nelder-Mead",
                        control = list(maxit = 500, reltol = 1e-8))
        incProgress(0.7)
      })

      opt <- result$par
      pred <- run_cohort_trace(opt[1], opt[2], opt[3], opt[4])

      list(
        params = list(p_MI = opt[1], p_SCD = opt[2], p_HF_Death = opt[3],
                      p_BG_Mort = opt[4], cf_MI = sp$cf_MI, cf_Stroke = sp$cf_Stroke),
        predictions = pred,
        score = result$value,
        convergence = result$convergence,
        targets = list(cv_mort_1yr = t_1yr_cv_mort, mace_1yr = t_1yr_mace, surv_5yr = t_5yr_surv)
      )
    })

    # Display results
    output$calib_status <- renderUI({
      req(calib_results())
      cr <- calib_results()
      div(class = "alert alert-success",
          icon("check-circle"),
          sprintf(" Calibration complete. Best fit score: %.6f", cr$score))
    })

    output$calib_params_table <- renderTable({
      req(calib_results())
      p <- calib_results()$params
      tp <- tp_base()
      data.frame(
        Parameter = c("Stable → MI (p_MI)", "Sudden CV Death (p_SCD)",
                      "HF Annual Mortality (p_HF_Death)", "Background Non-CV (p_BG_Mort)",
                      "Acute MI Case Fatality (cf_MI)", "Acute Stroke Case Fatality (cf_Stroke)"),
        Previous = c(tp$p_MI, tp$p_SCD, tp$p_HF_Death, tp$p_BG_Mort, tp$cf_MI, tp$cf_Stroke),
        Calibrated = c(p$p_MI, p$p_SCD, p$p_HF_Death, p$p_BG_Mort, p$cf_MI, p$cf_Stroke),
        Change = c(
          paste0(ifelse(p$p_MI > tp$p_MI, "+", ""), round((p$p_MI/tp$p_MI - 1)*100, 0), "%"),
          paste0(ifelse(p$p_SCD > tp$p_SCD, "+", ""), round((p$p_SCD/tp$p_SCD - 1)*100, 0), "%"),
          paste0(ifelse(p$p_HF_Death > tp$p_HF_Death, "+", ""), round((p$p_HF_Death/tp$p_HF_Death - 1)*100, 0), "%"),
          paste0(ifelse(p$p_BG_Mort > tp$p_BG_Mort, "+", ""), round((p$p_BG_Mort/tp$p_BG_Mort - 1)*100, 0), "%"),
          paste0(ifelse(p$cf_MI > tp$cf_MI, "+", ""), round((p$cf_MI/tp$cf_MI - 1)*100, 0), "%"),
          paste0(ifelse(p$cf_Stroke > tp$cf_Stroke, "+", ""), round((p$cf_Stroke/tp$cf_Stroke - 1)*100, 0), "%")
        )
      )
    }, digits = 4)

    output$calib_fit_table <- renderTable({
      req(calib_results())
      cr <- calib_results()
      data.frame(
        Outcome = c("1-Year CV Mortality", "1-Year MACE", "5-Year Survival"),
        Target = c(
          paste0(round(cr$targets$cv_mort_1yr * 100, 1), "%"),
          paste0(round(cr$targets$mace_1yr * 100, 1), "%"),
          paste0(round(cr$targets$surv_5yr * 100, 1), "%")
        ),
        Model = c(
          paste0(round(cr$predictions$cv_mort_1yr * 100, 1), "%"),
          paste0(round(cr$predictions$mace_1yr * 100, 1), "%"),
          paste0(round(cr$predictions$surv_5yr * 100, 1), "%")
        ),
        Gap = c(
          paste0(round((cr$predictions$cv_mort_1yr - cr$targets$cv_mort_1yr) * 100, 1), " pp"),
          paste0(round((cr$predictions$mace_1yr - cr$targets$mace_1yr) * 100, 1), " pp"),
          paste0(round((cr$predictions$surv_5yr - cr$targets$surv_5yr) * 100, 1), " pp")
        )
      )
    })

    output$calib_fit_status <- renderUI({
      req(calib_results())
      cr <- calib_results()
      gaps <- c(
        abs(cr$predictions$cv_mort_1yr - cr$targets$cv_mort_1yr),
        abs(cr$predictions$mace_1yr - cr$targets$mace_1yr),
        abs(cr$predictions$surv_5yr - cr$targets$surv_5yr)
      )
      max_gap <- max(gaps) * 100

      if (max_gap < 2) {
        div(class = "alert alert-success", icon("check"), " Excellent fit: all gaps < 2 percentage points")
      } else if (max_gap < 5) {
        div(class = "alert alert-info", icon("info-circle"), " Good fit: max gap < 5 percentage points")
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"),
            sprintf(" Moderate fit: max gap = %.1f pp. Consider adjusting targets or grid resolution.", max_gap))
      }
    })

    # Return calibrated parameters for application
    return(list(
      calibrated_params = reactive({
        req(calib_results())
        calib_results()$params
      }),
      apply_trigger = reactive({ input$apply_calib })
    ))
  })
}
