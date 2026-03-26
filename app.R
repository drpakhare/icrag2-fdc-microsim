library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)

# Source all project components
source("modules/transition_module.R")
source("modules/costing_module.R")
source("modules/utility_module.R")
source("modules/patient_profiles_module.R")
source("modules/dsa_module.R")
source("modules/psa_module.R")
source("modules/validation_module.R")
source("modules/calibration_module.R")
source("analytics_module.R")
source("engine_v2.R")
source("modules/model_diagram.R")
source("modules/methodology_module.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "FDC HTA: ICRAG-2 v2"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("0. Model Structure", tabName = "tab_diagram", icon = icon("project-diagram")),
      menuItem("1. Patient Profiles", tabName = "tab_profiles", icon = icon("users")),
      menuItem("2. Drug Strategy", tabName = "tab_costs", icon = icon("pills")),
      menuItem("3. Clinical Transitions", tabName = "tab_trans", icon = icon("heartbeat")),
      menuItem("4. QALY Utilities", tabName = "tab_utils", icon = icon("walking")),
      menuItem("5. Base Case Results", tabName = "tab_run", icon = icon("play-circle")),
      menuItem("6. Survival & Traces", tabName = "tab_clin", icon = icon("chart-line")),
      menuItem("7. DSA (Tornado)", tabName = "tab_dsa", icon = icon("wind")),
      menuItem("8. PSA (Probabilistic)", tabName = "tab_psa", icon = icon("dice")),
      menuItem("9. Calibration", tabName = "tab_calib", icon = icon("crosshairs")),
      menuItem("10. Validation Suite", tabName = "tab_valid", icon = icon("vial")),
      menuItem("11. Scenario Logger", tabName = "tab_log", icon = icon("database")),
      menuItem("12. Export Data", tabName = "tab_export", icon = icon("download")),
      menuItem("13. Study Methodology", tabName = "tab_meth", icon = icon("book-reader"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_diagram", modelDiagramUI("mod_diag")),
      tabItem(tabName = "tab_profiles", patientProfilesUI("mod_profiles")),
      tabItem(tabName = "tab_costs", costingUI("mod_costs")),
      tabItem(tabName = "tab_trans", transitionUI("mod_trans")),
      tabItem(tabName = "tab_utils", utilityUI("mod_utils")),
      tabItem(tabName = "tab_dsa",   dsaUI("mod_dsa")),
      tabItem(tabName = "tab_psa",   psaUI("mod_psa")),
      tabItem(tabName = "tab_calib", calibrationUI("mod_calib")),
      tabItem(tabName = "tab_valid", validationUI("mod_valid")),
      tabItem(tabName = "tab_meth", methodologyUI("mod_meth")),

      tabItem(tabName = "tab_run",
              fluidRow(
                box(title = "Simulation Control", width = 3, status = "primary", solidHeader = TRUE,
                    numericInput("sim_n", "Cohort Size", 10000, min = 100),
                    numericInput("sim_h", "Horizon (Years)", 30),
                    hr(),
                    numericInput("disc_cost", "Discount Rate: Costs (%)", 3.0),
                    numericInput("disc_util", "Discount Rate: Utilities (%)", 3.0),
                    numericInput("wtp", "WTP Threshold (₹)", 236000),
                    helpText("Default: 1× India GDP/capita (₹2,34,859 for FY2024-25, MOSPI)"),
                    hr(),
                    h4("V2 Engine Options"),
                    checkboxInput("use_heterogeneity", "Heterogeneous Patients", TRUE),
                    checkboxInput("use_adherence_decay", "FDC Adherence Decay", TRUE),
                    checkboxInput("use_age_mortality", "Age-Dependent Mortality", TRUE),
                    checkboxInput("use_age_utilities", "Age-Sex EQ-5D Norms", TRUE),
                    hr(),
                    actionButton("calc", "Run Base Case Analysis", class = "btn-success btn-lg", width = "100%")
                ),
                column(9,
                       uiOutput("box_icer"),
                       box(title = "Economic Summary", width = 12, tableOutput("tab_results")),
                       box(title = "Clinical Event Summary (V2)", width = 12, status = "info",
                           tableOutput("tab_events"))
                )
              )
      ),

      tabItem(tabName = "tab_clin",
              fluidRow(box(title = "Survival Probability", width = 12, plotlyOutput("plot_surv")),
                       box(title = "Markov Trace", width = 12, plotlyOutput("plot_trace")))
      ),

      tabItem(tabName = "tab_log",
              fluidRow(
                box(title = "Scenario History Audit Trail", width = 12, status = "info",
                    helpText("Logs clinical risks, component costs, perspective, and results for full reproducibility."),
                    DTOutput("log_dt"),
                    hr(),
                    div(style = "display: inline-block;",
                        actionButton("save_scenario", "Save Current Result", class = "btn-info btn-lg", icon = icon("save"))
                    ),
                    div(style = "display: inline-block; margin-left: 10px;",
                        actionButton("clear_log", "Clear History", class = "btn-danger btn-lg", icon = icon("trash-alt"))
                    )
                )
              )
      ),

      tabItem(tabName = "tab_export",
              fluidRow(box(title = "Download Analysis", width = 12, status = "warning",
                           helpText("Exports the Scenario Log as a structured CSV for external reporting."),
                           downloadButton("download_csv", "Download CSV Log", class = "btn-warning btn-lg")))
      )
    )
  )
)

server <- function(input, output, session) {
  modelDiagramServer("mod_diag")
  profiles_mod <- patientProfilesServer("mod_profiles")
  co_rx <- costingServer("mod_costs")
  tp_rx <- transitionServer("mod_trans", hr_rx = co_rx)
  ut_rx <- utilityServer("mod_utils")

  d_c <- reactive({ input$disc_cost / 100 })
  d_u <- reactive({ input$disc_util / 100 })
  horizon_rx <- reactive({ input$sim_h })

  dsaServer("mod_dsa", tp_rx, co_rx, ut_rx, n_pat = 1000, horizon = input$sim_h)
  psa_mod <- psaServer("mod_psa", tp_rx, co_rx, ut_rx, n_pat = 1000, horizon = input$sim_h, disc_cost = d_c, disc_util = d_u)
  calib_mod <- calibrationServer("mod_calib", tp_rx, co_rx, ut_rx)
  methodologyServer("mod_meth")

  # Apply calibrated parameters to transition module inputs
  observeEvent(calib_mod$apply_trigger(), {
    cp <- calib_mod$calibrated_params()
    if (!is.null(cp)) {
      updateNumericInput(session, "mod_trans-p_MI", value = cp$p_MI)
      updateNumericInput(session, "mod_trans-p_SCD", value = cp$p_SCD)
      updateNumericInput(session, "mod_trans-p_HF_Death", value = cp$p_HF_Death)
      updateNumericInput(session, "mod_trans-p_BG_Mort", value = cp$p_BG_Mort)
      updateNumericInput(session, "mod_trans-cf_MI", value = cp$cf_MI)
      updateNumericInput(session, "mod_trans-cf_Stroke", value = cp$cf_Stroke)
      showNotification("Calibrated parameters applied to Clinical Transitions tab!", type = "message", duration = 5)
    }
  })

  # PSA data reactive for validation convergence plot
  psa_data_for_valid <- reactive({
    if (!is.null(psa_mod) && !is.null(psa_mod$data_rx)) {
      tryCatch(psa_mod$data_rx(), error = function(e) NULL)
    } else NULL
  })

  validationServer("mod_valid", tp_rx, co_rx, ut_rx, horizon_rx, psa_data_for_valid)

  # --- COMPREHENSIVE AUDIT LOG INITIALIZATION ---
  scenario_history <- reactiveVal(data.frame(
    Timestamp    = character(),
    Scenario     = character(),
    Perspective  = character(),
    p_MI         = numeric(),
    p_Stroke     = numeric(),
    hr_MI        = numeric(),
    hr_Stroke    = numeric(),
    C_Asp        = numeric(),
    C_Clopi      = numeric(),
    C_Atorva     = numeric(),
    C_Rosuva     = numeric(),
    C_Telmi      = numeric(),
    C_Ramipril   = numeric(),
    C_Losartan   = numeric(),
    C_Amlod      = numeric(),
    C_Cilni      = numeric(),
    C_HCTZ       = numeric(),
    C_FDC_Daily  = numeric(),
    Incr_Cost    = numeric(),
    Incr_QALY    = numeric(),
    ICER         = character(),
    NMB          = numeric(),
    stringsAsFactors = FALSE
  ))

  # === BASE CASE: V2 ENGINE ===
  res_base <- eventReactive(input$calc, {
    # Generate patient cohort using profiles module
    cohort <- NULL
    if (input$use_heterogeneity) {
      cohort <- profiles_mod$generate(input$sim_n, seed = 42)
    }

    run_microsim_engine_v2(
      tp_rx(), co_rx(), ut_rx(),
      n_pat = input$sim_n,
      horizon = input$sim_h,
      disc_cost = d_c(),
      disc_util = d_u(),
      seed = 42,
      patient_profiles = cohort,
      use_heterogeneity = input$use_heterogeneity,
      use_adherence_decay = input$use_adherence_decay,
      use_age_mortality = input$use_age_mortality,
      use_age_utilities = input$use_age_utilities
    )
  })

  output$box_icer <- renderUI({ req(res_base()); render_icer_box(res_base(), input$wtp) })
  output$tab_results <- renderTable({
    req(res_base())
    df <- render_economic_summary(res_base(), input$wtp)
    return(df)
  },
  sanitize.text.function = function(x) x,
  digits = 0)

  # V2 event summary
  output$tab_events <- renderTable({
    req(res_base())
    res <- res_base()
    data.frame(
      Metric = c("Mean MI Events / Patient", "Mean Stroke Events / Patient",
                 "Adherence Decay", "Age-Dependent Mortality",
                 "Heterogeneous Profiles", "Age-Sex EQ-5D Norms"),
      SoC = c(
        round(res$SoC$mean_mi_events, 3),
        round(res$SoC$mean_stroke_events, 3),
        "N/A",
        ifelse(input$use_age_mortality, "Enabled", "Disabled"),
        ifelse(input$use_heterogeneity, "Enabled", "Disabled"),
        ifelse(input$use_age_utilities, "Enabled", "Disabled")
      ),
      FDC = c(
        round(res$FDC$mean_mi_events, 3),
        round(res$FDC$mean_stroke_events, 3),
        ifelse(input$use_adherence_decay, "Enabled", "Disabled"),
        ifelse(input$use_age_mortality, "Enabled", "Disabled"),
        ifelse(input$use_heterogeneity, "Enabled", "Disabled"),
        ifelse(input$use_age_utilities, "Enabled", "Disabled")
      )
    )
  })

  output$plot_surv <- renderPlotly({ req(res_base()); render_survival_plot(res_base(), input$sim_h) })
  output$plot_trace <- renderPlotly({ req(res_base()); render_trace_plot(res_base(), input$sim_h) })

  # OBSERVE: SAVE COMPREHENSIVE SCENARIO
  observeEvent(input$save_scenario, {
    req(res_base())
    req(tp_rx())
    req(co_rx())

    res     <- res_base()
    tp_data <- tp_rx()
    co_data <- co_rx()

    dC <- res$FDC$avg_cost - res$SoC$avg_cost
    dE <- res$FDC$avg_qaly - res$SoC$avg_qaly

    icer_disp <- if(dC < 0 & dE > 0) {
      "FDC Dominates"
    } else if(dC > 0 & dE < 0) {
      "FDC Dominated"
    } else if(dC < 0 & dE < 0) {
      paste0("Trade-off (", round(dC/dE, 0), ")")
    } else if(is.null(dE) || abs(dE) < 0.0001) {
      "Insignificant dE"
    } else {
      as.character(round(dC/dE, 0))
    }

    # Determine perspective label
    persp_label <- switch(
      if(!is.null(co_data$perspective)) co_data$perspective else "govt",
      "govt" = "Govt Rate Contract",
      "ja" = "Jan Aushadhi",
      "hs" = "Health System",
      "private" = "Private Branded",
      "Govt Rate Contract"
    )
    if (!is.null(co_data$use_societal) && co_data$use_societal) {
      persp_label <- paste0(persp_label, " + Societal")
    }

    new_entry <- data.frame(
      Timestamp    = format(Sys.time(), "%H:%M:%S"),
      Scenario     = if(!is.null(co_data$label)) co_data$label else "Default",
      Perspective  = persp_label,
      p_MI         = if(!is.null(tp_data$p_MI)) tp_data$p_MI else 0,
      p_Stroke     = if(!is.null(tp_data$p_Stroke)) tp_data$p_Stroke else 0,
      hr_MI        = if(!is.null(tp_data$hr_MI)) tp_data$hr_MI else 1,
      hr_Stroke    = if(!is.null(tp_data$hr_Stroke)) tp_data$hr_Stroke else 1,
      C_Asp        = if(!is.null(co_data$c_asp)) co_data$c_asp else 0,
      C_Clopi      = if(!is.null(co_data$c_clopi)) co_data$c_clopi else 0,
      C_Atorva     = if(!is.null(co_data$c_atorva)) co_data$c_atorva else 0,
      C_Rosuva     = if(!is.null(co_data$c_rosuvastatin)) co_data$c_rosuvastatin else 0,
      C_Telmi      = if(!is.null(co_data$c_telmi)) co_data$c_telmi else 0,
      C_Ramipril   = if(!is.null(co_data$c_ramipril)) co_data$c_ramipril else 0,
      C_Losartan   = if(!is.null(co_data$c_losartan)) co_data$c_losartan else 0,
      C_Amlod      = if(!is.null(co_data$c_amlod)) co_data$c_amlod else 0,
      C_Cilni      = if(!is.null(co_data$c_cilnidipine)) co_data$c_cilnidipine else 0,
      C_HCTZ       = if(!is.null(co_data$c_hctz)) co_data$c_hctz else 0,
      C_FDC_Daily  = if(!is.null(co_data$fdc_annual)) (co_data$fdc_annual / 365.25) else 0,
      Incr_Cost    = round(dC, 0),
      Incr_QALY    = round(dE, 4),
      ICER         = icer_disp,
      NMB          = round((dE * input$wtp) - dC, 0),
      stringsAsFactors = FALSE
    )

    current_history <- scenario_history()
    scenario_history(rbind(current_history, new_entry))

    showNotification(paste("Scenario Saved:", new_entry$Scenario), type = "message")
  })

  observeEvent(input$clear_log, {
    scenario_history(scenario_history()[0, ])
    showNotification("Scenario history cleared.", type = "warning")
  })

  output$log_dt <- renderDT({
    datatable(scenario_history(),
              options = list(pageLength = 5, scrollX = TRUE, dom = 'ftp'),
              selection = 'none', rownames = FALSE) %>%
      formatCurrency(columns = c('C_Asp', 'C_Clopi', 'C_Atorva', 'C_Rosuva', 'C_Telmi',
                                 'C_Ramipril', 'C_Losartan', 'C_Amlod', 'C_Cilni',
                                 'C_HCTZ', 'C_FDC_Daily', 'Incr_Cost', 'NMB'), currency = "₹")
  })

  output$download_csv <- downloadHandler(
    filename = function() { paste("HTA_Audit_Log_", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(scenario_history(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
