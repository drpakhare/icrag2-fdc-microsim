# modules/transition_module.R
# Clinical Transition Probabilities — March 2026
# HRs are now set by the Drug Strategy module (costing_module.R) and passed in via hr_rx.
# This module handles baseline risks, case fatalities, and matrix construction only.

transitionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = tagList(icon("heartbeat"), "1. Baseline Event Risks (SoC Arm)"),
          width = 6, status = "primary", solidHeader = TRUE,
          numericInput(ns("p_MI"), "Stable -> Recurrent MI (Year 1)", 0.0265, step = 0.005),
          numericInput(ns("p_MI_yr2"), "Stable -> Recurrent MI (Year 2+)", 0.012, step = 0.001),
          checkboxInput(ns("use_time_varying_mi"), "Use Time-Varying MI Rate", TRUE),
          numericInput(ns("p_Stroke"), "Stable -> Recurrent Stroke", 0.011, step = 0.001),
          numericInput(ns("p_HF"), "Stable -> Heart Failure", 0.023, step = 0.005),
          numericInput(ns("p_SCD"), "Sudden CV Death Risk", 0.008, step = 0.001)
      ),
      box(title = tagList(icon("skull-crossbones"), "2. Case Fatality & Background Mortality"),
          width = 6, status = "warning", solidHeader = TRUE,
          numericInput(ns("cf_MI"), "Case Fatality: Acute MI", 0.08),
          helpText("CREATE (Xavier 2008): STEMI 8.6%, NSTEMI 3.7%. Blended 8%."),
          numericInput(ns("cf_Stroke"), "Case Fatality: Acute Stroke", 0.20),
          helpText("Jones 2021: India range 18-42%. 20% for patients under care."),
          numericInput(ns("p_HF_Death"), "Annual HF Mortality", 0.181),
          helpText("ICC-NHFR (Jayagopal 2025): 18.1%, 17 centres nationally."),
          numericInput(ns("p_BG_Mort"), "Non-CV Background Mortality", 0.009),
          helpText("Kalliath 2021 (Kerala): 0.9% non-CVD death post-ACS.")
      )
    ),

    fluidRow(
      box(title = tagList(icon("shield-halved"), "3. FDC Hazard Ratios (set by Drug Strategy tab)"),
          width = 12, status = "success", solidHeader = TRUE,
          uiOutput(ns("hr_display")),
          helpText("HRs are controlled from the Drug Strategy tab (Tab 2). Change FDC category there to update.")
      )
    ),

    fluidRow(
      box(title = "Matrix Validation Status", width = 12, uiOutput(ns("risk_alert")))
    ),
    fluidRow(
      box(title = "4. Standard of Care (SoC) Matrix", width = 12, status = "info",
          tableOutput(ns("tp_matrix_soc"))),
      box(title = "5. FDC Strategy Matrix", width = 12, status = "success",
          tableOutput(ns("tp_matrix_fdc")))
    )
  )
}

transitionServer <- function(id, hr_rx = NULL) {
  moduleServer(id, function(input, output, session) {

    # Get HRs from the Drug Strategy module (costing module), or use defaults
    current_hrs <- reactive({
      if (!is.null(hr_rx)) {
        co <- tryCatch(hr_rx(), error = function(e) NULL)
        if (!is.null(co) && !is.null(co$hr_MI)) {
          return(list(
            hr_MI = co$hr_MI, hr_Stroke = co$hr_Stroke,
            hr_HF = co$hr_HF, hr_SCD = co$hr_SCD,
            tier = if(!is.null(co$evidence_tier)) co$evidence_tier else NA,
            source = if(!is.null(co$evidence_source)) co$evidence_source else "Unknown"
          ))
        }
      }
      # Defaults (SECURE) if no Drug Strategy module connected
      list(hr_MI = 0.70, hr_Stroke = 0.65, hr_HF = 0.80, hr_SCD = 0.80,
           tier = 1, source = "SECURE (default)")
    })

    output$hr_display <- renderUI({
      hrs <- current_hrs()
      tier_label <- if (!is.na(hrs$tier) && hrs$tier == 1) {
        span(class = "label label-success", "TIER 1")
      } else if (!is.na(hrs$tier) && hrs$tier == 2) {
        span(class = "label label-warning", "TIER 2")
      } else {
        span(class = "label label-default", "CUSTOM")
      }

      tagList(
        fluidRow(
          column(3, strong("HR MI: "), sprintf("%.2f", hrs$hr_MI)),
          column(3, strong("HR Stroke: "), sprintf("%.2f", hrs$hr_Stroke)),
          column(3, strong("HR HF: "), sprintf("%.2f", hrs$hr_HF)),
          column(3, strong("HR SCD: "), sprintf("%.2f", hrs$hr_SCD))
        ),
        div(style = "margin-top: 5px;",
            tier_label, " ", em(hrs$source))
      )
    })

    build_matrix <- function(p_mi, p_st, p_hf, p_scd) {
      states <- c("Stable", "Acute_MI", "Acute_Stroke", "Chronic_MI",
                   "Chronic_Stroke", "HF", "Death_CV", "Death_NonCV")
      m <- matrix(0, nrow = 8, ncol = 8, dimnames = list(states, states))

      exit_sum <- p_mi + p_st + p_hf + p_scd + input$p_BG_Mort
      if(exit_sum >= 1) return(NULL)

      m["Stable", "Acute_MI"]     <- p_mi
      m["Stable", "Acute_Stroke"] <- p_st
      m["Stable", "HF"]           <- p_hf
      m["Stable", "Death_CV"]     <- p_scd
      m["Stable", "Death_NonCV"]  <- input$p_BG_Mort
      m["Stable", "Stable"]       <- 1 - sum(m["Stable",])

      m["Acute_MI", "Death_CV"]   <- input$cf_MI
      m["Acute_MI", "Chronic_MI"] <- 1 - input$cf_MI

      m["Acute_Stroke", "Death_CV"]      <- input$cf_Stroke
      m["Acute_Stroke", "Chronic_Stroke"] <- 1 - input$cf_Stroke

      m["Chronic_MI", "Acute_MI"]     <- p_mi * 1.5
      m["Chronic_MI", "Acute_Stroke"] <- p_st
      m["Chronic_MI", "HF"]           <- p_hf * 2.0
      m["Chronic_MI", "Death_CV"]     <- p_scd * 1.3
      m["Chronic_MI", "Death_NonCV"]  <- input$p_BG_Mort
      m["Chronic_MI", "Chronic_MI"]   <- 1 - sum(m["Chronic_MI",])

      m["Chronic_Stroke", "Acute_MI"]     <- p_mi * 1.2
      m["Chronic_Stroke", "Acute_Stroke"] <- p_st * 1.5
      m["Chronic_Stroke", "HF"]           <- p_hf * 1.3
      m["Chronic_Stroke", "Death_CV"]     <- p_scd * 1.5
      m["Chronic_Stroke", "Death_NonCV"]  <- input$p_BG_Mort
      m["Chronic_Stroke", "Chronic_Stroke"] <- 1 - sum(m["Chronic_Stroke",])

      m["HF", "Acute_MI"]     <- p_mi * 0.5
      m["HF", "Acute_Stroke"] <- p_st * 0.75
      m["HF", "Death_CV"]     <- input$p_HF_Death
      m["HF", "Death_NonCV"]  <- input$p_BG_Mort
      m["HF", "HF"]           <- 1 - sum(m["HF",])

      m["Death_CV", "Death_CV"]       <- 1
      m["Death_NonCV", "Death_NonCV"] <- 1

      return(m)
    }

    soc_mat <- reactive({
      build_matrix(input$p_MI, input$p_Stroke, input$p_HF, input$p_SCD)
    })

    fdc_mat <- reactive({
      hrs <- current_hrs()
      build_matrix(
        input$p_MI * hrs$hr_MI,
        input$p_Stroke * hrs$hr_Stroke,
        input$p_HF * hrs$hr_HF,
        input$p_SCD * hrs$hr_SCD
      )
    })

    output$risk_alert <- renderUI({
      if (is.null(soc_mat())) {
        div(class = "alert alert-danger", icon("triangle-exclamation"),
            " Error: Total Risk Sum > 100%. Please reduce annual event probabilities.")
      } else {
        div(class = "alert alert-success", icon("check"), " Risks Validated: Sum <= 100%")
      }
    })

    output$tp_matrix_soc <- renderTable({ req(soc_mat()); soc_mat() }, rownames = TRUE, digits = 4)
    output$tp_matrix_fdc <- renderTable({ req(fdc_mat()); fdc_mat() }, rownames = TRUE, digits = 4)

    return(reactive({
      hrs <- current_hrs()
      list(
        soc = soc_mat(),
        fdc = fdc_mat(),
        p_MI = input$p_MI, p_Stroke = input$p_Stroke,
        p_HF = input$p_HF, p_SCD = input$p_SCD,
        p_MI_yr2 = input$p_MI_yr2,
        use_time_varying_mi = input$use_time_varying_mi,
        hr_MI = hrs$hr_MI, hr_Stroke = hrs$hr_Stroke,
        hr_HF = hrs$hr_HF, hr_SCD = hrs$hr_SCD,
        p_HF_Death = input$p_HF_Death, p_BG_Mort = input$p_BG_Mort,
        cf_MI = input$cf_MI, cf_Stroke = input$cf_Stroke
      )
    }))
  })
}
