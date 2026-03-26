# modules/patient_profiles_module.R
# Heterogeneous patient profile generator using Indian epidemiological data

patientProfilesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = tagList(icon("users"), "1. Cohort Demographics (Indian Population Data)"),
          width = 6, status = "primary", solidHeader = TRUE,
          column(6,
                 h4("Age Distribution"),
                 numericInput(ns("mean_age"), "Mean Age (years)", 58, min = 30, max = 80),
                 numericInput(ns("sd_age"), "Age SD", 12, min = 3, max = 20),
                 numericInput(ns("min_age"), "Min Age", 30),
                 numericInput(ns("max_age"), "Max Age", 85)
          ),
          column(6,
                 h4("Sex Distribution"),
                 sliderInput(ns("pct_male"), "% Male", 0, 100, 79),
                 helpText("CREATE Registry: 79% male; Kerala ACS: 77%")
          )
      ),

      box(title = tagList(icon("disease"), "2. Risk Factor Prevalence"),
          width = 6, status = "warning", solidHeader = TRUE,
          helpText("Source: CREATE Registry (prevalence in Indian ACS patients)"),
          sliderInput(ns("prev_dm"), "Diabetes Mellitus (%)", 0, 100, 31),
          sliderInput(ns("prev_htn"), "Hypertension (%)", 0, 100, 43),
          sliderInput(ns("prev_smoke"), "Current Smoking (%)", 0, 100, 40)
      )
    ),

    fluidRow(
      box(title = tagList(icon("scale-balanced"), "3. Risk Multipliers (Recurrent Event HRs)"),
          width = 6, status = "info", solidHeader = TRUE,
          helpText("HRs for RECURRENT events in secondary prevention populations.",
                   "Sources: REACH Registry (n=49,689; PMID 22727237),",
                   "Recurrent Stroke Meta-analysis (PMID 34153594),",
                   "JAHA 2022 Post-ACS Cohort (n=239,234).",
                   "Note: These are attenuated vs primary prevention (INTERHEART) ORs",
                   "because baseline risk is already elevated post-index event."),
          column(6,
                 h4("Recurrent MI Multipliers"),
                 numericInput(ns("rr_dm_mi"), "Diabetes → recurrent MI (HR)", 1.44, step = 0.05),
                 numericInput(ns("rr_htn_mi"), "Hypertension → recurrent MI (HR)", 1.21, step = 0.05),
                 numericInput(ns("rr_smoke_mi"), "Smoking → recurrent MI (HR)", 1.63, step = 0.05)
          ),
          column(6,
                 h4("Recurrent Stroke Multipliers"),
                 numericInput(ns("rr_dm_stroke"), "Diabetes → recurrent Stroke (HR)", 1.85, step = 0.05),
                 numericInput(ns("rr_htn_stroke"), "Hypertension → recurrent Stroke (HR)", 1.27, step = 0.05),
                 numericInput(ns("rr_smoke_stroke"), "Smoking → recurrent Stroke (HR)", 1.52, step = 0.05)
          )
      ),

      box(title = tagList(icon("table"), "4. EQ-5D-5L Population Norms (DEVINE Study)"),
          width = 6, status = "success", solidHeader = TRUE,
          helpText("Indian age-sex-specific utility norms (J Global Health 2023)"),
          tableOutput(ns("eq5d_norms_table")),
          footer = "Applied as baseline utility multiplier per patient age-sex profile"
      )
    ),

    fluidRow(
      box(title = tagList(icon("chart-pie"), "5. Generated Cohort Preview"),
          width = 12, status = "info", solidHeader = TRUE,
          actionButton(ns("preview_cohort"), "Generate Cohort Preview (100 patients)",
                       class = "btn-info", icon = icon("eye")),
          br(), br(),
          column(6, plotlyOutput(ns("age_dist_plot"), height = "300px")),
          column(6, tableOutput(ns("cohort_summary")))
      )
    )
  )
}

patientProfilesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Indian EQ-5D-5L population norms by age-sex (DEVINE / J Global Health 2023)
    eq5d_norms <- data.frame(
      age_lower = c(0, 20, 30, 40, 50, 60, 70),
      age_upper = c(19, 29, 39, 49, 59, 69, 99),
      male   = c(0.936, 0.920, 0.882, 0.833, 0.814, 0.780, 0.643),
      female = c(0.922, 0.908, 0.858, 0.831, 0.766, 0.669, 0.488)
    )

    # Look up utility for a given age and sex
    get_eq5d_norm <- function(age, is_male) {
      idx <- which(eq5d_norms$age_lower <= age & eq5d_norms$age_upper >= age)
      if (length(idx) == 0) idx <- nrow(eq5d_norms)
      if (is_male) eq5d_norms$male[idx] else eq5d_norms$female[idx]
    }

    # Display norms table
    output$eq5d_norms_table <- renderTable({
      data.frame(
        `Age Band` = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
        Male = eq5d_norms$male,
        Female = eq5d_norms$female,
        check.names = FALSE
      )
    }, digits = 3)

    # Generate a patient cohort
    generate_cohort <- function(n, seed = NULL) {
      if (!is.null(seed)) set.seed(seed)

      ages <- pmin(input$max_age, pmax(input$min_age,
                   round(rnorm(n, input$mean_age, input$sd_age))))
      is_male <- runif(n) < (input$pct_male / 100)
      has_dm <- runif(n) < (input$prev_dm / 100)
      has_htn <- runif(n) < (input$prev_htn / 100)
      has_smoke <- runif(n) < (input$prev_smoke / 100)

      # Compute per-patient risk multipliers (multiplicative)
      rr_mi <- rep(1, n)
      rr_stroke <- rep(1, n)

      rr_mi[has_dm] <- rr_mi[has_dm] * input$rr_dm_mi
      rr_mi[has_htn] <- rr_mi[has_htn] * input$rr_htn_mi
      rr_mi[has_smoke] <- rr_mi[has_smoke] * input$rr_smoke_mi

      rr_stroke[has_dm] <- rr_stroke[has_dm] * input$rr_dm_stroke
      rr_stroke[has_htn] <- rr_stroke[has_htn] * input$rr_htn_stroke
      rr_stroke[has_smoke] <- rr_stroke[has_smoke] * input$rr_smoke_stroke

      # Cap extreme multipliers (prevent unrealistic probabilities)
      rr_mi <- pmin(rr_mi, 10)
      rr_stroke <- pmin(rr_stroke, 10)

      # Baseline EQ-5D utility per patient
      base_utility <- mapply(get_eq5d_norm, ages, is_male)

      data.frame(
        id = 1:n,
        age = ages,
        is_male = is_male,
        has_dm = has_dm,
        has_htn = has_htn,
        has_smoke = has_smoke,
        rr_mi = rr_mi,
        rr_stroke = rr_stroke,
        base_utility = base_utility
      )
    }

    # Preview cohort
    preview_data <- eventReactive(input$preview_cohort, {
      generate_cohort(100, seed = 999)
    })

    output$age_dist_plot <- renderPlotly({
      req(preview_data())
      df <- preview_data()
      df$sex <- ifelse(df$is_male, "Male", "Female")

      plot_ly(df, x = ~age, color = ~sex, type = "histogram",
              barmode = "overlay", opacity = 0.7,
              colors = c("Male" = "#3498db", "Female" = "#e74c3c")) %>%
        layout(title = "Age-Sex Distribution",
               xaxis = list(title = "Age"), yaxis = list(title = "Count"))
    })

    output$cohort_summary <- renderTable({
      req(preview_data())
      df <- preview_data()
      data.frame(
        Characteristic = c("Mean Age", "% Male", "% Diabetes", "% Hypertension",
                          "% Smoking", "Mean RR(MI)", "Mean RR(Stroke)", "Mean Base Utility"),
        Value = c(
          round(mean(df$age), 1),
          paste0(round(mean(df$is_male) * 100, 1), "%"),
          paste0(round(mean(df$has_dm) * 100, 1), "%"),
          paste0(round(mean(df$has_htn) * 100, 1), "%"),
          paste0(round(mean(df$has_smoke) * 100, 1), "%"),
          round(mean(df$rr_mi), 2),
          round(mean(df$rr_stroke), 2),
          round(mean(df$base_utility), 3)
        )
      )
    })

    # Return reactive generator function and EQ-5D lookup
    return(list(
      generate = function(n, seed = NULL) generate_cohort(n, seed),
      eq5d_norms = eq5d_norms,
      get_eq5d_norm = get_eq5d_norm,
      # Return current settings as a list for the engine
      settings = reactive({
        list(
          mean_age = input$mean_age, sd_age = input$sd_age,
          min_age = input$min_age, max_age = input$max_age,
          pct_male = input$pct_male / 100,
          prev_dm = input$prev_dm / 100, prev_htn = input$prev_htn / 100,
          prev_smoke = input$prev_smoke / 100,
          rr_dm_mi = input$rr_dm_mi, rr_htn_mi = input$rr_htn_mi, rr_smoke_mi = input$rr_smoke_mi,
          rr_dm_stroke = input$rr_dm_stroke, rr_htn_stroke = input$rr_htn_stroke, rr_smoke_stroke = input$rr_smoke_stroke
        )
      })
    ))
  })
}
