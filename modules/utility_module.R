# modules/utility_module.R

utilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "1. Chronic Health State Utilities (0 to 1)", width = 12, status = "primary", solidHeader = TRUE,
          helpText("1.0 = Perfect Health, 0.0 = Death. Enter the long-term utility for each state."),
          helpText(tags$em("Defaults use Indian EQ-5D-5L population norms (DEVINE Study, J Global Health 2023). Mean age 50-59 male = 0.814.")),
          column(3, numericInput(ns("u_stable"), "Stable Post-MI", 0.814, min = 0, max = 1, step = 0.01)),
          column(3, numericInput(ns("u_mi"), "Chronic MI (Recurrent)", 0.82, min = 0, max = 1, step = 0.01)),
          column(3, numericInput(ns("u_stroke"), "Chronic Stroke", 0.65, min = 0, max = 1, step = 0.01)),
          column(3, numericInput(ns("u_hf"), "Heart Failure", 0.55, min = 0, max = 1, step = 0.01))
      )
    ),
    
    fluidRow(
      box(title = "2. Acute Event Disutilities (Temporary Hit)", width = 12, status = "warning", solidHeader = TRUE,
          helpText("The 'hit' to utility during the Acute Year tunnel."),
          column(6, sliderInput(ns("du_mi"), "Acute MI Disutility (0 to 1)", 0, 0.5, 0.15)),
          column(6, sliderInput(ns("du_stroke"), "Acute Stroke Disutility (0 to 1)", 0, 0.5, 0.25)),
          footer = "Acute Utility = (Stable Utility - Disutility)"
      )
    ),
    
    fluidRow(
      box(title = "3. Age-Related Utility Decay", width = 12, status = "info", solidHeader = TRUE,
          column(6, checkboxInput(ns("use_decay"), "Apply Annual Utility Decay", TRUE)),
          column(6, conditionalPanel(
            condition = paste0("input['", ns("use_decay"), "'] == true"),
            numericInput(ns("decay_rate"), "Annual Decay Factor (%)", 0.1, step = 0.05),
            helpText("Reduces utility of all states by this % every year to account for aging.")
          ))
      )
    ),
    
    fluidRow(
      box(title = "4. Final Utility Ledger (Calculated Inputs)", width = 12, status = "success",
          tableOutput(ns("utility_summary")),
          footer = "These values will be used by the engine to calculate lifetime QALYs.")
    )
  )
}

utilityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate Acute Utilities
    u_acute_mi <- reactive({ input$u_stable - input$du_mi })
    u_acute_stroke <- reactive({ input$u_stable - input$du_stroke })
    
    # Generate Summary Table
    output$utility_summary <- renderTable({
      data.frame(
        State = c("Stable", "Acute MI (Year 1)", "Acute Stroke (Year 1)", "Chronic MI", "Chronic Stroke", "Heart Failure"),
        Utility_Weight = c(
          input$u_stable, 
          u_acute_mi(), 
          u_acute_stroke(), 
          input$u_mi, 
          input$u_stroke, 
          input$u_hf
        ),
        Decay_Applied = if(input$use_decay) paste0(input$decay_rate, "% per year") else "None"
      )
    }, digits = 3)
    
    # Return values for the Engine
    return(reactive({
      list(
        u_stable = input$u_stable,
        u_acute_mi = u_acute_mi(),
        u_acute_stroke = u_acute_stroke(),
        u_mi = input$u_mi,
        u_stroke = input$u_stroke,
        u_hf = input$u_hf,
        use_decay = input$use_decay,
        decay_rate = input$decay_rate / 100
      )
    }))
  })
}