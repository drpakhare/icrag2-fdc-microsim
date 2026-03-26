# modules/model_diagram_module.R
library(DiagrammeR)

modelDiagramUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "CVD Markov State-Transition Diagram (CV Mortality Focus)", 
          width = 12, status = "primary", solidHeader = TRUE,
          helpText("Red edges represent Cardiovascular Death pathways. Dashed blue represents direct Heart Failure progression."),
          grVizOutput(ns("markov_plot"), height = "600px")
      )
    ),
    fluidRow(
      box(title = "Clinical Decision Logic", width = 6, status = "info",
          tags$ul(
            tags$li(strong("Acute Mortality:"), " Direct transitions from Acute MI/Stroke to CV Death are captured."),
            tags$li(strong("Chronic Progression:"), " Ongoing CV death risk is modeled for all post-event states."),
            tags$li(strong("Non-CV Mortality:"), " Background risk (gray edges) is based on age-specific Indian life tables.")
          )),
      box(title = "HTA India Standards", width = 6, status = "success",
          p(strong("Discounting:"), " 3% on costs and QALYs."),
          p(strong("Half-Cycle Correction:"), " Trapezoidal adjustment applied in engine.R."),
          p(strong("Microsimulation:"), " N=1,000 to 10,000 patients depending on UI settings.")
      )
    )
  )
}

modelDiagramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$markov_plot <- renderGrViz({
      grViz("
        digraph markov_model {
          graph [rankdir = LR, nodesep = 0.4, ranksep = 0.8, fontname = Helvetica]
          
          # Node Styles
          node [shape = box, style = 'filled, rounded', fontname = Helvetica, color = '#2c3e50', fillcolor = '#ecf0f1']
          Stable [label = 'Stable\\n(Baseline)']
          
          node [fillcolor = '#fff4e6', color = '#e67e22']
          AcuteMI [label = 'Acute MI\\n(Tunnel)']
          AcuteStr [label = 'Acute Stroke\\n(Tunnel)']
          
          node [fillcolor = '#e1f5fe', color = '#0288d1']
          ChronicMI [label = 'Chronic MI']
          ChronicStr [label = 'Chronic Stroke']
          HF [label = 'Heart Failure']
          
          # Separated Death States
          node [shape = doublecircle, fontname = Helvetica]
          DeathCV [label = 'Death\\n(CV Cause)', fillcolor = '#f9ebea', color = '#c0392b']
          DeathNonCV [label = 'Death\\n(Non-CV)', fillcolor = '#f4f6f7', color = '#7f8c8d']

          # 1. Baseline Transitions
          Stable -> Stable
          Stable -> AcuteMI
          Stable -> AcuteStr
          Stable -> HF [color = '#2980b9', style = dashed]
          Stable -> DeathCV [color = '#c0392b', penwidth = 2.0]
          Stable -> DeathNonCV [color = '#95a5a6']
          
          # 2. Acute Tunnel Transitions
          AcuteMI -> ChronicMI
          AcuteMI -> DeathCV [color = '#c0392b', penwidth = 2.0]
          
          AcuteStr -> ChronicStr
          AcuteStr -> DeathCV [color = '#c0392b', penwidth = 2.0]
          
          # 3. Post-Event/Chronic Transitions
          ChronicMI -> ChronicMI
          ChronicMI -> AcuteStr
          ChronicMI -> HF
          ChronicMI -> DeathCV [color = '#c0392b', penwidth = 2.0]
          ChronicMI -> DeathNonCV [color = '#95a5a6']
          
          ChronicStr -> ChronicStr
          ChronicStr -> AcuteMI
          ChronicStr -> HF
          ChronicStr -> DeathCV [color = '#c0392b', penwidth = 2.0]
          ChronicStr -> DeathNonCV [color = '#95a5a6']
          
          # 4. End-Stage HF
          HF -> HF
          HF -> DeathCV [color = '#c0392b', penwidth = 2.0]
          HF -> DeathNonCV [color = '#95a5a6']
          
          # Visual Tweak: Aligning States
          {rank = same; AcuteMI; AcuteStr}
          {rank = same; ChronicMI; ChronicStr}
          {rank = max; DeathCV; DeathNonCV}
        }
      ")
    })
  })
}