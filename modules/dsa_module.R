# modules/dsa_module.R
# Deterministic Sensitivity Analysis — expanded parameter set (March 2026)

dsaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # --- LEFT PANEL: Parameter Selection ---
      box(title = tagList(icon("list-check"), "1. Select DSA Parameters"),
          width = 8, status = "primary", solidHeader = TRUE,
          tabsetPanel(
            id = ns("dsa_tabs"),
            tabPanel("Clinical Risks",
                     br(),
                     checkboxGroupInput(ns("dsa_trans"), "Transition Probabilities (SoC):",
                                        choices = list(
                                          "Stable → MI" = "p_MI",
                                          "Stable → Stroke" = "p_Stroke",
                                          "Stable → HF" = "p_HF",
                                          "Sudden CV Death" = "p_SCD",
                                          "Background Non-CV Mort" = "p_BG_Mort",
                                          "HF Annual Mortality" = "p_HF_Death",
                                          "Acute MI Case Fatality" = "cf_MI",
                                          "Acute Stroke Case Fatality" = "cf_Stroke"
                                        ), inline = TRUE)
            ),
            tabPanel("FDC Efficacy",
                     br(),
                     checkboxGroupInput(ns("dsa_eff"), "Hazard Ratios (FDC Strategy):",
                                        choices = list(
                                          "HR: MI" = "hr_MI",
                                          "HR: Stroke" = "hr_Stroke",
                                          "HR: HF" = "hr_HF",
                                          "HR: SCD" = "hr_SCD"
                                        ), inline = TRUE)
            ),
            tabPanel("Costs",
                     br(),
                     checkboxGroupInput(ns("dsa_costs"), "Cost Parameters:",
                                        choices = list(
                                          "FDC Daily Cost" = "c_fdc",
                                          "SoC Daily Cost" = "c_soc",
                                          "Acute MI Hospitalisation" = "c_acute_mi",
                                          "Acute Stroke Hospitalisation" = "c_acute_stroke",
                                          "Stable Maintenance" = "c_maint_stable",
                                          "Chronic MI Maintenance" = "c_maint_mi",
                                          "Chronic Stroke Maintenance" = "c_maint_stroke",
                                          "HF Maintenance" = "c_maint_hf"
                                        ), inline = TRUE)
            ),
            tabPanel("Utilities",
                     br(),
                     checkboxGroupInput(ns("dsa_utils"), "Utility Weights:",
                                        choices = list(
                                          "Stable Utility" = "u_stable",
                                          "Chronic MI Utility" = "u_mi",
                                          "Chronic Stroke Utility" = "u_str",
                                          "HF Utility" = "u_hf",
                                          "Discount Rate (Cost)" = "disc_cost",
                                          "Discount Rate (Utility)" = "disc_util"
                                        ), inline = TRUE)
            )
          )
      ),

      # --- RIGHT PANEL: Global Simulation Controls ---
      box(title = tagList(icon("gears"), "2. Analysis Controls"),
          width = 4, status = "warning", solidHeader = TRUE,
          wellPanel(
            style = "background-color: #f9f9f9; border: 1px solid #ddd;",
            numericInput(ns("wtp_threshold"), "WTP Threshold (₹ / QALY)", 236000, step = 10000),
            helpText("Default: 1× India GDP/capita (FY2024-25). 3× = ₹7,05,000."),
            hr(),
            numericInput(ns("dsa_n_pat"), "Patients per Run", 500, min = 100, step = 100),
            helpText("Use 500 for speed, 1000+ for stability."),
            hr(),
            actionButton(ns("run_dsa"), "Run Sensitivity Analysis",
                         class = "btn-warning btn-block btn-lg",
                         icon = icon("play-circle"),
                         style = "white-space: normal; height: auto; padding: 10px;")
          )
      )
    ),

    # --- DYNAMIC SLIDER AREA ---
    fluidRow(
      column(12,
             uiOutput(ns("dynamic_sliders_ui"))
      )
    ),

    # --- RESULTS TABS ---
    fluidRow(
      tabBox(title = tagList(icon("chart-bar"), "3. Sensitivity Analysis Results"),
             width = 12, side = "right",
             tabPanel("ICER Tornado",
                      plotlyOutput(ns("tornado_plot_icer"), height = "600px"),
                      footer = "Deviation from Baseline ICER (Incremental Cost-Effectiveness Ratio)"),
             tabPanel("NMB Tornado",
                      plotlyOutput(ns("tornado_plot_nmb"), height = "600px"),
                      footer = "Deviation from Baseline NMB (Net Monetary Benefit)"),
             tabPanel("Summary Table",
                      DTOutput(ns("dsa_table")),
                      style = "padding-top: 20px;")
      )
    )
  )
}

dsaServer <- function(id, tp_base, cost_base, util_base, n_pat, horizon) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    safe_normalize <- function(mat) {
      for(i in 1:nrow(mat)) {
        if(rownames(mat)[i] %in% c("Death_CV", "Death_NonCV")) next
        mat[i, ] <- pmax(mat[i, ], 0)
        diag_idx <- i
        mat[i, diag_idx] <- 0
        sum_exits <- sum(mat[i, ])
        if(sum_exits >= 1) {
          mat[i, ] <- mat[i, ] * (0.99 / sum_exits)
          mat[i, diag_idx] <- 0.01
        } else {
          mat[i, diag_idx] <- 1 - sum_exits
        }
      }
      return(mat)
    }

    all_selected <- reactive({
      unique(c(input$dsa_trans, input$dsa_eff, input$dsa_costs, input$dsa_utils))
    })

    # Look up base value, label, and tier-aware bounds for each parameter
    param_info <- function(p) {
      tp <- tp_base(); co <- cost_base(); ut <- util_base()
      # Get evidence-tier HR bounds if available
      hr_bnds <- co$hr_bounds
      switch(p,
        # Clinical
        "p_MI"        = list(val = tp$p_MI,        label = "Stable → MI (p_MI)"),
        "p_Stroke"    = list(val = tp$p_Stroke,    label = "Stable → Stroke"),
        "p_HF"        = list(val = tp$p_HF,        label = "Stable → HF"),
        "p_SCD"       = list(val = tp$p_SCD,       label = "Sudden CV Death"),
        "p_BG_Mort"   = list(val = tp$p_BG_Mort,   label = "Background Non-CV Mort"),
        "p_HF_Death"  = list(val = tp$p_HF_Death,  label = "HF Annual Mortality"),
        "cf_MI"       = list(val = tp$cf_MI,        label = "Acute MI Case Fatality"),
        "cf_Stroke"   = list(val = tp$cf_Stroke,    label = "Acute Stroke Case Fatality"),
        # FDC efficacy — use scenario-specific bounds if available
        "hr_MI"       = list(val = tp$hr_MI,        label = "HR: MI (FDC)",
                             custom_lo = if(!is.null(hr_bnds)) hr_bnds$hr_MI_lo else NULL,
                             custom_hi = if(!is.null(hr_bnds)) hr_bnds$hr_MI_hi else NULL),
        "hr_Stroke"   = list(val = tp$hr_Stroke,    label = "HR: Stroke (FDC)",
                             custom_lo = if(!is.null(hr_bnds)) hr_bnds$hr_Stroke_lo else NULL,
                             custom_hi = if(!is.null(hr_bnds)) hr_bnds$hr_Stroke_hi else NULL),
        "hr_HF"       = list(val = tp$hr_HF,        label = "HR: HF (FDC)",
                             custom_lo = if(!is.null(hr_bnds)) hr_bnds$hr_HF_lo else NULL,
                             custom_hi = if(!is.null(hr_bnds)) hr_bnds$hr_HF_hi else NULL),
        "hr_SCD"      = list(val = tp$hr_SCD,       label = "HR: SCD (FDC)",
                             custom_lo = if(!is.null(hr_bnds)) hr_bnds$hr_SCD_lo else NULL,
                             custom_hi = if(!is.null(hr_bnds)) hr_bnds$hr_SCD_hi else NULL),
        # Costs
        "c_fdc"       = list(val = co$fdc_annual / 365.25,  label = "FDC Daily Cost (₹)"),
        "c_soc"       = list(val = co$soc_annual / 365.25,  label = "SoC Daily Cost (₹)"),
        "c_acute_mi"  = list(val = co$acute_mi,     label = "Acute MI Hospitalisation"),
        "c_acute_stroke" = list(val = co$acute_stroke, label = "Acute Stroke Hospitalisation"),
        "c_maint_stable" = list(val = co$maint_stable, label = "Stable Maintenance Cost"),
        "c_maint_mi"  = list(val = co$maint_mi,     label = "Chronic MI Maintenance"),
        "c_maint_stroke" = list(val = if(!is.null(co$maint_stroke)) co$maint_stroke else 20077, label = "Chronic Stroke Maintenance"),
        "c_maint_hf"  = list(val = co$maint_hf,     label = "HF Maintenance Cost"),
        # Utilities
        "u_stable"    = list(val = ut$u_stable,     label = "Utility: Stable"),
        "u_mi"        = list(val = ut$u_mi,         label = "Utility: Chronic MI"),
        "u_str"       = list(val = ut$u_stroke,     label = "Utility: Chronic Stroke"),
        "u_hf"        = list(val = ut$u_hf,         label = "Utility: HF"),
        # Discount rates
        "disc_cost"   = list(val = 0.03,            label = "Discount Rate: Costs"),
        "disc_util"   = list(val = 0.03,            label = "Discount Rate: Utilities"),
        list(val = 0.5, label = p)
      )
    }

    output$dynamic_sliders_ui <- renderUI({
      sel <- all_selected(); req(length(sel) > 0)
      lapply(sel, function(p) {
        info <- param_info(p)
        base_val <- info$val
        # Use evidence-tier bounds for HRs if available
        has_custom <- !is.null(info$custom_lo) && !is.null(info$custom_hi)
        if (has_custom) {
          lo <- info$custom_lo; hi <- info$custom_hi
          def_lo <- info$custom_lo; def_hi <- info$custom_hi
          stp <- 0.01
        } else if (p %in% c("disc_cost", "disc_util")) {
          lo <- 0; hi <- 0.08; def_lo <- 0.0; def_hi <- 0.05; stp <- 0.005
        } else if (base_val < 1 && base_val > 0) {
          lo <- signif(base_val * 0.1, 2)
          hi <- signif(min(base_val * 3.0, 0.95), 2)
          def_lo <- base_val * 0.8; def_hi <- base_val * 1.2
          stp <- 0.001
        } else {
          lo <- signif(base_val * 0.2, 2)
          hi <- signif(base_val * 3.0, 2)
          def_lo <- base_val * 0.8; def_hi <- base_val * 1.2
          stp <- if (base_val > 100) round(base_val * 0.05) else 1
        }
        box(title = info$label, width = 4, status = "info",
            helpText(paste0("Base: ", signif(base_val, 4))),
            sliderInput(ns(paste0(p, "_range")), "Plausible Bounds",
                        min = lo, max = hi,
                        value = c(def_lo, def_hi),
                        step = stp))
      })
    })

    dsa_results <- eventReactive(input$run_dsa, {
      sel <- all_selected(); dsa_df <- data.frame()
      wtp <- input$wtp_threshold
      n_dsa <- input$dsa_n_pat

      withProgress(message = 'Simulating Bounds...', value = 0, {
        for(p in sel) {
          bounds <- input[[paste0(p, "_range")]]
          if (is.null(bounds)) next

          for(type in c("Low", "High")) {
            val <- if(type == "Low") bounds[1] else bounds[2]
            tp_w <- tp_base(); co_w <- cost_base(); ut_w <- util_base()

            # Apply parameter perturbation
            if(p == "p_MI")           tp_w$p_MI <- val
            if(p == "p_Stroke")       tp_w$p_Stroke <- val
            if(p == "p_HF")           tp_w$p_HF <- val
            if(p == "p_SCD")          tp_w$p_SCD <- val
            if(p == "p_BG_Mort")      tp_w$p_BG_Mort <- val
            if(p == "p_HF_Death")     tp_w$p_HF_Death <- val
            if(p == "cf_MI")          tp_w$cf_MI <- val
            if(p == "cf_Stroke")      tp_w$cf_Stroke <- val
            if(p == "hr_MI")          tp_w$hr_MI <- val
            if(p == "hr_Stroke")      tp_w$hr_Stroke <- val
            if(p == "hr_HF")          tp_w$hr_HF <- val
            if(p == "hr_SCD")         tp_w$hr_SCD <- val
            if(p == "c_fdc")          co_w$fdc_annual <- val * 365.25
            if(p == "c_soc")          co_w$soc_annual <- val * 365.25
            if(p == "c_acute_mi")     co_w$acute_mi <- val
            if(p == "c_acute_stroke") co_w$acute_stroke <- val
            if(p == "c_maint_stable") co_w$maint_stable <- val
            if(p == "c_maint_mi")     co_w$maint_mi <- val
            if(p == "c_maint_stroke") co_w$maint_stroke <- val
            if(p == "c_maint_hf")     co_w$maint_hf <- val
            if(p == "u_hf")           ut_w$u_hf <- val
            if(p == "u_str")          ut_w$u_stroke <- val
            if(p == "u_stable")       ut_w$u_stable <- val
            if(p == "u_mi")           ut_w$u_mi <- val

            # Rebuild simplified 7-state transition matrices
            states <- c("Stable", "Acute", "Chronic_MI", "Chronic_Stroke", "HF", "Death_CV", "Death_NonCV")
            for(strat in c("soc", "fdc")) {
              m <- matrix(0, 7, 7, dimnames = list(states, states))
              mi <- if(strat == "soc") tp_w$p_MI else tp_w$p_MI * tp_w$hr_MI
              st <- if(strat == "soc") tp_w$p_Stroke else tp_w$p_Stroke * tp_w$hr_Stroke
              hf <- if(strat == "soc") tp_w$p_HF else tp_w$p_HF * tp_w$hr_HF
              scd <- if(strat == "soc") tp_w$p_SCD else tp_w$p_SCD * tp_w$hr_SCD
              bg <- tp_w$p_BG_Mort
              m["Stable", "Acute"] <- mi + st
              m["Stable", "HF"] <- hf
              m["Stable", "Death_CV"] <- scd
              m["Stable", "Death_NonCV"] <- bg
              p_tot <- mi + st
              cf_mi_w <- tp_w$cf_MI; cf_st_w <- tp_w$cf_Stroke
              if(p_tot > 0) {
                avg_cf <- (mi/p_tot * cf_mi_w) + (st/p_tot * cf_st_w)
                m["Acute", "Death_CV"] <- avg_cf
                m["Acute", "Chronic_MI"] <- (1 - avg_cf) * (mi / p_tot)
                m["Acute", "Chronic_Stroke"] <- (1 - avg_cf) * (st / p_tot)
              }
              m["Chronic_MI", "Acute"] <- (mi * 1.5) + st
              m["Chronic_MI", "HF"] <- hf * 2.0
              m["Chronic_MI", "Death_CV"] <- scd * 1.3
              m["Chronic_MI", "Death_NonCV"] <- bg
              m["Chronic_MI", "Chronic_MI"] <- 1 - sum(m["Chronic_MI",])
              m["Chronic_Stroke", "Acute"] <- mi + (st * 1.5)
              m["Chronic_Stroke", "HF"] <- hf * 1.3
              m["Chronic_Stroke", "Death_CV"] <- scd * 1.5
              m["Chronic_Stroke", "Death_NonCV"] <- bg
              m["Chronic_Stroke", "Chronic_Stroke"] <- 1 - sum(m["Chronic_Stroke",])
              m["HF", "Acute"] <- (mi * 0.5) + (st * 0.75)
              m["HF", "Death_CV"] <- tp_w$p_HF_Death
              m["HF", "Death_NonCV"] <- bg
              m["HF", "HF"] <- 1 - sum(m["HF",])
              m["Death_CV", "Death_CV"] <- 1; m["Death_NonCV", "Death_NonCV"] <- 1
              tp_w[[strat]] <- safe_normalize(m)
            }

            # Handle discount rate perturbation
            disc_c <- if(p == "disc_cost") val else 0.03
            disc_u <- if(p == "disc_util") val else 0.03

            res <- tryCatch(
              run_microsim_engine(tp_w, co_w, ut_w, n_pat = n_dsa, horizon = horizon,
                                  disc_cost = disc_c, disc_util = disc_u),
              error = function(e) NULL
            )
            if (is.null(res) || is.null(res$FDC) || is.null(res$SoC)) next
            dC <- res$FDC$avg_cost - res$SoC$avg_cost
            dE <- res$FDC$avg_qaly - res$SoC$avg_qaly
            if (length(dC) == 0 || length(dE) == 0 || is.na(dC) || is.na(dE)) next
            icer <- if(abs(dE) > 0.0001) dC / dE else NA
            nmb <- (dE * wtp) - dC

            info <- param_info(p)
            dsa_df <- rbind(dsa_df, data.frame(
              Parameter = info$label, Param_ID = p,
              Type = type, Value = val, ICER = icer, NMB = nmb,
              stringsAsFactors = FALSE
            ))
          }
          incProgress(1/length(sel))
        }
      })
      dsa_df
    })

    # Tornado Logic Factory
    render_tornado <- function(df, metric, base_val, title_text, xaxis_text) {
      req(nrow(df) > 0, "Parameter" %in% names(df))
      plot_df <- df %>%
        select(Parameter, Type, !!sym(metric)) %>%
        pivot_wider(names_from = Type, values_from = !!sym(metric)) %>%
        mutate(Swing = abs(High - Low)) %>%
        arrange(Swing)

      plot_df$Parameter <- factor(plot_df$Parameter, levels = plot_df$Parameter)

      plot_ly(plot_df) %>%
        add_bars(y = ~Parameter, x = ~(Low - base_val), name = "Low Bound",
                 orientation = 'h', marker = list(color = '#3498db')) %>%
        add_bars(y = ~Parameter, x = ~(High - base_val), name = "High Bound",
                 orientation = 'h', marker = list(color = '#e74c3c')) %>%
        layout(title = title_text, barmode = 'relative',
               xaxis = list(title = xaxis_text),
               yaxis = list(title = ""),
               margin = list(l = 200),
               shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = -1,
                                  y1 = length(plot_df$Parameter),
                                  line = list(dash = "dash"))))
    }

    output$tornado_plot_icer <- renderPlotly({
      df <- dsa_results()
      req(df, nrow(df) > 0)
      b_res <- run_microsim_engine(tp_base(), cost_base(), util_base(), 500, horizon)
      req(!is.null(b_res$FDC), !is.null(b_res$SoC))
      b_dC <- b_res$FDC$avg_cost - b_res$SoC$avg_cost
      b_dE <- b_res$FDC$avg_qaly - b_res$SoC$avg_qaly
      b_icer <- if(length(b_dE) > 0 && abs(b_dE) > 0.0001) b_dC / b_dE else 0
      render_tornado(df, "ICER", b_icer,
                     "ICER Sensitivity (Tornado Diagram)", "Deviation in ICER (₹)")
    })

    output$tornado_plot_nmb <- renderPlotly({
      df <- dsa_results()
      req(df, nrow(df) > 0)
      b_res <- run_microsim_engine(tp_base(), cost_base(), util_base(), 500, horizon)
      req(!is.null(b_res$FDC), !is.null(b_res$SoC))
      b_dC <- b_res$FDC$avg_cost - b_res$SoC$avg_cost
      b_dE <- b_res$FDC$avg_qaly - b_res$SoC$avg_qaly
      b_nmb <- if(length(b_dE) > 0) (b_dE * input$wtp_threshold) - b_dC else 0
      render_tornado(df, "NMB", b_nmb,
                     "Net Monetary Benefit Sensitivity", "Deviation in NMB (₹)")
    })

    output$dsa_table <- renderDT({
      req(dsa_results(), nrow(dsa_results()) > 0, "Parameter" %in% names(dsa_results()))
      dsa_results() %>%
        select(Parameter, Type, Value, ICER, NMB) %>%
        pivot_wider(names_from = Type, values_from = c(Value, ICER, NMB)) %>%
        mutate(ICER_Swing = abs(ICER_High - ICER_Low),
               NMB_Swing = abs(NMB_High - NMB_Low)) %>%
        arrange(desc(ICER_Swing)) %>%
        datatable(options = list(pageLength = 15, scrollX = TRUE),
                  caption = "DSA: Parameter Bounds, ICER & NMB Impact (sorted by ICER swing)",
                  rownames = FALSE) %>%
        formatCurrency(columns = c("ICER_Low", "ICER_High", "ICER_Swing",
                                    "NMB_Low", "NMB_High", "NMB_Swing"), currency = "₹")
    })

    return(list(
      data_rx   = reactive({ dsa_results() }),
      wtp_rx    = reactive({ input$wtp_threshold }),
      plot_icer = reactive({ output$tornado_plot_icer }),
      plot_nmb  = reactive({ output$tornado_plot_nmb }),
      wtp_val   = reactive({ input$wtp_threshold })
    ))
  })
}
