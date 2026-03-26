# modules/costing_module.R
# Drug Strategy & Microcosting Module — March 2026
# Combines FDC scenario selection (with evidence-tier HR linkage) + pricing + state costs
# This module is the FIRST clinical tab in the workflow.

costingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ===== SECTION 1: SCENARIO & FDC SELECTION =====
    fluidRow(
      box(title = tagList(icon("pills"), "1. FDC Scenario Selection"), width = 12,
          status = "danger", solidHeader = TRUE,
          column(5,
                 selectInput(ns("fdc_category"), "Rational FDC Category",
                             choices = list(
                               "SECURE-type Polypill (ASA+Atorva+Ramipril)" = "secure",
                               "Triple DAPT+Atorvastatin (ASA+Clopi+Atorva)" = "dapt_atorva",
                               "Triple DAPT+Rosuvastatin (ASA+Clopi+Rosuva)" = "dapt_rosuva",
                               "DAPT only (ASA+Clopidogrel)" = "dapt_only",
                               "Custom (enter manually)" = "custom"
                             ), selected = "secure")
          ),
          column(3,
                 uiOutput(ns("evidence_tier_badge"))
          ),
          column(4,
                 uiOutput(ns("evidence_source_info"))
          )
      )
    ),

    # ===== SECTION 1b: TIER 2 WARNING =====
    fluidRow(
      column(12, uiOutput(ns("tier2_warning")))
    ),

    # ===== SECTION 1c: AUTO-POPULATED HRs =====
    fluidRow(
      box(title = tagList(icon("shield-halved"), "FDC Efficacy — Hazard Ratios (auto-set by scenario)"),
          width = 12, status = "success", solidHeader = TRUE,
          column(3,
                 numericInput(ns("hr_MI"), "HR: Recurrent MI", 0.70, step = 0.05),
                 uiOutput(ns("hr_mi_ci"))
          ),
          column(3,
                 numericInput(ns("hr_Stroke"), "HR: Recurrent Stroke", 0.65, step = 0.05),
                 uiOutput(ns("hr_stroke_ci"))
          ),
          column(3,
                 numericInput(ns("hr_HF"), "HR: Heart Failure", 0.80, step = 0.05),
                 uiOutput(ns("hr_hf_ci"))
          ),
          column(3,
                 numericInput(ns("hr_SCD"), "HR: Sudden CV Death", 0.80, step = 0.05),
                 uiOutput(ns("hr_scd_ci"))
          ),
          column(12,
                 uiOutput(ns("hr_override_note"))
          )
      )
    ),

    # ===== SECTION 2: PRICING PERSPECTIVE =====
    fluidRow(
      box(title = "2. Scenario Metadata & Pricing Perspective", width = 12,
          status = "primary", solidHeader = TRUE,
          column(4, textInput(ns("scenario_label"), "Scenario Name", "Base Case — Govt Rate Contract")),
          column(4, radioButtons(ns("pricing_perspective"), "Drug Pricing Perspective",
                                 choices = list(
                                   "Government Rate Contract (MP State)" = "govt",
                                   "Jan Aushadhi (PMBJP)" = "ja",
                                   "Health System (20% below JA)" = "hs",
                                   "Private Branded (MRP)" = "private"
                                 ), selected = "govt")),
          column(4,
                 checkboxInput(ns("use_societal"), "Include Societal Perspective (Indirect Costs)", FALSE),
                 helpText("Toggles wage loss, travel, caregiver costs."))
      )
    ),

    # ===== SECTION 3: FDC & SoC DRUG COSTS =====
    fluidRow(
      box(title = "3. FDC & SoC Drug Costs", width = 12, status = "info", solidHeader = TRUE,
          column(3,
                 numericInput(ns("c_fdc_daily"), "FDC Daily Price (₹)", 5.73),
                 uiOutput(ns("fdc_price_range_info"))
          ),
          column(3, h4("Antiplatelets & Statins"),
                 numericInput(ns("c_asp"), "Aspirin (75mg)", 0.19),
                 numericInput(ns("c_clopi"), "Clopidogrel (75mg)", 0),
                 numericInput(ns("c_atorva"), "Atorvastatin (10/20mg)", 0.218),
                 numericInput(ns("c_rosuvastatin"), "Rosuvastatin (10mg)", 0)),
          column(3, h4("Antihypertensives"),
                 numericInput(ns("c_telmi"), "Telmisartan (40mg)", 0.0),
                 numericInput(ns("c_ramipril"), "Ramipril (5mg)", 0.396),
                 numericInput(ns("c_losartan"), "Losartan (50mg)", 0.0),
                 numericInput(ns("c_amlod"), "Amlodipine (5mg)", 0.0),
                 numericInput(ns("c_cilnidipine"), "Cilnidipine (10mg)", 0.0),
                 numericInput(ns("c_hctz"), "HCTZ (12.5mg)", 0.0)),
          column(3, h4("SoC Daily Summary"),
                 uiOutput(ns("soc_daily_summary"))
          )
      )
    ),

    # ===== SECTION 4: STATE MAINTENANCE COSTS =====
    fluidRow(
      box(title = "4. State Maintenance Costs (Annual per State)", width = 12,
          status = "info", solidHeader = TRUE,
          column(3, h4("Stable/Chronic MI"),
                 numericInput(ns("m_stable"), "Stable Maint.", 7475),
                 numericInput(ns("m_mi"), "Chronic MI Maint.", 20077),
                 numericInput(ns("o_mi"), "Other (MI/Stable)", 0)),
          column(3, h4("Chronic Stroke"),
                 numericInput(ns("m_stroke"), "Stroke Maint.", 18000),
                 numericInput(ns("o_stroke"), "Other (Stroke)", 0)),
          column(3, h4("Heart Failure"),
                 numericInput(ns("m_hf"), "HF Maint.", 11000),
                 numericInput(ns("o_hf"), "Other (HF)", 0)),
          column(3, h4("Common Background"),
                 numericInput(ns("c_bg_meds"), "Other Meds Daily", 12.0),
                 numericInput(ns("o_bg"), "Misc Background", 0))
      )
    ),

    # ===== SECTION 5: SOCIETAL COSTS =====
    fluidRow(
      box(title = "5. Societal Perspective (Indirect Costs)", width = 12,
          status = "primary", solidHeader = TRUE,
          conditionalPanel(condition = paste0("input['", ns("use_societal"), "'] == true"),
                           column(4, h4("Acute Event Losses"),
                                  numericInput(ns("i_wage_acute"), "Patient Wage Loss/Event", 8145),
                                  numericInput(ns("i_travel_acute"), "Travel/Food OOP/Event", 564)),
                           column(4, h4("Chronic Disability"),
                                  numericInput(ns("i_wage_chronic"), "Annual Wage Loss", 40000),
                                  numericInput(ns("i_caregiver"), "Annual Caregiver Cost", 39975)),
                           column(4, h4("Misc Societal"),
                                  numericInput(ns("i_other_soc"), "Other Societal Costs", 0))
          ),
          conditionalPanel(condition = paste0("input['", ns("use_societal"), "'] == false"),
                           helpText("Societal perspective disabled. Only direct medical costs included."))
      )
    ),

    # ===== SECTION 6: ACUTE HOSPITALISATION =====
    fluidRow(
      box(title = "6. Acute Event Hospitalization (Weighted)", width = 12,
          status = "warning", solidHeader = TRUE,
          column(6, h4("Acute MI Revascularization"),
                 numericInput(ns("c_PCI"), "PCI (Stent) Cost", 72521),
                 sliderInput(ns("w_PCI"), "% PCI Weight", 0, 100, 77),
                 numericInput(ns("c_CABG"), "CABG Surgery Cost", 217876),
                 sliderInput(ns("w_CABG"), "% CABG Weight", 0, 100, 23),
                 numericInput(ns("c_Med_Acute"), "Medical Management Cost", 24400)),
          column(6, h4("Acute Stroke"),
                 numericInput(ns("c_stroke_hosp"), "Stroke Hospitalization", 214013),
                 numericInput(ns("o_acute"), "Other Acute Medical", 0))
      )
    ),

    # ===== SECTION 7: FINAL LEDGER =====
    fluidRow(
      box(title = "7. Final Input Cost Accounting (Annual per State)", width = 12,
          status = "success",
          tableOutput(ns("final_ledger")),
          footer = "Total Cost = (Daily Pharmacy Sum * 365.25) + Maintenance + Societal (if enabled).")
    )
  )
}

costingServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ================================================================
    # EVIDENCE TABLE: FDC scenario → HRs + evidence tier
    # ================================================================
    # Tier 1: Direct RCT comparing FDC vs same drugs as separate pills in secondary prev
    # Tier 2: Derived from adherence improvement literature (no direct MACE trial for FDC vs separate)
    #
    # Tier 2 derivation:
    #   - FDC improves adherence by ~11-13% (FOCUS, polypill meta-analyses)
    #   - Non-adherence increases MACE OR ~1.10-1.44 (Chowdhury 2013 Eur Heart J)
    #   - Conservative: adherence_gain=0.12, OR_nonadherence=1.25
    #   - HR_fdc ≈ 1 / (1 + adherence_gain * (OR_nonadherence - 1)) ≈ 0.97
    #   - But this is only the adherence-mediated effect on COMPOSITE MACE
    #   - We apply it uniformly to all components (no evidence to disaggregate)
    #   - Wide uncertainty: 0.80 - 1.05 (includes possibility of null effect)
    # ================================================================

    scenario_evidence <- list(
      secure = list(
        tier = 1,
        label = "SECURE-type Polypill",
        hr_MI = 0.70, hr_Stroke = 0.65, hr_HF = 0.80, hr_SCD = 0.80,
        hr_MI_lo = 0.55, hr_MI_hi = 0.90,
        hr_Stroke_lo = 0.50, hr_Stroke_hi = 0.85,
        hr_HF_lo = 0.60, hr_HF_hi = 1.00,
        hr_SCD_lo = 0.60, hr_SCD_hi = 1.00,
        source = "SECURE Trial (Castellano et al., NEJM 2022)",
        note = "Direct RCT: FDC vs same 3 drugs as separate pills, post-MI, N=2,499"
      ),
      dapt_atorva = list(
        tier = 2,
        label = "Triple DAPT + Atorvastatin",
        hr_MI = 0.95, hr_Stroke = 0.95, hr_HF = 0.97, hr_SCD = 0.97,
        hr_MI_lo = 0.80, hr_MI_hi = 1.05,
        hr_Stroke_lo = 0.80, hr_Stroke_hi = 1.05,
        hr_HF_lo = 0.85, hr_HF_hi = 1.05,
        hr_SCD_lo = 0.85, hr_SCD_hi = 1.05,
        source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
        note = "No direct MACE trial for this FDC vs separate pills. HR from adherence-outcome modelling. Uniform across components."
      ),
      dapt_rosuva = list(
        tier = 2,
        label = "Triple DAPT + Rosuvastatin",
        hr_MI = 0.95, hr_Stroke = 0.95, hr_HF = 0.97, hr_SCD = 0.97,
        hr_MI_lo = 0.80, hr_MI_hi = 1.05,
        hr_Stroke_lo = 0.80, hr_Stroke_hi = 1.05,
        hr_HF_lo = 0.85, hr_HF_hi = 1.05,
        hr_SCD_lo = 0.85, hr_SCD_hi = 1.05,
        source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
        note = "Same adherence-derived HRs as DAPT+Atorva. Rosuvastatin vs atorvastatin efficacy difference not modelled."
      ),
      dapt_only = list(
        tier = 2,
        label = "DAPT only (ASA + Clopidogrel)",
        hr_MI = 0.96, hr_Stroke = 0.96, hr_HF = 0.98, hr_SCD = 0.98,
        hr_MI_lo = 0.82, hr_MI_hi = 1.05,
        hr_Stroke_lo = 0.82, hr_Stroke_hi = 1.05,
        hr_HF_lo = 0.88, hr_HF_hi = 1.05,
        hr_SCD_lo = 0.88, hr_SCD_hi = 1.05,
        source = "Derived: Adherence model (FOCUS 2014, Chowdhury 2013)",
        note = "2-drug FDC: smaller adherence gain expected (~8-10%). Conservative HR estimate."
      )
    )

    # Track whether user manually overrode HRs
    hr_user_override <- reactiveVal(FALSE)

    # ================================================================
    # Drug price lookup tables (unchanged)
    # ================================================================
    drug_prices <- list(
      govt = list(asp=0.19, clopi=0.59, atorva=0.218, rosuva=0.6685, telmi=0.3636,
                  ramipril=0.396, losartan=0.345, amlod=0.1112, cilni=1.01, hctz=0.1215),
      ja   = list(asp=0.33, clopi=1.50, atorva=0.88, rosuva=1.20, telmi=0.80,
                  ramipril=1.26, losartan=0.75, amlod=0.25, cilni=2.50, hctz=0.20),
      hs   = list(asp=0.264, clopi=1.20, atorva=0.704, rosuva=0.96, telmi=0.64,
                  ramipril=1.008, losartan=0.60, amlod=0.20, cilni=2.00, hctz=0.16),
      private = list(asp=0.38, clopi=7.114, atorva=5.31, rosuva=26.53, telmi=7.22,
                     ramipril=3.27, losartan=15.30, amlod=2.667, cilni=16.594, hctz=1.857)
    )

    # FDC price + SoC composition lookup
    fdc_lookup <- list(
      secure     = list(price=5.73, low=1.25, high=7.50,
                        soc_drugs=c("asp","atorva","ramipril"),
                        label="ASA 75 + Atorvastatin 20 + Ramipril 5"),
      dapt_atorva = list(price=6.90, low=3.20, high=10.40,
                         soc_drugs=c("asp","clopi","atorva"),
                         label="ASA 75 + Clopidogrel 75 + Atorvastatin 20"),
      dapt_rosuva = list(price=14.90, low=8.00, high=26.53,
                         soc_drugs=c("asp","clopi","rosuva"),
                         label="ASA 75 + Clopidogrel 75 + Rosuvastatin 10"),
      dapt_only  = list(price=1.72, low=0.78, high=7.50,
                        soc_drugs=c("asp","clopi"),
                        label="ASA 75 + Clopidogrel 75")
    )

    # ================================================================
    # OBSERVER: FDC category change → update HRs + prices + SoC
    # ================================================================
    observeEvent(input$fdc_category, {
      if (input$fdc_category == "custom") return()
      cat <- input$fdc_category

      # Update HRs from evidence table
      ev <- scenario_evidence[[cat]]
      if (!is.null(ev)) {
        updateNumericInput(session, "hr_MI", value = ev$hr_MI)
        updateNumericInput(session, "hr_Stroke", value = ev$hr_Stroke)
        updateNumericInput(session, "hr_HF", value = ev$hr_HF)
        updateNumericInput(session, "hr_SCD", value = ev$hr_SCD)
        hr_user_override(FALSE)
      }

      # Update FDC price (perspective-aware for actual formulary rates)
      fdc <- fdc_lookup[[cat]]
      if (!is.null(fdc)) {
        fdc_price <- fdc$price  # default
        # DAPT+Atorva has actual Jan Aushadhi formulary price
        if (cat == "dapt_atorva" && input$pricing_perspective == "ja") {
          fdc_price <- 4.125
        }
        updateNumericInput(session, "c_fdc_daily", value = fdc_price)
      }

      # Update SoC drug composition
      prices <- drug_prices[[input$pricing_perspective]]
      all_drugs <- c("c_asp","c_clopi","c_atorva","c_rosuvastatin","c_telmi",
                     "c_ramipril","c_losartan","c_amlod","c_cilnidipine","c_hctz")
      drug_map <- c(c_asp="asp", c_clopi="clopi", c_atorva="atorva", c_rosuvastatin="rosuva",
                    c_telmi="telmi", c_ramipril="ramipril", c_losartan="losartan",
                    c_amlod="amlod", c_cilnidipine="cilni", c_hctz="hctz")

      if (!is.null(fdc)) {
        for (d in all_drugs) {
          short <- drug_map[d]
          if (short %in% fdc$soc_drugs) {
            updateNumericInput(session, d, value = prices[[short]])
          } else {
            updateNumericInput(session, d, value = 0)
          }
        }
      }
    })

    # Track manual HR overrides
    observeEvent(input$hr_MI, {
      cat <- input$fdc_category
      if (cat != "custom" && !is.null(scenario_evidence[[cat]])) {
        ev <- scenario_evidence[[cat]]
        if (abs(input$hr_MI - ev$hr_MI) > 0.001) hr_user_override(TRUE)
      }
    }, ignoreInit = TRUE)

    # ================================================================
    # OBSERVER: Pricing perspective change
    # ================================================================
    observeEvent(input$pricing_perspective, {
      prices <- drug_prices[[input$pricing_perspective]]
      updateNumericInput(session, "c_asp", value = prices$asp)
      updateNumericInput(session, "c_atorva", value = prices$atorva)
      updateNumericInput(session, "c_ramipril", value = prices$ramipril)
      if (input$c_clopi > 0) updateNumericInput(session, "c_clopi", value = prices$clopi)
      if (input$c_rosuvastatin > 0) updateNumericInput(session, "c_rosuvastatin", value = prices$rosuva)
      if (input$c_telmi > 0) updateNumericInput(session, "c_telmi", value = prices$telmi)
      if (input$c_losartan > 0) updateNumericInput(session, "c_losartan", value = prices$losartan)
      if (input$c_amlod > 0) updateNumericInput(session, "c_amlod", value = prices$amlod)
      if (input$c_cilnidipine > 0) updateNumericInput(session, "c_cilnidipine", value = prices$cilni)
      if (input$c_hctz > 0) updateNumericInput(session, "c_hctz", value = prices$hctz)

      # Perspective-specific FDC prices (actual formulary/RC rates only)
      # DAPT+Atorva: actual Jan Aushadhi price = Rs 4.125 (in JA national formulary)
      cat <- input$fdc_category
      if (cat == "dapt_atorva" && input$pricing_perspective == "ja") {
        updateNumericInput(session, "c_fdc_daily", value = 4.125)
      } else if (cat != "custom") {
        fdc <- fdc_lookup[[cat]]
        if (!is.null(fdc)) updateNumericInput(session, "c_fdc_daily", value = fdc$price)
      }
    })

    # ================================================================
    # UI RENDERS: Evidence tier badge, source info, warnings
    # ================================================================
    output$evidence_tier_badge <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") {
        return(div(class = "alert alert-secondary",
                   icon("pen"), " Custom scenario — enter HRs manually."))
      }
      ev <- scenario_evidence[[cat]]
      if (ev$tier == 1) {
        div(class = "alert alert-success",
            style = "padding: 8px; margin-top: 25px; font-size: 16px;",
            icon("check-circle"), strong(" TIER 1: Direct RCT Evidence"))
      } else {
        div(class = "alert alert-warning",
            style = "padding: 8px; margin-top: 25px; font-size: 16px;",
            icon("exclamation-triangle"), strong(" TIER 2: Adherence-Derived HRs"))
      }
    })

    output$evidence_source_info <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(NULL)
      ev <- scenario_evidence[[cat]]
      tagList(
        helpText(strong("Source: "), ev$source),
        helpText(ev$note)
      )
    })

    output$tier2_warning <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(NULL)
      ev <- scenario_evidence[[cat]]
      if (ev$tier == 2) {
        div(class = "alert alert-warning",
            style = "margin: 0 15px;",
            icon("exclamation-triangle"),
            strong(" Tier 2 Evidence Notice: "),
            "No direct RCT exists comparing this FDC against the same drugs given as separate pills ",
            "with MACE outcomes in secondary prevention. HRs are derived from adherence improvement ",
            "modelling (FOCUS Project, Chowdhury 2013 meta-analysis). Composite MACE HR applied ",
            "uniformly across MI, Stroke, HF, SCD. Wide uncertainty bounds used in PSA. ",
            "Results should be interpreted with caution and clearly flagged in reporting.")
      }
    })

    output$hr_mi_ci <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(helpText("Custom"))
      ev <- scenario_evidence[[cat]]
      helpText(paste0("95% range: ", ev$hr_MI_lo, " – ", ev$hr_MI_hi))
    })
    output$hr_stroke_ci <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(helpText("Custom"))
      ev <- scenario_evidence[[cat]]
      helpText(paste0("95% range: ", ev$hr_Stroke_lo, " – ", ev$hr_Stroke_hi))
    })
    output$hr_hf_ci <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(helpText("Custom"))
      ev <- scenario_evidence[[cat]]
      helpText(paste0("95% range: ", ev$hr_HF_lo, " – ", ev$hr_HF_hi))
    })
    output$hr_scd_ci <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(helpText("Custom"))
      ev <- scenario_evidence[[cat]]
      helpText(paste0("95% range: ", ev$hr_SCD_lo, " – ", ev$hr_SCD_hi))
    })

    output$hr_override_note <- renderUI({
      if (hr_user_override()) {
        div(class = "alert alert-info", style = "margin-top: 5px;",
            icon("pen"), " HRs manually overridden from scenario defaults.")
      }
    })

    # ================================================================
    # REMAINING UI RENDERS (price info, SoC summary, ledger)
    # ================================================================
    output$fdc_price_range_info <- renderUI({
      cat <- input$fdc_category
      if (cat == "custom") return(helpText("Custom FDC — enter price manually."))
      fdc <- fdc_lookup[[cat]]
      if (is.null(fdc)) return(NULL)
      tagList(
        helpText(paste0("Composition: ", fdc$label)),
        helpText(paste0("Price range: ₹", fdc$low, " – ₹", fdc$high, "/day"))
      )
    })

    output$soc_daily_summary <- renderUI({
      total <- sum(c(input$c_asp, input$c_clopi, input$c_atorva, input$c_rosuvastatin,
                     input$c_telmi, input$c_ramipril, input$c_losartan,
                     input$c_amlod, input$c_cilnidipine, input$c_hctz), na.rm = TRUE)
      active <- c()
      if (input$c_asp > 0) active <- c(active, "Asp")
      if (input$c_clopi > 0) active <- c(active, "Clopi")
      if (input$c_atorva > 0) active <- c(active, "Atorva")
      if (input$c_rosuvastatin > 0) active <- c(active, "Rosuva")
      if (input$c_ramipril > 0) active <- c(active, "Ramipril")
      if (input$c_telmi > 0) active <- c(active, "Telmi")
      if (input$c_losartan > 0) active <- c(active, "Losartan")
      if (input$c_amlod > 0) active <- c(active, "Amlod")
      if (input$c_cilnidipine > 0) active <- c(active, "Cilni")
      if (input$c_hctz > 0) active <- c(active, "HCTZ")
      tagList(
        h4(paste0("SoC Daily: ₹", round(total, 2))),
        helpText(paste0("Active: ", paste(active, collapse = " + "))),
        helpText(paste0("Annual: ₹", format(round(total * 365.25, 0), big.mark = ","))),
        helpText(paste0("FDC Daily: ₹", round(input$c_fdc_daily, 2))),
        helpText(paste0("FDC Annual: ₹", format(round(input$c_fdc_daily * 365.25, 0), big.mark = ",")))
      )
    })

    # ================================================================
    # COST CALCULATIONS
    # ================================================================
    soc_daily <- reactive({
      sum(c(input$c_asp, input$c_clopi, input$c_atorva, input$c_rosuvastatin,
            input$c_telmi, input$c_ramipril, input$c_losartan,
            input$c_amlod, input$c_cilnidipine, input$c_hctz), na.rm = TRUE)
    })

    bg_annual <- reactive({
      (input$c_bg_meds * 365.25) + input$o_bg +
        (if(input$use_societal) (input$i_wage_chronic + input$i_caregiver + input$i_other_soc) else 0)
    })

    weighted_mi <- reactive({
      w_pci <- input$w_PCI/100; w_cabg <- input$w_CABG/100; w_med <- max(0, 1 - (w_pci + w_cabg))
      direct <- (input$c_PCI * w_pci) + (input$c_CABG * w_cabg) + (input$c_Med_Acute * w_med)
      direct + (if(input$use_societal) (input$i_wage_acute + input$i_travel_acute) else 0)
    })

    weighted_stroke <- reactive({
      input$c_stroke_hosp + input$o_acute +
        (if(input$use_societal) (input$i_wage_acute + input$i_travel_acute) else 0)
    })

    output$final_ledger <- renderTable({
      soc_yr <- soc_daily() * 365.25; fdc_yr <- input$c_fdc_daily * 365.25; bg <- bg_annual()
      data.frame(
        State = c("Stable", "Chronic MI", "Chronic Stroke", "HF",
                  "Acute MI (Event)", "Acute Stroke (Event)"),
        SoC_Total = c(soc_yr + bg + input$m_stable + input$o_mi,
                      soc_yr + bg + input$m_mi + input$o_mi,
                      soc_yr + bg + input$m_stroke + input$o_stroke,
                      soc_yr + bg + input$m_hf + input$o_hf,
                      weighted_mi(), weighted_stroke()),
        FDC_Total = c(fdc_yr + bg + input$m_stable + input$o_mi,
                      fdc_yr + bg + input$m_mi + input$o_mi,
                      fdc_yr + bg + input$m_stroke + input$o_stroke,
                      fdc_yr + bg + input$m_hf + input$o_hf,
                      weighted_mi(), weighted_stroke()),
        Difference = c(rep(fdc_yr - soc_yr, 4), 0, 0)
      )
    }, digits = 0)

    # ================================================================
    # RETURN: costs + HRs + evidence tier + scenario metadata
    # ================================================================
    return(reactive({
      cat <- input$fdc_category
      ev <- if (cat != "custom" && !is.null(scenario_evidence[[cat]])) scenario_evidence[[cat]] else NULL

      list(
        # Scenario metadata
        label = input$scenario_label,
        perspective = input$pricing_perspective,
        fdc_category = input$fdc_category,
        use_societal = input$use_societal,

        # Costs
        acute_mi = weighted_mi(), acute_stroke = weighted_stroke(),
        maint_stable = input$m_stable + input$o_mi + bg_annual(),
        maint_mi = input$m_mi + input$o_mi + bg_annual(),
        maint_stroke = input$m_stroke + input$o_stroke + bg_annual(),
        maint_hf = input$m_hf + input$o_hf + bg_annual(),
        soc_annual = soc_daily() * 365.25,
        fdc_annual = input$c_fdc_daily * 365.25,

        # HRs (from this module, NOT transition module)
        hr_MI = input$hr_MI,
        hr_Stroke = input$hr_Stroke,
        hr_HF = input$hr_HF,
        hr_SCD = input$hr_SCD,

        # Evidence tier for PSA/DSA uncertainty
        evidence_tier = if (!is.null(ev)) ev$tier else NA,
        evidence_source = if (!is.null(ev)) ev$source else "Custom",
        hr_bounds = if (!is.null(ev)) list(
          hr_MI_lo = ev$hr_MI_lo, hr_MI_hi = ev$hr_MI_hi,
          hr_Stroke_lo = ev$hr_Stroke_lo, hr_Stroke_hi = ev$hr_Stroke_hi,
          hr_HF_lo = ev$hr_HF_lo, hr_HF_hi = ev$hr_HF_hi,
          hr_SCD_lo = ev$hr_SCD_lo, hr_SCD_hi = ev$hr_SCD_hi
        ) else NULL
      )
    }))
  })
}
