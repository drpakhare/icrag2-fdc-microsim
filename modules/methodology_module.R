# modules/methodology_module.R
# Updated: March 2026 — reflects RRC HTA evidence-based parameters

methodologyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = tagList(icon("book"), "Technical Methodology: The ICRAG-2 HTA Framework"),
        width = 12, status = "primary", solidHeader = TRUE,
        tabsetPanel(
          id = ns("meth_tabs"),

          # --- TAB 1: MODEL ARCHITECTURE ---
          tabPanel("1. The 'Digital Twin' Simulation",
                   br(),
                   h3("From Averages to Individuals: First-Order Microsimulation"),
                   p("Standard health economic models often utilize 'Cohort Markov' approaches, which move a hypothetical group of people through health states as a single average. However, cardiovascular disease is highly heterogeneous. The ICRAG-2 study utilizes a ", strong("First-Order Monte Carlo Microsimulation"), " approach."),
                   p("In this framework, the model creates 1,000 unique 'Digital Twins' of Indian patients. Each patient is simulated individually, year-by-year, over a 30-year lifetime horizon. This allows the model to capture:"),
                   tags$ul(
                     tags$li(strong("Patient Memory:"), " Future risks are conditional on an individual's specific history (e.g., a patient who survived a stroke carries a higher hazard for a second event than a stable patient)."),
                     tags$li(strong("Stochastic Variation:"), " Captures the random nature of clinical events (MI, Stroke, Death) at the individual level, providing a more realistic distribution of outcomes.")
                   ),
                   tags$div(style = "text-align: center; padding: 20px;",
                            icon("project-diagram", "fa-3x"),
                            p(em("Markov State Transition Architecture"))),

                   h4("The 8-State Journey"),
                   p("Every simulated patient exists in one of eight mutually exclusive health states:"),
                   tags$ol(
                     tags$li(strong("Stable:"), " The entry point for the secondary prevention cohort."),
                     tags$li(strong("Acute MI & Acute Stroke:"), " High-intensity 'tunnel' states representing the first 12 months post-event."),
                     tags$li(strong("Chronic MI & Chronic Stroke:"), " Long-term maintenance states for survivors."),
                     tags$li(strong("Heart Failure (HF):"), " A high-morbidity state resulting from cardiac remodeling. HF patients remain at risk of subsequent MI and Stroke events."),
                     tags$li(strong("Death (CV & Non-CV):"), " Absorbing states distinguishing between heart-related mortality and background life-table mortality.")
                   ),

                   h4("V2 Engine Features"),
                   p("The upgraded engine incorporates:"),
                   tags$ul(
                     tags$li(strong("Heterogeneous patient profiles:"), " Demographics drawn from the CREATE Registry (79% male, DM 31%, HTN 43%, smoking 40%)."),
                     tags$li(strong("Age-dependent background mortality:"), " Indian SRS life tables by age-sex group."),
                     tags$li(strong("Adherence decay:"), " FDC adherence modelled with exponential decay (fitted from UMPIRE/FOCUS/TIPS-3), blending FDC and SoC matrices over time."),
                     tags$li(strong("Time-varying MI risk:"), " Year 1 recurrent MI rate (2.65%) declines to 1.2% from Year 2 onwards (Dev 2025, Kerala), reflecting the well-established front-loading of early recurrent events."),
                     tags$li(strong("Recurrent event escalation:"), " Patients with prior events face escalating risk (caps at 2x), modelling cumulative vascular damage.")
                   )
          ),

          # --- TAB 2: TRANSITION LOGIC ---
          tabPanel("2. Clinical Logic & Evidence Base",
                   br(),
                   h3("Transition Probabilities: Indian Evidence"),
                   h4("Baseline Event Risks (from Stable Post-MI State)"),
                   p("All transition probabilities are sourced from Indian post-MI registries:"),
                   tags$ul(
                     tags$li(strong("Recurrent MI:"), " Year 1: 2.65%/yr (Dev 2025, Kerala, N=234, post-MI specific). Year 2+: 1.2%/yr. Previous default was 4.0% (Isezuo mixed ACS)."),
                     tags$li(strong("Stroke:"), " 1.1%/yr (Isezuo 2014, Chennai, N=1,468, mixed ACS). Halved from previous 2.0%."),
                     tags$li(strong("Heart Failure:"), " 2.3%/yr (Huffman & Prabhakaran 2010, HOPE/EUROPA range 0.4-2.3%). Unchanged."),
                     tags$li(strong("Sudden CV Death:"), " 0.8%/yr (retained per PI decision). Kalliath 2021 reports SCD at 1.8% (subset of total CVD death 5.4%)."),
                     tags$li(strong("Non-CV Death:"), " 0.9%/yr (Kalliath 2021, Kerala — only Indian post-ACS paper with explicit non-CVD split). Increased from 0.48%.")
                   ),

                   h4("Case Fatality & Chronic State Mortality"),
                   tags$ul(
                     tags$li(strong("Acute MI case fatality:"), " 8% (Xavier/CREATE 2008, blended STEMI 8.6% + NSTEMI 3.7%)."),
                     tags$li(strong("Acute Stroke case fatality:"), " 20% (Jones 2021 systematic review, range 18-42% in India). Conservative estimate for patients already under care."),
                     tags$li(strong("HF annual mortality:"), " 18.1% (ICC-NHFR 2025, 17 centres, N=5,182). Corroborated by Chopra 17.6%, Sanjay ~18%, INTER-CHF 23%.")
                   ),

                   h4("The Tunnel State Mechanism"),
                   p("The first year following a cardiovascular event is the most lethal and resource-intensive. ", strong("Tunnel States"), " (Acute_MI and Acute_Stroke) ensure patients spend exactly one cycle (1 year) in the high-cost acute phase before transitioning to chronic maintenance or death."),

                   h4("Risk Escalation & Structural Multipliers"),
                   p("Transition probabilities are not static. Multipliers reflect elevated risk in post-event states:"),
                   tags$ul(
                     tags$li("Chronic MI: 1.5x recurrent MI, 2.0x HF, 1.3x SCD (REACH/PEGASUS registry data)"),
                     tags$li("Chronic Stroke: 1.2x MI, 1.5x recurrent stroke, 1.3x HF, 1.5x SCD"),
                     tags$li("HF: 0.5x MI, 0.75x stroke (different mechanism, thromboembolic risk)")
                   ),

                   h4("Half-Cycle Correction"),
                   p("Trapezoidal half-cycle correction assumes transitions occur at the mid-point of each year, ensuring survival and cost estimates align with continuous-time data.")
          ),

          # --- TAB 3: ECONOMIC VALUATION ---
          tabPanel("3. Economic Microcosting",
                   br(),
                   h3("Valuing Health in the Indian Context"),
                   h4("The Multi-Perspective Framework"),
                   p("The model supports four drug pricing perspectives and an optional societal perspective:"),
                   tags$ul(
                     tags$li(strong("Government Rate Contract (base case):"), " Madhya Pradesh state procurement prices. SoC 3-drug daily cost: ~Rs 0.80. Represents the lowest available price for public sector procurement."),
                     tags$li(strong("Jan Aushadhi (PMBJP):"), " Government generic pharmacy prices. SoC ~Rs 2.47/day."),
                     tags$li(strong("Health System:"), " 20% discount below Jan Aushadhi rates. SoC ~Rs 1.98/day."),
                     tags$li(strong("Private Branded (MRP):"), " Maximum retail price. SoC ~Rs 8.96/day.")
                   ),
                   p("The pricing perspective selector automatically repopulates all drug costs. FDC prices are from primary data collection by RRC HTA staff (government procurement where available, online pharmacies otherwise)."),

                   h4("Rational FDC Categories"),
                   p("The model evaluates six FDC formulations across four categories:"),
                   tags$ul(
                     tags$li(strong("SECURE-type 3-drug:"), " Ramipril 5 + Aspirin 75 + Atorvastatin 10 (Rs 5.73/day, range 1.25-7.50)"),
                     tags$li(strong("Triple DAPT+Statin:"), " Aspirin 75 + Clopidogrel 75 + Atorvastatin 20 (Rs 6.90/day, range 3.2-10.4)"),
                     tags$li(strong("Polycap 5-drug:"), " Simvastatin + Ramipril + HCTZ + Atenolol + Aspirin (Rs 34.73/day)"),
                     tags$li(strong("2-drug combinations:"), " Atorvastatin+Clopidogrel, Clopidogrel+Aspirin, Atorvastatin+Aspirin (Rs 0.68-7.50/day)")
                   ),

                   h4("State Costs (Inflation-Adjusted to Dec 2025)"),
                   tags$ul(
                     tags$li("Stable maintenance: Rs 7,475/yr (UMPIRE trial, CPI-adjusted)"),
                     tags$li("Chronic MI: Rs 20,077/yr (NHSCDI, CPI-adjusted)"),
                     tags$li("HF: Rs 11,000/yr (PMJAY follow-up packages)"),
                     tags$li("PCI: Rs 72,521 (NHSCDI), CABG: Rs 2,17,876 (NHSCDI), weights 77%/23% (PMJAY)"),
                     tags$li("Acute Stroke hospitalisation: Rs 2,14,013 (NHSCDI, CPI-adjusted)")
                   ),

                   h4("Quality-Adjusted Life Years (QALYs)"),
                   p("Utility weights from Indian EQ-5D-5L norms (DEVINE Study, J Global Health 2023): Stable 0.814, Chronic MI 0.82, Chronic Stroke 0.65, HF 0.55. Annual age-related decay of 0.1% applied. Age-sex-specific EQ-5D norms from DEVINE used when age-dependent utilities are enabled.")
          ),

          # --- TAB 4: UNCERTAINTY & POLICY ---
          tabPanel("4. Uncertainty & Policy Validation",
                   br(),
                   h3("Stress-Testing the Model"),
                   p("No model is a perfect crystal ball. To ensure policymakers can trust these results, we utilize three layers of analysis:"),

                   h4("1. Deterministic Sensitivity Analysis (DSA)"),
                   p("Key parameters are varied one-by-one across their plausible ranges to identify the primary drivers of the ICER. Visualized via the ", strong("Tornado Diagram"), ". Parameters include transition probabilities, hazard ratios, drug costs, hospitalisation costs, and utility weights."),

                   h4("2. Probabilistic Sensitivity Analysis (PSA)"),
                   p("The simulation is run across multiple iterations where all key parameters vary simultaneously:"),
                   tags$ul(
                     tags$li("Hazard ratios: Log-normal sampling (preserves ratio symmetry)"),
                     tags$li("Costs: Gamma sampling (ensures non-negative)"),
                     tags$li("Utilities: Beta sampling (bounded 0-1)"),
                     tags$li("Transition probabilities: Beta sampling")
                   ),
                   p("This produces the ", strong("Cost-Effectiveness Acceptability Curve (CEAC)"), " and the ", strong("CE Plane"), " with quadrant analysis."),

                   h4("3. Calibration"),
                   p("The model is auto-calibrated against Indian registry targets (CREATE, Kerala ACS cohorts) using Nelder-Mead optimisation over a deterministic cohort trace. Calibration adjusts p_MI, p_SCD, p_HF_Death, and p_BG_Mort to match observed 1-year CV mortality, 1-year MACE rate, and 5-year survival. Setting-dependent case fatality reflects the PCI/thrombolysis/conservative mix."),

                   h4("Epistemic Position"),
                   p("Strongest evidence: FDC hazard ratios (SECURE RCT, NEJM 2022) and drug prices (verifiable procurement data). Moderate: transition probabilities from single-centre Indian studies. Weakest: utility weights (no Indian post-MI EQ-5D primary data) and structural multipliers (international registry-derived). See the Master Input Parameter Workbook for full source documentation.")
          ),

          # --- TAB 5: DATA SOURCES ---
          tabPanel("5. Data Sources",
                   br(),
                   h3("Key References"),
                   tags$ol(
                     tags$li("Castellano JM et al. SECURE Trial. NEJM 2022;387:967-977 — FDC hazard ratios"),
                     tags$li("Xavier D et al. CREATE Registry. Lancet 2008;371:1435-42 — Indian ACS baseline"),
                     tags$li("Dev MP et al. 2025 — Recurrent MI Year 1 probability (2.65%, Kerala)"),
                     tags$li("Isezuo et al. Indian Heart J 2014 — Stroke probability (1.1%, Chennai)"),
                     tags$li("Kalliath et al. Kerala Heart J 2021 — CVD death 5.4%, Non-CVD death 0.9%"),
                     tags$li("Jayagopal et al. ICC-NHFR 2025 — HF mortality 18.1% (17 centres)"),
                     tags$li("NHSCDI — PCI, CABG, Stroke hospitalisation costs"),
                     tags$li("PMJAY Health Benefit Packages — HF maintenance, procedure weights"),
                     tags$li("MP State Government Rate Contract — Drug procurement prices"),
                     tags$li("DEVINE Study, J Global Health 2023 — Indian EQ-5D-5L population norms"),
                     tags$li("Thom S et al. UMPIRE trial, JAMA 2013 — Stable state maintenance cost"),
                     tags$li("CPI data from MOSPI — Inflation adjustment indices"),
                     tags$li("Huffman & Prabhakaran, Natl Med J India 2010 — HF incidence range")
                   ),
                   p(em("Full reference list with PMIDs and DOIs available in the Master Input Parameter Workbook (ICRAG2_Master_Input_Parameters.xlsx), References sheet."))
          )
        )
      )
    )
  )
}

methodologyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static content for documentation purposes
  })
}
