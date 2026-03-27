library(shiny)
library(DT)
library(shinyWidgets)
library(bslib)
source("Shiny_Sim_10_03.R")

presets <- list(
  kein_gpg = list(
    prob_W3 = 50, prob_female = 50, prob_female_W3 = 50,
    gender_factor_new = 1, gender_factor_verl = 1, gender_factor_entf = 1,
    gender_factor_lfunk = 1, gender_factor_lfunk_verlust = 1,
    gender_factor_bb_start = 1, gender_factor_bb_gesamt = 1, gender_factor_bb_unbefr = 1,
    gender_factor_bb_lambda = 1,
    gender_factor_tpf = 1,
    p0_kinder_mann = 0.3, p0_kinder_frau = 0.3,
    lambda_kinder_mann = 1.5, lambda_kinder_frau = 1.5
  ),
  deutlicher_gpg = list(
    prob_W3 = 50, prob_female = 40, prob_female_W3 = 15,
    gender_factor_new = 0.6, gender_factor_verl = 0.7, gender_factor_entf = 0.6,
    gender_factor_lfunk = 0.6, gender_factor_lfunk_verlust = 1,
    gender_factor_bb_start = 0.6, gender_factor_bb_gesamt = 0.6, gender_factor_bb_unbefr = 0.5,
    gender_factor_bb_lambda = 0.6,
    gender_factor_tpf = 0.5,
    p0_kinder_mann = 0.2, p0_kinder_frau = 0.4,
    lambda_kinder_mann = 1.7, lambda_kinder_frau = 1.2
  )
)

# ── Paarungs-Diagnose ────────────────────────────────────────────────────

check_pairing_struktur <- function(df_aktuell, df_kein) {
  
  # 1. Gleiche EmpNums?
  ids_aktuell <- sort(unique(df_aktuell$EmpNum))
  ids_kein    <- sort(unique(df_kein$EmpNum))
  cat("── IDs identisch:", identical(ids_aktuell, ids_kein), "\n")
  cat("   Nur in Aktuell:", length(setdiff(ids_aktuell, ids_kein)), "\n")
  cat("   Nur in Kein GPG:", length(setdiff(ids_kein, ids_aktuell)), "\n\n")
  
  # 2. Gleiche Anzahl Jahre pro Person?
  jahre_aktuell <- df_aktuell %>% count(EmpNum, name = "n_aktuell")
  jahre_kein    <- df_kein    %>% count(EmpNum, name = "n_kein")
  jahre_check   <- left_join(jahre_aktuell, jahre_kein, by = "EmpNum") %>%
    mutate(gleich = n_aktuell == n_kein)
  cat("── Jahre pro Person identisch:\n")
  cat("   Gleich:", sum(jahre_check$gleich, na.rm = TRUE), "/", nrow(jahre_check), "\n")
  cat("   Abweichungen:\n")
  print(jahre_check %>% filter(!gleich))
  
  # 3. Gleiche Geburtsjahre?
  if ("Geburtsjahr" %in% names(df_aktuell)) {
    geburt_aktuell <- df_aktuell %>% distinct(EmpNum, Geburtsjahr) %>% rename(geb_aktuell = Geburtsjahr)
    geburt_kein    <- df_kein    %>% distinct(EmpNum, Geburtsjahr) %>% rename(geb_kein    = Geburtsjahr)
    geburt_check   <- left_join(geburt_aktuell, geburt_kein, by = "EmpNum") %>%
      mutate(gleich = geb_aktuell == geb_kein)
    cat("\n── Geburtsjahre identisch:\n")
    cat("   Gleich:", sum(geburt_check$gleich, na.rm = TRUE), "/", nrow(geburt_check), "\n")
    cat("   Abweichungen:\n")
    print(geburt_check %>% filter(!gleich))
  }
  
  # 4. Gleiche Vergütungsgruppe?
  if ("Vergütungsgruppe" %in% names(df_aktuell)) {
    vg_aktuell <- df_aktuell %>% distinct(EmpNum, Vergütungsgruppe) %>% rename(vg_aktuell = Vergütungsgruppe)
    vg_kein    <- df_kein    %>% distinct(EmpNum, Vergütungsgruppe) %>% rename(vg_kein    = Vergütungsgruppe)
    vg_check   <- left_join(vg_aktuell, vg_kein, by = "EmpNum") %>%
      mutate(gleich = vg_aktuell == vg_kein)
    cat("\n── Vergütungsgruppe identisch:\n")
    cat("   Gleich:", sum(vg_check$gleich, na.rm = TRUE), "/", nrow(vg_check), "\n")
  }
}

# UI ----------------------------------------------------------------------



ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "sandstone"
  ) |> bs_add_rules("
  .nav-tabs > li.active > a {
    background-color: $secondary;
    color: white;
  }
"),
  titlePanel("GPG Simulation"),
  
  
  # Parameter ---------------------------------------------------------------
  
  tabsetPanel(
    tabPanel("Parameter",
             tabsetPanel(
               tabPanel("Grundstruktur der Simulation",
                        numericInput("seed", label="Seed", value=1910),
                        actionButton("rdm_seed", label="Zufälligen Seed generieren", class="btn btn-outline-secondary"),
                        numericInput("no_researchers",
                                     label= "Anzahl Professor:innen",
                                     value= 314,
                                     min= 1,
                                     max= 1000),
                        hr(),
                        h4("Zeitraum der Simulation"),
                        fluidRow(
                          column(3, sliderInput("year_start", "Startjahr", 
                                                min = 1900, max = 2024, 
                                                value = 1900, step = 1, sep = "")),
                          column(3, numericInput("year_start_num", NULL, 
                                                 value = 1900, min = 1900, max = 2024))
                        ),
                        fluidRow(
                          column(3, sliderInput("act_year", "Endjahr", 
                                                min = 2000, max = 2050, 
                                                value = 2025, step = 1, sep = "")),
                          column(3, numericInput("act_year_num", NULL, 
                                                 value = 2025, min = 2000, max = 2050))
                        ),
                        h4("Zeitraum der Ausgabe"),
                        fluidRow(
                          column(3, sliderInput("output_year_start", "Output ab Jahr",
                                                min = 1900, max = 2024,
                                                value = 2005, step = 1, sep = "")),
                          column(3, numericInput("output_year_start_num", NULL,
                                                 value = 2005, min = 1800, max = 2024))
                        ),
                        
                        h4("Variablen Auswahl"),
                        hr(),
                        h4("Hauptvariablen"),
                        fluidRow(
                          column(3,
                                 h5("Persönlich"),
                                 checkboxGroupInput("cols_personal", NULL,
                                                    choices  = c("Geschlecht", "Vergütungsgruppe", "Fachgebiet"),
                                                    selected = c("Geschlecht", "Vergütungsgruppe", "Fachgebiet"),
                                 ),
                                 checkboxInput("select_all_personal",    "Alle", value = TRUE)
                                 
                          ),
                          column(3,
                                 h5("Demografie"),
                                 checkboxGroupInput("cols_demo", NULL,
                                                    choices  = c("Kinder", "MaritalStatus", "Nationality"),
                                                    selected = c("Kinder", "MaritalStatus", "Nationality"),
                                 ),
                                 checkboxInput("select_all_demo",        "Alle", value = TRUE)
                                 
                          ),
                          column(3,
                                 h5("Vergütung"),
                                 checkboxGroupInput("cols_verguetung", NULL,
                                                    choices  = c("ThirdPartyFunding", "lfunk_euro",
                                                                 "lbe_befr_euro", "lbe_unbefr_euro",
                                                                 "bb_befr_betrag", "bb_unbefr_betrag"),
                                                    selected = c("ThirdPartyFunding", "lfunk_euro",
                                                                 "lbe_befr_euro", "lbe_unbefr_euro",
                                                                 "bb_befr_betrag", "bb_unbefr_betrag"),
                                 ), 
                                 checkboxInput("select_all_verguetung",  "Alle", value = TRUE)
                                 
                          )
                        ),
                        hr(),
                        h4("Hilfsvariablen"),
                        fluidRow(
                          column(3,
                                 h5("LBe"),
                                 checkboxGroupInput("cols_help_lbe", NULL,
                                                    choices = c("lbe_s1_status", "lbe_s1_jahre",
                                                                "lbe_s2_status", "lbe_s2_jahre",
                                                                "lbe_s3_status", "lbe_s3_jahre",
                                                                "lbe_s4_status", "lbe_s4_jahre",
                                                                "lbe_befr_gesamt", "lbe_unbefr_gesamt","lbe_gesamt_euro"),
                                                    selected = c(),
                                 ),
                                 checkboxInput("select_all_help_lbe",    "Alle", value = FALSE)
                                 
                          ),
                          column(3,
                                 h5("LFunk"),
                                 checkboxGroupInput("cols_help_lfunk", NULL,
                                                    choices = c("LFunk", "lfunk_jahre"),
                                                    selected = c("LFunk", "lfunk_jahre"),
                                 ),
                                 checkboxInput("select_all_help_lfunk",  "Alle", value = TRUE)
                                 
                          ),
                          column(3,
                                 h5("BB"),
                                 checkboxGroupInput("cols_help_bb", NULL,
                                                    choices = c("bb_befr_jahre", "bb_letzte_vergabe"),
                                                    selected = c(),
                                 ),
                                 checkboxInput("select_all_help_bb",     "Alle", value = FALSE)
                                 
                          ),
                          column(3,
                                 h5("Rest"),
                                 checkboxGroupInput("cols_help_rest", NULL,
                                                    choices = c("experience_this_year", "entry_age",
                                                                "retirement_ages", "Erfahrung", "Geburtsjahr"),
                                                    selected = c("experience_this_year", "entry_age",
                                                                 "retirement_ages", "Erfahrung", "Geburtsjahr"),
                                 ),
                                 checkboxInput("select_all_help_rest",   "Alle", value = TRUE)
                                 
                          )
                        )
                        
               ),
               
               tabPanel("Personal",
                        h4("Beschäftigungsdauer"),
                        fluidRow(
                          column(4,sliderInput("mu_entry",    "Ø Berufungsalter",      min=35, max=60, value=44)),
                          column(4,sliderInput("max_age",     "Ruhestandsalter",       min=60, max=75, value=69)),
                          column(4,sliderInput("early_exit",  "Vorzeitiger Austritt (%)", min=0, max=20, value=2, step=0.2)),
                        ),
                        hr(),
                        h4("Kinder"),
                        fluidRow(
                          column(4, sliderInput("p0_kinder_mann",     "Kinderlos Männer",    min=0, max=1, value=0.30, step=0.01)),
                          column(4, sliderInput("p0_kinder_frau",     "Kinderlos Frauen",    min=0, max=1, value=0.30, step=0.01),
                                 helpText("Kinderlosigkeit Frauen bei 0.4 laut Göttingen"))
                        ),
                        fluidRow(
                          column(4, sliderInput("lambda_kinder_mann", "Ø Kinder Männer (λ)",     min=0, max=5, value=1.5,  step=0.1)),
                          column(4, sliderInput("lambda_kinder_frau", "Ø Kinder Frauen (λ)",     min=0, max=5, value=1.5,  step=0.1),
                                 helpText("Ø Kinder Frauen bei 1.3 laut Göttingen"))
                        ),
                        hr(),
                        h4("Nationalität"),
                        fluidRow(
                          column(4,
                                 sliderInput("p_german_mann", "Anteil Deutsche Männer (%)", min=0, max=1, value=0.90, step=0.01)
                          ),
                          column(4,
                                 sliderInput("p_german_frau", "Anteil Deutsche Frauen (%)", min=0, max=1, value=0.89, step=0.01)
                          ),
                          column(4,
                                 sliderInput("p_eu", "Anteil EU unter Nicht-Deutschen (%)", min=0, max=1, value=0.60, step=0.01)
                          )
                        ),
                        hr(),
                        h4("Drittmittel"),
                        fluidRow(
                          column(4, sliderInput("mu_tpf",    "Ø Drittmittel (mu)",   min=100, max=2000, value=500,  step=50)),
                          column(4, sliderInput("sigma_tpf", "Streuung (sigma)",      min=0.1, max=5,    value=2,    step=0.1))
                        )
               ),
               tabPanel("Vergütung",
                        h4("W2 / W3"),
                        fluidRow(
                          column(4, sliderInput("prob_W3",        "Gesamtanteil W3 (%)",    min=0, max=100, value=50)),
                          column(4, sliderInput("prob_female",    "Gesamtanteil Frauen (%)", min=0, max=100, value=40)),
                          column(4, sliderInput("prob_female_W3", "Anteil Frauen in W3 (%)", min=0, max=100, value=30))
                        ),
                        fluidRow(
                          column(4,
                                 tags$div(
                                   style = "pointer-events: none;",
                                   sliderInput("prob_female_W2", "Anteil Frauen in W2 (auto)", min=0, max=100, value=50)
                                 )
                          )
                        ),
                        hr(),
                        h4("Leistungsbezüge"),
                        fluidRow(
                          column(4, sliderInput("p_lbe_new",        "Neue LBe Wahrsch.",      min=0, max=1, value=0.6, step=0.01)),
                          column(4, sliderInput("p_lbe_verlaenger", "Verlängerung Wahrsch.",  min=0, max=1, value=0.8, step=0.01)),
                          column(4, sliderInput("p_lbe_entfrist",   "Entfristung Wahrsch.",   min=0, max=1, value=0.4, step=0.01))
                        ),
                        hr(),
                        h4("LFunk"),
                        fluidRow(
                          column(4, sliderInput("p_lfunk",         "Wahrscheinlichkeit LFunk",        min=0, max=1, value=0.2, step=0.01)),
                          column(4, sliderInput("p_lfunk_verlust", "Wahrscheinlichkeit LFunk Verlust", min=0, max=1, value=0.1, step=0.01))
                        ),
                        hr(),
                        h4("Berufungs-/Bleibezuschläge"),
                        fluidRow(
                          column(4, sliderInput("p_bb_start",         "Startzuschlag BB",              min=0, max=1,    value=0.2, step=0.01)),
                          column(4, sliderInput("p_bb_gesamt",        "Jährl. Vergabewahrsch. BB",         min=0, max=1,    value=0.2, step=0.01)),
                          column(4, sliderInput("p_bb_unbefr_anteil", "BB-Anteil unbefristet",             min=0, max=1,    value=0.2,  step=0.01))
                        ),
                        fluidRow(
                          column(4, sliderInput("lambda_bb",     "Schnitt BB in €",                   min=0, max=2000, value=500, step=50)),
                          column(4, sliderInput("bb_befr_dauer", "Laufzeit BB befristet (Jahre)",     min=1, max=20,   value=7,   step=1)),
                          column(4, sliderInput("min_jahre_neu", "Mindestabstand Vergabe BB (Jahre)", min=1, max=10,   value=3,   step=1))
                        )
               ),
               
               tabPanel("Gender",
                        h4("LBe"),
                        fluidRow(
                          column(4,sliderInput("gender_factor_new",  "Gender Faktor (Frauen): Neue LBe",       min=0, max=2, value=1, step=0.01)),
                          column(4,sliderInput("gender_factor_verl", "Gender Faktor: LBe Verlängerung",   min=0, max=2, value=1, step=0.01)),
                          column(4,sliderInput("gender_factor_entf", "Gender Faktor: LBe Entfristung",    min=0, max=2, value=1, step=0.01)),
                        ),
                        hr(),
                        h4("LFunk"),
                        fluidRow(
                          column(4,sliderInput("gender_factor_lfunk",         "Gender Faktor: LFunk Vergabe",   min=0, max=2, value=1, step=0.01)),
                          column(4,sliderInput("gender_factor_lfunk_verlust", "Gender Faktor: LFunk Verlust",   min=0, max=2, value=1, step=0.01)),
                        ),
                        hr(),
                        h4("Berufungs-/Bleibezuschläge"),
                        fluidRow(
                          column(4,sliderInput("gender_factor_bb_start",  "Gender Faktor: BB Startzuschlag",    min=0, max=2, value=1, step=0.01)),
                          column(4,sliderInput("gender_factor_bb_gesamt", "Gender Faktor: BB Vergabewahrsch.",  min=0, max=2, value=1, step=0.01)),
                          column(4,sliderInput("gender_factor_bb_unbefr", "Gender Faktor: BB Anteil unbefristet", min=0, max=2, value=1, step=0.01)),
                          column(4, sliderInput("gender_factor_bb_lambda", "Gender Faktor: BB Betrag (lambda)", min=0, max=2, value=1, step=0.01)),
                        ),
                        hr(),
                        h4("Drittmittel"),
                        fluidRow(
                          column(4,sliderInput("gender_factor_tpf", "Gender Faktor: Drittmittel", min=0, max=2, value=1, step=0.01)),
                        )
               ),
             ),
             hr(),
             h4("Szenarien"),
             fluidRow(
               column(6, actionButton("preset_kein",     "Kein GPG",       
                                      class = "btn btn-outline-success",  width = "100%")),
               column(6, actionButton("preset_deutlich", "Deutlicher GPG", 
                                      class = "btn btn-outline-danger",   width = "100%"))
             ),
             hr(),
             br(),
             actionButton("run_sim", "Simulation starten", 
                          class = "btn btn-secondary", width="100%", style= "margin-bottom: 20px;")
    ),
    
    
    # Checking ----------------------------------------------------------------
    
    
    tabPanel("Checking",
             tabsetPanel(
               tabPanel("Fakultäten",
                        plotOutput("plot_faculty", height="400px")
               ),
               tabPanel("Beschäftigungsdauer",
                        plotOutput("plot_hist_years", height="400px")
               ),
               tabPanel("Drittmittel",
                        plotOutput("plot_tpf", height="400px")
               ),
               tabPanel("Kinder",
                        plotOutput("plot_kinder", height="400px")
               )
             ),
             downloadButton("dl_csv", "⬇ CSV herunterladen")
    ),
    tabPanel("Validierung",
             checkboxInput("val_compare_kein_gpg",
                           "Vergleich mit 'Kein GPG' Preset",
                           value = TRUE),
             numericInput("val_n_runs",
                          "Anzahl Simulationsläufe (n)",
                          value = 200,
                          min   = 10,
                          max   = 100000,
                          step  = 1),
             actionButton("run_validation", 
                          "Validierung starten", 
                          class = "btn btn-secondary",
                          width = "100%"),
             helpText("Hinweis: Die Validierung kann je nach Anzahl der Läufe und verfügbaren CPU-Kernen einige Minuten dauern."),
             conditionalPanel(
               condition = "input.run_validation > 0",
               hr(),
               h4("T-Test: Mann vs. Frau"),
               tableOutput("table_ttest"),
               downloadButton("dl_ttest", "⬇ T-Test herunterladen"),
               hr(),
               h4("GPG Vergleich: Kein GPG vs. Aktuell"),
               tableOutput("table_gpg_vergleich"),
               downloadButton("dl_gpg_vergleich", "⬇ GPG Vergleich herunterladen"),
               hr(),
               h4("Frauenanteile"),
               plotOutput("val_plot_struktur", height = "350px"),
               downloadButton("dl_plot_struktur", "⬇ Plot herunterladen"),
               hr(),
               h4("GPG je Komponente"),
               plotOutput("val_plot_gpg", height = "400px"),
               downloadButton("dl_plot_gpg", "⬇ Plot herunterladen"),
               hr(),
               h4("Vergütungskomponenten Mann vs. Frau"),
               plotOutput("val_plot_comp", height = "400px"),
               downloadButton("dl_plot_comp", "⬇ Plot herunterladen"),
               hr(),
               h4("LBe-Stufen"),
               plotOutput("val_plot_lbe", height = "300px"),
               downloadButton("dl_plot_lbe", "⬇ Plot herunterladen"),
               hr(),
               h4("Anteile LFunk & BB"),
               plotOutput("val_plot_anteile", height = "300px"),
               downloadButton("dl_plot_anteile", "⬇ Plot herunterladen"),
               hr(),
               h4("Kinder"),
               plotOutput("val_plot_demo", height = "300px"),
               downloadButton("dl_plot_demo", "⬇ Plot herunterladen")
             )
    ),
    tabPanel("Datenvorschau",
             DTOutput("table_preview")
    )
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  
  observeEvent(input$rdm_seed, {
    updateNumericInput(session, "seed", value = sample(1:10000, 1))
  })
  
  apply_preset <- function(p) {
    updateSliderInput(session, "prob_W3",        value = p$prob_W3)
    updateSliderInput(session, "prob_female",     value = p$prob_female)
    updateSliderInput(session, "prob_female_W3",  value = p$prob_female_W3)
    updateSliderInput(session, "gender_factor_new",           value = p$gender_factor_new)
    updateSliderInput(session, "gender_factor_verl",          value = p$gender_factor_verl)
    updateSliderInput(session, "gender_factor_entf",          value = p$gender_factor_entf)
    updateSliderInput(session, "gender_factor_lfunk",         value = p$gender_factor_lfunk)
    updateSliderInput(session, "gender_factor_lfunk_verlust", value = p$gender_factor_lfunk_verlust)
    updateSliderInput(session, "gender_factor_bb_start",      value = p$gender_factor_bb_start)
    updateSliderInput(session, "gender_factor_bb_gesamt",     value = p$gender_factor_bb_gesamt)
    updateSliderInput(session, "gender_factor_bb_unbefr",     value = p$gender_factor_bb_unbefr)
    updateSliderInput(session, "gender_factor_bb_lambda",     value = p$gender_factor_bb_lambda)
    updateSliderInput(session, "gender_factor_tpf",           value = p$gender_factor_tpf)
    updateSliderInput(session, "p0_kinder_mann",              value = p$p0_kinder_mann)
    updateSliderInput(session, "p0_kinder_frau",              value = p$p0_kinder_frau)
    updateSliderInput(session, "lambda_kinder_mann",          value = p$lambda_kinder_mann)
    updateSliderInput(session, "lambda_kinder_frau",          value = p$lambda_kinder_frau)
  }
  
  observeEvent(input$preset_kein,     { apply_preset(presets$kein_gpg) })
  observeEvent(input$preset_leicht,   { apply_preset(presets$leichter_gpg) })
  observeEvent(input$preset_deutlich, { apply_preset(presets$deutlicher_gpg) })
  
  
  sim_result <- eventReactive(input$run_sim, {
    
    structure_params <- make_structure_params(
      no_researchers = input$no_researchers,
      seed           = input$seed,
      year_start     = input$year_start,
      act_year       = input$act_year
    )
    
    emp_params <- make_employee_params(
      mu_entry        = input$mu_entry,
      sigma_entry     = input$sigma_entry,
      max_age         = input$max_age,
      lambda          = input$lambda,
      early_exit_prob = input$early_exit / 100,
      mu_tpf          = input$mu_tpf,
      sigma_tpf       = input$sigma_tpf,
      p0_kinder_mann     = input$p0_kinder_mann,
      p0_kinder_frau     = input$p0_kinder_frau,
      lambda_kinder_mann = input$lambda_kinder_mann,
      lambda_kinder_frau = input$lambda_kinder_frau,
      p_german_mann = input$p_german_mann,
      p_german_frau = input$p_german_frau,
      p_eu          = input$p_eu
    )
    
    salary_params <- make_salary_params(
      prob_W3  = input$prob_W3 / 100,
      p_lfunk         = input$p_lfunk,
      p_lfunk_verlust = input$p_lfunk_verlust,
      p_bb_start         = input$p_bb_start,
      p_bb_gesamt        = input$p_bb_gesamt,
      p_bb_unbefr_anteil = input$p_bb_unbefr_anteil,
      lambda_bb          = input$lambda_bb,
      bb_befr_dauer      = input$bb_befr_dauer,
      min_jahre_neu      = input$min_jahre_neu,
      gender_factor_bb_start  = input$gender_factor_bb_start,
      gender_factor_bb_gesamt = input$gender_factor_bb_gesamt,
      gender_factor_bb_unbefr = input$gender_factor_bb_unbefr,
      gender_factor_bb_lambda = input$gender_factor_bb_lambda,
      p_lbe_new        = input$p_lbe_new,
      p_lbe_verlaenger = input$p_lbe_verlaenger,
      p_lbe_entfrist   = input$p_lbe_entfrist
    )
    
    gender_params <- make_gender_params(
      prob_female_W2     = input$prob_female_W2 / 100,
      prob_female_W3     = input$prob_female_W3 / 100,
      gender_factor_new  = input$gender_factor_new,
      gender_factor_verl = input$gender_factor_verl,
      gender_factor_entf = input$gender_factor_entf,
      gender_factor_lfunk         = input$gender_factor_lfunk,
      gender_factor_lfunk_verlust = input$gender_factor_lfunk_verlust,
      gender_factor_tpf = input$gender_factor_tpf
      
    )

    run_sim(structure_params, emp_params, salary_params, gender_params)
  })
  
  sim_filtered <- reactive({
    df <- sim_result()
    df <- df[df$Berichtsjahr >= input$output_year_start, ]
    
    id_cols <- c("EmpNum", "Berichtsjahr")
    
    selected_cols <- c(
      id_cols,
      input$cols_personal,
      input$cols_demo,
      input$cols_verguetung,
      input$cols_help_lbe,
      input$cols_help_lfunk,
      input$cols_help_bb,
      input$cols_help_rest
    )
    
    selected_cols <- intersect(selected_cols, colnames(df))
    df[, selected_cols]
  })
  
  

# Validierung -------------------------------------------------------------

  
  val_results <- eventReactive(input$run_validation, {
    req(input$run_validation)
    source("Shiny_Sim_10_03.R")
    
    library(dplyr)
    library(ggplot2)
    library(tidyr)
    library(parallel)

    n_runs <- input$val_n_runs
    base_year <- input$act_year
    
    # Parameter direkt aus den aktuellen App-Inputs
    employee_p <- make_employee_params(
      mu_entry           = input$mu_entry,
      sigma_entry        = input$sigma_entry,
      max_age            = input$max_age,
      lambda             = input$lambda,
      early_exit_prob    = input$early_exit / 100,
      mu_tpf             = input$mu_tpf,
      sigma_tpf          = input$sigma_tpf,
      p0_kinder_mann     = input$p0_kinder_mann,
      p0_kinder_frau     = input$p0_kinder_frau,
      lambda_kinder_mann = input$lambda_kinder_mann,
      lambda_kinder_frau = input$lambda_kinder_frau,
      p_german_mann      = input$p_german_mann,
      p_german_frau      = input$p_german_frau,
      p_eu               = input$p_eu
    )
    
    salary_p <- make_salary_params(
      prob_W3             = input$prob_W3 / 100,
      p_lfunk             = input$p_lfunk,
      p_lfunk_verlust     = input$p_lfunk_verlust,
      p_bb_start          = input$p_bb_start,
      p_bb_gesamt         = input$p_bb_gesamt,
      p_bb_unbefr_anteil  = input$p_bb_unbefr_anteil,
      lambda_bb           = input$lambda_bb,
      bb_befr_dauer       = input$bb_befr_dauer,
      min_jahre_neu       = input$min_jahre_neu,
      gender_factor_bb_start  = input$gender_factor_bb_start,
      gender_factor_bb_gesamt = input$gender_factor_bb_gesamt,
      gender_factor_bb_unbefr = input$gender_factor_bb_unbefr,
      gender_factor_bb_lambda = input$gender_factor_bb_lambda,
      p_lbe_new           = input$p_lbe_new,
      p_lbe_verlaenger    = input$p_lbe_verlaenger,
      p_lbe_entfrist      = input$p_lbe_entfrist
    )
    
    gender_p <- make_gender_params(
      prob_female_W2              = input$prob_female_W2 / 100,
      prob_female_W3              = input$prob_female_W3 / 100,
      gender_factor_new           = input$gender_factor_new,
      gender_factor_verl          = input$gender_factor_verl,
      gender_factor_entf          = input$gender_factor_entf,
      gender_factor_lfunk         = input$gender_factor_lfunk,
      gender_factor_lfunk_verlust = input$gender_factor_lfunk_verlust,
      gender_factor_tpf           = input$gender_factor_tpf
    )
    
    # ── Kein GPG Preset (Vergleich) ──────────────────────
    if (isTRUE(input$val_compare_kein_gpg)) {
      employee_p_kein <- make_employee_params()
      salary_p_kein <- make_salary_params(
        gender_factor_bb_start  = 1.0,
        gender_factor_bb_gesamt = 1.0,
        gender_factor_bb_unbefr = 1.0,
        gender_factor_bb_lambda = 1.0
      )
      gender_p_kein <- make_gender_params(
        prob_female_W2              = 0.50,
        prob_female_W3              = 0.50,
        gender_factor_new           = 1.00,
        gender_factor_verl          = 1.00,
        gender_factor_entf          = 1.00,
        gender_factor_lfunk         = 1.00,
        gender_factor_lfunk_verlust = 1.00,
        gender_factor_tpf           = 1.00
      )
    }

    
    # Hilfsfunktion: Kennzahlen aus einem Simulationslauf

    extract_kennzahlen <- function(last) {
      m <- last[last$Geschlecht == 1, ]
      f <- last[last$Geschlecht == 2, ]
      bb_gesamt_m <- m$bb_unbefr_betrag + m$bb_befr_betrag
      bb_gesamt_f <- f$bb_unbefr_betrag + f$bb_befr_betrag
      
      data.frame(
        # [1] Struktur
        frauenanteil             = mean(last$Geschlecht == 2),
        frauenanteil_W3          = mean(last$Geschlecht[last$Vergütungsgruppe == "W3"] == 2),
        frauenanteil_W2          = mean(last$Geschlecht[last$Vergütungsgruppe == "W2"] == 2),
        anteil_W3                = mean(last$Vergütungsgruppe == "W3"),
        # [2–4] LBe
        mean_lbe_mann            = mean(m$lbe_gesamt_euro),
        mean_lbe_frau            = mean(f$lbe_gesamt_euro),
        mean_lbe_befr_mann       = mean(m$lbe_befr_euro),
        mean_lbe_befr_frau       = mean(f$lbe_befr_euro),
        mean_lbe_unbefr_mann     = mean(m$lbe_unbefr_euro),
        mean_lbe_unbefr_frau     = mean(f$lbe_unbefr_euro),
        mean_lbe_befr_stufen_mann  = mean(m$lbe_befr_gesamt),
        mean_lbe_befr_stufen_frau  = mean(f$lbe_befr_gesamt),
        mean_lbe_unbefr_stufen_mann = mean(m$lbe_unbefr_gesamt),
        mean_lbe_unbefr_stufen_frau = mean(f$lbe_unbefr_gesamt),
        # [5–6] LFunk
        mean_lfunk_mann          = mean(m$lfunk_euro),
        mean_lfunk_frau          = mean(f$lfunk_euro),
        anteil_lfunk_mann        = mean(m$LFunk == 1),
        anteil_lfunk_frau        = mean(f$LFunk == 1),
        mean_lfunk_jahre_mann    = mean(m$lfunk_jahre),
        mean_lfunk_jahre_frau    = mean(f$lfunk_jahre),
        # [7] TPF
        mean_tpf_mann            = mean(m$ThirdPartyFunding),
        mean_tpf_frau            = mean(f$ThirdPartyFunding),
        # [8–11] BB
        mean_bb_mann             = mean(bb_gesamt_m),
        mean_bb_frau             = mean(bb_gesamt_f),
        mean_bb_befr_mann        = mean(m$bb_befr_betrag),
        mean_bb_befr_frau        = mean(f$bb_befr_betrag),
        mean_bb_unbefr_mann      = mean(m$bb_unbefr_betrag),
        mean_bb_unbefr_frau      = mean(f$bb_unbefr_betrag),
        anteil_bb_mann           = mean(bb_gesamt_m > 0),
        anteil_bb_frau           = mean(bb_gesamt_f > 0),
        anteil_bb_unbefr_mann    = ifelse(any(bb_gesamt_m > 0),
                                          mean(m$bb_unbefr_betrag[bb_gesamt_m > 0] > 0), NA_real_),
        anteil_bb_unbefr_frau    = ifelse(any(bb_gesamt_f > 0),
                                          mean(f$bb_unbefr_betrag[bb_gesamt_f > 0] > 0), NA_real_),
        # Kinder
        mean_kinder_mann      = mean(m$Kinder),
        mean_kinder_frau      = mean(f$Kinder),
        anteil_kinderlos_mann = mean(m$Kinder == 0),
        anteil_kinderlos_frau = mean(f$Kinder == 0)
      )
    }
    

    add_gpg <- function(df) {
      df %>% mutate(
        gpg_lbe        = round((mean_lbe_mann        - mean_lbe_frau)        / mean_lbe_mann,        3),
        gpg_lbe_befr   = round((mean_lbe_befr_mann   - mean_lbe_befr_frau)   / mean_lbe_befr_mann,   3),
        gpg_lbe_unbefr = round((mean_lbe_unbefr_mann - mean_lbe_unbefr_frau) / mean_lbe_unbefr_mann, 3),
        gpg_lfunk      = round((mean_lfunk_mann       - mean_lfunk_frau)      / mean_lfunk_mann,      3),
        gpg_tpf        = round((mean_tpf_mann         - mean_tpf_frau)        / mean_tpf_mann,        3),
        gpg_bb         = round((mean_bb_mann          - mean_bb_frau)         / mean_bb_mann,         3),
        gpg_bb_befr    = round((mean_bb_befr_mann     - mean_bb_befr_frau)    / mean_bb_befr_mann,    3),
        gpg_bb_unbefr  = round((mean_bb_unbefr_mann   - mean_bb_unbefr_frau)  / mean_bb_unbefr_mann,  3)
      )
    }
    

    n_cores <- max(1, detectCores() - 1)  # einen Kern für OS freihalten
    cat("Parallelisierung mit", n_cores, "Kernen\n")
    
    run_lauf <- function(i, employee_p, salary_p, gender_p, base_year) {
      last <- run_sim(make_structure_params(seed = i), employee_p, salary_p, gender_p)
      last <- last[last$Berichtsjahr == base_year, ]
      data.frame(run = i, extract_kennzahlen(last))
    }
    
    cl <- makeCluster(n_cores)
    clusterExport(cl, varlist = c(
      "run_sim", "make_structure_params", "extract_kennzahlen",
      "update_lbe_status", "update_lfunk", "update_bb",
      "base_year"
    ), envir = environment())
    clusterEvalQ(cl, {
      library(dplyr)
      library(gamlss)
    })
    withProgress(message = "Validierung läuft...", value = 0, {
      
      setProgress(0.1, detail = "Starte aktuelle Einstellungen...")
      results_aktuell <- parLapply(cl, 1:n_runs, run_lauf,
                                   employee_p = employee_p,
                                   salary_p   = salary_p,
                                   gender_p   = gender_p,
                                   base_year  = base_year)
      
      if (isTRUE(input$val_compare_kein_gpg)) {
        setProgress(0.6, detail = "Starte Kein-GPG-Läufe...")
        results_kein <- parLapply(cl, 1:n_runs, run_lauf,
                                  employee_p = employee_p_kein,
                                  salary_p   = salary_p_kein,
                                  gender_p   = gender_p_kein,
                                  base_year  = base_year)
      }
      
      setProgress(0.9, detail = "Berechne Kennzahlen...")
      stopCluster(cl)
      
      df_aktuell <- add_gpg(bind_rows(results_aktuell))
      df_kein    <- if (isTRUE(input$val_compare_kein_gpg)) add_gpg(bind_rows(results_kein)) else NULL
    

    summarise_szenario <- function(df) {
      df %>%
        summarise(across(-run, list(
          mean = ~round(mean(., na.rm = TRUE), 3),
          sd   = ~round(sd(.,   na.rm = TRUE), 3),
          min  = ~round(min(.,  na.rm = TRUE), 3),
          max  = ~round(max(.,  na.rm = TRUE), 3)
        ))) %>%
        pivot_longer(everything(),
                     names_to  = c("variable", "stat"),
                     names_sep = "_(?=[^_]+$)") %>%
        pivot_wider(names_from = stat, values_from = value)
    }
    
    sum_kein <- summarise_szenario(df_kein) %>% rename(mean_kein = mean, sd_kein = sd)
    sum_aktuell <- summarise_szenario(df_aktuell) %>% rename(mean_aktuell = mean, sd_aktuell = sd)
    
    vergleich <- if (isTRUE(input$val_compare_kein_gpg)) {
      sum_kein <- summarise_szenario(df_kein) %>% rename(mean_kein = mean, sd_kein = sd)
      sum_kein %>%
        select(variable, mean_kein, sd_kein) %>%
        left_join(sum_aktuell %>% select(variable, mean_aktuell, sd_aktuell), by = "variable") %>%
        mutate(
          differenz   = round(mean_aktuell - mean_kein, 3),
          richtung_ok = case_when(
            grepl("^gpg_", variable) ~ ifelse(mean_aktuell > mean_kein, "✓ GPG höher", "✗ unerwartet"),
            grepl("frauenanteil_W3|frauenanteil_W2|frauenanteil$", variable) ~
              ifelse(mean_aktuell < mean_kein, "✓ Frauenanteil niedriger", "✗ unerwartet"),
            TRUE ~ ""
          )
        )
    } else {
      sum_aktuell
    }
    
    ttest_mann_frau <- function(df, szenario_label) {
      komponenten <- list(
        "LBe gesamt"     = c("mean_lbe_mann",        "mean_lbe_frau"),
        "LBe befristet"  = c("mean_lbe_befr_mann",    "mean_lbe_befr_frau"),
        "LBe entfristet" = c("mean_lbe_unbefr_mann",  "mean_lbe_unbefr_frau"),
        "LFunk"          = c("mean_lfunk_mann",        "mean_lfunk_frau"),
        "ThirdPartyFunding" = c("mean_tpf_mann",       "mean_tpf_frau"),
        "BB gesamt"      = c("mean_bb_mann",           "mean_bb_frau"),
        "BB befristet"   = c("mean_bb_befr_mann",      "mean_bb_befr_frau"),
        "BB unbefristet" = c("mean_bb_unbefr_mann",    "mean_bb_unbefr_frau"),
        "Kinder (Anzahl)"    = c("mean_kinder_mann",     "mean_kinder_frau"),
        "Kinderlos (Anteil)" = c("anteil_kinderlos_mann", "anteil_kinderlos_frau")
      )
      
      do.call(rbind, lapply(names(komponenten), function(name) {
        v <- komponenten[[name]]
        x <- df[[v[1]]]
        y <- df[[v[2]]]
        tt <- t.test(x, y, paired = T)
        data.frame(
          Szenario    = szenario_label,
          Komponente  = name,
          Mean_Mann   = round(mean(x, na.rm = TRUE), 2),
          Mean_Frau   = round(mean(y, na.rm = TRUE), 2),
          Differenz   = round(mean(x - y, na.rm = TRUE), 2),
          t_Wert      = round(tt$statistic, 3),
          p_Wert      = round(tt$p.value, 4),
          Signifikant = ifelse(tt$p.value < 0.05, "✓", "✗")
        )
      }))
    }
    
    ttest_aktuell <- ttest_mann_frau(df_aktuell, "Aktuell")
    ttest_gesamt  <- if (isTRUE(input$val_compare_kein_gpg)) {
      rbind(ttest_mann_frau(df_kein, "Kein GPG"), ttest_aktuell)
    } else {
      ttest_aktuell
    }
    
    gpg_aktuell <- df_aktuell %>%
      summarise(across(starts_with("gpg_"), ~round(mean(., na.rm = TRUE), 3))) %>%
      pivot_longer(everything(), names_to = "komponente", values_to = "aktuell")
    
    gpg_vergleich <- if (isTRUE(input$val_compare_kein_gpg)) {
      df_kein %>%
        summarise(across(starts_with("gpg_"), ~round(mean(., na.rm = TRUE), 3))) %>%
        pivot_longer(everything(), names_to = "komponente", values_to = "kein_gpg") %>%
        left_join(gpg_aktuell, by = "komponente") %>%
        mutate(
          differenz   = round(aktuell - kein_gpg, 3),
          richtung_ok = ifelse(aktuell > kein_gpg, "✓", "✗ PROBLEM")
        )
    } else {
      gpg_aktuell
    }
    
    list(
      df_kein       = df_kein,
      df_aktuell    = df_aktuell,
      ttest         = ttest_gesamt,
      gpg_vergleich = gpg_vergleich,
      compare       = isTRUE(input$val_compare_kein_gpg)
    )
  })
  })  
    

# Observer ----------------------------------------------------------------


  observeEvent(c(input$prob_female, input$prob_female_W3, input$prob_W3), {
    req(input$prob_female, input$prob_female_W3, input$prob_W3)
    prob_female_W2 <- (input$prob_female/100 - (input$prob_W3/100) * (input$prob_female_W3/100)) / 
      (1 - input$prob_W3/100)
    # Warnung wenn außerhalb 0-1
    if (prob_female_W2 < 0 || prob_female_W2 > 1) {
      showNotification(
        "Ungültige Kombination: Frauenanteil W2 außerhalb 0-100%. Parameter anpassen.",
        type = "warning",
        duration = 5
      )
    }
    prob_female_W2 <- round(pmax(0, pmin(1, prob_female_W2)) * 100)
    updateSliderInput(session, "prob_female_W2", value = prob_female_W2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$year_start, {
    updateNumericInput(session, "year_start_num", value = input$year_start)
  })
  observeEvent(input$year_start_num, {
    updateSliderInput(session, "year_start", value = input$year_start_num)
  })
  
  observeEvent(input$act_year, {
    updateNumericInput(session, "act_year_num", value = input$act_year)
  })
  observeEvent(input$act_year_num, {
    updateSliderInput(session, "act_year", value = input$act_year_num)
  })
  
  observeEvent(c(input$year_start, input$act_year), {
    updateSliderInput(session, "check_year", 
                      min = input$year_start, 
                      max = input$act_year,
                      value = input$act_year)
  })
  
  observeEvent(input$act_year, {
    updateSliderInput(session, "output_year_start",
                      min   = input$year_start,
                      max   = input$act_year,
                      value = min(input$output_year_start, input$act_year))
  })
  observeEvent(input$output_year_start, {
    updateNumericInput(session, "output_year_start_num", value = input$output_year_start)
  })
  
  observeEvent(input$output_year_start_num, {
    updateSliderInput(session, "output_year_start", value = input$output_year_start_num)
  })
  
  observeEvent(input$select_all_personal, {
    if (input$select_all_personal) {
      updateCheckboxGroupInput(session, "cols_personal",
                               selected = c("Geschlecht", "Vergütungsgruppe", "Fachgebiet"))
    } else {
      updateCheckboxGroupInput(session, "cols_personal", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_demo, {
    if (input$select_all_demo) {
      updateCheckboxGroupInput(session, "cols_demo",
                               selected = c("Kinder", "MaritalStatus", "Nationality"))
    } else {
      updateCheckboxGroupInput(session, "cols_demo", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_verguetung, {
    if (input$select_all_verguetung) {
      updateCheckboxGroupInput(session, "cols_verguetung",
                               selected = c("ThirdPartyFunding", "lfunk_euro",
                                            "lbe_befr_euro", "lbe_unbefr_euro", 
                                            "bb_befr_betrag", "bb_unbefr_betrag"))
    } else {
      updateCheckboxGroupInput(session, "cols_verguetung", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_help_lbe, {
    if (input$select_all_help_lbe) {
      updateCheckboxGroupInput(session, "cols_help_lbe",
                               selected = c("lbe_s1_status", "lbe_s1_jahre",
                                            "lbe_s2_status", "lbe_s2_jahre",
                                            "lbe_s3_status", "lbe_s3_jahre",
                                            "lbe_s4_status", "lbe_s4_jahre",
                                            "lbe_befr_gesamt", "lbe_unbefr_gesamt", "lbe_gesamt_euro"))
    } else {
      updateCheckboxGroupInput(session, "cols_help_lbe", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_help_lfunk, {
    if (input$select_all_help_lfunk) {
      updateCheckboxGroupInput(session, "cols_help_lfunk",
                               selected = c("LFunk", "lfunk_jahre"))
    } else {
      updateCheckboxGroupInput(session, "cols_help_lfunk", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_help_bb, {
    if (input$select_all_help_bb) {
      updateCheckboxGroupInput(session, "cols_help_bb",
                               selected = c("bb_befr_jahre", "bb_letzte_vergabe"))
    } else {
      updateCheckboxGroupInput(session, "cols_help_bb", selected = character(0))
    }
  })
  
  observeEvent(input$select_all_help_rest, {
    if (input$select_all_help_rest) {
      updateCheckboxGroupInput(session, "cols_help_rest",
                               selected = c("experience_this_year", "entry_age",
                                            "retirement_ages", "Erfahrung", "Geburtsjahr"))
    } else {
      updateCheckboxGroupInput(session, "cols_help_rest", selected = character(0))
    }
  })
  

# Output ------------------------------------------------------------------

  output$dl_ttest <- downloadHandler(
    filename = function() paste0("val_ttest_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(val_results()$ttest, file, row.names = FALSE)
  )
  
  output$dl_gpg_vergleich <- downloadHandler(
    filename = function() paste0("val_gpg_vergleich_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(val_results()$gpg_vergleich, file, row.names = FALSE)
  )
  
  output$plot_faculty <- renderPlot({
    df <- sim_result()
    
    faculty_year <- df %>%
      group_by(Berichtsjahr, Fachgebiet) %>%
      summarise(n = n(), .groups = "drop")
    
    ggplot(faculty_year, aes(x = Berichtsjahr, y = n, color = Fachgebiet)) +
      geom_line(linewidth = 1) +
      labs(title = "Entwicklung der Fakultätsgrößen",
           x = "Jahr", y = "Anzahl Beschäftigte") +
      theme_minimal()
  })
  output$plot_hist_years <- renderPlot({
    df <- sim_result()
    hist(table(df$EmpNum), breaks=seq(0,50,1),
         main="Histogram of Years Employed",
         xlab="Years Employed", ylab="Frequency")
  })
  
  output$plot_tpf <- renderPlot({
    df <- sim_result()
    ggplot(df, aes(x=ThirdPartyFunding, fill=factor(Geschlecht))) +
      geom_density(alpha=0.5) +
      scale_fill_manual(name="Geschlecht",
                        labels=c("Männer","Frauen"),
                        values=c("orange","lightgrey")) +
      labs(title="Dichteverteilung der Drittmittel",
           x="Drittmittel (in 1.000 Euro)", y="Dichte") +
      theme_minimal()
  })
  
  output$plot_kinder <- renderPlot({
    df <- sim_result()
    ggplot(df, aes(x=Kinder, fill=factor(Geschlecht))) +
      geom_bar(position="dodge") +
      scale_fill_manual(name="Geschlecht",
                        labels=c("Männer","Frauen"),
                        values=c("orange","lightgrey")) +
      labs(title="Verteilung der Kinderzahlen nach Geschlecht",
           x="Anzahl Kinder", y="Anzahl Personen") +
      theme_minimal()
  })
  output$dl_csv <- downloadHandler(
    filename = function() {paste0("GPG_simulation_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")},
    content  = function(file) {write.csv(sim_filtered(), file, row.names=FALSE)}
  )
  output$table_preview <- renderDT({
    sim_filtered()
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  output$table_ttest        <- renderTable({ req(val_results()); val_results()$ttest })
  output$table_gpg_vergleich <- renderTable({ req(val_results()); val_results()$gpg_vergleich })
  output$val_plot_struktur <- renderPlot({
    req(val_results())
    df <- val_results()$df_aktuell %>%
      select(run, frauenanteil, frauenanteil_W3, frauenanteil_W2) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, frauenanteil, frauenanteil_W3, frauenanteil_W2) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario), names_to = "variable", values_to = "wert") %>%
      ggplot(aes(x = wert, fill = szenario)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~variable, scales = "free") +
      scale_fill_manual(values = c("Kein GPG" = "steelblue", "Aktuell" = "tomato")) +
      labs(title = "Frauenanteile", x = NULL, y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  output$val_plot_gpg <- renderPlot({
    req(val_results())
    df <- val_results()$df_aktuell %>%
      select(run, starts_with("gpg_")) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, starts_with("gpg_")) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario), names_to = "komponente", values_to = "gpg") %>%
      mutate(komponente = gsub("gpg_", "", komponente)) %>%
      ggplot(aes(x = gpg, fill = szenario)) +
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      facet_wrap(~komponente, scales = "free") +
      scale_fill_manual(values = c("Kein GPG" = "steelblue", "Aktuell" = "tomato")) +
      labs(title = "GPG je Komponente", x = "GPG = (Mann − Frau) / Mann",
           y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  output$val_plot_comp <- renderPlot({
    req(val_results())
    comp_vars <- c("mean_lbe_mann", "mean_lbe_frau", "mean_lfunk_mann", "mean_lfunk_frau",
                   "mean_tpf_mann", "mean_tpf_frau", "mean_bb_mann",   "mean_bb_frau")
    df <- val_results()$df_aktuell %>%
      select(run, all_of(comp_vars)) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, all_of(comp_vars)) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario),
                   names_to = c("komponente", "geschlecht"),
                   names_pattern = "mean_(.+)_(mann|frau)") %>%
      mutate(
        geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
        komponente = factor(komponente, levels = c("lbe","lfunk","tpf","bb"),
                            labels = c("LBe","LFunk","TPF","BB")),
        gruppe = paste(szenario, geschlecht)
      ) %>%
      ggplot(aes(x = value, fill = gruppe)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~komponente, scales = "free") +
      scale_fill_manual(values = c(
        "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
        "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
      )) +
      labs(title = "Vergütungskomponenten Mann vs. Frau",
           x = "EUR", y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  output$val_plot_lbe <- renderPlot({
    req(val_results())
    lbe_vars <- c("mean_lbe_befr_stufen_mann", "mean_lbe_befr_stufen_frau",
                  "mean_lbe_unbefr_stufen_mann", "mean_lbe_unbefr_stufen_frau")
    df <- val_results()$df_aktuell %>%
      select(run, all_of(lbe_vars)) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, all_of(lbe_vars)) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario),
                   names_to = c("typ", "geschlecht"),
                   names_pattern = "mean_lbe_(.+)_stufen_(mann|frau)") %>%
      mutate(
        geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
        typ        = factor(typ, levels = c("befr","unbefr"), labels = c("befristet","entfristet")),
        gruppe     = paste(szenario, geschlecht)
      ) %>%
      ggplot(aes(x = value, fill = gruppe)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~typ, scales = "free") +
      scale_fill_manual(values = c(
        "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
        "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
      )) +
      labs(title = "Ø LBe-Stufen", x = "Ø Stufen", y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  output$val_plot_anteile <- renderPlot({
    req(val_results())
    anteil_vars <- c("anteil_lfunk_mann", "anteil_lfunk_frau",
                     "anteil_bb_mann",    "anteil_bb_frau")
    df <- val_results()$df_aktuell %>%
      select(run, all_of(anteil_vars)) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, all_of(anteil_vars)) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario),
                   names_to = c("komponente", "geschlecht"),
                   names_pattern = "anteil_(.+)_(mann|frau)") %>%
      mutate(
        geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
        komponente = factor(komponente, levels = c("lfunk","bb"), labels = c("LFunk","BB")),
        gruppe     = paste(szenario, geschlecht)
      ) %>%
      ggplot(aes(x = value, fill = gruppe)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~komponente, scales = "free") +
      scale_fill_manual(values = c(
        "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
        "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
      )) +
      labs(title = "Anteile LFunk & BB", x = "Anteil", y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  output$val_plot_demo <- renderPlot({
    req(val_results())
    demo_vars <- c("mean_kinder_mann", "mean_kinder_frau",
                   "anteil_kinderlos_mann", "anteil_kinderlos_frau")
    df <- val_results()$df_aktuell %>%
      select(run, all_of(demo_vars)) %>%
      mutate(szenario = "Aktuell")
    if (val_results()$compare) {
      df <- bind_rows(
        val_results()$df_kein %>%
          select(run, all_of(demo_vars)) %>%
          mutate(szenario = "Kein GPG"),
        df
      )
    }
    df %>%
      pivot_longer(-c(run, szenario),
                   names_to  = c("komponente", "geschlecht"),
                   names_pattern = "(.+)_(mann|frau)") %>%
      mutate(
        geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
        komponente = factor(komponente,
                            levels = c("mean_kinder", "anteil_kinderlos"),
                            labels = c("Ø Kinder", "Anteil Kinderlos")),
        gruppe = paste(szenario, geschlecht)
      ) %>%
      ggplot(aes(x = value, fill = gruppe)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~komponente, scales = "free") +
      scale_fill_manual(values = c(
        "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
        "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
      )) +
      labs(title = "Kinder", x = NULL, y = "Dichte", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  
  output$dl_plot_struktur <- downloadHandler(
    filename = function() paste0("val_plot_struktur_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      df <- val_results()$df_aktuell %>%
        select(run, frauenanteil, frauenanteil_W3, frauenanteil_W2) %>%
        mutate(szenario = "Aktuell")
      if (val_results()$compare) {
        df <- bind_rows(
          val_results()$df_kein %>%
            select(run, frauenanteil, frauenanteil_W3, frauenanteil_W2) %>%
            mutate(szenario = "Kein GPG"),
          df
        )
      }
      p <- df %>%
        pivot_longer(-c(run, szenario), names_to = "variable", values_to = "wert") %>%
        ggplot(aes(x = wert, fill = szenario)) +
        geom_density(alpha = 0.5) +
        facet_wrap(~variable, scales = "free") +
        scale_fill_manual(values = c("Kein GPG" = "steelblue", "Aktuell" = "tomato")) +
        labs(title = "Frauenanteile", x = NULL, y = "Dichte", fill = NULL) +
        theme_minimal(base_size = 13)
      ggsave(file, plot = p, width = 10, height = 5, dpi = 150)
    }
  )
  
  output$dl_plot_gpg <- downloadHandler(
    filename = function() paste0("val_plot_gpg_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      df <- val_results()$df_aktuell %>%
        select(run, starts_with("gpg_")) %>%
        mutate(szenario = "Aktuell")
      if (val_results()$compare) {
        df <- bind_rows(
          val_results()$df_kein %>% select(run, starts_with("gpg_")) %>% mutate(szenario = "Kein GPG"),
          df
        )
      }
      p <- df %>%
        pivot_longer(-c(run, szenario), names_to = "komponente", values_to = "gpg") %>%
        mutate(komponente = gsub("gpg_", "", komponente)) %>%
        ggplot(aes(x = gpg, fill = szenario)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        facet_wrap(~komponente, scales = "free") +
        scale_fill_manual(values = c("Kein GPG" = "steelblue", "Aktuell" = "tomato")) +
        labs(title = "GPG je Komponente", x = "GPG = (Mann − Frau) / Mann", y = "Dichte", fill = NULL) +
        theme_minimal(base_size = 13)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150)
    }
  )
  
  output$dl_plot_comp <- downloadHandler(
    filename = function() paste0("val_plot_comp_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      comp_vars <- c("mean_lbe_mann","mean_lbe_frau","mean_lfunk_mann","mean_lfunk_frau",
                     "mean_tpf_mann","mean_tpf_frau","mean_bb_mann","mean_bb_frau")
      df <- val_results()$df_aktuell %>% select(run, all_of(comp_vars)) %>% mutate(szenario = "Aktuell")
      if (val_results()$compare) {
        df <- bind_rows(
          val_results()$df_kein %>% select(run, all_of(comp_vars)) %>% mutate(szenario = "Kein GPG"),
          df
        )
      }
      p <- df %>%
        pivot_longer(-c(run, szenario), names_to = c("komponente","geschlecht"),
                     names_pattern = "mean_(.+)_(mann|frau)") %>%
        mutate(
          geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
          komponente = factor(komponente, levels = c("lbe","lfunk","tpf","bb"),
                              labels = c("LBe","LFunk","TPF","BB")),
          gruppe = paste(szenario, geschlecht)
        ) %>%
        ggplot(aes(x = value, fill = gruppe)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~komponente, scales = "free") +
        scale_fill_manual(values = c(
          "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
          "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
        )) +
        labs(title = "Vergütungskomponenten Mann vs. Frau", x = "EUR", y = "Dichte", fill = NULL) +
        theme_minimal(base_size = 13)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150)
    }
  )
  
  output$dl_plot_lbe <- downloadHandler(
    filename = function() paste0("val_plot_lbe_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      lbe_vars <- c("mean_lbe_befr_stufen_mann","mean_lbe_befr_stufen_frau",
                    "mean_lbe_unbefr_stufen_mann","mean_lbe_unbefr_stufen_frau")
      df <- val_results()$df_aktuell %>% select(run, all_of(lbe_vars)) %>% mutate(szenario = "Aktuell")
      if (val_results()$compare) {
        df <- bind_rows(
          val_results()$df_kein %>% select(run, all_of(lbe_vars)) %>% mutate(szenario = "Kein GPG"),
          df
        )
      }
      p <- df %>%
        pivot_longer(-c(run, szenario), names_to = c("typ","geschlecht"),
                     names_pattern = "mean_lbe_(.+)_stufen_(mann|frau)") %>%
        mutate(
          geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
          typ        = factor(typ, levels = c("befr","unbefr"), labels = c("befristet","entfristet")),
          gruppe     = paste(szenario, geschlecht)
        ) %>%
        ggplot(aes(x = value, fill = gruppe)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~typ, scales = "free") +
        scale_fill_manual(values = c(
          "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
          "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
        )) +
        labs(title = "Ø LBe-Stufen", x = "Ø Stufen", y = "Dichte", fill = NULL) +
        theme_minimal(base_size = 13)
      ggsave(file, plot = p, width = 10, height = 5, dpi = 150)
    }
  )
  
  output$dl_plot_anteile <- downloadHandler(
    filename = function() paste0("val_plot_anteile_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      anteil_vars <- c("anteil_lfunk_mann","anteil_lfunk_frau","anteil_bb_mann","anteil_bb_frau")
      df <- val_results()$df_aktuell %>% select(run, all_of(anteil_vars)) %>% mutate(szenario = "Aktuell")
      if (val_results()$compare) {
        df <- bind_rows(
          val_results()$df_kein %>% select(run, all_of(anteil_vars)) %>% mutate(szenario = "Kein GPG"),
          df
        )
      }
      p <- df %>%
        pivot_longer(-c(run, szenario), names_to = c("komponente","geschlecht"),
                     names_pattern = "anteil_(.+)_(mann|frau)") %>%
        mutate(
          geschlecht = factor(geschlecht, levels = c("mann","frau"), labels = c("Mann","Frau")),
          komponente = factor(komponente, levels = c("lfunk","bb"), labels = c("LFunk","BB")),
          gruppe     = paste(szenario, geschlecht)
        ) %>%
        ggplot(aes(x = value, fill = gruppe)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~komponente, scales = "free") +
        scale_fill_manual(values = c(
          "Kein GPG Mann" = "steelblue", "Kein GPG Frau" = "lightblue",
          "Aktuell Mann"  = "tomato",    "Aktuell Frau"  = "salmon"
        )) +
        labs(title = "Anteile LFunk & BB", x = "Anteil", y = "Dichte", fill = NULL) +
        theme_minimal(base_size = 13)
      ggsave(file, plot = p, width = 10, height = 5, dpi = 150)
    }
  )
  output$dl_plot_demo <- downloadHandler(
    filename = function() paste0("val_plot_kinder_", Sys.Date(), ".png"),
    content  = function(file) {
      req(val_results())
      ggsave(file, plot = build_plot_demo(val_results()), width = 10, height = 5, dpi = 150)
    }
  )

  # Gender Check ----------------------------------------------------------
  
  observeEvent(input$check_year, {
    updateNumericInput(session, "check_year_num", value = input$check_year)
  })
  
  observeEvent(input$check_year_num, {
    updateSliderInput(session, "check_year", value = input$check_year_num)
  })
  
  output$table_gender_check <- renderTable({
    df <- sim_result()
    last_year <- df[df$Berichtsjahr == input$check_year, ]
    
    variablen <- list(
      "LBe Gesamt (€)"     = "lbe_gesamt_euro",
      "LFunk (€)"          = "lfunk_euro",
      "BB Befristet (€)"   = "bb_befr_betrag",
      "BB Unbefristet (€)" = "bb_unbefr_betrag",
      "Drittmittel"        = "ThirdPartyFunding"
    )
    
    results <- lapply(names(variablen), function(name) {
      var <- variablen[[name]]
      maenner <- last_year[[var]][last_year$Geschlecht == 1]
      frauen  <- last_year[[var]][last_year$Geschlecht == 2]
      test    <- t.test(maenner, frauen)
      
      data.frame(
        Variable     = name,
        Mean_Männer  = round(mean(maenner), 2),
        Mean_Frauen  = round(mean(frauen), 2),
        Differenz    = round(mean(maenner) - mean(frauen), 2),
        p_Wert       = round(test$p.value, 4),
        Signifikant  = ifelse(test$p.value < 0.05, "✓", "")
      )
    })
    
    do.call(rbind, results)
  })
}



shinyApp(ui, server)

