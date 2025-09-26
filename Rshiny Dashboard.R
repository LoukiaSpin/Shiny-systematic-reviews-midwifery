### Example of Rshiny dashboard

## Load libraries
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(ggiraph)
library(readxl)
library(writexl)
library(reshape2)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(shinyWidgets)
library(fresh)
library(tidytext)
library(forcats)
library(markdown)
library(ggimage)


## Load data from local file
# Load AMSTAR, author location, reporting and methods transparency
data_sr <- read_excel("data/systematic reviews & included trials.xlsx", sheet = "systematic reviews")[, -1] 

# Load searched databases
databases <- read_excel("data/systematic reviews & included trials.xlsx", sheet = 2, na = "NA")

# Load participants characteristics (inclusion & exclusion criteria)
data_partic <- read_excel("data/systematic reviews & included trials.xlsx", sheet = 3, na = "NA")

# Load compared intervention characteristics 
data_interv <- read_excel("data/systematic reviews & included trials.xlsx", sheet = 4, na = "NA")

# Load investigated outcomes 
data_outcome <- read_excel("data/systematic reviews & included trials.xlsx", sheet = 5, na = "NA")


## Customising the style of the app (Hebammen Logo: Light blue is #89ccc4, Teal Green is #00847e, #00817b, #00807a, #007f79, #00837d, #00827c, Orange is #f08300, #ef7e00)
my_theme = create_theme(
  adminlte_color(
    light_blue = "#00847e" # "#008080"
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Research Fingerprint"),
  dashboardSidebar(  # The left panel
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),  # A few words about the app
      menuItem("Quality summary", tabName = "amstar_tab", icon = icon("dashboard")),
      menuItem("Map", tabName = "map_tab", icon = icon("globe")),
      menuItem("AMSTAR 2", tabName = "amstarreview_tab", icon = icon("ranking-star")),
      menuItem("PICO features", tabName = "pico_tab", icon = icon("magnifying-glass-chart"),
               menuSubItem("Features summary", tabName = "pico_summary"),
               menuSubItem("Participant characteristics", tabName = "participant"),
               menuSubItem("Intervention characteristics", tabName = "interventions"),
               menuSubItem("Outcomes investigated", tabName = "outcomes")),
      menuItem("Systematic review collection", tabName = "review_tab", icon = icon("th")),
      sliderTextInput(
        inputId = "year_range",
        label = "Select Year Range:",
        choices = sort(unique(data_sr$Year)),
        selected = c(min(data_sr$Year), max(data_sr$Year)),
        grid = TRUE, 
        dragRange = TRUE
      )#,
      #div(textInput("search",label = NULL, placeholder = "Search..."),  # Search box
      #    class = "custom-search"
      #)
    ), 
    div( # Adding logo
      img(src = "Logo_Hebammenwissenschaft.png", height = "85px"),  # put your logo in www/logo.png
      style = "position: absolute; bottom: 30px; left: 8px;"
    )
  ),
  dashboardBody(  # The right panel
    use_theme(my_theme),
    tags$head(
      tags$style(HTML("
        .custom-search .form-control {
         color: #c7ece7 !important;
         border-color: #89ccc4 !important; 
         background-color: #00847e  !important;
         border-radius: 5px;
         padding: 5px;
        }
        .custom-search .form-control::placeholder {
         color: #89ccc4;
        }
        .skin-blue .main-sidebar {
         background-color: #89ccc4 !important;  
         font-weight: bold;
        }
        .skin-blue .main-sidebar .sidebar a {
         color: #00847e !important;  
        }
        .js-irs-0 .irs-bar {
         background-color: #00847e;
         border-top-color: #00847e;
         border-bottom-color: #00847e;
        }
        .js-irs-0 .irs-grid-text {
         font-family: 'arial'; 
         color: #00847e; 
         font-size: 11px;
        }
        .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-single {
         background-color: #00847e;
        }
        .js-irs-0 .irs-handle {
         background-color: #f08300;
         border-color: #f08300;
        }
        .skin-blue .main-sidebar .control-label {
        color: #00847e !important;
        font-family: 'Arial';
        }
        footer {
        flex-shrink: 0;
        text-align: center;
        color: #777;
        padding: 10px;
        font-size: 12px;
        border-top: 1px solid #ddd;
        }
      "))
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              h2(""),
              includeMarkdown("About_text.md")
      ),
      # Second tab content
      tabItem(tabName = "amstar_tab",
              h2("Research quality of systematic reviews on labour duration"),
              fluidRow(
                box(title = "AMSTAR 2 overall confidence", status = "primary", solidHeader = TRUE, girafeOutput("plotamstar", width = "100%", height = "350px")),
                box(title = "Reporting & methodology transparency", status = "primary", solidHeader = TRUE, girafeOutput("plotvarious", width = "100%", height = "350px")),
                ),
              fluidRow(
                box(title = "AMSTAR 2 item ratings", status = "primary", solidHeader = TRUE, tags$br(), girafeOutput("plotdomains", width = "100%", height = "380px")),
                box(title = "Databases for literature searches", status = "primary", solidHeader = TRUE, girafeOutput("plotdatabases", width = "100%", height = "400px"))
              )
              ),
      # Third tab content
      tabItem(tabName = "map_tab",
              h2("Geographic location of authors"),
              leafletOutput("map", height = "1200px")
              ),
      # Fourth tab content
      tabItem(tabName = "amstarreview_tab",
              h2("AMSTAR 2 detailed assessment"),
              girafeOutput("amstarreview")
              ),
      # Fifth tab content
      tabItem(tabName = "pico_summary", 
              h3("PICO features summary"),
              fluidRow(
                box(title = "Inclusion & exclusion criteria", status = "primary", solidHeader = TRUE, girafeOutput("plotparticipant", width = "100%", height = "400px")),
                box(title = "Maternal & neonatal outcomes", status = "primary", solidHeader = TRUE, girafeOutput("plotmaternalneonatal", width = "100%", height = "400px"))
              ),
              fluidRow(
                box(title = "Compared interventions characteristics", status = "primary", solidHeader = TRUE, girafeOutput("plotintervention", width = "100%", height = "350px")),
                box(title = "Labour duration outcomes", status = "primary", solidHeader = TRUE, girafeOutput("plotlabourduration", width = "100%", height = "350px"))
              )
      ),
      tabItem(tabName = "participant",   
              h3("Inclusion & exclusion criteria: were they reported?"),
              girafeOutput("participantbubble")
      ),
      tabItem(tabName = "interventions",   
              h3("Characteristics of compared interventions: were they reported?"),
              girafeOutput("interventionbubble")
      ),
      tabItem(tabName = "outcomes",   
              h3("Labour duration, maternal & neonatal outcomes: were they reported?"),
              girafeOutput("outcomebubble")
      ),
      # Sixth tab content
      tabItem(tabName = "review_tab",
              h2("Systematic reviews on labour duration"),
              downloadButton("downloadData", "Download Excel"),
              br(), br(),
              DTOutput("rawDataTable")
              )
      ),
    tags$footer(
      HTML(sprintf("Developed by %s · %s", "Loukia Spineli", sprintf(format(Sys.Date(), "%Y"))
                   )
           )
      ),
    )  # dashboardBody
)

server <- function(input, output, session) { 
  
  #*****************************************************************************
  #*
  #*             Data preparation to create the plots/figures below      
  #*                (All data are filtered by publication year)                  
  #*
  #*****************************************************************************

  ## Filter data by publication year the 'data_sr' dataset
  filtered_sr_count <- reactive({
    data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ]
  })
  
  ## amstar_tab (amstar_tab 1): AMSTAR overall confidence ----
  amstar_tab <- reactive({
    # First, filter data by year range
    filtered_data <- data_sr[data_sr$Year >= input$year_range[1] &
                               data_sr$Year <= input$year_range[2], ]
    
    # Indicate all possible levels of AMSTAR 
    filtered_data$AMSTAR <- factor(filtered_data$AMSTAR, levels = c("High", "Moderate", "Low", "Critically low"))
    
    # Create frequency table and proportion
    tab <- table(filtered_data$AMSTAR)
    prop <- prop.table(tab)
    
    # Combine into data frame
    data.frame(AMSTAR = names(tab),
               Freq = as.vector(tab),
               perc = as.vector(prop))
  })
  
  ## amstardomains_tab (amstar_tab 2): Frequency of each AMSTAR items' level (yes, partly yes, no) ----
  amstardomains_tab <- reactive({
    # Filter by selected year range
    data_various <- melt(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], c(1, seq(17, 47, 2))], id.vars = c("Authors"))[, -1] %>%
      group_by(variable, value) %>% 
      summarise(count = n(), .groups = "drop") %>% 
      group_by(variable) %>%
      mutate(perc = count / sum(count))
    
    # Indicate the critical domains
    data_various$critical <- ifelse(is.element(data_various$variable, c(paste("Domain", c(2, 4, 7, 9, 11, 13, 15)))), "Critical", "Non-critical") 
    
    # Rename the domains (giving a very brief title)
    levels(data_various$variable) <- c("1. PICO  features reported", "2. Protocol and documented deviations", "3. Selected study design explained", "4. Comprehensive search strategy",
                                       "5. Study selection in duplicate", "6. Data extraction in duplicate", "7. List of excluded studies with justifications", "8. Included studies adequately described",
                                       "9. Satisfactory tool for assessing risk of bias", "10. Funding sources of included studies", "11. Appropriate statistical methods", "12. Assessing risk of bias impact on results",
                                       "13. Risk of bias impact on conclusions", "14. Heterogeneity satisfactorily discussed", "15. Publication bias adequately investigated", "16. Conflict of interest disclosed"); data_various
  })
    
  ## various_tab (amstar_tab 3): Reporting and methodology transparency ----
  various_tab <- reactive({
    # Filter by selected year range
    data_various <- melt(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], c(1, 7:10)], id.vars = c("Authors"))
    
    # Clean up values
    data_various$value <- ifelse(data_various$value %in% c("Not reported", "Not mentioned", "CONSORT 2020", "No"), "No", "Yes")
    
    # Rename variable levels
    levels(data_various$variable)[1:2] <- c("Protocol available", "PRISMA mentioned") 
    
    # Tabulation
    tab <- as.data.frame(melt(table(data_various$variable, data_various$value)))
    
    # Add proportion column (denominator = number of filtered rows)
    tab$perc <- tab$value / nrow(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ]); tab
  })
  
  ## databases_data (amstar_tab 4): Databases for literature searches ----
  databases_data <- reactive({
    # Keep the necessary columns and filter by year range
    databases_short <- databases[databases$Year >= input$year_range[1] & databases$Year <= input$year_range[2], -3]
    
    # Calculate the number of databases per systematic review
    databases_new <- data.frame(review = databases_short[, 1],
                                n = rowSums(!is.na(databases_short[, 3:15])))
    
    # Turn *databases* into long format 
    databases_long <- data.frame(year = rep(databases_new[, 1], databases_new[, 2]),
                                 databases = na.omit(unname(unlist(c(databases_short[, c(-1, -2)])))))
    #databases_long$databases <- ifelse(is.element(databases_long$databases, c("USDD", "IranDoc", "NDLTD Taiwan", "CNKI", "SID", "Airiti Library")), "Others", databases_long$databases)  
    
    # Set of grey literature sources
    grey_sources <- c("Reference list", "Open Grey", "ProQuest", "Google Scholar", "ClinicalTrials.gov", "WHO", "Others",
                      "NY Academy of Medicine", "PROSPERO", "Pharmaceutical companies", "Conference proceedings", "Field experts",
                      "Virtual Health Library", "CENTRAL", "USDD", "IranDoc", "NDLTD Taiwan", "CNKI", "SID", "Airiti Library")
    
    # Data-frame with unique databases: counts
    databases_long_new <- data.frame(table(na.omit(databases_long$databases)))
    colnames(databases_long_new) <- c("values", "n")
    
    # Data-frame with unique databases: percentages
    databases_long_new$perc<- databases_long_new$n / dim(databases_short)[1]
    
    # Include an indicator on whether a database is grey literature source or not
    databases_long_new$grey <- ifelse(is.element(databases_long_new$values, grey_sources), "Yes",  "No") 
    
    # Sort databases in decreasing order of their frequency
    databases_long_new <- databases_long_new %>%
      group_by(grey) %>%
      arrange(desc(n), .by_group = TRUE) %>%
      mutate(values_ord = fct_reorder(values, n, .desc = TRUE)) %>%
      ungroup()
    
    # Preparing for circular plot. Source: https://r-graph-gallery.com/297-circular-barplot-with-groups.html 
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 1
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(databases_long_new$grey), ncol(databases_long_new)))
    colnames(to_add) <- colnames(databases_long_new)
    to_add$grey <- rep(levels(databases_long_new$grey), each = empty_bar)
    databases_long_new <- rbind(databases_long_new, to_add)
    databases_long_new <- databases_long_new %>% arrange(grey)
    databases_long_new$id <- seq(1, nrow(databases_long_new))
    databases_long_new
  })
  
  ## location_data (map_tab): Geographic location of authors ----
  location_data <- reactive({
    # Considering the year
    geo_location <- subset(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ], select = c("Iran", "Egypt", "Italy", "USA", "Taiwan", "South Africa"))
    
    # Sum across rows
    geo_location_sum <- ifelse(colSums(geo_location) < 1, NA, colSums(geo_location))
    names(geo_location_sum)[4] <- "United States of America"
    
    # Turn 'geo_location' into 2x2 data.frame
    tab <- data.frame(country = names(geo_location_sum), melt(geo_location_sum)); tab
  })
  
  ## participant_data (pico_summary 1, participants): PICO features summary & participants ----
  participant_summary <- reactive({
    # Filtering by year
    partic_filtered <- data_partic[data_partic$Year >= input$year_range[1] & data_partic$Year <= input$year_range[2], ]
    
    # First author with publication year
    partic_filtered$`First author` <- paste(partic_filtered$`First author`, partic_filtered$Year)
    
    # Calculate the absolute frequency of each characteristic
    partic_frequency <- colSums(!is.na(partic_filtered[, -c(1, 2)]))
    
    # Data-frame
    partic_long <- data.frame(review = unname(unlist(rep(partic_filtered[, 1], length(colnames(partic_filtered)[-c(1, 2)])))),
                              variable = rep(colnames(partic_filtered)[-c(1, 2)], each = dim(partic_filtered)[1]),
                              values = unname(unlist(c(partic_filtered[, -c(1, 2)]))),
                              freq = rep(partic_frequency, each = dim(partic_filtered)[1]),
                              perc = rep(partic_frequency, each = dim(partic_filtered)[1]) / dim(partic_filtered)[1])
    
    # Include an indicator on whether a characteristic is an inclusion criterion
    partic_long$inclusion <- 
      ifelse(!is.element(partic_long$variable, c("(Pre)eclampsia", "Placenta abruption", "Placenta previa", "Other placental conditions", 
                                                 "Previous uterine surgery", "Vaginal delivery contraindications", "Previous caesarian delivery", 
                                                 "Pregnancy-induced illness", "Chronic illness")), "Inclusion criteria", "Exclusion criteria")
    
    # Shorten the name of some variables
    partic_long_new <- partic_long %>%
      mutate(variable_new = variable) %>%    # Copy 'variable' into 'variable_new'
      mutate(variable_new = recode(variable_new, 
                                   #"Singleton pregnancy" = "Singleton\npregnancy",
                                   "Gestational age" = "Gestational\nage",
                                   #"Vertex positioning" = "Vertex\npositioning",
                                   #"Membranes intact" = "Membranes\nintact",
                                   #"Uterine contractions" = "Uterine\ncontractions",
                                   #"Cervical dilatation" = "Cervical\ndilatation",
                                   "Labour management" = "Labour\nmanagement",
                                   #"Pregnancy risk" = "Pregnancy\nrisk",
                                   "Placenta abruption" ="Placenta\nabruption",
                                   "Previous uterine surgery" = "Previous uterine\nsurgery",
                                   "Vaginal delivery contraindications" = "Vaginal delivery\ncontraindications",
                                   "Previous caesarian delivery" = "Previous caesarian\ndelivery",
                                   "Pregnancy-induced illness" = "Pregnancy-induced\nillness",
                                   "Other placental conditions" = "Other placental\nconditions"),)
    
    # Include an indicator on whether a characteristic was reported or not
    partic_long_new$indicator <- ifelse(is.na(partic_long_new$values), "No", "Yes")
    
    # Replace NA in 'values' with no (i.e., not reported)
    partic_long_new$values_new <- ifelse(is.na(partic_long_new$values), "No", partic_long_new$values); partic_long_new
  })
  
  ## intervention_data (pico_summary 2, interventions): PICO features summary & interventions ----
  intervention_summary <- reactive({
    # Filtering by year
    interv_filtered <- data_interv[data_interv$Year >= input$year_range[1] & data_interv$Year <= input$year_range[2], ]
    
    # First author with publication year
    interv_filtered$`First author` <- paste(interv_filtered$`First author`, interv_filtered$Year)
    
    # Calculate the absolute frequency of each characteristic
    interv_frequency <- colSums(!is.na(interv_filtered[, -c(1, 2)]))
    
    # Data-frame
    interv_long <- data.frame(review = unname(unlist(rep(interv_filtered[, 1], length(colnames(interv_filtered)[-c(1, 2)])))),
                              variable = rep(colnames(interv_filtered)[-c(1, 2)], each = dim(interv_filtered)[1]),
                              values = unname(unlist(c(interv_filtered[, -c(1, 2)]))),
                              freq = rep(interv_frequency, each = dim(interv_filtered)[1]),
                              perc = rep(interv_frequency, each = dim(interv_filtered)[1]) / dim(interv_filtered)[1])
    
    # Include an indicator on whether a characteristic is intervention or comparison
    interv_long$intervention <- 
      ifelse(!is.element(interv_long$variable, c("Placebo",	"No treatment", "Other drug",	"Admin_route_comp",	
                                                 "Dose_comp",	"Dose_frequency_comp")), "Intervention", "Comparator")
    
    # Shorten the name of some variables
    interv_long_new <- interv_long %>%
      mutate(variable  = recode(variable , 
                                "Admin_route_int" = "Administration\nroute",
                                "Dose_int" = "Dose",
                                "Dose_number_int" = "Number of doses",
                                "Dose_frequency_int" = "Dose frequency",
                                "HBB_allergy_int" = "Intervention\nallergies",
                                "Admin_route_comp" = "Administration\nroute",
                                "Dose_comp" = "Dose",
                                "Dose_frequency_comp" = "Dose frequency"))
    
    # Include an indicator on whether a characteristic was reported or not
    interv_long_new$indicator <- ifelse(is.na(interv_long_new$values), "No", "Yes")
    
    # Replace NA in 'values' with no (i.e., not reported)
    interv_long_new$values_new <- ifelse(is.na(interv_long_new$values), "No", interv_long_new$values); interv_long_new
  })
  
  ## outcome_data (pico_summary 3 & 4, outcomes): PICO features summary & outcomes ----
  outcome_summary <- reactive({
    # Filtering by year
    outcome_filtered <- data_outcome[data_outcome$Year >= input$year_range[1] & data_outcome$Year <= input$year_range[2], ]
    
    # First author with publication year
    outcome_filtered$`First author` <- paste(outcome_filtered$`First author`, outcome_filtered$Year)
    
    # Calculate the absolute frequency of each characteristic
    outcome_frequency <- colSums(!is.na(outcome_filtered[, -c(1, 2)]))
    
    # Data-frame
    outcome_long <- data.frame(review = unname(unlist(rep(outcome_filtered[, 1], length(colnames(outcome_filtered)[-c(1, 2)])))),
                              variable = rep(colnames(outcome_filtered)[-c(1, 2)], each = dim(outcome_filtered)[1]),
                              values = unname(unlist(c(outcome_filtered[, -c(1, 2)]))),
                              freq = rep(outcome_frequency, each = dim(outcome_filtered)[1]),
                              perc = rep(outcome_frequency, each = dim(outcome_filtered)[1]) / dim(outcome_filtered)[1])
    
    # Include an indicator on whether a characteristic is duration, maternal or neonatal
    outcome_long$outcome <- ifelse(is.element(outcome_long$variable, c("First stage duration",	"Active phase duration", "Second stage duration",	
                                                                       "Third stage duration",	"Total duration")), "Labour duration", 
                                   ifelse(is.element(outcome_long$variable, c("Apgar score", "NICU admission", "Resuscitation need", "Fetal distress", 
                                                                              "Fetal bradycardia", "Fetal tachycardia", "Meconium-stained liquor")), "Neonatal outcomes", "Maternal outcomes"))
    # Shorten the name of some variables
    #outcome_long_new <- outcome_long %>%
    #  mutate(variable  = recode(variable , 
    #                            "Postpartum hemorrhage rate" = "Postpartum\nhaemorrhage",
    #                            "Blood loss after delivery" = "Blood loss\nafter delivery",
    #                            "Blood transfusion" = "Blood\ntransfusion",
    #                            "Maternal tachycardia" = "Maternal\ntachycardia",
    #                            "Maternal mouth dryness" = "Mouth\ndryness",
    #                            "Maternal headache" = "Maternal\nheadache",
    #                            "Maternal nausea" = "Maternal\nnausea",
    #                            "Maternal vomiting" = "Maternal\nvomiting",
    #                            "Maternal dizziness" = "Maternal\ndizziness",
    #                            "Maternal giddiness" = "Maternal\ngiddiness",
    #                            "Maternal face flushing" = "Maternal\nface flushing",
    #                            "Urinary retention" = "Urinary\nretention",
    #                            "Cervical laceration" = "Cervical\nlaceration"))
    
    # Include an indicator on whether a characteristic was reported or not
    outcome_long$indicator <- ifelse(is.na(outcome_long$values), "No", "Yes")
    
    # Replace NA in 'values' with no outcome_long_new
    outcome_long$values_new <- ifelse(is.na(outcome_long$values), "No", outcome_long$values); outcome_long
  })
  
  ## rawdata (review_tab): Dataset in Table (Systematic review collection) ----
  rawdata <- reactive({
    # Filter or subset if needed (example: by year slider)
    data_filtered <- data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ]
    
    # Return the filtered dataset (subset columns)
    subset(data_filtered, select = c("Authors", "Title", "Journal", "Year", "Access", 
                                     "AMSTAR", "Protocol", "PRISMA", 
                                     "Risk of bias assessed", "GRADE applied"))
  })

  
  #*****************************************************************************
  #*
  #*                   Creating the figures for the dashboard                                          
  #*
  #*****************************************************************************

  ## amstar_tab (amstar_tab 1): Bar plot with AMSTAR overall confidence ----
  output$plotamstar <- renderGirafe({
    # Basic plot
    interactive_amstar <- ggplot(amstar_tab(),
                                 aes(x = factor(AMSTAR, levels = c("High", "Moderate", "Low", "Critically low")),
                                     y = Freq,
                                     fill = factor(AMSTAR, levels = c("High", "Moderate", "Low", "Critically low")),
                                     tooltip = paste0(Freq, " (", round(perc * 100), "%)"), 
                                     data_id = AMSTAR)) +
      geom_bar_interactive(stat = "identity",
                           show.legend = TRUE,
                           hover_nearest = TRUE) +  
      scale_fill_manual(breaks = c("High", "Moderate", "Low", "Critically low"),
                        values = c("#00847e", "#FFC20A", "#D55E00", "#DC3220"),
                        limits = c("High", "Moderate", "Low", "Critically low"),
                        drop = FALSE) +
      labs(x = "",
           y = "Number of systematic reviews",
           fill = "") + 
      ylim(0, dim(filtered_sr_count())[1]) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            legend.position = "none",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_amstar,
           width_svg = 9.5,
           height_svg = 5,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)) )
  })
  
  ## amstardomains_tab (amstar_tab 2): Bar plot on AMSTAR domains with level frequency ----
  output$plotdomains <- renderGirafe({
    # Basic plot
    interactive_various <- ggplot(amstardomains_tab(),
                                  aes(x = factor(variable, levels = rev(levels(variable))),
                                      y = count,
                                      fill = factor(value, levels = rev(c("Yes", "Partial yes", "No"))),
                                      tooltip = paste0(value, ": ", count, " (", round(perc * 100), "%)"), 
                                      data_id = value)) +
      geom_bar_interactive(aes(alpha = critical),
                           stat = "identity",
                           position = "stack",
                           show.legend = TRUE) +     
      scale_fill_manual(breaks = c("Yes", "Partial yes", "No"),
                        values = c("#00847e", "#FFC20A", "#D55E00"),
                        limits = c("Yes", "Partial yes", "No"),
                        drop = FALSE) +
      scale_alpha_manual(breaks = c("Critical", "Non-critical"),
                         values = c(1, 0.6)) +
      labs(x = "",
           y = "Number of systematic reviews",
           fill = "",
           alpha = "Domain is") + 
      guides(fill = FALSE) +
      ylim(0, dim(filtered_sr_count())[1]) +
      coord_flip() +
      theme_minimal() + 
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_various,
           width_svg = 9.5,
           height_svg = 5,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)) )
  })
  
  ## various_tab (amstar_tab 3): Bar plot on reporting and methodology transparency (protocol, PRISMA, RoB, GRADE) ----
  output$plotvarious <- renderGirafe({
    # Basic plot
    interactive_various <- ggplot(various_tab(),
                                  aes(x = factor(Var2 , levels = c("Yes", "No")),
                                      y = value,
                                      fill = factor(Var2, levels = c("Yes", "No")),
                                      tooltip = paste0(Var2, ": ", value, " (", round(perc * 100), "%)"), 
                                      data_id = Var2)) +
      geom_bar_interactive(stat = "identity",
                           show.legend = TRUE) +     
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00"),
                        limits = c("Yes", "No"),
                        drop = FALSE) +
      facet_wrap(vars(factor(Var1, levels = c("Protocol available", "PRISMA mentioned", "Risk of bias assessed", "GRADE applied"))),
                 ncol = 4) +
      labs(x = "",
           y = "Number of systematic reviews",
           fill = "") + 
      ylim(0, dim(filtered_sr_count())[1]) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_various,
           width_svg = 9.5,
           height_svg = 5,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })

  ## databases_data (amstar_tab 4): Circular bar plot on databases ----
  output$plotdatabases <- renderGirafe({
    
    # Get the name and the y position of each label
    label_data <- databases_data()
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # Basic plot
    interactive_databases <- ggplot(databases_data(),
                                    aes(x = as.factor(id), 
                                        y = n, 
                                        fill = factor(grey, levels = c("Yes", "No")),
                                        tooltip = paste0(n, " (", round(perc * 100), "%)"), 
                                        data_id = values)) +  
      geom_bar_interactive(stat = "identity", 
                           alpha = 0.5) +
      geom_text(data = label_data,
                aes(x = as.factor(id), 
                    y = n + 0.5, 
                    label = values, 
                    hjust = hjust), 
                color = "black", 
                fontface = "bold", 
                alpha = 0.6, 
                size = 4.0, 
                angle = label_data$angle,
                inherit.aes = FALSE) + 
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#89ccc4"),
                        drop = FALSE) +
      geom_image(data = data.frame(x = 0, y = -11, image = "www/ChatGPT_databases.png"), 
                 aes(x, 
                     y, 
                     image = image), 
                 size = 0.3,
                 inherit.aes = FALSE) + 
      labs(fill = "Contains grey literature") +
      ylim(-12, 12) +
      coord_polar() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
           # plot.margin = unit(rep(-1 ,4), "cm"),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) 
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_databases,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
    
  })
  
  ## location_data (map_tab): Geographic location of the authors ----
  # Load world polygons
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    group_by(admin) %>%
    summarize(geometry = st_union(geometry), .groups = "drop")
  
  output$map <- renderLeaflet({
    # Join counts to world polygons
    world <- left_join(world, location_data(), by = c("admin" = "country"))
    
    # Define color palette
    pal <- colorNumeric("YlOrRd", domain = world$value, na.color = "transparent")
    
    leaflet(world) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(admin, ": ", ifelse(is.na(value), 0, value)),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  })
  
  ## amstarreview_tab: Bubble plot on AMSTAR2 detailed results per systematic review ----
  output$amstarreview <- renderGirafe({
    # Create long dataset for AMSTAR scores per domain and review
    data_amstar <- melt(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], c(1, seq(17, 47, 2))], id.vars = c("Authors")) 
    
    # Create long dataset for AMSTAR comments per domain and review
    data_comments <- melt(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], c(1, seq(18, 48, 2))], id.vars = c("Authors")) 
    
    # Merge both aforementioned datasets 
    data_complete <- data.frame(data_amstar, data_comments[, 3]); colnames(data_complete)[4] <- "comment"
    
    # Indicate the critical domains
    data_complete$critical <- ifelse(is.element(data_complete$variable, c(paste("Domain", c(2, 4, 7, 9, 11, 13, 15)))), "Critical", "Non-critical") 
    
    # Rename the domains
    levels(data_complete$variable) <- c("1. PICO  features reported", "2. Protocol and documented deviations", "3. Selected study design explained", "4. Comprehensive search strategy",
                                        "5. Study selection in duplicate", "6. Data extraction in duplicate", "7. List of excluded studies with justifications", "8. Included studies adequately described",
                                        "9. Satisfactory tool for assessing risk of bias", "10. Funding sources of included studies", "11. Appropriate statistical methods", "12. Assessing risk of bias impact on results",
                                        "13. Risk of bias impact on conclusions", "14. Heterogeneity satisfactorily discussed", "15. Publication bias adequately investigated", "16. Conflict of interest disclosed")
    
    # Add the year after filtering
    data_complete$Year <- rep(c(melt(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], 4])$value), 16)
    
    # First author with publication year
    data_complete$Authors <- paste(data_complete$Authors, "\n", data_complete$Year)
    
    # Basic plot
    interactive_amstarreview <- ggplot(data_complete, 
                                       aes(x = factor(Authors, levels = unique(Authors)),
                                           y = factor(variable, levels = rev(levels(variable))),
                                           tooltip = comment, 
                                           data_id = Authors)) +
      geom_point_interactive(aes(colour = value,
                                 fill = critical),
                             size = 6,
                             shape = 21,
                             stroke = 2.5) +
      scale_colour_manual(name = "Item is flawless",
                          breaks = c("Yes", "Partial yes", "No"),
                          values = c("#00847e", "#FFC20A", "#D55E00")) +
      scale_fill_manual(name = "Critical item",
                        breaks = c("Critical", "Non-critical"),
                        values = c("black", "white"),
                        labels = c("Critical" = "Yes", "Non-critical" = "No")) +
      guides(fill = guide_legend(override.aes = list(size = 3, stroke = 1.8)),
             colour = guide_legend(override.aes = list(size = 3, stroke = 1.8), order = 1)) + 
      labs(x = "",
           y = "",
           fill = "",
           colour = "") + 
      scale_x_discrete(position = 'top') +
      theme_classic() +
      theme(axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9),
            axis.ticks.x = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(t = -10),
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 9))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_amstarreview,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## plotparticipant (pico_summary 1): Circular bar plot on the frequency of inclusion & exclusion criteria ----
  output$plotparticipant <- renderGirafe({
    # Sort databases in decreasing order of their frequency
    participant_data <- participant_summary() %>% 
      filter(!duplicated(variable)) %>% 
      select(-one_of(c("review", "values", "indicator", "values_new"))) %>%
      group_by(inclusion) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      mutate(values_ord = fct_reorder(variable, freq, .desc = TRUE)) %>%
      ungroup()
    
    # Preparing for circular plot. Source: https://r-graph-gallery.com/297-circular-barplot-with-groups.html 
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 1
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(participant_data$inclusion), ncol(participant_data)))
    colnames(to_add) <- colnames(participant_data)
    to_add$inclusion <- rep(levels(participant_data$inclusion), each = empty_bar)
    participant_data <- rbind(participant_data, to_add)
    participant_data <- participant_data %>% arrange(inclusion)
    participant_data$id <- seq(1, nrow(participant_data));participant_data
    
    # Get the name and the y position of each label
    label_data <- participant_data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # Basic plot
    interactive_inclusion <- ggplot(participant_data,
                                    aes(x = as.factor(id), 
                                        y = freq, 
                                        fill = inclusion,
                                        tooltip = paste0(freq, " (", round(perc * 100), "%)"), 
                                        data_id = variable)) +      
      geom_bar_interactive(stat = "identity", 
                           alpha = 0.5) +
      geom_text(data = label_data,
                aes(x = as.factor(id), 
                    y = freq + 0.5, 
                    label = variable_new, 
                    hjust = hjust), 
                color = "black", 
                fontface = "bold", 
                alpha = 0.6, 
                size = 4.0, 
                angle = label_data$angle,
                inherit.aes = FALSE) + 
      #geom_image(data = data.frame(x = 0, y = -11, image = "www/ChatGPT_databases.png"), 
      #           aes(x, 
      #               y, 
      #               image = image), 
      #           size = 0.3,
      #           inherit.aes = FALSE) + 
      scale_fill_manual(breaks = c("Inclusion criteria", "Exclusion criteria"),
                        values = c("#00847e", "#89ccc4"),
                        drop = FALSE) +
      labs(fill = " ") +
      ylim(-12, 12) +
      coord_polar() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            #plot.margin = unit(rep(-1 ,4), "cm"),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) 
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_inclusion,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## plotintervention (pico_summary 2): Bar plot on the frequency of the interventions characteristics ----
  output$plotintervention <- renderGirafe({
    # Sort databases in decreasing order of their frequency
    intervention_data <- intervention_summary() %>% 
      group_by(intervention) %>%
      filter(!duplicated(variable)) %>% 
      select(-one_of(c("review", "values", "indicator", "values_new"))) %>%
      group_by(intervention) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      mutate(values_ord = fct_reorder(variable, freq, .desc = TRUE)) %>%
      ungroup()
    
    # Number of reviews (based on filtering by year)
    reviews_count <- intervention_summary() %>% filter(!duplicated(review))
    
    # Add the 'no' absolute and relative frequency
    intervention_data_new <- data.frame(variable = rep(intervention_data$variable, 2),
                                        reported = rep(c("Yes", "No"), rep(dim(intervention_data)[1], 2)),
                                        freq = c(intervention_data$freq, dim(reviews_count)[1] - intervention_data$freq),
                                        perc = c(intervention_data$perc, 1 - intervention_data$perc),
                                        intervention = rep(intervention_data$intervention, 2))
    intervention_data_new$variable[intervention_data_new$variable == "Intervention"] <- "Hyoscine butylbromide"
    
    # Basic plot
    interactive_interv <- ggplot(intervention_data_new,
                                aes(x = factor(variable, levels = rev(c("Hyoscine butylbromide", "Placebo", "Other drug", "No treatment", "Administration\nroute",
                                                                        "Dose", "Number of doses", "Dose frequency", "Intervention\nallergies"))),
                                    y = freq,
                                    fill = factor(reported, levels = c("Yes", "No")),
                                    tooltip = paste0(reported, ": ", freq, " (", round(perc * 100), "%)"), 
                                    data_id = variable)) +
      geom_bar_interactive(stat = "identity", 
                           show.legend = TRUE) +     
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00"),
                        limits = c("Yes", "No"),
                        drop = FALSE) +
      facet_wrap(vars(factor(intervention, levels = c("Intervention", "Comparator")))) +
      labs(x = "",
           y = "Number of systematic reviews",
           fill = "") + 
      ylim(0, dim(filtered_sr_count())[1]) +
      coord_flip() +
      theme_minimal() + 
      theme(axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_interv,
           width_svg = 9.5,
           height_svg = 5,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
    
  })
  
  ## plotmaternalneonatal (pico_summary 3): Circular bar plot on the frequency of maternal & neonatal outcomes ----
  output$plotmaternalneonatal <- renderGirafe({
    # Sort databases in decreasing order of their frequency
    maternalneonatal_data <- outcome_summary() %>% 
      filter(!duplicated(variable) & !(outcome == "Labour duration")) %>% 
      select(-one_of(c("review", "values", "indicator"))) %>%
      group_by(outcome) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      mutate(values_ord = fct_reorder(variable, freq, .desc = TRUE)) %>%
      ungroup()
    
    # Preparing for circular plot. Source: https://r-graph-gallery.com/297-circular-barplot-with-groups.html 
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 1
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(maternalneonatal_data$outcome), ncol(maternalneonatal_data)))
    colnames(to_add) <- colnames(maternalneonatal_data)
    to_add$inclusion <- rep(levels(maternalneonatal_data$outcome), each = empty_bar)
    maternalneonatal_data <- rbind(maternalneonatal_data, to_add)
    maternalneonatal_data <- maternalneonatal_data %>% arrange(outcome)
    maternalneonatal_data$id <- seq(1, nrow(maternalneonatal_data));maternalneonatal_data
    
    # Get the name and the y position of each label
    label_data <- maternalneonatal_data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # Basic plot
    interactive_maternalneonatal <- 
      ggplot(maternalneonatal_data,
             aes(x = as.factor(id), 
                 y = freq, 
                 fill = outcome,
                 tooltip = paste0(freq, " (", round(perc * 100), "%)"), 
                 data_id = variable)) +      
      geom_bar_interactive(stat = "identity", 
                           alpha = 0.5) +
      geom_text(data = label_data,
                aes(x = as.factor(id), 
                    y = freq + 0.5, 
                    label = variable, 
                    hjust = hjust), 
                color = "black", 
                fontface = "bold", 
                alpha = 0.6, 
                size = 4.0, 
                angle = label_data$angle,
                inherit.aes = FALSE) + 
      #geom_image(data = data.frame(x = 0, y = -11, image = "www/ChatGPT_databases.png"), 
      #           aes(x, 
      #               y, 
      #               image = image), 
      #           size = 0.3,
      #           inherit.aes = FALSE) + 
      scale_fill_manual(breaks = c("Maternal outcomes", "Neonatal outcomes"),
                        values = c("#00847e", "#89ccc4"),
                        drop = FALSE) +
      labs(fill = " ") +
      ylim(-20, 20) +
      coord_polar() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
           # plot.margin = unit(rep(-1 ,4), "cm"),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) 
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_maternalneonatal,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## plotlabourduration (pico_summary 4): Bar plot on the frequency of labour duration outcomes ----
  output$plotlabourduration <- renderGirafe({
    # Sort databases in decreasing order of their frequency
    labourduration_data <- outcome_summary() %>% 
      filter(!duplicated(variable) & (outcome == "Labour duration")) %>% 
      select(-one_of(c("review", "values", "indicator"))) %>%
      group_by(outcome) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      mutate(values_ord = fct_reorder(variable, freq, .desc = TRUE)) %>%
      ungroup()
    
    # Number of reviews (based on filtering by year)
    reviews_count <- outcome_summary() %>% filter(!duplicated(review))
    
    # Add the 'no' absolute and relative frequency
    labourduration_data_new <- data.frame(variable = rep(labourduration_data$variable, 2),
                                          reported = rep(c("Yes", "No"), rep(dim(labourduration_data)[1], 2)),
                                          freq = c(labourduration_data$freq, dim(reviews_count)[1] - labourduration_data$freq),
                                          perc = c(labourduration_data$perc, 1 - labourduration_data$perc),
                                          outcome = rep(labourduration_data$outcome, 2))
    
    # Basic plot
    interactive_labourdur <- ggplot(labourduration_data_new,
                                    aes(x = factor(variable, levels = rev(c("First stage duration", "Active phase duration", "Second stage duration", "Third stage duration", "Total duration"))),
                                     y = freq,
                                     fill = factor(reported, levels = c("Yes", "No")),
                                     tooltip = paste0(reported, ": ", freq, " (", round(perc * 100), "%)"), 
                                     data_id = variable)) +
      geom_bar_interactive(stat = "identity", 
                           show.legend = TRUE) +     
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00"),
                        limits = c("Yes", "No"),
                        drop = FALSE) +
      #facet_wrap(vars(factor(intervention, levels = c("Intervention", "Comparator")))) +
      labs(x = "",
           y = "Number of systematic reviews",
           fill = "") + 
      ylim(0, dim(filtered_sr_count())[1]) +
      coord_flip() +
      theme_minimal() + 
      theme(axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_labourdur,
           width_svg = 9.5,
           height_svg = 5,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
    
  })

  ## participant_summary (participants): Bubble plot on participant characteristics per review ----
  output$participantbubble <- renderGirafe({
    # Basic plot
    interactive_partic <- ggplot(participant_summary(), 
                                 aes(x = variable, 
                                     y = factor(review, levels = unique(review)),
                                     fill = indicator,
                                     colour = indicator,
                                     tooltip = values_new, 
                                     data_id = review)) +
      geom_point_interactive(size = 6,
                             shape = 21,
                             alpha = 0.6) +
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00")) +
      scale_colour_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00")) +
      facet_grid(cols = vars(factor(inclusion, levels = unique(inclusion))), 
                 scales = "free") +
      scale_x_discrete(position = 'top') +
      labs(x = "",
           y = "",
           fill = "Characteristic was reported") + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9, angle = 90, hjust = 0, vjust = 0),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            strip.placement = "outside",
            strip.text = element_text(size = 9.5, colour = "white"),
            strip.background = element_rect(fill = "#00847e", colour = "#00847e"),
            strip.switch.pad.grid = unit(10, "pt"))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_partic,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## intervention_summary (interventions): Bubble plot on compared interventions per review ----
  output$interventionbubble <- renderGirafe({
    # Basic plot
    interactive_interv <- ggplot(intervention_summary(), 
                                 aes(x = factor(variable, levels = c("Intervention", "Placebo", "Other drug", "No treatment", "Administration\nroute",
                                                                      "Dose", "Number of doses", "Dose frequency", "Intervention\nallergies")), 
                                     y = factor(review, levels = unique(review)),
                                     fill = indicator,
                                     colour = indicator,
                                     tooltip = values_new, 
                                     data_id = review)) +
      geom_point_interactive(size = 6,
                             shape = 21,
                             alpha = 0.6) +
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00")) +
      scale_colour_manual(breaks = c("Yes", "No"),
                          values = c("#00847e", "#D55E00")) +
      facet_grid(cols = vars(factor(intervention, levels = unique(intervention))), 
                 scales = "free") +
      scale_x_discrete(position = 'top') +
      labs(x = "",
           y = "",
           fill = "Characteristic was reported") + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9, angle = 90, hjust = 0, vjust = 0),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            strip.placement = "outside",
            strip.text = element_text(size = 9.5, colour = "white"),
            strip.background = element_rect(fill = "#00847e", colour = "#00847e"),
            strip.switch.pad.grid = unit(10, "pt"))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_interv,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## outcome_summary (outcomes): Bubble plot on investigated outcomes per review ----
  output$outcomebubble <- renderGirafe({
    # Basic plot
    interactive_outcome <- ggplot(outcome_summary(), 
                                  aes(x = variable, 
                                      y = factor(review, levels = unique(review)),
                                      fill = indicator,
                                      colour = indicator,
                                      tooltip = values_new, 
                                      data_id = review)) +
      geom_point_interactive(size = 6,
                             shape = 21,
                             alpha = 0.6) +
      scale_fill_manual(breaks = c("Yes", "No"),
                        values = c("#00847e", "#D55E00")) +
      scale_colour_manual(breaks = c("Yes", "No"),
                          values = c("#00847e", "#D55E00")) +
      facet_grid(cols = vars(factor(outcome, levels = c("Labour duration", "Maternal outcomes", "Neonatal outcomes"))), 
                 scales = "free_x",
                 space = "free_x") +
      scale_x_discrete(position = 'top') +
      labs(x = "",
           y = "",
           fill = "Characteristic was reported") + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9, angle = 90, hjust = 0, vjust = 0),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            strip.placement = "outside",
            strip.text = element_text(size = 9.5, colour = "white"),
            strip.background = element_rect(fill = "#00847e", colour = "#00847e"),
            strip.switch.pad.grid = unit(10, "pt"))
    
    # Convert to ggiraph object
    girafe(ggobj = interactive_outcome,
           width_svg = 9.5,
           height_svg = 6.0,
           options = list(opts_hover(css = ''), 
                          opts_hover_inv(css = "opacity:0.1;"), 
                          opts_sizing(rescale = TRUE)))
  })
  
  ## rawdata (review_tab): Add the coloured AMSTAR cells and the links ----
  output$rawDataTable <- renderDT({
    
    # The filtered dataset
    datatable(rawdata(), 
              escape = FALSE, 
              options = list(pageLength = 10)) %>%
      # Apply color to the AMSTAR column
      formatStyle("AMSTAR", backgroundColor = styleEqual(c("High", "Moderate", "Low", "Critically low"),
                                                         c("#00847e77", "#FFC20A77", "#D55E0077", "#DC322077")),
                  color = 'black')
  })
  
  # Download data ----
  output$downloadData <- 
    downloadHandler(filename = function() {
      paste("data_sr-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(data_sr, file)
        }
      )
  
  }

shinyApp(ui, server)
