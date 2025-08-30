### Example of Rshiny dashboard

## Load libraries
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(ggiraph)
library(readxl)
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


## Load data from local file
# Load AMSTAR, author location, reporting and methods transparency
data_sr <- read_excel("data/systematic reviews & included trials.xlsx", sheet = "systematic reviews")[, -1] 

# Load searched databases
databases <- read_excel("data/systematic reviews & included trials.xlsx", sheet = 2)


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
      "))
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              h2(""),
              p("This dashboard")
      ),
      # Second tab content
      tabItem(tabName = "amstar_tab",
              h2("Research quality of systematic reviews on labour duration"),
              fluidRow(
                box(title = "AMSTAR overall confidence", status = "primary", solidHeader = TRUE, girafeOutput("plotamstar", width = "100%", height = "350px")),
                box(title = "Reporting & methodology transparency", status = "primary", solidHeader = TRUE, girafeOutput("plotvarious", width = "100%", height = "350px")),
                ),
              fluidRow(
                box(title = "AMSTAR item ratings", status = "primary", solidHeader = TRUE, tags$br(), girafeOutput("plotdomains", width = "100%", height = "380px")),
                box(title = "Databases for literature searches", status = "primary", solidHeader = TRUE, girafeOutput("plotdatabases", width = "100%", height = "400px"))
              )
              ),
      # Third tab content
      tabItem(tabName = "map_tab",
              h2("Geographic location of authors"),
              leafletOutput("map", height = "1200px")
              ),
      # Fourth tab content
      tabItem(tabName = "review_tab",
              h2("Systematic reviews on labour duration"),
              DTOutput("rawDataTable")
              )
      )
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
  filtered_data <- reactive({
    data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ]
  })
  
  ## amstar_tab (amstar_tab 1): AMSTAR overall confidence
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
  
  ## amstardomains_tab (amstar_tab 2): Frequency of each AMSTAR items' level (yes, partly yes, no)
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
    
  ## various_tab (amstar_tab 3): Reporting and methodology transparency
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
  
  ## databases_data (amstar_tab 4): Databases for literature searches
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
    
    # Get the name and the y position of each label
    databases_long_new$angle <- 90 - 360 * (databases_long_new$id - 0.5) / nrow(databases_long_new)     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    databases_long_new$hjust <- ifelse(databases_long_new$angle < -90, 1, 0)
    databases_long_new$angle <- ifelse(databases_long_new$angle < -90, databases_long_new$angle + 180, databases_long_new$angle)
    databases_long_new
  })
  
  ## location_data (map_tab): Geographic location of authors
  location_data <- reactive({
    # Considering the year
    geo_location <- subset(data_sr[data_sr$Year >= input$year_range[1] & data_sr$Year <= input$year_range[2], ], select = c("Iran", "Egypt", "Italy", "USA", "Taiwan", "South Africa"))
    
    # Sum across rows
    geo_location_sum <- ifelse(colSums(geo_location) < 1, NA, colSums(geo_location))
    names(geo_location_sum)[4] <- "United States of America"
    
    # Turn 'geo_location' into 2x2 data.frame
    tab <- data.frame(country = names(geo_location_sum), melt(geo_location_sum)); tab
  })
  
  ## rawdata (review_tab): Dataset in Table (Systematic review collection)
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

  ## amstar_tab (amstar_tab 1): Bar plot with AMSTAR overall confidence
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
      ylim(0, dim(filtered_data())[1]) +
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
  
  ## amstardomains_tab (amstar_tab 2): Bar plot on AMSTAR domains with level frequency 
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
           fill = "") + 
      ylim(0, dim(filtered_data())[1]) +
      coord_flip() +
      theme_minimal() + 
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "none",
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
  
  ## various_tab (amstar_tab 3): Bar plot on reporting and methdology transparency (protocol, PRISMA, RoB, GRADE)
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
      ylim(0, dim(filtered_data())[1]) +
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
                          opts_sizing(rescale = TRUE)) )
  })

  ## databases_data (amstar_tab 4): Circular bar plot on databases
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
                                        fill = grey,
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
                        limits = c("Yes", "No"),
                        drop = FALSE) +
      ylim(-12, 12) +
      coord_polar() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(rep(-1 ,4), "cm"),
            legend.position = "none",
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
  
  ## location_data (map_tab): Geographic location of the authors
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
  
  ## rawdata (review_tab): Add the coloured AMSTAR cells and the links
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
  
  
  }

shinyApp(ui, server)
