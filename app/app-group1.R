#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("ggvis")) {
  install.packages("ggvis")
  library(ggvis)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)
}
if (!require("zipcodeR")) {
  install.packages("zipcodeR")
  library(zipcodeR)
}

if (!require("sf")) {
  install.packages("sf")
  library(sf)
}


#=============================================== Shiny UI =========================================================
ui <- navbarPage(
  "U.S Disasters",
  tabPanel(
    "Introduction",
    tags$img(src = "https://media.istockphoto.com/id/1333043586/photo/tornado-in-stormy-landscape-climate-change-and-natural-disaster-concept.jpg?s=612x612&w=0&k=20&c=uo4HoloU79NEle1-rgVoLhKBE-RrfPSeinKAdczCo2I=", 
            width = "100%", style = "opacity: 0.90"),
    fluidRow(
      absolutePanel(
        style = "background-color: white",
        top = "40%",
        left = "25%",
        right = "25%",
        height = 170,
        tags$p(
          style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%",
          "In an era marked by climate change and environmental upheaval, our analysis delves into the frequency and distribution of natural disasters across the United States. We've meticulously compiled data, revealing patterns in disaster occurrences, distinguishing prevalent types, and identifying the most affected states. This investigation not only highlights the resilience of communities but also underscores the pressing need for robust disaster preparedness and adaptive strategies. By understanding the past, we aim to equip policymakers and stakeholders with insights to fortify against the inevitable challenges of tomorrow."
        )
      )
    )
  ),
  tabPanel(
    "Heatmaps",
    fluidRow(
      column(
        6,
        leafletOutput("heatmapBefore2000", height = "500px"),
        tags$h3("Total Disaster by State Before 2000")
      ),
      column(
        6,
        leafletOutput("heatmapAfter2000", height = "500px"),
        tags$h3("Total Disaster by State After 2000")
      )
    )
  ),
  tabPanel(
    "Number of Disaster Declarations by Incident Type",
    tags$h2("Number of Disaster Declarations by Incident Type", align = "center"),
    fluidRow(
      column(12, plotOutput("plotByIncidentRegion",width = "50%", height = "600px"), align = "center"),
      column(12, plotOutput("plotByIncidentYear", width = "50%", height = "600px"), align = "center"),
      column(12, plotOutput("plotByIncidentHazard", width = "50%", height = "600px"), align = "center")
    )
  ),
  tabPanel(
    "Annual Disaster Trends Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("disasterType", "Select Disaster Type:", choices = NULL) # Choices will be updated in the server.
      ),
      mainPanel(
        plotOutput("disasterPlot", height = "700px")
      )
    )
  ),
  tabPanel(
    "Disaster Declarations Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("disasterTypeAnalysis", "Select Disaster Type:", choices = NULL), # Updated input ID
        selectInput("yearAnalysis", "Select Year:", choices = NULL) # Updated input ID
      ),
      mainPanel(
        plotOutput("disasterAnalysisPlot")
      )
    )
  ),
  
  tabPanel(
    "References",
    tags$h2(
      "Data Sources"
    ),
    tags$a(
      href = "https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2",
      "fema.gov/openfema-data-page/disaster-declarations-summaries-v2", 
    ),br(),
    tags$h2(
      "Contributors"
    ),
    tags$p(
      "Zhengfei Chen"
    ),
    tags$p(
      "Jingqi Wang"
    ),
    tags$p(
      "Jitian Zhou"
    ),
    tags$p(
      "Jiaqi Liu"
    ),
    tags$p(
      "Chulin Tang"
    ),
    tags$h2(
      "GitHub Repository"
    ),
    tags$a(
      href = "https://github.com/Jingqi-Wang-1217/ADS-Spring2024-Project2-ShinyApp-Group-1",
      "https://github.com/Jingqi-Wang-1217/ADS-Spring2024-Project2-ShinyApp-Group-1"
    )
  )
)

#=============================================== Shiny SERVER =====================================================
server <- function(input, output, session) {
  disaster_df <- read_csv("disasternumber.csv")
  # Pre-process data: Aggregate based on year 2000
  disaster_df_before_2000 <- disaster_df %>%
    filter(fyDeclared < 2000) %>%
    group_by(state) %>%
    summarise(totalDisasters = sum(disasterNumber))
  
  disaster_df_after_2000 <- disaster_df %>%
    filter(fyDeclared >= 2000) %>%
    group_by(state) %>%
    summarise(totalDisasters = sum(disasterNumber))
  
  # States data for mapping
  states_data <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")%>% 
    st_transform(crs = 4326)  # Transform the CRS to WGS84

  incident_df <- read_csv("disaster_incidentType.csv")
  
  observe({
    choices <- c("All Types", unique(incident_df$incidentType))
    updateSelectInput(session, "disasterType", choices = choices)
  })
  
  filtered_data <- reactive({
    if (input$disasterType == "All Types") {
      incident_df %>%
        group_by(fyDeclared) %>%
        summarise(totalIncidents = sum(disasterNumber, na.rm = TRUE))
    } else {
      incident_df %>%
        filter(incidentType == input$disasterType) %>%
        group_by(fyDeclared) %>%
        summarise(totalIncidents = sum(disasterNumber, na.rm = TRUE))
    }
  })
  
  full_disaster_df <- read_csv("DisasterDeclarationsSummaries.csv")
  observe({
    updateSelectInput(session, "disasterTypeAnalysis", choices = unique(full_disaster_df$incidentType))
    updateSelectInput(session, "yearAnalysis", choices = unique(full_disaster_df$fyDeclared))
  })
  
  
  # Function to render heatmap
  renderHeatmap <- function(data, outputId) {
    output[[outputId]] <- renderLeaflet({
      states_joined <- left_join(states_data, data, by = c("STUSPS" = "state"))
      
      # If the joined data has NAs for totalDisasters, replace them with 0
      states_joined$totalDisasters[is.na(states_joined$totalDisasters)] <- 0
      
      pal <- colorNumeric(palette = "YlOrRd", domain = states_joined$totalDisasters, na.color = "#808080")
      
      leaflet(states_joined) %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lng = -100, lat = 40, zoom = 3) %>% 
        addPolygons(fillColor = ~pal(totalDisasters),weight = 1,opacity = 1,color = "white",dashArray = "",
                    fillOpacity = 0.9,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#000000",
                                                 dashArray = "",
                                                 bringToFront = TRUE),
                    label = ~paste(STUSPS, "Total Disasters: ", totalDisasters),
                    labelOptions = labelOptions(direction = 'auto')) %>%
        addLegend(pal = pal, values = ~totalDisasters, opacity = 0.7, position = "bottomright")
    })
  }
  
  # Render heatmaps for before and after 2000
  renderHeatmap(disaster_df_before_2000, "heatmapBefore2000")
  renderHeatmap(disaster_df_after_2000, "heatmapAfter2000")
  
  output$plotByIncidentRegion <- renderPlot({
    ggplot(incident_df, aes(x = incidentType, fill = Region)) + 
      geom_bar() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = 22, face = "bold")) + 
      labs(title = "By Region", 
           x = "Incident Type", 
           y = "Count")
  })
  
  # Render plot by incident type in year
  output$plotByIncidentYear <- renderPlot({
    ggplot(incident_df, aes(x = incidentType, fill = fyDeclared > 2000)) + 
      geom_bar() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = 22, face = "bold")) + 
      labs(title = "By year Before / After 2000", 
           x = "Incident Type", 
           y = "Count",
           fill = 'After year 2000')
  })
  
  # Render plot by incident type in Hazard Mitigation Program Declaration
  output$plotByIncidentHazard <- renderPlot({
    ggplot(incident_df, aes(x = incidentType, fill = hmProgramDeclared == 1)) + 
      geom_bar() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(size = 22, face = "bold")) + 
      labs(title = "Hazard Mitigation Program Claimed or Not", 
           x = "Incident Type", 
           y = "Count",
           fill = "Hazard Mitigation\nProgram Declared")
  })
  
  output$disasterPlot <- renderPlot({
    data_to_plot <- filtered_data()
    ggplot(data_to_plot, aes(x = fyDeclared, y = totalIncidents)) +
      geom_line(color = "blue") +
      geom_point() +
      theme(plot.title = element_text(size = 22, face = "bold")) +
      expand_limits(y = 0) +
      labs(title = if(input$disasterType == "All Types") {
        "Annual Trend for All Incident Types"
      } else {
        paste("Annual Trend for", input$disasterType, "Incidents")
      },
      x = "Year",
      y = "Total Number of Incidents")
  })
  
  output$disasterAnalysisPlot <- renderPlot({
    filtered_data <- full_disaster_df %>%
      filter(incidentType == input$disasterTypeAnalysis & fyDeclared == input$yearAnalysis)
    
    ggplot(filtered_data, aes(x = state, fill = state)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 22, face = "bold")) +
      labs(title = paste("Disasters by State in", input$yearAnalysis),
           x = "State",
           y = "Number of Disasters")
  })
  
  
}

# Run the app
shinyApp(ui, server)