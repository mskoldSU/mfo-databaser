library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)
data <- read_csv("fish.csv", col_types = "ccffDfdddddddf") %>% 
    mutate(accnr = paste0('<a href="http://esbase.nrm.se/accession?id=', id, '">', accnr, '</a>')) %>% 
    select(-id)
columns <- names(data)
shinyApp(
    ui <- dashboardPage(
        dashboardHeader(title = "ESBase 2.0"),
        dashboardSidebar(fluidPage(selectInput("Columns", "Columns", 
                                               choices = columns, 
                                               selected = columns[1:5],
                                               multiple = TRUE))),
        dashboardBody(fluidPage(tabBox(width = 12,
            tabPanel("Tabell", DTOutput('tbl')),
            tabPanel("Karta", leafletOutput("map"))
        )))
    ),
    server = function(input, output) {
        output$tbl = renderDT(
            data %>% 
                select(input$Columns) %>% 
                datatable(extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), autoWidth = TRUE, dom = 'lfBrtip'), 
                          filter = 'top',
                          escape = FALSE,
                          selection = 'none') 
            
            
        )
        map_data <- reactive({
            data[input$tbl_rows_all,] %>% 
                select(longitude, latitude) %>% 
                na.omit() %>% 
                distinct() %>% 
                slice(1:1000)}
        )
        output$map = renderLeaflet(
            leaflet() %>% addTiles() %>% addMarkers(lng = map_data()$latitude,
                                                    lat = map_data()$longitude)
        )
    }
)