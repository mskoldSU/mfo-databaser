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
                                       tabPanel("Karta", leafletOutput("map")),
                                       tabPanel("Statistik", 
                                                fluidRow(selectInput("PlotColumn", "PlotColumn",
                                                                     choices = columns[-1],
                                                                     selected = columns[1]),
                                                         plotOutput("plot")))
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
        plot_data <- reactive({
            tibble(x = data[input$tbl_rows_all,] %>% pull(input$PlotColumn))
        }
        )
        output$map = renderLeaflet(
            leaflet() %>% addTiles() %>% addMarkers(lng = map_data()$latitude,
                                                    lat = map_data()$longitude)
        )
        output$plot <- renderPlot(
            if (is.numeric(plot_data()$x)){
                ggplot(plot_data(), aes(x = x)) + geom_density() + geom_rug() + xlab(input$PlotColumn)
            }
            else
            {
                ggplot(plot_data(), aes(y = x)) + geom_bar() + ylab(input$PlotColumn)
            }
        )
    }
)