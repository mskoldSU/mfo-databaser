library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)

con <- DBI::dbConnect(RSQLite::SQLite(), "mysqlite3.db")

accession <- tbl(con, "accession") %>% 
    filter(catalog_id == 2)
fish <- tbl(con, "fish")
locality <- tbl(con, "locality")
project <- tbl(con, "project")
species <- tbl(con, "species")
person <- tbl(con, "person")
age <- tbl(con, "agerecord_row")
gender <- tbl(con, "gender")
material <- tbl(con, "material")
material_storage <- tbl(con, "material_storage")
material_type <- tbl(con, "material_type")


accession_tab_lazy <- accession %>% 
    inner_join(fish, by = c("id" = "accession_id"), suffix = c("", ".fish")) %>% 
    left_join(locality, by = c("locality_id" = "id"), suffix = c("", ".locality")) %>% 
    left_join(project, by = c("project_id" = "id"), suffix = c("", ".project")) %>% 
    left_join(species, by = c("species_id" = "id"), suffix = c("", ".species")) %>% 
    left_join(gender, by = c("gender_id" = "id"), suffix = c("", ".gender")) %>% 
    left_join(person, by = c("created_by" = "id")) %>% 
    left_join(age, by = c("id" = "accession_id"), suffix = c("", ".age")) %>% 
    mutate(accnr = paste0("C20", str_sub(id, 3, 4), "/", str_sub(id, 5)),
           accnr = paste0('<a href="http://esbase.nrm.se/accession?id=', id, '">', accnr, '</a>'),
           created_by = paste(firstname, lastname)
    ) %>% 
    rename(long = latitude,
           lat = longitude) %>% 
    select(id, accnr,  species = swe_name, project = name.project, 
           date = arrival_date, locality = name, longitude = long, latitude = lat, 
           gender = swe_name.gender,
           age = age_start, gonadweight, 
           liverweight, totallength, bodylength, created_by)

accession_tab <- collect(accession_tab_lazy) %>% 
    mutate(age = as.numeric(age), date = as.Date(date), across(c(where(is.character), -accnr), as.factor)) %>% 
    select(-id)

material_tab_lazy <- right_join(accession_tab_lazy, material, by = c("id" = "accession_id"), suffix = c("", ".material")) %>%
    filter(!is.na(accnr)) %>% 
    left_join(material_type, by = c("material_type_id" = "id"), suffix = c("", ".material_type")) %>% 
    left_join(material_storage, by = c("storage_type_id" = "id"), suffix = c("", ".material_storage")) %>% 
    select(accnr, species, project, date, locality, gender, age, 
           material = swe_name, amount_original, amount_left, storage = name, storage_id, storage_note)

material_tab <- collect(material_tab_lazy) %>% 
    mutate(age = as.numeric(age), date = as.Date(date), across(c(where(is.character), -c("accnr", "storage_note")), as.factor))


accession_columns <- names(accession_tab)
material_columns <- names(material_tab)
shinyApp(
    ui <- dashboardPage(
        dashboardHeader(title = "ESBase 2.0"),
        dashboardSidebar(  
            sidebarMenu(
                menuItem("Accessions", tabName = "accessions"),
                menuItem("Materials", tabName = "materials")
            )),
        dashboardBody(
            tabItems(
                tabItem("accessions",
                        fluidPage(tabBox(width = 12,
                                         tabPanel("Tabell", 
                                                  fluidPage(
                                                      fluidRow(
                                                          selectInput("accession_columns", "Columns",
                                                                      choices = accession_columns, 
                                                                      selected = accession_columns[1:5],
                                                                      multiple = TRUE),
                                                          DTOutput('acctbl')
                                                      )
                                                  )
                                         ),
                                         tabPanel("Karta", leafletOutput("map")),
                                         tabPanel("Statistik", 
                                                  fluidPage(
                                                      fluidRow(selectInput("PlotColumn", "PlotColumn",
                                                                           choices = accession_columns[-1],
                                                                           selected = accession_columns[2]),
                                                               plotOutput("plot"))
                                                  )
                                         )
                        ))),
                tabItem("materials",
                        fluidPage(
                            fluidRow(
                                selectInput( "material_columns", "Columns",
                                            choices = material_columns, 
                                            selected = c(material_columns[1:5], "material", "amount_left", "storage", "storage_note"),
                                            multiple = TRUE),
                                DTOutput('mattbl')
                            )
                        ))
            ))
    ),
    server = function(input, output) {
        output$acctbl = renderDT(
            accession_tab %>% 
                select(input$accession_columns) %>% 
                datatable(extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), autoWidth = TRUE, dom = 'lfBrtip'), 
                          filter = 'top',
                          escape = FALSE,
                          selection = 'none') 
            
            
        )
        output$mattbl = renderDT(
            material_tab %>% 
                select(input$material_columns) %>% 
                datatable(extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), autoWidth = TRUE, dom = 'lfBrtip'), 
                          filter = 'top',
                          escape = FALSE,
                          selection = 'none') 
        )
        map_data <- reactive({
            accession_tab[input$acctbl_rows_all, ] %>% 
                select(longitude, latitude) %>% 
                na.omit() %>% 
                distinct() %>% 
                slice(1:1000)}
        )
        plot_data <- reactive({
            tibble(x = accession_tab[input$acctbl_rows_all,] %>% pull(input$PlotColumn))
        }
        )
        output$map = renderLeaflet(
            leaflet() %>% addTiles() %>% addMarkers(lng = map_data()$longitude,
                                                    lat = map_data()$latitude)
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