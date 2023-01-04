#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(sp)
library(rmarkdown)
library(dplyr)
library(lubridate)
#library(ggmap)
library(ggplot2)
library(viridis)
library(rcartocolor)
library(plotly)
library(sf)
library(DT)
vars_dep <- peru_d$NOMBDEP

ui <- navbarPage("PeruSeism",
                 tabPanel("Nacional",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Tipo de mapa",
                                           c("De calor 1"= 1,
                                             "De calor 2"= 2,
                                             "Por Magnitud" = 3,
                                             "Por profundidad" = 4)
                              ),
                              strong("Años"),
                                    checkboxGroupInput(inputId = "syear", label = NULL, 
                                                       choices = 1960:2021, selected = 2000:2021, inline = T),
                              actionButton("aplicar","Mostrar mapa"),
                              width = 3,
                              tags$head(tags$style("#plot1{height:85vh !important;}"))
                            ),
                            mainPanel(
                              plotlyOutput("plot1"),
                              width = 9
                            )
                          )
                 ),
                 tabPanel("Regional",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('departamento', 'Departamento', vars_dep, selected = vars_dep[1]),
                              radioButtons("plotType_Reg", "Tipo de mapa",
                                           c("De calor 1"= 1,
                                             "De calor 2"= 2,
                                             "Por Magnitud" = 3,
                                             "Por profundidad" = 4)
                              ),
                              strong("Años"),
                              checkboxGroupInput(inputId = "syear_Reg", label = NULL, 
                                                 choices = 1960:2021, selected = 2000:2021, inline = T),
                              actionButton("aplicar_Reg","Mostrar mapa"),
                              width = 3,
                              tags$head(tags$style("#plot_Reg{height:85vh !important;}"))
                            ),
                            mainPanel(
                              plotlyOutput("plot_Reg"),
                              width = 9
                            )
                          )
                 ),
                 navbarMenu("More",
                            tabPanel("Table",
                                     DT::dataTableOutput("table")
                            ),
                            tabPanel("Estadísticas",
                            )
                 ),
                 theme = shinytheme("flatly")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Nacional
  selectedType <- eventReactive(input$aplicar,{
    input$plotType
  })
  
  selectedYear <- eventReactive(input$aplicar,{
    input$syear
  })
  
  output$plot1 <- renderPlotly({
    p_nacional(selectedType(),selectedYear())
  })
  
  #Regional
  selectedDepartament <- eventReactive(input$aplicar_Reg,{
    input$departamento
  })
  
  selectedType_Reg <- eventReactive(input$aplicar_Reg,{
    input$plotType_Reg
  })
  
  selectedYear_Reg <- eventReactive(input$aplicar_Reg,{
    input$syear_Reg
  })
  
  output$plot_Reg <- renderPlotly({
    p_departamento(selectedType_Reg(),selectedDepartament(),selectedYear_Reg())
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(datos)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
