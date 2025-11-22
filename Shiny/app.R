#Paquetes
library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(thematic)
library(shinydashboard)
shinyWidgetsGallery()

#Mapas
library(sf)
library(leaflet)
library(spData)

#Lectura y manipulación de datos
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(htmltools)


#Importación de datos-----------------------------------------------------------------------------------

datash <- read_delim("data_limpio.csv", delim = ",")

meses_es <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
              "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

datash$Mes <- factor(datash$Mes, levels = meses_es, ordered = TRUE)


#------------------------------- APP-------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Loyalz Metrics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Planes", tabName = "planes", icon = icon("layer-group")),
      menuItem("Países", tabName = "paises", icon = icon("globe")),
      menuItem("Tarjetas", tabName = "tarjetas", icon = icon("id-card")),
      menuItem("Ingresos", tabName = "ingresos", icon = icon("dollar-sign"))
      )
  ),
  
  dashboardBody(
    tabItems(
      
      # -------------------- PLANES --------------------------------
      tabItem(tabName = "planes",
              fluidRow(
                box(width = 4, title = "Filtros", solidHeader = TRUE, 
                    
                    numericInput("Año", "Seleccioná Año:",
                                 min = min(datash$Año),
                                 max = max(datash$Año),
                                 value = 2025),
                    
                    pickerInput("plan", "Seleccionar Plan(es):",
                                choices = sort(unique(datash$plan)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = unique(datash$plan))
                ),
                box(width = 8, title = "Cuentas por Plan",
                    plotlyOutput("GraficoPlanes"))
              )
      ),
      
      # -------------------- PAISES ------------------------------
      tabItem(tabName = "paises",
              fluidRow(
                box(width = 4, title = "Filtros", solidHeader = TRUE,
                    
                    numericInput("Año_pais", "Seleccioná Año:",
                                 min = min(datash$Año),
                                 max = max(datash$Año),
                                 value = 2025),
                    
                    pickerInput("paises", "Seleccionar País(es):",
                                choices = sort(unique(datash$`Country code`)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = unique(datash$`Country code`))
                ),
                box(width = 8, title = "Cuentas por País",
                    plotlyOutput("GraficoPaises"))
              )
      ),
      
      # -------------------- TARJETAS ----------------------------
      tabItem(tabName = "tarjetas",
              fluidRow(
                box(width = 4, title = "Filtros", solidHeader = TRUE,
                    
                    numericInput("Año_tarjetas", "Seleccioná Año:",
                                 min = min(datash$Año),
                                 max = max(datash$Año),
                                 value = 2025),
                    
                    pickerInput("planes_tarjeta", "Seleccionar Plan(es):",
                                choices = sort(unique(datash$plan)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = unique(datash$plan))
                ),
                box(width = 8, title = "Tarjetas Activas por Plan",
                    plotlyOutput("GraficoTarjetas"))
              )
      ),
      
      # -------------------- INGRESOS ----------------------------
      tabItem(tabName = "ingresos",
              fluidRow(
                box(width = 4, title = "Filtros", solidHeader = TRUE,
                    
                    numericInput("Año_monto", "Seleccioná Año:",
                                 min = min(datash$Año),
                                 max = max(datash$Año),
                                 value = 2025),
                    
                    pickerInput("planes_monto", "Seleccionar Plan(es):",
                                choices = sort(unique(datash$plan)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = unique(datash$plan))
                ),
                box(width = 8, title = "Monto total cobrado por Plan",
                    plotlyOutput("GraficoMonto"))
              )
      )
    )
  )
)
#--------------------SERVIDOR ---------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  # ---- GRAFICO DE PLANES ----
  output$GraficoPlanes <- renderPlotly({
    df <- datash %>% 
      filter(Año == input$Año,
             plan %in% input$plan) %>%
      count(Mes, plan)
    
    g <- df %>%
      ggplot(aes(Mes, n, color = plan)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2) +
      theme_minimal()
    
    ggplotly(g)
  })
  
  # ---- GRAFICO DE PAISES ----
  output$GraficoPaises <- renderPlotly({
    df <- datash %>%
      filter(Año == input$Año_pais,
             `Country code` %in% input$paises) %>%
      count(`Country code`)
    
    g <- df %>%
      ggplot(aes(reorder(`Country code`, n), n, fill = `Country code`)) +
      geom_col() +
      coord_flip() +
      theme_minimal()
    
    ggplotly(g)
  })
  
  # ---- GRAFICO DE TARJETAS ----
  output$GraficoTarjetas <- renderPlotly({
    
    df <- datash %>%
      filter(Año == input$Año_tarjetas,
             plan %in% input$planes_tarjeta) %>%
      group_by(plan) %>%
      summarise(Tarjetas = sum(Tarjetas_Activas, na.rm = TRUE))
    
    g <- df %>% 
      ggplot(aes(plan, Tarjetas, fill = plan)) +
      geom_col() +
      theme_minimal()
    
    ggplotly(g)
  })
  
  # ---- GRAFICO DE INGRESOS ----
  output$GraficoMonto <- renderPlotly({
    
    df <- datash %>%
      filter(Año == input$Año_monto,
             Estado_Pago == "Pago",
             plan %in% input$planes_monto) %>%
      group_by(plan) %>%
      summarise(Monto = sum(importe_plan, na.rm = TRUE))
    
    g <- df %>%
      ggplot(aes(plan, Monto, fill = plan)) +
      geom_col() +
      theme_minimal()
    
    ggplotly(g)
  })
  
}
#--------------Corremos La APP---------------------
shinyApp(ui, server)
