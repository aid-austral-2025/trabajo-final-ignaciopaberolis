#Paquetes
library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)
shinyWidgetsGallery()

#Importación de datos

datash <- read_delim("data_limpio.csv", delim = ",")


# Listado de planes disponibles
plan <- datash %>% 
  pull(plan) %>% 
  unique() %>% 
  sort()

# INTERFAZ
MiInterfaz <- fluidPage(
  titlePanel("Loyalz Metrics"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "Año",
        label = "Año de Pago",
        value = min(datash$Año),
        min = min(datash$Año),
        max = max(datash$Año)
      ),
      radioButtons(
        inputId = "plan",
        label = "Plan de la cuenta",
        choices = plan,
        selected = plan[1]
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "Grafico")
    )
  )
)

# SERVIDOR
MiServidor <- function(input, output) {
  
  Año_reactivo <- reactive({input$Año})
  plan_reactivo <- reactive({input$plan})
  
  output$Grafico <- renderPlotly({
    
    grafico <- datash %>% 
      filter(
        Año == Año_reactivo(),
        plan == plan_reactivo(),
        Estado_Pago == "Pago"
      ) %>% 
      count(plan) %>% 
      ggplot(aes(x = plan, y = n)) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Cantidad de cuentas por plan en", input$Año),
        x = "Plan",
        y = "Cantidad"
      ) +
      theme_minimal()
    
    ggplotly(grafico)
  })
}

# Lanzamiento
shinyApp(ui = MiInterfaz, server = MiServidor)


