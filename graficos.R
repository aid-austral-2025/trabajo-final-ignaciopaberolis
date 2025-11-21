#----------------------- EXPLORACION DE LOS DATOS ------------------------------------
install.packages("shiny")

#Paquetes
library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)



# VAMOS A MOSTRAR CANTIDAD DE PLANES PAGOS ESTE 2025 #


grafico <- data %>% 
  filter(Año == 2025,
         Estado_Pago == "Pago"
         ) %>%          # filtra solo año 2025
  count(plan) %>%                   # cuenta cuántos hay por plan
  ggplot(aes(x = plan, y = n)) + 
  geom_col(fill = "steelblue") + 
  labs(
    title = "Cantidad de cuentas por plan - Año 2025",
    x = "Plan",
    y = "Cantidad"
  ) +
  theme_minimal()

grafico


