#--------------------------- PUESTA EN MARCHA --------------------------------#

----------#Lectura del Dataset#----------------

data <- readxl::read_excel("Data Set/report_21.11.2025.xlsx")

library(dplyr)
library(lubridate)
---------#Exploracion Y limpieza#--------------------

str(data)

summary(data)

-------#Vamos a modificar nombres de columnas para entenderlo mejor----------



#Cambiamos el nombre por Estado Pago

data <- data %>%
  rename(Estado_Pago= TariffStatus)


#Cambiamos el nombre por Ultimo Pago
data <- data %>%
  rename(Ultimo_Pago= TariffActivationDate)

#Cambiamos el nombre por Plan
data <- data %>%
  rename(plan= Tariff)

#Agregamos una columna que tome el importe por Plan 

# planes : START $39 - GROW $89 - BUSINESS $249.

data <- data %>%
  mutate(
    importe_plan = case_when(
      plan == "Start"    ~ 39,
      plan == "Grow"     ~ 89,
      plan == "Business" ~ 249,)
  )

# Tomar la fecha de pago para separarla en el mes y el año en columnas

data <- data %>%
  mutate(
    Año = year(ymd_hms(Ultimo_Pago)),
    Mes  = toupper(format(ymd_hms(Ultimo_Pago), "%B"))
  )

#Para facilitar el analisis vamos a modificar de la columna años los valores 2026,2027 y 2028 por 2025, debido a que son cuentas que
#utilizan contratos diferentes y se les otorga vencimientos mas largos a fin de practicidad pero corresponden al año corriente
# y se controla su pago mes a mes, si se da de baja se registra por lo que corresponden al 2025.

data$Año[data$Año %in% c(2026, 2027, 2028)] <- 2025


#Convertir los codigos de los Paises en el nombre del pais.

# Leo cuantos codigos distintos tengo
data %>% 
  distinct(`Country code`) %>% 
  print(n = Inf)

# Los empiezo a modificar.


data <- data %>%
  mutate(
    `Country code` = case_when(
     `Country code` == "AR" ~ "ARGENTINA",
    `Country code` == "MX" ~ "MEXICO",
    `Country code` == "CL" ~ "CHILE",
    `Country code` == "BZ" ~ "BRASIL",
    `Country code` == "ES" ~ "ESPAÑA",
    `Country code` == "PE" ~ "PERU",
    `Country code` == "GT" ~ "GUATEMALA",
    `Country code` == "FR" ~ "FRANCIA",
      `Country code` == "US" ~ "ESTADOS UNIDOS",
      `Country code` == "DO" ~ "REPUBLICA DOMINICANA",
      `Country code` == "EC" ~ "ECUADOR",
      `Country code` == "UY" ~ "URUGUAY",
      `Country code` == "CA" ~ "CANADA",
      `Country code` == "CR" ~ "COSTA RICA",
      `Country code` == "CO" ~ "COLOMBIA",
      `Country code` == "DE" ~ "ALEMANIA",
      `Country code` == "PA" ~ "PANAMA",
      `Country code` == "GB" ~ "REINO UNIDO",
      `Country code` == "NI" ~ "NICARAGUA",
      `Country code` == "HN" ~ "HONDURAS",
      `Country code` == "BO" ~ "BOLIVIA",
      `Country code` == "IT" ~ "ITALIA",
      `Country code` == "VE" ~ "VENEZUELA",
      `Country code` == "SV" ~ "EL SALVADOR",
      `Country code` == "AU" ~ "AUSTRALIA",
      `Country code` == "MT" ~ "MALTA",
      `Country code` == "PR" ~ "PUERTO RICO",
      `Country code` == "PY" ~ "PARAGUAY",
      `Country code` == "NO" ~ "NORUEGA",
      `Country code` == "BD" ~ "BANGLADESH",
      `Country code` == "MA" ~ "MARRUECOS",
      `Country code` == "PK" ~ "PAKISTAN",
      `Country code` == "KR" ~ "COREA DEL SUR",
      `Country code` == "CU" ~ "CUBA",
      `Country code` == "PL" ~ "POLONIA",
      `Country code` == "CH" ~ "SUIZA",)
  )


# Chequeamos de nuevo si quedo alguno sin transformar.
data %>% 
  distinct(`Country code`) %>% 
  print(n = Inf)

# Modificar el estado del plan a español

data <- data %>%
  mutate(
    `Estado_Pago` = case_when(
      `Estado_Pago`== "unpaid" ~ "No Pago",
      `Estado_Pago`== "paid" ~ "Pago",
      `Estado_Pago`== "trial" ~ "prueba",)
  )
 # Renombramos la columna de tarjetas y vemos que str


data <- data %>%
  rename(Tarjetas_Activas = `Cards count`)

str(data$Tarjetas_Activas)



# --------------- TERMINAMOS LIMPIEZA Y PREPARACCION---------------------------#

# Guardo el archivo limpio con oto nombre para dsp mandar a shiny

write.csv(data, "Shiny/data_limpio.csv", row.names = FALSE)


