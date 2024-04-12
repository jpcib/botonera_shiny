library(shinyauthr)
library(bslib)
library(conflicted)
library(dplyr)
library(reshape2)
library(rintrojs)
library(plotly)
library(scales)
library(sf)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggplot2)
library(stringr)
library(tidyr)
library(shinythemes)
library(rsconnect)

base_usuarios <- tibble::tibble(
  user = c("sintys", 'botonera'),
  password = c("sintys.1998", 'botonera2024'),
  permissions = c("admin",'standard'),
  name = c("Sintys", 'Botonera')
)

assign('tabla_luz', read.csv((file = file.path('files',"BOTONERA_LUZ.csv")), encoding = "UTF-8"))
assign('tabla_gar', read.csv((file = file.path('files',"BOTONERA_LUZ_GARRAFA.csv")), encoding = "UTF-8"))
assign('tabla_nat', read.csv((file = file.path('files',"BOTONERA_LUZ_GAS_NAT.csv")), encoding = "UTF-8"))

users <- c("German" = "German.2024")

campos_basicos <- c(
  'DECIL_INGRESO',
  'id_zona_bio',
  'TIPO_VIVIENDA',
  'CANT_TOTAL_HOGARES'
)


zonas <- c('Todas las zonas',"Muy cálido","Cálido","Templado cálido","Templado frío", "Frío", "Muy frío")
select_zona <- setNames(c(7,1,2,3,4,5,6),zonas)

personas <- c('Todos los grupos','1 o 2', '3 o 4', 'Más de 4')
select_personas <- setNames(c('4','1', '2', '3'),personas)

porcentajes <- c('0%','2%','4%',"6%","8%","10%")
select_porcentaje <- setNames(c('CRITERIO_0','CRITERIO_2', 'CRITERIO_4','CRITERIO_6','CRITERIO_8','CRITERIO_10'),porcentajes)

servicios <- c('Todos','Electricidad','Electricidad y Gas Natural','Electricidad y GLP')
select_servicio <- setNames(c('todos','tabla_luz','tabla_nat','tabla_gar'),servicios)

bienes <- c("Aeronaves", "Embarcaciones", "2 o más inmuebles")
select_bienes <- setNames(c('AERONAVES',"EMBARCACIONES",'INMUEBLES_2_O_MAS'),bienes)

autos <- c("Auto de menos de 2 años","Auto de menos de 5 años","Auto de menos de 10 años",'No excluir por auto')
select_autos <- setNames(c('AUTO_2','AUTO_5','AUTO_10','ninguno'),autos)

f <- function(n) {
  formatted <- format(n, big.mark = ".", decimal.mark = ',',scientific = FALSE)
  return(formatted)
}

f2 <- function(n) {
  rounded <- round(n, 2)  # Redondear a 2 decimales
  formatted <- format(rounded, decimal.mark = ',')
  return(formatted)
}

