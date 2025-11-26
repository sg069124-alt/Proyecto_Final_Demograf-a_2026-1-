#*#******************************#
#******************************#
#
#                        Trabajo Final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                       Tabla de mortalidad México 2010, 2020
#                                    Defunciones 
#
#         Creado por:               Andrés Peña M.
#         Fecha de creación:        11/11/2025
#         Actualizado por:          Sebastian Garces Jimenez y Gabriel Marin Robles 
#         Fecha de actualización:   14/11/2025
#         Contacto:                 sebastiangj7@ciencias.unam.mx
#
#******************************#
#******************************#

# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("script/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tablas de datos ----
def_pro <- fread("data/def_pro.csv") %>% 
  .[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]


## calculo del promedio para el ano de referencia
def_pro[ , year_new := ifelse( year %in% 2009:2011, 
                               2010,
                               ifelse( year %in% 2018:2019,
                                       2019,
                                       year ) ) ]

# datos preparados de defunciones
def <- 
  def_pro[ , 
           .( deaths = mean( deaths ) ),
           .( year = year_new, sex, age ) ] 

# Guardar tabla de DEF ----
write.csv(def, "data/def.csv", row.names = F)

