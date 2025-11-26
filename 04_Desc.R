##**********************************************************************************************************
##**********************************************************************************************************
#
#                          Trabajo Final del curso de Demografía
#                                        2026-1
#                         Facultad de Ciencias UNAM
#                             Tabla de mortalidad Michoacán 2010,2020
#                                     DESCOMPOSICIÓN
#
#         Creado por:               Marin Robles Gabriel
#                                   Cesar David Garcia Santos y Sebastián Garcés Jiménez
#         Fecha de creación:        24/11/2025
#         Fecha de actualización:   24/11/2025
#         Contacto:                 tugabo@ciencias.unam.mx
#
##**********************************************************************************************************
##**********************************************************************************************************


# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tablas de datos ----
df <- fread("data/lt_vrz.csv")

setDT(df)
df[ , year := as.numeric(year)]  # Asegurar que year es numérico
df[ , sex := as.character(sex)]  # Asegurar que sex es character
df[ , age := as.numeric(age)]    # Asegurar que age es numérico


# ----------------------------------------------------------
# DESCOMPOSICIÓN DE ARRIAGA (1984) - VERSIÓN MÁS ROBUSTA
# ----------------------------------------------------------

descomp_arriaga <- function(data, year1, year2, sex){
  
  # Convertir parámetros a los tipos correctos
  year1 <- as.numeric(year1)
  year2 <- as.numeric(year2)
  sex <- as.character(sex)
  
  # Filtrar datos por año y sexo
  d1 <- data[year == year1 & sex == sex]
  d2 <- data[year == year2 & sex == sex]
  
  # Verificar que hay datos
  if(nrow(d1) == 0) {
    stop(paste("No hay datos para año", year1, "y sexo", sex))
  }
  if(nrow(d2) == 0) {
    stop(paste("No hay datos para año", year2, "y sexo", sex))
  }
  
  # Ordenar por edad
  d1 <- d1[order(age)]
  d2 <- d2[order(age)]
  
  # Comprobar que tengan las mismas edades
  if(nrow(d1) != nrow(d2)){
    stop("Las tablas de ambos años no tienen las mismas edades.")
  }
  
  # Extraer variables
  q1 <- d1$qx
  q2 <- d2$qx
  L1 <- d1$Lx
  L2 <- d2$Lx
  T1 <- d1$Tx
  l1 <- d1$lx
  edad <- d1$age
  
  # l0 típico (100000)
  l0 <- l1[1]
  
  # ------- COMPONENTE DIRECTO DE ARRIAGA -------
  CD <- (L2 - L1) / l0
  
  # ------- COMPONENTE INDIRECTO DE ARRIAGA -------
  CI <- ((q1 - q2) * (T1 - L1)) / l0
  
  # ------- COMPONENTE DE INTERACCIÓN -------
  INT <- 0.5 * ((q2 - q1) * (L2 - L1)) / l0
  
  # Contribución total por edad
  contrib <- CD + CI + INT
  
  # Crear tabla de salida
  out <- data.frame(
    age = edad,
    direct = CD,
    indirect = CI,
    interaction = INT,
    contrib = contrib
  )
  
  # Cambios totales
  out$total_change <- sum(out$contrib)
  
  # Guardar e0 inicial y final
  out$e0_year1 <- d1$ex[d1$age == 0]
  out$e0_year2 <- d2$ex[d2$age == 0]
  out$change_e0 <- out$e0_year2[1] - out$e0_year1[1]
  
  return(out)
}

# Aplicar la descomposición
tryCatch({
  # 1. 2010 → 2019 (masculino)
  res_2010_2019_m <- descomp_arriaga(df, 2010, 2019, "m")
  print("2010-2019 Hombres:")
  print(head(res_2010_2019_m))
  
  # 2. 2010 → 2019 (femenino)
  res_2010_2019_f <- descomp_arriaga(df, 2010, 2019, "f")
  print("2010-2019 Mujeres:")
  print(head(res_2010_2019_f))
  
  # 3. 2019 → 2021 (masculino)
  res_2019_2021_m <- descomp_arriaga(df, 2019, 2021, "m")
  print("2019-2021 Hombres:")
  print(head(res_2019_2021_m))
  
  # 4. 2019 → 2021 (femenino)
  res_2019_2021_f <- descomp_arriaga(df, 2019, 2021, "f")
  print("2019-2021 Mujeres:")
  print(head(res_2019_2021_f))
  
}, error = function(e) {
  print(paste("Error:", e$message))
})

# Función para gráficas ----
plot_arriaga <- function(result, title = "Contribución por edad") {
  ggplot(result, aes(x = age, y = contrib)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(
      title = title,
      x = "Edad",
      y = "Contribución a Δe₀ (años)"
    ) +
    geom_hline(yintercept = 0, color = "red") +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 13)
    )
}

# Generar gráficas 
if(exists("res_2010_2019_m")) {
  plot_arriaga(res_2010_2019_m, "Descomposición de Arriaga: Hombres 2010–2019")
}
if(exists("res_2010_2019_f")) {
  plot_arriaga(res_2010_2019_f, "Descomposición de Arriaga: Mujeres 2010–2019")
}
if(exists("res_2019_2021_m")) {
  plot_arriaga(res_2019_2021_m, "Descomposición de Arriaga: Hombres 2019–2021")
}
if(exists("res_2019_2021_f")) {
  plot_arriaga(res_2019_2021_f, "Descomposición de Arriaga: Mujeres 2019–2021")
}

# -------- FIN ----------*
