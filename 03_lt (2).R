#**************************************************************************************#
#**************************************************************************************#
#
#                        Trabajo Final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                      Tablas de mortalidad México 2010, 2019
#                            Construcción de las tablas 
#
#         Creado por:               Andrés Peña M.
#                                   Sebastián Garcés Jiménez  
#         Fecha de creación:        14/11/2025
#         Actualizado por:          Sebastián Garcés JIménez y Gabriel Marin Robles 
#         Fecha de actualización:   15/11/2025
#         Contacto:                 sebastiangj7@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

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

# Carga de tablas de datos ----
def <- fread("data/def.csv")
apv <- fread("data/apv.csv") 

# Unión de tablas de Años Persona Vividos y Defunciones ----
lt_input <- setDT(left_join(apv, def, by = c("year", "sex", "age")))

# Cálculo de mx ----
lt_input[ , mx := deaths/N]
lt_input[ , sex := if_else(sex=="male", "m", "f")]

## Gráfica - mx por año y sexo ----
ggplot(lt_input, aes(x = age, y = log(mx), color = sex, group = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ year, ncol = 2) +
  scale_color_manual(
    values = c("m" = "steelblue", "f" = "lightcoral"),
    labels = c("m" = "Hombres", "f" = "Mujeres")
  ) +
  labs(
    title = "Tasa de mortalidad de Chiapas por año y sexo",
    x = "Edad",
    y = "log(mx)",
    color = "Sexo"
  ) +
  theme_minimal() 

## Gráfica - mx por sexo y año ----
ggplot(lt_input, aes(x = age, y = log(mx), color = factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  facet_grid(. ~ sex, labeller = as_labeller(c("m" = "Hombres", "f" = "Mujeres"))) +
  scale_color_manual(
    values = c("2010" = "steelblue", "2019" = "brown", "2021" = "pink"),
    name = "Año"
  ) +
  labs(
    title = "Evolución de la tasa de mortalidad de Chiapas",
#    subtitle = "México",
    x = "Edad",
    y = "log(mx)"
  ) +
  theme_minimal()



# Tablas de mortalidad nacional - eevv + censales 2010, 2019 ----

lt_output <- data.table()

for( s in c( 'm', 'f' ) ){
  for( y in unique( lt_input$year ) ){
    
    temp_dt <- lt_input[ sex == s & year == y ]
    
    temp_lt <-
      lt_abr(x = temp_dt$age, 
             mx = temp_dt$mx, 
             sex = s) %>%
      setDT %>%
             .[ , year := y ] %>%
             .[ , sex := s ]
    
    lt_output <- 
      rbind(
        lt_output,
        temp_lt[ , .( lt_desc = 'LT VR/Census, MEX',
                      year = y, 
                      sex,
                      age = x, 
                      mx = round( mx, 6 ), 
                      qx = round( qx, 6 ),
                      ax = round( ax, 2 ), 
                      lx = round( lx, 0 ), 
                      dx = round( dx, 0 ), 
                      Lx = round( Lx, 0 ), 
                      Tx = round( Tx, 0 ), 
                      ex = round( ex, 2 )) ]
      )
    
  }
  
}

## Esperanzas de vida al nacer ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'ex' )

## Mortalidad infantil ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'qx' )

## Gráfica - qx por sexo y año ---- 
lt_output %>%
  ggplot( ) +
  geom_line( aes( x = age, y = qx, color = factor( year ), group = factor(year ) ), size = 1 ) +
  scale_y_log10() +
  scale_color_manual(
    values = c(
      "2010" = "#C75DAA",  # Pinkish-purple
      "2019" = "#40E0D0",  # Turquoise
      "2021" = "darkgreen", 
      "Other Years"      = "gray70"    # Light gray for other years
    ),
    name = "Años"
  ) +
  facet_wrap( ~ sex, ) +
  labs(color='año') +   
  # theme_bw()
  theme_classic() +
  ylab("Probabilidad de muerte (qx)") +
  xlab("Edad") +
  labs(colour = "Años")


write.csv(lt_output, "script/lt_output.csv", row.names = F)

# Cuadro de esperanza de vida al nacer ----

ev_nacer <- lt_output[age == 0] %>%
  dcast(year ~ sex, value.var = 'ex')

setnames(ev_nacer, c("m", "f"), c("Hombres_e0", "Mujeres_e0"))

# Ordenar años para diferencias
setorder(ev_nacer, year)

# Cálculo de variaciones
ev_nacer[, `ΔHombres` := Hombres_e0 - shift(Hombres_e0)]
ev_nacer[, `ΔMujeres` := Mujeres_e0 - shift(Mujeres_e0)]

# Redondeo
ev_nacer[, (2:5) := lapply(.SD, function(x) round(x, 2)), .SDcols = 2:5]

# Guardar tabla final
write.csv(ev_nacer, "script/esperanza_vida_mejorada.csv", row.names = FALSE)

ev_nacer
# -------- FIN ----------*

