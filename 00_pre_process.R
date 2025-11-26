#******************************#
#******************************#
#
#                          Trabajo Final del Curso de Demografía 
#                                        2026_1
#                         Facultad de Ciencias UNAM
#
#                 Tablas de mortalidad San Luis Potosí 2010,2020
#                                 Preprocesamiento
#
#
#         Creado por:               Andrés Peña M. 
#         Fecha de creación:        16/11/2025
#         Actualizado por:          Sebastián Garcés Jiménez y Gabriel Marin Robles
#         Fecha de actualización:   25/11/2025
#         Contacto:                 sebastiangj7@ciencias,unam,mx
#
#******************************#
#******************************#

# Preambulo ----

## Limpieza de memoria ----

rm(list = ls())

## Carga de paquetes y Funciones ----

source("script/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tablas de datos ----

c2010 <- read_xlsx("data/INEGI_censo2010.xlsx", sheet = 1, 
                   range = "A6:D26")
c2020 <- read_xlsx("data/INEGI_censo2020.xlsx", sheet = 1, 
                   range = "A6:D26")

# Preprocesamiento censo 2010 ----



names(c2010) <- c("age", "tot", "male", "female")
setDT(c2010)

c2010 <- c2010[-1 , ] 

c2010[ , age := gsub("De ", "", age)]
c2010[ , age := substr(age, 1, 2)]
c2010[age=="No", age:=NA]
c2010[ , age:=as.numeric(age)]

c2010[ , tot := as.numeric(gsub(",", "", tot))]
c2010[ , male := as.numeric(gsub(",", "", male))]
c2010[ , female := as.numeric(gsub(",", "", female))]

c2010 <- c2010[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2010 <- melt.data.table(c2010, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2010[ , year:=2010]

# Preprocesamiento censo 2020 ----



names(c2020) <- c("age", "tot", "male", "female")
setDT(c2020)

c2020 <- c2020[-1 , ] 

c2020[ , age := gsub("De ", "", age)]
c2020[ , age := substr(age, 1, 2)]
c2020[age=="No", age:=NA]
c2020[ , age:=as.numeric(age)]

c2020[ , tot := as.numeric(gsub(",", "", tot))]
c2020[ , male := as.numeric(gsub(",", "", male))]
c2020[ , female := as.numeric(gsub(",", "", female))]

c2020 <- c2020[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2020 <- melt.data.table(c2020, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2020[ , year:=2020]


#Unir los dos censos

censos <- rbind(c2010, c2020)

#Prorrateo de los valores perdidos (missing)

# Número de observaciones con edad NA
n_na <- censos[is.na(age), .N]

if (n_na == 0L) {
  # No hay edades faltantes: solo copiamos la tabla tal cual
  censos_pro <- copy(censos)
} else {
  # Hay edades faltantes: aplicamos prorrateo
  censos_pro <- censos[ !is.na(age) ] %>% 
    .[ , p_pop := pop / sum(pop), .(year, sex)] %>% 
    merge(
      censos[ is.na(age), .(sex, year, na_pop = pop) ],
      by = c("sex", "year")
    ) %>% 
    .[ , pop_adj := pop + na_pop * p_pop ] %>% 
    .[ , .(year, sex, age, pop = pop_adj) ]
}

# Comprobación de prorrateo / totales
censos_pro[ , sum(pop), .(year, sex)]
censos[ , sum(pop), .(year, sex)]

## Guardar tabla de censos ----
write.csv(censos_pro, "data/censos_pro.csv", row.names = FALSE)

# Preprocesamiento de defunciones 1990-2024----
def <- read_xlsx("data/INEGI_def.xlsx", sheet = 1, 
                 range = "A6:G4803")

names(def) <- c("age", "year", "reg", 
                "tot", "male", "female", "ns")
setDT(def)

# Filtro
def <- def[age!="Total" & year!="Total" & year>=1990]

def[ , .N, .(age)]

def[ , age := gsub("Menores de ", "", age)]
def[ , age := substr(age, 1, 2)]
def[age=="1 ", age:=0]
def[age=="1-", age:=1]
def[age=="5-", age:=5]
def[age=="No", age:=NA] # prorrateo
def[ , age:=as.numeric(age)]


def[ , tot := as.numeric(gsub(",", "", tot))]
def[ , male := as.numeric(gsub(",", "", male))]
def[ , female := as.numeric(gsub(",", "", female))]
def[ , ns := as.numeric(gsub(",", "", ns))]

# Tabla de defunciones - comprobación
def_comp <- def[ , .(tot=sum(tot, na.rm = T),
                     male=sum(male, na.rm = T), 
                     female=sum(female, na.rm = T),
                     ns=sum(ns, na.rm = T)), 
                 .(year)]

# Imputación
def[year=="No especificado", year:=reg] 
def[ , year:=as.numeric(year)] 
def_comp[ , sum(tot)]


# Tabla final de defunciones 
def_pro <- def[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]


# Prorrateo de los valores perdidos (missing)
def_pro[ , tot:=male+female][ , `:=`(p_male=male/tot, p_female=female/tot)]
def_pro[ , `:=`(male_adj=male+p_male*ns, female_adj=female+p_female*ns)]
def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]
sum(def_pro$male)+sum(def_pro$female)


def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")
sum(def_pro$deaths)
def_pro[ , sum(deaths), .(year, sex)]

#
def_pro <- def_pro[ !is.na(age) ] %>% 
  .[ , p_deaths := deaths / sum(deaths), .(year, sex)] %>% 
  merge( def_pro[ is.na(age), 
                  .(sex, year, na_deaths=deaths)], 
         by = c("sex", "year")) %>% 
  .[ , deaths_adj := deaths + na_deaths * p_deaths] %>% 
  .[ , .(year, sex, age, deaths = deaths_adj) ]

def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]


# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro.csv", row.names = F)

# -------- FIN ----------*


