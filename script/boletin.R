####  SEGUMIENTO DE CASOS DE NEUMONIA E INFECCION RESPIRATORIA AGUDA 
####  Fuente: Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica
####  Pedro Guerrero (@mellamopeter)
####  Escrito intencionalmente sin acentos

  options(scipen = 999)
#### DEFINE EL DIRECTORIO DE TRABAJO AQUI ####
  setwd("D:/OneDrive - Centro de Investigacion y Docencia Economicas CIDE/Documentos/COVID")

#### INSTALACION Y CARGA DE PAQUETES ####
  install.packages(c("tidyverse", "readxl", "forecast"))
  library(tidyverse)
  library(readxl)
  library(forecast)

#### CARGA DE LA BASE DE NEUMONÍA ####
  neumonia <- read_excel(path = "boletin.xlsx", sheet = "neumonia")

#### CONVERTIMOS LA BASE A UNA SERIE ####
  neumonia <- neumonia %>% gather("ano","casos",2:7)
  neumonia <- ts(neumonia$casos, start = 2015, frequency = 52)

#### GRAFICAS ####
  ##Serie de tiempo (solo tiene hasta la semana 11 de cada año)
  ts.plot(neumonia)
  
  ##Grafica estacional por semana
  ggseasonplot(neumonia) + ylab("Casos reportados por semana") + 
    labs(title = "Nuevos casos de neumonía reportados", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
    xlab("Semana epidemiológica") +theme(legend.title=element_blank())

  
#### CARGA DE LA BASE DE ira ####
  ira <- read_excel(path = "boletin.xlsx", sheet = "ira")
  
#### CONVERTIMOS LA BASE A UNA SERIE ####
  ira <- ira %>% gather("ano","casos",2:7)
  ira <- ts(ira$casos, start = 2015, frequency = 52)
  
#### GRAFICAS ####
##Serie de tiempo (solo tiene hasta la semana 11 de cada año)
  ts.plot(ira)
  
##Grafica estacional por semana
  ggseasonplot(ira) + ylab("Casos reportados por semana") + 
    labs(title = "Nuevos casos de Infección Respiratoria Aguda reportados", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
    xlab("Semana epidemiológica") +theme(legend.title=element_blank())

