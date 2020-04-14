####  SEGUMIENTO DE CASOS DE NEUMONIA E INFECCION RESPIRATORIA AGUDA 
####  Fuente: Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica
####  Pedro Guerrero (@mellamopeter)
####  Escrito intencionalmente sin acentos
####  Ultima actualizacion: semana 12 (publicada el 30 de marzo)

  options(scipen = 999)
#### DEFINE EL DIRECTORIO DE TRABAJO AQUI ####
  setwd("D:/OneDrive - Centro de Investigacion y Docencia Economicas CIDE/Documentos/COVID")

#### INSTALACION Y CARGA DE PAQUETES ####
  #install.packages(c("tidyverse", "readxl", "forecast"))
  library(tidyverse)
  library(readxl)
  library(forecast)
  library(psych)

#### CARGA DE LA BASE DE NEUMONÍA ####
  neumonia <- read_excel(path = "datos/boletin.xlsx", sheet = "neumonia")

#### CONVERTIMOS LA BASE A UNA SERIE ####
  neumonia <- neumonia %>% gather("ano","casos",2:7)
  neumonia <- neumonia %>% filter(semana < 15)
  
#### CANAL ENDEMICO ####
  
  ##Saquemos la serie de 2020
  veinte <- neumonia %>% filter(ano == 2020)
  veinte$tipo <- as.character(2020)
  
  ##Necesitamos la media geometrica y los intervalos de confianza
  geom_casos <- neumonia %>% group_by(semana) %>% summarise(mean = geometric.mean(casos), sd = sd(casos))
  
  geom_casos$lo <- geom_casos$mean - 2.78*geom_casos$sd/sqrt(5)

  geom_casos$hi <- geom_casos$mean + 2.78*geom_casos$sd/sqrt(5)
  
  geom_casos$sd <- NULL
  
  ##Para efectos de visualizacion, le restamos el intervalo inferior a la media y al alto
  geom_casos$hi <- geom_casos$hi - geom_casos$mean
  
  geom_casos$mean <- geom_casos$mean - geom_casos$lo
  
  geom_casos <- geom_casos %>% gather("tipo", "casos", 2:4) 
  
  geom_casos$tipo <- factor(geom_casos$tipo , levels=c("hi", "mean", "lo"))
  
  ##Grafica estacional por semana
  ggplot(geom_casos) + aes(x = semana, y = casos, fill = tipo) + geom_area() + 
    geom_line(data = veinte, aes(x=semana, y=casos)) + 
    scale_color_manual(values = c("black", "red", "green", "yellow")) +
    scale_fill_manual(limits = c("2020", "hi", "mean", "lo"),labels = c("2020", "Alarma", "Seguridad", "Éxito") ,values = c("black", "red", "yellow", "green")) +
                ylab("Casos reportados por semana") + 
    labs(title = "Canal endémico de neumonías", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
       theme(legend.title=element_blank()) + 
    scale_x_continuous(name = "Semana epidemiológica", breaks = seq(1,14,1)) +
    theme_bw()+
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  
#### Comportamiento estacional ####
  ##Pasemos la base a serie
  
  neumonia <- ts(neumonia$casos, start = 2015, frequency = 14)
  
  ##Grafica estacional por semana
  ggseasonplot(neumonia, season.labels = c(1:14)) + ylab("Casos reportados por semana") + 
    labs(title = "Nuevos casos de neumonía reportados", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
    xlab("Semana epidemiológica") + theme(legend.title=element_blank()) 


  
#### CARGA DE LA BASE DE IRA ####
  ira <- read_excel(path = "datos/boletin.xlsx", sheet = "ira")
  
#### CONVERTIMOS LA BASE A UNA SERIE ####
  ira <- ira %>% gather("ano","casos",2:7)
  ira <- ira %>% filter(semana < 15)
  
#### CANAL ENDEMICO ####
  
  ##Saquemos la serie de 2020
  veinte <- ira %>% filter(ano == 2020)
  veinte$tipo <- as.character(2020)
  
  ##Necesitamos la media geometrica y los intervalos de confianza
  geom_casos <- ira %>% group_by(semana) %>% summarise(mean = geometric.mean(casos), sd = sd(casos))
  
  geom_casos$lo <- geom_casos$mean - 2.78*geom_casos$sd/sqrt(5)
  
  geom_casos$hi <- geom_casos$mean + 2.78*geom_casos$sd/sqrt(5)
  
  geom_casos$sd <- NULL
  
  ##Para efectos de visualizacion, le restamos el intervalo inferior a la media y al alto
  geom_casos$hi <- geom_casos$hi - geom_casos$mean
  
  geom_casos$mean <- geom_casos$mean - geom_casos$lo
  
  geom_casos <- geom_casos %>% gather("tipo", "casos", 2:4) 
  
  geom_casos$tipo <- factor(geom_casos$tipo , levels=c("hi", "mean", "lo"))
  
  ##Grafica estacional por semana
  ggplot(geom_casos) + aes(x = semana, y = casos, fill = tipo) + geom_area() + 
    geom_line(data = veinte, aes(x=semana, y=casos)) + 
    scale_color_manual(values = c("black", "red", "green", "yellow")) +
    scale_fill_manual(limits = c("2020", "hi", "mean", "lo"),labels = c("2020", "Alarma", "Seguridad", "Éxito") ,values = c("black", "red", "yellow", "green")) +
    ylab("Casos reportados por semana") + 
    labs(title = "Canal endémico de Infección Respiratoria Aguda", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
    theme(legend.title=element_blank()) + 
    scale_x_continuous(name = "Semana epidemiológica", breaks = seq(1,14,1)) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  
 
  
#### Comportamiento estacional ####
  ##Pasemos la base a serie
  ira <- ts(ira$casos, start = 2015, frequency = 14)
  
  ##Grafica estacional por semana
  ggseasonplot(ira, season.labels = 1:14) + ylab("Casos reportados por semana") + 
    labs(title = "Nuevos casos de Infección Respiratoria Aguda reportados", 
         subtitle = "Boletín Epidemiológico del Sistema Nacional de Vigilancia Epidemiológica",
         caption = "Elaboración propia con datos de la Secretaría de Salud | @mellamopeter") +
    xlab("Semana epidemiológica") +theme(legend.title=element_blank())
  