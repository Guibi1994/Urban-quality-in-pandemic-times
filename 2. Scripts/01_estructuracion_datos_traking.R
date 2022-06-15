# Revisión de datos de movilidad


# 0. Pasos previos ----

# 0.1. Librerias
library(dplyr)
library(ggplot2)

# 0.2. Datos
a0_raw <- readRDS("0. Datos/1. Serivi-informacion 2020.RDS")

names(a0_raw)
paste0("From ",min(a0_raw$date)," to ",max(a0_raw$date))

# 2. Resumenes iniciales ----

## 2.1. Traking points por ID y mes  ----
R1_IDs_por_mes <- a0_raw %>% 
  # Determinar mes del punto
  mutate(month = as.Date(paste0(substr(date,1,8),"01"))) %>% 
  # Agrupar por ID y mes
  group_by(identifier, month) %>% 
  summarize(poitns = n()) %>% 
  as.data.frame()
  
## 2.2. Identificación de ID's sobrevivientes ----
  ###
R2_IDs_sobrevivientes <- R1_IDs_por_mes %>% 
  group_by(identifier) %>% 
  mutate(peridos = n()) %>% 
  as.data.frame()
  
pr %>% ggplot(aes(peridos)) +
  geom_bar()


# View(head(a0_raw,100))
