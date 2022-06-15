# Revisión de datos de movilidad


# 0. Pasos previos

# 0.1. Librerias
library(dplyr)

# 0.2. Datos
a0_raw <- readRDS("0. Datos/1. Serivi-informacion 2020.RDS")

names(a0_raw)
paste0("From ",min(a0_raw$date)," to ",max(a0_raw$date))

# 2.



# View(head(a0_raw,100))
