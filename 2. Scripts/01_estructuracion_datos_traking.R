# Revisión de datos de movilidad

# Unidades target
  # Espacial: Barrio
  # Temporal: Semana

# 0. Pasos previos ----

# 0.1. Librerias
library(dplyr)
library(ggplot2)

# 0.2. Datos
a0_raw <- readRDS("0. Datos/1. Serivi-informacion 2020.RDS")

# 2. Selección Muestral ----


  
## 2.2. ID's con superviviencia digital ----

### 2.2.1. Traking points por ID y mes  ----
R1_IDs_por_mes <- a0_raw %>% 
  # Determinar mes del punto
  mutate(month = as.Date(paste0(substr(date,1,8),"01"))) %>% 
  # Agrupar por ID y mes
  group_by(identifier, month) %>% 
  summarize(points = n()) %>% 
  as.data.frame() %>% 
  group_by(identifier) %>% 
  mutate(periodos = n(),
         minimos_TK = min(points))


### 2.2.2. Loop vaiando por mínimo número de puntos ----

pr <- data.frame(
  puntos_mes = numeric(),
  IDs_totales = numeric(),
  traking_points = numeric())


for (i in seq(15,120)) {
  x <- R1_IDs_por_mes %>% 
    group_by(identifier) %>% 
    mutate(min_TK = min(points,na.rm = T),
           peridos = n()) %>% 
    as.data.frame() %>% 
    filter(min_TK >= i) %>% 
    filter(peridos >= 6) %>% 
    as.data.frame()
  
  y <- data.frame(
    puntos_mes = i,
    IDs_totales = nlevels(as.factor(x$identifier)),
    traking_points = sum(x$points, na.rm = T))
  
  pr <- rbind(pr,y)
  print(paste0("Completado al ",round(((i-14)/106)*100,2),"%"))
  
}


### 2.2.3. Graficación de resultados ----
pt <- ggpubr::ggarrange(
  (pr %>%  ggplot(aes(puntos_mes,traking_points/nrow(a0_raw)))+
     geom_path(color ="cyan4") +
     scale_y_continuous(labels = scales::percent_format(),
                        breaks = scales::pretty_breaks(12)) +
     scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
     geom_vline(xintercept = c(30,60,90,120),lty = 2, 
                color = c("green",sample("grey50",3, replace = T)))+
     labs(title = "Relación entre el umbral de putnos mensuales y la reducción \nmuestral de traking-poitns",
          subtitle = "*Prefiltro de individuos con 6 meses o mas se superviviencia",
          x = "Mínimo de traking points mensuales",
          y = "% de la muestra utilizado (IDs)")+
     theme_minimal()+
     theme(text = element_text(family = "serif"),
           plot.subtitle = element_text(color = "grey50", hjust = 0.5),
           plot.title = element_text(hjust = 0.5))),
  (ggpubr::ggarrange(
    (pr %>%  ggplot(aes(puntos_mes,IDs_totales))+
       geom_path(color ="red") +
       scale_y_continuous(labels = scales::comma,
                          breaks = scales::pretty_breaks(12)) +
       scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
       geom_vline(xintercept = c(30,60,90,120),lty = 2, 
                  color = c("green",sample("grey50",3, replace = T)))+
       labs(title = "Comportamiento del tamaño muestral variando el\nmínimo de traking poitns al mes",
            #subtitle = "*Prefiltro de individuos con 9 meses o mas se superviviencia",
            x = "Mínimo de traking points mensuales",
            y = "Tamaño muestral final (IDs)")+
       theme_minimal()+
       theme(text = element_text(family = "serif"),
             plot.subtitle = element_text(color = "grey50"))),
    (pr %>%  ggplot(aes(puntos_mes,IDs_totales/524020))+
       geom_path(color ="red") +
       scale_y_continuous(labels = scales::percent_format(),
                          breaks = scales::pretty_breaks(12)) +
       scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
       geom_vline(xintercept = c(30,60,90,120),lty = 2, 
                  color = c("green",sample("grey50",3, replace = T)))+
       labs(title = "Relación entre el umbral de putnos mensuales y la reducción \nmuestral de Individuos",
            #subtitle = "*Prefiltro de individuos con 9 meses o mas se superviviencia",
            x = "Mínimo de traking points mensuales",
            y = "% de la muestra utilizado (IDs)")+
       theme_minimal()+
       theme(text = element_text(family = "serif"),
             plot.subtitle = element_text(color = "grey50"))),
    ncol = 2)),nrow = 2)


ggsave("3. Graficas/0. Seleccion muestral Traking points.png",pt, w = 10, h = 10)

### 2.2.4. Identificación de IDs con condiciones ideales


pr <- R1_IDs_por_mes %>% 
  filter(periodos >= 6 & minimos_TK >=30) %>% 
  group_by(identifier) %>% 
  summarise(start = min(month),
            end = max(month),
            periodos = mean(periodos,na.rm = T),
            total_poitns = sum(points, na.rm = T),
            monthly_points = mean(points, na.rm = T)) %>% 
  as.data.frame()


pr %>% ggplot(aes(monthly_points))+
  geom_histogram(fill = "cyan3", color = "cyan3", alpha = 0.1)+
  geom_vline(xintercept = c(mean(pr$monthly_points), 
                            median(pr$monthly_points)),
             lty = c(1,2)) +
  labs(title = "Average monthly poitns per indiviudal",
       subtitle = paste0("Sample size = ", nrow(pr)),
       x = "Average montlhy traking poitns",
       y = "Individuals",
       caption = 
         "* Doted line: sample's mean (205)\n**Continous line: sample's median (177)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_minimal()+
  theme(text = element_text(family = "serif", size = 8))

ggsave("3. Graficas/01. Average monthly TK per samlped user.png", w = 4, h = 5)


# A) Mínimo 30 TK por mes
# B) Mínimo 6 periodos (meses) en la base













