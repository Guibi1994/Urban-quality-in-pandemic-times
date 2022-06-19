# Revisión de datos de movilidad

# Unidades target
  # Espacial: Barrio
  # Temporal: Semana

# 0. Pasos previos ----

# 0.1. Librerias
Sys.setlocale("LC_TIME", "English")
library(dplyr)
library(ggplot2)

# 0.2. Datos
a0_raw <- readRDS("0. Datos/1. Serivi-informacion 2020.RDS") %>% 
  # Change UTM(0) tu UTM(-5)
  mutate(date = date-((1/24)*5)+((1/24)*hour),
         hour = ifelse(
           hour >=5, hour-5, (hour+24)-5))


# 2. Selección Muestral ----


  
## 2.1. ID's con superviviencia digital ----

### 2.1.1. Traking points por ID y mes  ----
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


### 2.1.2. Loop vaiando por mínimo número de puntos ----

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
  rm(x,y)
  
}

### 2.1.4. Identificación de IDs con condiciones ideales ----


R2_IDs_muestra_inicial <- R1_IDs_por_mes %>% 
  filter(periodos >= 6 & minimos_TK >=30) %>% 
  group_by(identifier) %>% 
  summarise(start = min(month),
            end = max(month),
            periodos = mean(periodos,na.rm = T),
            total_poitns = sum(points, na.rm = T),
            monthly_points = mean(points, na.rm = T)) %>% 
  as.data.frame() %>% 
  arrange(total_poitns, periodos)


### 2.1.3. Graficación de resultados ----

#### 2.1.3.1. Sampling behaviur by minimum acceptable monthly TK ----
pt <- ggpubr::ggarrange(
  (pr %>%  ggplot(aes(puntos_mes,traking_points/nrow(a0_raw)))+
     geom_path(color ="cyan4") +
     scale_y_continuous(labels = scales::percent_format(),
                        breaks = scales::pretty_breaks(12)) +
     scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
     geom_vline(xintercept = c(30,60,90,120),lty = 2, 
                color = c("green",sample("grey50",3, replace = T)))+
     labs(title = "Relationship between the threshold of monthly points and the \nsample reduction of tracking-points",
          subtitle = "*Prefilter of individuals with 6 months or more of survival",
          x = "Minimum monthly tracking points",
          y = "% of the total TK")+
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
       labs(title = "Behavior of the sample size varying the\nminimum of tracking points per month",
            #subtitle = "*Prefiltro de individuos con 9 meses o mas se superviviencia",
            x = "Minimum monthly tracking points",
            y = "Total IDs")+
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
       labs(title = "Relationship between the threshold of monthly points and the \nsample reduction of Individuals",
            #subtitle = "*Prefiltro de individuos con 9 meses o mas se superviviencia",
            x = "Minimum monthly tracking points",
            y = "% of the total IDs")+
       theme_minimal()+
       theme(text = element_text(family = "serif"),
             plot.subtitle = element_text(color = "grey50"))),
    ncol = 2)),nrow = 2)

pt
ggsave("3. Graficas/0. Seleccion muestral Traking points.png",pt, w = 10, h = 10)



#### 2.1.3.2. Average monthly TK and individual traking intervals ----
pt <- ggpubr::ggarrange(
  # Histogram of monthly poins per individual
  R2_IDs_muestra_inicial %>% ggplot(aes(monthly_points))+
    geom_histogram(fill = "cyan3", color = "cyan3", alpha = 0.1)+
    geom_vline(xintercept = c(mean(R2_IDs_muestra_inicial$monthly_points), 
                              median(R2_IDs_muestra_inicial$monthly_points)),
               lty = c(1,2)) +
    labs(title = "Average monthly poitns per indiviudal",
         subtitle = paste0("Sample size = ", nrow(pr)),
         x = "Average montlhy traking poitns",
         y = "Individuals",
         caption = 
           "* Doted line: sample's mean (205)\n**Continous line: sample's median (177)")+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
    theme_minimal()+
    theme(text = element_text(family = "serif", size = 8)) ,
  
  # Hotspot table of starting and ending traked months
  R2_IDs_muestra_inicial %>% 
    group_by(start, end) %>% 
    summarise(individuals = n(),
              total_poitns = sum(total_poitns)) %>%
    ggplot(aes(start, end))+
    geom_tile(aes(fill = individuals/sum(individuals))) +
    scale_fill_gradient(high = c("red","red2","red4"),
                        low = c("cyan4","cyan3"))+
    
    scale_y_date(breaks = scales::pretty_breaks(n = 10))+
    scale_x_date(breaks = scales::pretty_breaks(n = 10))+
    geom_text(aes(label =  paste0(
      (round((100*individuals/sum(individuals)),1))
      ,"%"),
      family = "serif",), color = "white")+
    labs(title = "Sampling distribution by individual's date intervals",
         x  = "First traked month",y ="Last traked month")+
    theme_minimal() +
    theme(text = element_text(family = "serif",siz = 8),
          legend.position = "none"))
pt
ggsave("3. Graficas/01. Average monthly TK per samlped user.png",pt, w = 10, h = 7)


## 2.2. Residence location ----

### 2.2.1. Selectin TK poitns from ideal individuals ----
a1_intial_sample <- a0_raw %>% 
  filter(identifier %in% R2_IDs_muestra_inicial$identifier)
rm(a0_raw)

### 2.1.3.


pr <- a1_intial_sample %>% 
  group_by(identifier,hour) %>% 
  summarise(lon_sd = sd(lon, na.rm = T),
            lat_sd = sd(lat, na.rm = T)) %>% 
  as.data.frame() 


pr %>% ggplot(aes(stringr::str_pad(hour,width = 2,pad = "0"), 
                  lon_sd)) +
  geom_boxplot(outlier.shape = NA, color = "cyan4")+
  coord_cartesian(ylim = c(0,0.08))+
  stat_summary(fun.y = mean, geom = "point",
               shape = 20, size = 4, color = "brown3")+
  labs(title = "Variación en la longitud de las coordenadas",
       x = "Hora", y  ="Desvición estándar (SD)")+
theme_minimal() +
  theme(text = element_text(family = "serif"))

# pr %>% ggplot(aes(stringr::str_pad(hour-5,width = 2,pad = "0"), 
#                   lon_sd)) +
#   geom_boxplot(outlier.shape = NA, color = "cyan4")+
#   coord_cartesian(ylim = c(0,0.08))+
#   
#   theme_minimal()





