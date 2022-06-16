library(dplyr)
Sys.setenv(LANG = "en")
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","English United States")

# Identificador de semanas

pr <- data.frame(
  id_dia = seq(0,365),
  date = as.Date("2020-01-01")+seq(0,365))



pr <- pr %>% 
  mutate(dia = weekdays(date),
         day = lubridate::wday(date,abbr = T)-1,
         day = ifelse(day == 0,7,day),
         week = date - (day-1)) %>% 
  group_by(week) %>% 
  mutate(week_code = cur_group_id())



pr <- pr %>% 
  filter(between(date,as.Date("2020-01-06"), as.Date("2020-12-27")))
