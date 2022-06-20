library(dplyr)
Sys.setenv(LANG = "en")
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","English United States")

# Cambio de una base UTM(0) a UTM(-5)



pr <- data.frame(
  "date" = as.Date("2020-01-01"),
  "hour" = seq(0,23))


pr <- pr %>% 
  mutate(new_date = date-((1/24)*5)+((1/24)*hour),
         new_hour = ifelse(
           hour >=5, hour-5, (hour+24)-5))





seq(0,23)[5]
