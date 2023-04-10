library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
vaccination <- read.csv("vaccination.csv")
vaccination <- vaccination %>% filter(vaccinationprovinceid != 1)
vaccination$vaccinationprovinceid <- factor(vaccination$vaccinationprovinceid)
vaccination$vaccinationdate <- ymd(vaccination$vaccinationdate)
str(vaccination)

# Plot that shows how the number of partially vaccinated people vary during the 26 months

partially_vac_grouped <- vaccination %>% group_by(year(vaccinationdate),month(vaccinationdate),vaccinationprovinceid) %>%
                summarise(mean(partiallyvaccinated))
partially_vac_grouped <- partially_vac_grouped %>% 
  rename(
    "province"= "vaccinationprovinceid",
    "year" = "year(vaccinationdate)",
    "month" = "month(vaccinationdate)",
    "partiallyvac" = "mean(partiallyvaccinated)"
  )
partially_vac_grouped$date <- as.Date(with(partially_vac_grouped, paste(year, month, 1,sep="-")),"%Y-%m-%d")


ggplot(data= partially_vac_grouped, aes(x = date, y = partiallyvac, colour = province)) + 
  geom_point() + 
  geom_line()




fully_vac_grouped <- vaccination %>% group_by(year(vaccinationdate),month(vaccinationdate),vaccinationprovinceid) %>%
  summarise(mean(fullyvaccinated))
fully_vac_grouped <- fully_vac_grouped %>% 
  rename(
    "province"= "vaccinationprovinceid",
    "year" = "year(vaccinationdate)",
    "month" = "month(vaccinationdate)",
    "fullyvac" = "mean(fullyvaccinated)"
  )
fully_vac_grouped$date <- as.Date(with(fully_vac_grouped, paste(year, month, 1,sep="-")),"%Y-%m-%d")


ggplot(data= fully_vac_grouped, aes(x = date, y = fullyvac, colour = province)) + 
  geom_point() + 
  geom_line()



