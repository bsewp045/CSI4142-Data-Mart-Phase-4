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


# Plot that shows how the number of fully vaccinated people vary during the 26 months

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

cases <- read.csv("cases.csv")
cases <- cases %>% filter(caseprovinceid != 1 & caseprovinceid != 99)
cases$caseprovinceid <- factor(cases$caseprovinceid)
cases$casedate <- ymd(cases$casedate)

# Plot that shows how the number of deaths due to covid vary during the 26 months
death_cases_grouped <- cases %>% group_by(year(casedate), month(casedate),caseprovinceid) %>% 
                        summarise(mean(numberdeaths))
death_cases_grouped <- death_cases_grouped %>% rename(
                      "year" = "year(casedate)",
                      "month" = "month(casedate)",
                      "province" = "caseprovinceid",
                      "deaths" = "mean(numberdeaths)"
)
death_cases_grouped$date <- as.Date(with(death_cases_grouped, paste(year, month, 1,sep="-")),"%Y-%m-%d")

ggplot(data= death_cases_grouped, aes(x = date, y = deaths, colour = province)) + 
  geom_point() + 
  geom_line()

# Plot that shows how the number of deaths since jan 2020 due to covid vary during the 26 months
death_cases_grouped <- cases %>% group_by(year(casedate), month(casedate),caseprovinceid) %>% 
  summarise(mean(numberdeaths))
death_cases_grouped <- death_cases_grouped %>% rename(
  "year" = "year(casedate)",
  "month" = "month(casedate)",
  "province" = "caseprovinceid",
  "deaths" = "mean(numberdeaths)"
)
death_cases_grouped$date <- as.Date(with(death_cases_grouped, paste(year, month, 1,sep="-")),"%Y-%m-%d")

ggplot(data= death_cases_grouped, aes(x = date, y = deaths, colour = province)) + 
  geom_point() + 
  geom_line()

# Plot that shows how the number of cases since jan 2020 due to covid vary during the 26 months
cases_grouped <- cases %>% group_by(year(casedate), month(casedate),caseprovinceid) %>% 
  summarise(mean(totalcases))
cases_grouped <- cases_grouped %>% rename(
  "year" = "year(casedate)",
  "month" = "month(casedate)",
  "province" = "caseprovinceid",
  "totalcases" = "mean(totalcases)"
)
cases_grouped$date <- as.Date(with(cases_grouped, paste(year, month, 1,sep="-")),"%Y-%m-%d")

ggplot(data= cases_grouped, aes(x = date, y = totalcases, colour = province)) + 
  geom_point() + 
  geom_line()

weather <- read.csv("weather.csv")
weather$weatherprovinceid <- factor(weather$weatherprovinceid)
weather$weatherdate <- ymd(weather$weatherdate)
weather_grouped <- weather %>% group_by(weatherdate,weatherprovinceid) %>% 
  summarise(mean(temperature))
weather_grouped <- weather_grouped %>% rename(
  "date" = "weatherdate",
  "province" = "weatherprovinceid",
  "temperature" = "mean(temperature)"
)
ggplot(data=weather_grouped, aes(x=date, y=temperature, colour = province)) +
  geom_point() + 
  geom_line()
