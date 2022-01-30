### Read the data
wb <- read.csv("WBdata.csv", sep = ';',  header=T, na.string="NA")

library("tidyverse")
##

unwated <- c('OSS', 'LTE', 'AFW')



wbl <- wb %>% pivot_wider(names_from = indicatorcode, values_from = value) %>%
  filter (year == 2015) 
  select (code, countryname, NY.GDP.PCAP.PP.CD, gdp=NY.GDP.PCAP.CD, co2=EN.ATM.CO2E.PC) %>%
  
    filter (! code %in% unwated)

plot(wbl$gdp, wbl$co2)

model1 <- lm( co2 ~ gdp, data = wbl)

summary(model1)

plot(wbl$gdpppp, wbl$co2)


model2 <- lm( co2 ~ gdpppp, data = wbl)
summary(model2)
