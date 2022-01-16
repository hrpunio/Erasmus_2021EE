##
## title: "HICP" (inflation)
## https://ec.europa.eu/eurostat/web/hicp
## https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=prc_hicp_manr&lang=en
library("tidyverse")
#library("stringr") ?
library("ISOweek") ###
library("knitr")
library("eurostat")
## Importing data
## you can use
## f <- read.csv(URL)
## but URL is frequently obfuscated and/or data is `dirty'
## but there are some packages enable direct import from the Internet
##
## unit = RCH_A	Annual rate of chang
## coicop = (All-items HICP = CP00 ; Food including alcohol and tobacco = FOOD)
hicp <- get_eurostat("prc_hicp_manr",  stringsAsFactors = FALSE) %>%
  mutate (time = as.character(time)) %>%
  mutate (coicop = factor(coicop)) %>%
  mutate (year = as.numeric(substr(time, 1, 4)), 
          month = as.numeric(substr(time, 6,7)),
          geo = as.factor(geo)) %>%
  select (geo, cooicop, year, month, value=values)
## **During Rstudio session the result is catched **
##
## dplyr/tidyverse

levels(hicp$coicop)

##  

head(hicp)

hicp.pl <- hicp %>% filter (geo == 'PL') %>%
  filter (coicop == 'FOOD')


library("ggplot2")

pf <- ggplot(hicp.pl, aes(x=as.Date(time), y=values, colour=geo)) +
  geom_line(size=.8, alpha=.3) +
  geom_point(size=.4, alpha=.5) +
  xlab(label="") +
  ylab(label="") +
  ggtitle("HICP")
pf

hicp.xx <- hicp %>% filter (geo %in% c('PL', 'DE', 'IT', 'CZ')) %>%
  filter (coicop == 'FOOD')

px <- ggplot(hicp.xx, aes(x=as.Date(time), y=values, colour=geo)) +
  geom_line(size=.8, alpha=.6) +
  geom_point(size=.4, alpha=.5) +
  xlab(label="") +
  ylab(label="") +
  ggtitle("HICP: food")
px

## 
