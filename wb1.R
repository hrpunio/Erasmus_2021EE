##
library("tidyverse")
library("ggplot2")
library("ggfortify")
library("forecast")
library("knitr")
library("lmtest")
library("car") # D-W test

### check also: https://www.youtube.com/watch?v=0T0KHNqDwc4
###  =======================

### Read all the data
##'NY.GDP.MKTP.KD',    = GDP constant US$ 2015
##'SP.POP.TOTL',       = Population total
##'EN.ATM.CO2E.PC',    = CO2 emission in tons per/capita
##"EG.ELC.RNWX.ZS",    =  Electricity from renovable sources % of total
##'NY.GDP.PCAP.CD',    = GDP per capita (current US$) 
##"NY.GDP.PCAP.PP.CD", = GDP per capita, PPP (current international $) 
##'EG.ELC.COAL.ZS',    = Electricity produced from coal % of total
##'AG.PRD.LVSK.XD',    = Livstock production index (2014--2016=100)
##"AG.CON.FERT.ZS",    = Fertilizer consumption (kg/hectare of arable land)
##"EG.USE.PCAP.KG.OE"  = Energy use (kg of oil equivalent per capita)

wb <- read.csv("WBdata.csv", sep = ';',  header=T, na.string="NA")


### Clean the data
##unwated <- c('OSS', 'LTE', 'AFW', 'LCN', 'LCD')
## wb_groups_list.csv file contains definitions of all country-groups
## so we can filter-out wb file using information form wb_groups_list.csv
wb.unwanted <- read.csv("wb_groups_list.csv", 
    sep = ';',  header=T, na.string="NA")

## Remove country-groups: filter (! code %in% wb.unwanted$code )
## Transform to _wider_ format: pivot_wider
## Change colum names to something less complicated and select the columns we plan to use: select
## Crossectional data for 2015: filter (year == 2015)
## Remove small countries: filter (pop > 1000000)
wbl <- wb %>% filter (! code %in% wb.unwanted$code ) %>%
  pivot_wider( names_from = indicatorcode, values_from = value) %>%
  select (countryname, code, year, 
          co2=EN.ATM.CO2E.PC, 
          gdp=NY.GDP.PCAP.PP.CD, 
          e4coal=EG.ELC.COAL.ZS,
          fcons=AG.CON.FERT.ZS, 
          euse=EG.USE.PCAP.KG.OE, 
          gdpt=NY.GDP.MKTP.KD,  
          pop=SP.POP.TOTL) %>%
  filter (year == 2015) %>%
  filter  (pop > 1000000)

## check sample size:
nrow(wbl)

## scatter-plot matrix for co2, gdp, fcons, euse
wbl %>% select (co2, gdp, fcons, euse, e4coal) %>% pairs()

## print descriptive statistics for our data-set
summary(wbl)

## print correlation matrix for co2, gdp, fcons, euse, e4coal
## NOTE: na.omit function omits rows with NA values
wb.corr <- wbl %>% select (co2, gdp, fcons, euse, e4coal) %>% 
  na.omit() %>% cor()
wb.corr
##


## Model 0
## $CO2 = a + b GDP$

lm0 <- lm(co2 ~ gdp, data=wbl)
summary(lm0)


## Model 1
## $\ln CO2 = a + b \ln GDP$

lm1 <- lm(log(co2) ~ log(gdp), data=wbl)
summary(lm1)

## Model 2
## $\ln CO2 = a + b \ln GDP $
lm2 <- lm(log(co2) ~ log(gdp) + I(log(gdp)^2 ), data=wbl)
summary(lm2)

## Perhaps some other models ...

## For each model perform diagnostics

## chart residuals vs fitted
plot(lm2, which = 1) 

## Check heteroscedasticity
## Breusch-Pagan Test For Homoscedasticity
## high p = no heteroscedasticity
bptest(lm2)

## Check autocorrelation of error term
## Durbin Watson Test for Autocorrelation
## high p = errors not autocorrelated
durbinWatsonTest(model1)

## Normality of residuals
## visually looking at qq-plot
plot(lm2, which = 2) 
qqPlot(lm2)

shapiro.test(lm2$residuals)

## Check multicollinearity
## for models with 
vif(lm2)

## lm2 is highly collinear but it is OK

## Select between nested models with anova
## (significance of difference of variance explainded)
anova(lm1, lm2, test="F") 

# Model 2 is significantly better than model 1

## Repeat for other sample

wb.highmid <- read.csv("wb_groups.csv", 
                        sep = ';',  header=T, na.string="NA") %>%
  ## about 30 countries:
  ##filter ( GroupName == 'Low income')
  ##filter ( GroupName == 'High income')
  filter ( GroupName == 'High income' | GroupName == 'Middle income')

nrow(wb.highmid)

wbl <- wb %>% filter (code %in% wb.highmid$code ) %>%
  pivot_wider( names_from = indicatorcode, values_from = value) %>%
  select (countryname, code, year, 
          co2=EN.ATM.CO2E.PC, 
          gdp=NY.GDP.PCAP.PP.CD, 
          e4coal=EG.ELC.COAL.ZS,
          fcons=AG.CON.FERT.ZS, 
          euse=EG.USE.PCAP.KG.OE, 
          gdpt=NY.GDP.MKTP.KD,  
          pop=SP.POP.TOTL) %>%
  filter (year == 2015)
nrow(wbl)

