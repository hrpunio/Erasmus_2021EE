##
## Linear regression with TS data
#
#Time series regressions are often fitted by OLS (using lm)
#
#The problem is lm expects undated data. The result of computations
#are undated as well.
#
library("AER")
#library("dynlm")
data("USMacroG")
class(USMacroG)

## lm()
cons_lm0 <- lm(consumption ~ dpi + lag(dpi), data=USMacroG)
class(cons_lm0)

summary(cons_lm0)
f0 <- fitted(cons_lm0)
class(f0)

##
library("forecast")
library("fpp2") ## uschange and other examples

uschange
f9 <- tslm(Consumption ~ Income, data=uschange)
summary(f9)

## error:
## No lags allowed in tslm
f9.e <- tslm(Consumption ~ Income + lag(Income), data=uschange)
summary(f9.e)



#There are two possible solutions
# 1. dynlm/dyn library
library("dynlm")

##install.packages("dynlm", dependencies = T)

## C_i = \beta_1 + \beta_2 DPI_i + \beta_3 dpi_{i-1} + \epsilon_i

cons_lm1 <- dynlm(consumption ~ dpi + L(dpi), data=USMacroG)

summary(cons_lm1)

## information stamp lost
f1 <- fitted(cons_lm1)
class(f1)

###
## library dyn
install.packages("dyn")
library("dyn")

cons_lm2 <- dyn(lm(consumption ~ dpi + lag(dpi), data=USMacroG))

summary(cons_lm2)

##
## Is there a relationship between economic!!!!!!!
## growth and carbon dioxide emissions? !!!!!!!!!!

## Proposed project
## 
## http://data.worldbank.org/data-catalog
## https://data.worldbank.org/indicator?tab=all

##install.packages("WDI")

library("WDI")
hdi.i <- WDIsearch(string = "hdi", field = "name", short = TRUE, cache = NULL)
gdp.i <- WDIsearch(string = "gdp", 
                   field = "name", short = TRUE, cache = NULL)
#s.a <- WDIbulk()

## Human Dev Index / GDP in PL and RU
ii <- WDI(country=c("PL", "RU"), 
    indicator=c("UNDP.HDI.XD","NY.GDP.PCAP.CD"),
    start=1990, end=2020)
##





## Crime
## https://github.com/gregtozzi/north_carolina_crime