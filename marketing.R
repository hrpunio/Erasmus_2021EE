## -- R --
## Analysis of cross-cestional data
## 
library("tidyverse") 
library("caret")
library("ggfortify")


library("car")
head(Salaries, n=9)

levels(Salaries$rank)
levels(Salaries$discipline)

##install.packages("caret")

load ("marketing.rda")
 
## 
training.samples <- marketing$sales %>%
  createDataPartition(p=0.8, list = F)

train.data <- marketing[training.samples, ]
test.data <- marketing[ -training.samples, ]

##
plot(train.data)


?autoplot
autoplot(as.matrix(train.data), facet=F)

## autoplot ale ts
## https://rstudio-pubs-static.s3.amazonaws.com/325621_fe5c68d1df274c6b8b287428bb484698.html

## Simple regression
## sales = a + b

# lm function 1st argument is called 'formula argument' and specifies
# the model
# ~ sign separates the dependent variables with explanatory/independent (left)
#  variables (right). Variable on the left side of the sign is the response
#  variable and on the right are explanatory variables.
# + var adds variable - var removes variable
# 1 means intrecept (included by default)
# so to specify regression by origin we should type
# sales ~ youtube - 1

# tilde ~
model1 <- lm(sales ~ youtube, data = train.data)

# components of lm model (formally R `object`):
names(model1)

# summary statistics of the model are provided with the summary
# function in R. With summary statistics, we can assess (among other
# things) the prediction accuracy of model. 

summary(model1)

## Call: model specification
## 
## Residuals: distribution of residuals
## Coefficients: estimated coefficients, their standard values, t-values
## and probabilities
##
## Residual standard error:  \sqrt { RSS/DF }
##  RSS = \sum ( y - \hat y)^2
##  DF = n -k  where k = number of independent variables + intercept
##    (or n - k - 1 if k means independent variables w/o intercept;
##     anyway DF means number of parameters in the model to estimate)
## in theory should be normal with mean 0
## Multiple R-squared: coefficient of determination
##  współczynnik determinacji (in polish); R^2
##  the proportion of the variation that is explained by the regression line
##  (ESS) to total variation (TSS): ie
## R^2 = ESS/TSS = \sum (\hat y - \bar y)^ / \sum (y - \bar y)^2
## R^2 BTW is also squared correlation between y and \hat y
##
## BTW \bar y = \bar \hat y (property of regression line)
## Adjusted R^2 = (1- R^2)\times (n-1)/(n- k -1)
## 
## F-statistic test the hypothesis H_0: all coefficients are equal to 0
## (except for the intercept)
## The distribution of F(k -1, n - k)
## for small probability H_0 should be rejected

## residuals
residuals(model1)
fitted(model)
##
coefficients(model1)["youtube"]


confint(model1)

# sales = 8.43 + 0.048 youtube

####
# Default plot
# 2nd argument indicates type of plot
# 1 -- residuals vs fitted
plot (model1, which=1)

library("ggplot2")
library('ggfortify')
# install.packages('ggfortify')
autoplot(model1, which=1)

## influential values
## plot (model1, which=4)

## Breusch-Pagan Test For Homoscedasticity
library(lmtest)
bptest(model1)

# Durbin Watson Test for Autocorrelation
#
# The Durbin Watson examines whether the errors are autocorrelated with themselves. 
# The null states that they are not autocorrelated (what we want). 
# This test could be especially useful when you conduct a multiple 
# (times series) regression.

durbinWatsonTest(model1)

# Multic

# VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 
# are signs of serious multicollinearity requiring correction.
library("car")
vif(model1)

###### ############################
# predictions

?predict

#predictions1 <- predict(model1, test.data, interval='confidence')
predictions1 <- predict(model1, test.data )

predictions1

plot(sales ~ youtube, data = train.data )
 points (predictions1 ~ test.data$youtube, col='blue')
 lines (predictions1 ~ test.data$youtube, col='red')
  
#plot(model1, which=1) 

library("car")
Salaries
model.9 <- lm(salary ~ yrs.service + rank + discipline + sex, data=Salaries)
summary(model.9)

anova(model.9)


## #####################
## #####################
## Multiple regression

model2 <- lm(sales ~ youtube + facebook, data=train.data)

summary(model2)

## multicollinearity
vif(model2)

## correlation matrix

corMatrix <- cor(train.data)
corMatrix

##
library(corrplot)
corrplot(corMatrix, method = 'number')
corrplot(corMatrix, method = 'color', order = 'alphabet')

# Model comparison/selection
# the anova function compares nested models. 
# Where we are dealing with regression models, then we apply the F-Test 
# and where we are dealing with logistic regression models, 
# then we apply the Chi-Square Test. 
# By nested, we mean that the independent variables of the simple model 
# will be a subset of the more complex model. 
# Note that we should fit the models on the same dataset.

# we test if we should include facebook or not:
anova(model2, model1, test="F") 

#  we should prefer m.model

m2.model <- lm(sales ~ youtube + facebook + newspaper, data=train.data)
summary(m2.model)

anova(m.model, m2.model, test="F") 

anova(model, model1, model2, test="F") 

## ######################
## more Multiple regression
library("AER")
data("CPS1988")

str(CPS1988)
summary(CPS1988)
## data.frame:	28155 obs. of  7 variables:
## wage      : num  355 123 370 755 594 ...
## education : int  7 12 9 11 12 16 8 12 12 14 ...
## experience: int  45 1 9 46 36 22 51 34 0 18 ...
## ethnicity : Factor w/ 2 levels "cauc","afam": 1 1 1 1 1 1 1 1 1 1 ...
## smsa      : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
## region    : Factor w/ 4 levels "northeast","midwest",..: 1 1 1 1 1 1 1 1 1 1 ...
## parttime  : Factor w/ 2 levels "no","yes": 1 2 1 1 1 1 1 1 1 1 ...

## use dplyr
## https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corMatrix <- CPS1988 %>% select (wage, education, experience) %>% cor()
corMatrix

corrplot(corMatrix, method = 'color', order = 'alphabet')
corrplot(corMatrix, method = 'number')

## wage $$ / week
## education and experience are measured in years
## ethnicity
levels(CPS1988$ethnicity)

## Parttime
levels(CPS1988$parttime)
## this is pretty big
nrow(CPS1988)

## later
##plot(CPS1988)
##

model3 <- lm ( log(wage) ~ experience + I(experience^2) + education + ethnicity,
               data = CPS1988 )
## NOTE: Inside formula +,*,/,and ^ has special meaning
## If one has to specify x^2 one has to use I() function as in the example above
summary(model3)

## ethnicityafam means afam is 1 and cauc is 0
## so negative coefficient can be interpreted as 
## difference between (log) wage between afam (1) and cauc(0)
## In R factors are handled as (set) of dychotomous variables automatically
## If factor contains more than 2 levels, l-1 dychotomous variables
## will be created


## Simpler model w/o ethnicity
model3a <- lm ( log(wage) ~ experience + I(experience^2) + education,
               data = CPS1988 )
summary(model3a)

## models are 'nested'
## 
anova(model3a, model3)

## Akaike information criterion (AIC) is a metric that is used to compare 
## the fit of several regression models
## smaller AIC = model is a better fit
AIC(model3a,model3)

## Ehnicity is significant at any reasonable level

## Interactions
model3b <- lm ( log(wage) ~ experience + I(experience^2) + education*ethnicity,
                data = CPS1988 )
summary(model3b)
anova(model3b, model3)

## interaction  education*ethnicity is significant at any reasonable level

## See also
## https://rpubs.com/dvallslanaquera/lm_cigarette