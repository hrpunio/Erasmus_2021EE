##---
##title: "Forecasting: very short introduction :-)"
##author: "TP"
##date: "01/01/2022"
##output: html_document
##   
##---
##
##```{r setup, include=FALSE}
##knitr::opts_chunk$set(echo = TRUE)
##```
##
## Introduction
##
##Data input/chart/
##
##```{r}
library("forecast")
library("fpp")

data(melsyd)

## Data input
z0 <- read.csv("MZM.csv", dec=".", sep = ';',  header=T, na.string="NA");

## What inside MZM.csv ?
str(z0)
## or in left/top window RStudio (Environment tab)
##
is.ts(z0)

## convert column `razem' from frame `z' to time-series (object)
## frequency monthly first element = 2015/1
z <-ts(z0$razem, start=c(2015, 1), frequency=12)

## creating multi column TS from data-frame (complicated)
zz <-ts(matrix(c(z0$razem, z0$krajowi), 
               dimnames = list(NULL, c('razem', 'krajowi')),
               ncol=2, byrow = F), 
        start=c(2015, 1), frequency=12)

## check if z is of TS type
is.ts(z)
is.ts(zz)

## vistors total
zz[,"razem"]


## check frequecy/start/end
frequency(z)
start(z)
end(z)

## plot/chart (basic):

plot(z)

## forecast
autoplot(z) + ylab ("persons")

autoplot(zz[,c("razem","krajowi")], facets=TRUE) + 
  ylab("")


## seasonality plot:

seasonplot(z, year.labels = TRUE, main="TITLE")

ggseasonplot(z ) + 
  ylab("persons") +
ggtitle("Malbork castle visitors")

## summary statistics
summary(z)

# time-window from 4 month 2016
# ?window
z1 <- window (z, start=c(2016, 4))

z1 <- window (z, end=c(2018, 12))
## test set
z2 <- window (z, start=c(2019, 1))

h <- 4
fit.rwf <- rwf(z1, h, drift=TRUE)

accuracy(fit.rwf, z2)

fit.lm.s <- tslm(z1 ~ trend + season )

str(fit.lm.s)

ff <- forecast(fit.lm.s, h=3)
accuracy(ff, z2)

?accuracy

##```

?ets

## Time series decomposition


W szeregu czasowym można zwykle wyróżnić długookresową tendencję (trend);
powtarzalne wahania (sezonowość); resztę traktuje się jako wartości
przypadkowe. Reasumując:

$$TS = T + S + E$$

lub 

$$TS = T \cdot S \cdot E$$

Pierwszy wariant nazywa się **addytywny** drugi **multiplikatywny**. 
W wariancie addytywnym zmiany
(trendu/sezonowości) okres/okres są stałe; w wariancie multiplikatywnym **tempo zmiany** jest stałe, tj. zjawisko okres/okres rośnie/spada o x%. W jednostkach bezwzględnych
oznacza to, że rośnie/spada coraz szybciej.

## Ocena modelu

### Ocena jakości dopasowania

* reszta (*error*) to różnica między wartością obserwowaną ($y_i$)
a wartością wyznaczoną z modelu $\hat y_i$,  czyli $e_i = y_i - \hat y_i$ 

* MSE -- średnia z kwadratów reszt (*mean square error*)

* RMSE -- pierwiastek kwadratowy z MSE (*root mean square error*)
     
* błąd procentowy -- udział reszty w wartości obserwowanej $p_i = e_i/y_i \cdot 100$
     
* MAPE -- średni bezwględny błąd procentowy (suma wartości bezwzględnych $p_i$)

UWAGA: MAPE ma fundamentalną wadę, mianowicie jeżeli $y_i$ jest bliskie zero,
to $p_i$ jest albo nieskończonością albo jest absurdalnie duże

* wielkość zbioru testowego to zwykle 20% całości (a zbiór uczący 80%). Alternatywnie
wielkość zbióru testowego jest równa horyzontowi prognozy.

* zawsze można dopasować model perfekcyjnie, ale
model dopasowany perfekcyjnie do danych niekoniecznie będzie dobrze przewidywał
(*over-fitting* czyli **przeuczenie**)

ggAcf(z)
Acf(z)

res <- residuals(naive(z))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")
### Ocena reszt

set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
```


     
Wymagane jest aby:

* reszty były nieskorelowane (jeżeli są to prognoza nie wykorzystuje
     całej dostępnej informacji, innymi słowy można ją poprawić)

* średnia reszt jest równa zero (są **nieobciążone**, nie ma błędu
systematycznego zwanego **obciążeniem**)
     
Pożądane jest aby:
     
* wariancja reszt była stała

* rozkład reszt był normalny

powyższe pozwala konstruuować lepsze prognozy przedziałowe (od/do)

```
## wyznaczenie reszt estimated.model to obiekt R
## powstały w wyniku zastosowania funkcji prognozowania np. ses
## tj fit <- ses (...)
res <- residuals (fit)
     
## wizualna ocena nieobciążoności/stałości wariancji
plot (res, main='tytuł-wykresu', xlab='ośX', ylab='ośY')

## wizualna ocena nieskorelowania
## funkcja autokorelacji
Acf(res, main='...')

## albo test Boxa-Ljunga, duże wartości p świadczą o braku AC
Box.test(res, type='Ljung-Box')

## wizualna ocena normalności reszt
hist(res)
```

### Uproszczona procedura wyboru modelu

* oszacować kilka konkurencyjnych modeli;

* wybrać ten, dla którego dopasowanie dla zbioru testowego jest najlepsze; 

* powtórzyć oszacowanie dla całości dostępnych danych

## Wybrane metody prognozowania

Metod jest *multum*. Poniżej trzy klasyczne podejścia.

## Prosty model trendu 

Zjawisko ma charakter liniowy. Prognozowanie polega na wyznaczeniu
prostej najlepiej dopasowanej do danych. Jeżeli występuje trend
zakłada się że amplituda wahań sezonowych jest stała (co do wartości bezwględnych (addytywne)lub
względnych (multiplikatywne))

```
fit <- tslm(razem.learn ~ trend )
## z sezonowością
fit <- tslm(razem.learn ~ trend + season )

## a potem
summary(fit)
```

## Wygładzanie wykładnicze 
     
proste wygładzanie wykładnicze (*simple exponential smoothing*)
**Szereg nie wykazuje się trendem/sezonowością**

Prognozowanie na podstawie ostatniej wartości:

$$y_{T+h} = y_T$$

Prognozowanie na podstawie  średniej z wartości empirycznych

$$y_{T+h} = y_T$$

SES

$$y_{T+1} = a Y_T + a(a-1)^1 Y_{T-1} + a(a-1)^2 Y_{T-2} + \cdots$$

można powiedzieć że $y_{T+1}$ to ważona średnia z wartości empirycznych. Jak bardzo
ważona zależy od parametru $a$. Im $a$ jest bliższe zeru tym szybciej
wartość wag spada (poetycko się mówi: pamięć modelu jest krótsza)

```
## ses
fit <- ses(x, h=, alpha=)

## initial określenie sposobu inicjalizacji algorytmu 
fit <- ses(x, alpha=a, initial='simple', h=3)

## Holt trend bez sezonowości
fit <- holt(x, alpha=a, beta=b, initial='simple', h=3)

## Holt-Winters trend/sezonowość
fit <- hw(x, seasonal='additive', h=3)
fit <- hw(x, seasonal='multiplicative', h=3)

## albo
## Funkcja automatycznie wybierająca
## najlepszy wariant ES
fit <- ets(x, h=)

## dalej `jedziemy' tak:
plot(fit)
summary(fit)
checkresiduals(fit)
```

## ARIMA
     
### Stacjonarność 

Szereg czasowy którego właściwości nie zmieniają się nazywa się
*stacjonarnym*. Występowanie trendu lub sezonowości oznacza brak
stacjonarności

### Różnicowanie
     
Różnicowanie pozwala wyeliminować trend/sezonowość:

$$y_t' = y_t - y_{t-1}$$
     
można różnicować różnicowane:

$$y_t'' = y_t' - y_{t-1}'$$

Różnicowanie skraca szereg czasowy (o jeden okres)

W przypadku sezonowości różnicujemy o liczbę okresów
sezonowości (np. $m=12$ dla danych miesięcznych)

$$y_t' = y_t - y_{t-m}$$

```
## Do różnicowania używamy funkcji lag
lag <-12
lagged_x <- diff(x, lag)
```

### Modele autoregresyjne (AR)

bieżąca wartość jest średnią (liniową kombinacją) z wartości
poprzednich (albo regresja liniowa na wartościach opóźnionych)

$$y_t = c_0 + c_1 y_{t-1} + c_2 y_{t-2}+ ... + c_p y_{t-p} + e_t$$

założenie: stacjonarność 

### Modele średniej ruchomej (MA)

bieżąca wartość jest średnią (liniową kombinacją) z błędów dla
wartości opóźnionych:
     
$$y_t = c_0 + c_1 e_{t-1} + c_2 e_{t-2}+ ... + e_p y_{t-q}$$

założenie: stacjonarność

ARIMA to AR + MA albo dokładniej ARIMA(p, d, q) to AR(p) +
MA(q) gdzie $d$ oznacza rząd różnicowania

```
## oszacowanie modelu w R
fit <- Arima(x, order=c(p, d, q) )
## czyli Arima(x, c(0,0,q)) = MA(q)
## czyli Arima(x, c(p,0,0)) = AR(p)
##
## funkcja auto.arima wybiera sam najlepszy model
fit <- auto.arima(x)
```

