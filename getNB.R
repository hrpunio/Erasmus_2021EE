## --R--
library("dplyr")
nl0 <- read.csv("https://api.nobelprize.org/2.1/laureates?format=csv&limit=1200", header=T)

nrow(nl0)

names(nl0)

## empty cells to NA
nl <- nl0 %>% select (name, gender, birthdate, category, year) %>%
  filter (gender != 'org') %>%
  mutate_all(na_if,"") %>%
  mutate (birthYr = substr(birthdate, 1, 4)) %>%
  mutate (age = year - as.numeric(birthYr))

#colnames(nl) <- c("premium","change","newprice")

## Faceted histograms
library("ggplot")
ggplot(nl, aes(x=age, fill=category)) + 
  geom_histogram(binwidth=5) + 
  ggtitle("Age of nobel winners by category")
  facet_grid(category ~ .)

## Boxplots
ggplot(nl, aes(x=category, y=age, fill=category)) + geom_boxplot() + 
  ylab("years") + 
  xlab("");
## quick  
qplot(data=nl, nl$age, facets = .  ~ nl$category)

