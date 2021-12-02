require(ggplot2)
library("dplyr")
library(RColorBrewer)
library(ggpubr)

## Bare;Cropland;Tree;Grassland;Shrubland;Sparse;Artificial;InlandWater;Wetland
##  1    2        3   4         5         6       7          8           9
mycolors <- c( '#e6550d', '#636363', '#31a354', '#e5f5e0', '#3182bd', '#feb24c', '#ffffb3', '#005a32', '#543005')



showBP <- function(country, cname) {

  p <- ggplot(d, aes(x = class, y=country, fill=class )) +
    geom_bar(stat="identity") +
    xlab(label="") + ylab(label="% ") +
    ##coord_cartesian(ylim = c(0, 60)) +
    scale_y_continuous(name="?", limits=c(0, 60)) +
    coord_flip()+ ggtitle(sprintf ("Share of land use %s", cname)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = mycolors) +
    geom_text(aes(label=sprintf ("%.1f", country)), hjust=-.08, color="black", size=3.0) +
    ###scale_color_discrete(guide=F) +
    theme(legend.position="none")

  return(p)
}


d <- read.csv("land_cover_2015_percent_byCountry.csv", sep = ';', dec = ".",  header=T, na.string="NA");

p1 <- showBP(d$Poland, 'PL');
##p2 <- showBP(d$Uzbekistan, 'UZ');
p2 <- showBP(d$Georgia, 'GE');
p3 <- showBP(d$Germany, 'DE');
p4 <- showBP(d$France, 'FR');
p5 <- showBP(d$Spain, 'ES');
p6 <- showBP(d$Italy, 'IT');
p7 <- showBP(d$EU28, 'EU28');
p8 <- showBP(d$World, 'World');

ggarrange(p1, p2, p3,p4, p5, p6, p7, p8, ncol = 2, nrow = 4)

