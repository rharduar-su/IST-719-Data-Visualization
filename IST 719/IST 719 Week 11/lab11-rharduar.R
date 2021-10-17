#Rayanna Harduarsingh
#Lab 11
#April 28th, 2021

library(tidyverse)
library(ggplot2)

sales <- read_csv(paste0("/Users/rayannaharduarsingh/Desktop/data/sales.csv"))
file.choose()
View(sales)

my.cols <- c("deeppink", "hotpink1", "palevioletred1", "pink1", "mistyrose1")
pie(rep(1,5), col = my.cols)

#violin
ggplot(sales) + aes(x= rep.region, y=recipt, fill = rep.region) + geom_boxplot()

ggplot(sales) + aes(x= rep.region, y=recipt, fill = rep.region) +
   geom_violin() +
   scale_fill_manual(values = my.cols) +
   theme_minimal() +
   ggtitle("Violin Plots")

ggplot(sales) + aes(x = rep.region, y = recipt, fill = type) +
  geom_violin() +
  scale_fill_manual(values = c("maroon", "gold")) +
  theme_minimal() +
  ggtitle("Violin Plots by Type and Region")


#stacked area plot
reg.year <- aggregate(sales$units.sold
                      , list(year = sales$year, reg = sales$rep.region)
                      , sum)

ggplot(reg.year) + aes(x = year, y = x, fill = reg) +
  geom_area() +
  scale_fill_manual(values = my.cols) +
  theme_minimal() +
  ggtitle("Area Plots")

#mosaic 
install.packages("ggmosaic")
library(ggmosaic)

wine.reg <- aggregate(sales$units.sold
                      , list(w = sales$wine, r = sales$rep.region)
                      , sum)
ggplot(wine.reg) + 
  geom_mosaic(aes(weight = x, x = product(r), fill = w)) 

ggplot(wine.reg) + 
  geom_mosaic(aes(weight = x, x = product(w), fill = r)) +
  scale_fill_manual(values = my.cols) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Mosaic Plot") +
  xlab("") + ylab("") +
  theme(legend.position = "none"
        , panel.grid = element_blank())

#treemap

install.packages("treemapify")
library(treemapify)
my.df <- aggregate(sales$units.sold
                   ,list(t = sales$type, w = sales$wine, r = sales$rep.region)
                   ,sum)
ggplot(my.df) + aes(area = x, fill = r, subgroup = r) +
  geom_treemap() +
  geom_treemap_subgroup_text(color = "white") +
  geom_treemap_text(aes(label = w), color = "black") +
  scale_fill_manual(values = my.cols) +
  guides(fill = FALSE) +
  ggtitle("Treemap!!!")

#ggalluvial

install.packages("ggalluvial")
library(ggalluvial)

ggplot(my.df) + aes(y = x, axis1 = r, axis2 = t, axis3 = w) +
  geom_alluvium(aes(fill = r)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))

ggplot(my.df) + aes(y = x, axis1 = r, axis2 = t, axis3 = w) +
  geom_alluvium(aes(fill = r)) +
  geom_stratum(alpha = .1, color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  scale_fill_manual(values = my.cols) +
  ggtitle("Alluvial Plot")

#circlize
install.packages("circlize")
library(circlize)

sale_wind <- tapply(sales$units.sold
                    , list(sales$rep.region, sales$wine)
                    , sum)

chordDiagram(sale_wind)
chordDiagram(sale_wind, transparency = .1)
chordDiagram(sale_wind, transparency = .5, big.gap = 100)
chordDiagram(sale_wind, transparency = .1, small.gap = 10)
chordDiagram(sale_wind, transparency = .1, big.gap = 20, small.gap = 5)
chordDiagram(sale_wind, transparency = .1, big.gap = 20, small.gap = 0)

unique(sales$rep.region)
unique(sales$wine)
grid.col <- c(Central = my.cols[1]
              , East = my.cols[2]
              , North = my.cols[3]
              , South = my.cols[4]
              , West = my.cols[5]
              , Riesling = "gray"
              , `Pinot Gris` = "gray"
              , Chardonnay = "gray"
              , `Cabernet Sauvignon` = "gray"
              , Merlot = "gray"
              , `Sauvignon Blanc` = "gray"
              , Shiraz = "gray"
)

circos.clear()
circos.par(start.degree = 45, clock.wise = FALSE)

chordDiagram(sale_wind, transparency = .5, grid.col = grid.col
             , big.gap = 20, small.gap = 0)
  

