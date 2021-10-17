#Author: Rayanna Harduarsingh
#Week 8 Lab 8

x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11) 
y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)

plot(x,y,type = "l")

#map packages deal with aspect

install.packages("maps")
library(maps)

map("state")
map("world")
#chlorpleth

setwd("/Users/rayannaharduarsingh/Desktop/data")
load("shootings.Rda")

shootings <- data.frame(shootings)
colnames(shootings)
shootings$State

#regular expressions
gsub("^\\s+|\\s+$", "", "       Rayanna Harduarsingh")

index <- grep("^\\s+|\\s+$", shootings$State)
shootings$State[index]
shootings$State <- gsub("^\\s+|\\s+$", ""
                        , shootings$State)
agg.dat <- aggregate(shootings$Total.Number.of.Victims
                      , list(state = shootings$State)
                      , sum)
colnames(agg.dat) <- c("state", "victims")
agg.dat

num.cols <- 10
my.pal <- rev(heat.colors(num.cols))

#rev(c(1,2,3,4))

pie(rep(1, num.cols), col = my.pal)

my.pal[c(1,1,1,1,1,9,1,1,2,3,1,1,10,1,1,1)]

range(agg.dat$victims)

tmp <- agg.dat$victims/max(agg.dat$victims)
tmp <- 1 + (num.cols - 1) * tmp
tmp <- round(tmp)
range(tmp)

agg.dat$col.index <- tmp
agg.dat$color <- my.pal[agg.dat$col.index]

map("state")
state.order <- match.map("state"
                         , regions = agg.dat$state
                         , exact = FALSE
                         , warn = TRUE)
map("state", col = agg.dat$color[state.order]
    ,fill = TRUE, lty = 1, border = "tan")

# rworldmaps - vector
# ggmaps - not pdf
# ggplot - pdf
# maps - pdf
# leaflet - not pdf

#install.packages("plotrix")
#library(plotrix)
#rescale(agg.dat$victims, c(1,10))

us.cities
head(us.cities)

map("county", "new york")
map("county", "california")

my.col <- rep(rgb(1, .6, .2, .7), nrow(us.cities))
#blue dots
my.col[us.cities$capital > 0] <- rgb(.2, .6, 1, .9)

pop.size <- 1 + 7 * us.cities$pop/max(us.cities$pop)

#points on map with bubble
map("state")
points(us.cities$long, us.cities$lat, cex = pop.size
       , col = my.col, pch = 16)







