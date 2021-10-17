#
#Author Jeff Hemsley
#Lab 5
#

my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"
sales <- read.csv(paste0(my.dir, "sales.csv"), header = T, stringsAsFactors = F)

install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.all()
rand.dat <-replicate(8, rnorm(25))
boxplot(rand.dat, col = brewer.pal(8, "Set3"))
boxplot(rand.dat, col = brewer.pal(8, "Dark2")
        ,main = "RColorBrewer Awesomeness")

num.cols <- 8
FUN <- colorRampPalette(c("gold", "brown", "cadetblue"))

#colorRampPalette is a function

rand.dat <-replicate(8, rnorm(25))
boxplot(rand.dat, col = colorRampPalette(c("gold", "brown", "cadetblue"))(8)
        ,main = "Color Ramp Awesomeness")

colnames(sales)[11] <- "recipt"

#overplotting points over other points
plot(sales$expenses, sales$recipt, pch = 16)

plot(sales$expenses, sales$recipt, pch = 16, cex = .3)
#cex changes point size

#rgb makes a red, green, and blue
my.col <- rgb(217, 132, 31, alpha= 40, maxColorValue = 255)
plot(sales$expenses, sales$recipt, pch = 16, cex = .25, col = my.col, main = "dealing with overplotting")


my.col <- rgb(100, 70, 20, maxColorValue = 100)
plot(sales$expenses, sales$recipt, pch = 16, cex = .25, col = my.col, main = "dealing with overplotting")

plot(sales$expenses, sales$recipt, pch=16, col = "#F08010")


my.c.vec <- rep("#F08010", nrow(sales))
my.c.vec[sales$type == "red"] <- "#1080F0"
#if sales rep sex is 1, then it will be in red
plot(sales$expenses, sales$recipt, pch = 16, col = my.c.vec)

my.c.vec <- rep("#D9841F50", nrow(sales))
my.c.vec[sales$unit.price > 10] <- "#1A638A50"
my.c.vec[sales$unit.price > 14] <- "#21B01950"
plot(sales$expenses, sales$recipt, pch = 16, col = my.c.vec, cex = .5, main = "Using Color to Find Factors")


agg.dat.1 <- aggregate(sales$units.sold
                       , list(type = sales$type, wine = sales$wine)
                       , sum)
agg.dat.2 <- aggregate(sales$recipt
                       , list(type = sales$type, wine = sales$wine)
                       , sum)

colnames(agg.dat.1)[3] <- "units" 

agg.dat.1$recipts <- agg.dat.2$x

agg.dat.1

options(scipen = 99)
plot(agg.dat.1$units, agg.dat.1$recipts, pch = 16, col="orange"
     , xlab = "Units Sold", ylab = "Sales", bty = "n"
     , xlim = c(0, max(agg.dat.1$units) * 1.25)
     , ylim = c(0, max(agg.dat.1$recipts) * 1.25))

install.packages("png")
library(png)

ima <- readPNG(paste0(my.dir, "bottles.png"))
r1 <- readPNG(paste0(my.dir, "R1.png"))
w1 <- readPNG(paste0(my.dir, "W1.png"))

lim <- par()
lim$usr
#rasterImage(ima, 0, 0, 60000, 400000)
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rect(lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]
     , col = rgb(1,1,1, alpha = .85), border = "white", lwd =10)

#red wine pics
r1.x1 <- agg.dat.1$units[agg.dat.1$type == "red"]
r1.x2 <- r1.x1 + 3000
r1.y1 <- agg.dat.1$recipts[agg.dat.1$type == "red"]
r1.y2 <- r1.y1 + 65000
rasterImage(r1, r1.x1, r1.y1, r1.x2, r1.y2)

#white wine pics
w1.x1 <- agg.dat.1$units[agg.dat.1$type == "white"]
w1.x2 <- w1.x1 + 3000
w1.y1 <- agg.dat.1$recipts[agg.dat.1$type == "white"]
w1.y2 <- w1.y1 + 65000
rasterImage(w1, w1.x1, w1.y1, w1.x2, w1.y2)

text(agg.dat.1$units + 3100, agg.dat.1$recipts +  10000
     , labels = agg.dat.1$wine, adj = 0, cex =1.2)

mtext(text = "IST719 Wine Sales Data", side=3, line=1, cex = 2)

