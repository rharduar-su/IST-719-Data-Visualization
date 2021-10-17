# Author: Rayanna Harduarsingh
# HW 3: Skill Building 
#

#loading files
my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"
list.files(my.dir)
art <- read.csv(paste0(my.dir, "art.csv")
                , header = TRUE
                , stringsAsFactors = FALSE)

View(art)

#1. Is there a relationship between the unit price of art goods and their units sold? If so, what kind of relationship is it?

art$unit.price
art$units.sold

#plotting two variables
plot(art$unit.price ~ art$units.sold
     , xlab = "Units Sold "
     , ylab = "Unit Price "
     , main = "Relationship between Units Sold  & Unit Price"
     , col = "royalblue2"
     , ylim=c(0,30))

abline(lm(art$unit.price ~ art$units.sold), col = "red", lty = 3, lwd = 3)

#Looking at the plot, there seems to be a relationship in which the higher the price of the art goods, less units are sold.
#Even after plotting a trend line, I see that correlation is negative. 

#2. Does the art company sell more units of drawing paper or watercolor paper?

drawing <- sum(art$units.sold[art$paper == "drawing"])
drawing
watercolor <- sum(art$units.sold[art$paper == "watercolor"])
watercolor

paper <- c(drawing, watercolor)
paper

barplot(paper, names.arg = c("Drawing", "Watercolor")
        , col = c("palegreen", "orchid")
        , ylab = "Units Sold"
        , xlab = "Paper"
        , main = "Units Sold of Paper")

#The art company sells more units of watercolor paper. 

#3. Does the art company bring in more money (revenue) selling drawing paper or watercolor paper? 

d <- sum(art$total.sale[art$paper == "drawing"])
d
w <- sum(art$total.sale[art$paper == "watercolor"])
w

revenue <- c(d, w)
revenue

barplot(revenue, names.arg = c("Drawing", "Watercolor")
        , col = c("gold", "mediumpurple")
        , ylab = "Revenue"
        , xlab = "Paper"
        , main = "Revenue of Paper"
        , ylim = c(0,120000))

#The art company brings in more revenue selling watercolor paper.

#4. Each paper (watercolor and drawing) has different subtypes. It is possible that at some stores, some subtypes sell better than others. 
#For drawing paper only, make a plot that allows the viewer to compare which subtypes of drawing paper sell more (and less) units 
#across the stores.

d <- art[art$paper == "drawing", ]
d
drawing2 <- tapply(art$units.sold,list(art$paper.type,art$store),sum)
drawing2
class(drawing2)

barplot(drawing2
        , main="Units Sold of Drawing Paper Types"
        , ylab="Units Sold"
        , xlab="City"
        , col=c("burlywood","darkolivegreen1", "lightpink1","lightcyan1", "lightgoldenrod1", "lavender")
        , beside = TRUE
        , ylim = c(0,4000))

legend("topleft", legend = rownames(drawing2)
                , lwd = 3
                , bty = "n"
                , col = c("burlywood", "darkolivegreen1", "lightpink1", "lightcyan1", "lightgoldenrod1", "lavender"))


#5. The dataset covers 4 years of data. Compare the revenue gained each year from the sales drawing paper with that of watercolor paper. 
#Are sales growing over time for both?



M <- tapply(art$total.sale
            ,list(art$paper, art$year)
            ,sum)
M
options(scipen = 99)
x <- as.numeric(colnames(M))
plot(x, M[1, ], type = "n"
     ,ylim = c(0, max(M))
     ,ylab = "Sales"
     ,xlab = "Year"
     ,xaxt = "n"
     ,bty = "n"
     ,main = "Paper Sales over Time")
lines(x, M[1, ], col = "lightsalmon2", lwd = 3 )
lines(x, M[2, ], col = "lightskyblue", lwd = 3 )

legend("bottomleft", legend = rownames(M)
       , lwd = 3
       , bty = "n"
       , col = c("lightsalmon2", "lightskyblue"))
axis(1,
     at = 2012:2015,
     labels = 2012:2015)

#Sales are growing over time for both papers at the Art company,