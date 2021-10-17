#
# Author: Rayanna Harduarsingh
# Lab 1: Basics
#

###Pie Chart###

pie(c(12,7,10,14))

#creating a pie chart

pie(c(12,7,10,14), main= "Rayanna's Pie")

#adding a title

pie(c(12,7,10,14), main= "Rayanna's Pie", col = c("mistyrose", "moccasin", "lightskyblue1", "palegreen"))

#concatenating different colors of your choice such as cadetblue, forestgreen, burlywood. I chose pastels for fun :). 
#refer to color chart pdf

pie(c(12,7,10,14), main= "Rayanna's Pie", col = c("mistyrose", "moccasin", "lightskyblue1", "palegreen"), labels = c("Ops", "Sales", "Transport", "Execs"))

#naming the columns/sections. ALWAYS separate by commas


###Plot###

plot(c(7,9,8,5,10))

#creating a plot

plot(c(7,9,8,5,10), main = "Yearly Sales")

#adding a title

plot(c(7,9,8,5,10), main = "Yearly Sales", col = "orange")

#making the plots orange

plot(c(7,9,8,5,10), main = "Yearly Sales", col = "orange", pch=8)

#paramete "pch" to make point different symbols. 16 is a whole point
#google r pch values for different symbols

plot(c(7,9,8,5,10), main = "Yearly Sales", col = "orange", pch=8, cex = 3)

#cex changes size

plot(c(7,9,8,5,10), main = "Yearly Sales", 
     col = c("red", "orange"), 
     pch= c(15,16,17,18,19), 
     cex = 3)

#can also change the symbol/color/size for each point respectively to the original vector

n <- 25
x <- 1:n
y <- 1:n
plot(x,y,pch = x)


plot(c(7,9,8,5,10), main = "Yearly Sales", 
     type = "l"
     )
#s is step chart
#h is histogram

plot(c(7,9,8,5,10), main = "Yearly Sales", 
     type = "l",
     lwd = 4,
     col = "orange",
     ylab = "sales", ylim = c(0,12),
     xlab = "years"
)

#xlab and ylab change the axis names
#xlim and ylim sets limits


###Vectors/Sample###

letters
letters [1]
letters [3:5]
#letters c, d, e
letters [7:5]
#backwards letters g, f, e

my.rand.dat <- sample(letters[7:5],
                      size = 25,
                      replace = TRUE)

#samples data

table(my.rand.dat)
#creating a table of the sample

###BarPlot###

barplot(table(my.rand.dat))
#creating a barplot

?barplot()
#get the help window for the barplot function

barplot(table(my.rand.dat), main = "My Bars",
        horiz = T)

barplot(table(my.rand.dat), main = "My Bars",
        horiz = T, density = 50,
        col = c("cornsilk", "lavender", "lightblue"),
        xpd = TRUE,
        border = "black",
        width = c(2,3,1)
        )

###Normalization###

x <- rnorm(n = 1000, mean = 72, sd = 15)
x <- rlnorm(n = 1000)
x <- runif(n = 1000)
hist(x)
boxplot(x)
sort(x)[500]
median(x)
mean(x)

?hist()

hist(x, main = "Rayanna's Histogram",
     ylab = "Sales",
     xlab = "Weeks",
     col = "mistyrose",
     plot = TRUE
     )
    















