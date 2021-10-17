#author: Rayanna Harduarsingh
# Lab 3: Skilling Building
#


my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"

list.files(my.dir)

fname <- paste0(my.dir, "sales.csv")

sales <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

View(sales)

#Question: What is the relationship between expense and receipts?

sales$expenses
sales$recipt

#x: cause, y: effect

plot(sales$expenses, sales$recipt, main = "Positive relationship")

# 2 measures or 2 dimensions

#adding lm horizontal line
abline(lm(sales$recipt ~ sales$expenses), col = "red", lty = 3, lwd = 3)
#lty stands for line type
#lwd stands for line weight

abline(h = mean(sales$recipt), col = "blue", lty = 3, lwd = 2 )

#adding vertical line
abline(v = mean(sales$expenses), col = "blue", lty = 3, lwd = 2 )

#side: bottom, left, top, right
rug(sales$expenses, side = 1, col = "orange")
rug(sales$recipt, side = 4, col = "pink")
#rug tells us a little about the distribution such as where it's thicker/thinner

#adding text onto plot
text(x = 4, y = 600, labels = "HERE")

#adds an arrow onto plot
arrows(4,550,10,800)

#Question: What is the relationship between receipt and type?

sales$type
# are the central tendencies different by category

boxplot(sales$recipt ~ sales$type
        , main = "cat by continuous")
abline(h = median(sales$recipt), col = "red")
abline(h = mean(sales$recipt), col = "blue")

# Question: which region sells the most units? 
sales$rep.region
sales$units.sold

boxplot(sales$units.sold ~ sales$rep.region)

sum(sales$units.sold)
sum(sales$units.sold[sales$rep.region == "East"])
sum(sales$units.sold[sales$rep.region == "West"])

#getting the sum of each group faster instead of one by one
units.by.reg <- aggregate(sales$units.sold
          , list(sales$rep.region)
          , sum)
units.by.reg
class(units.by.reg)

units.by.reg$Group.1
units.by.reg$x
colnames(units.by.reg) <- c("reg", "units")
units.by.reg
barplot(units.by.reg$units, names.arg = units.by.reg$reg)

M.units.by.reg <- tapply(sales$units.sold
                          , list(sales$rep.region)
                          , sum)
M.units.by.reg
barplot(M.units.by.reg, main = "grouped aggregated data")

#tapply is the same thing as aggregate and could be faster
#same function different format

# Question: how do the units sold of red vs white differ by region?

units.by.type.reg <- tapply(sales$units.sold
                         , list(sales$rep.region, sales$type)
                         , sum)
units.by.type.reg
rownames(units.by.type.reg)
colnames(units.by.type.reg)
class(units.by.type.reg)

#stacked bar plot
barplot(units.by.type.reg)

barplot(units.by.type.reg, beside = TRUE)
barplot(t(units.by.type.reg), beside = TRUE)

units.by.type.reg <- tapply(sales$units.sold
                            , list(sales$type, sales$rep.region)
                            , sum)
barplot(units.by.type.reg, beside = TRUE, 
        main = "many small groups"
        ,legend.text = rownames(units.by.type.reg)
        ,col = c("maroon", "floralwhite"))
#few large groups vs many small groups 

#Question: are receipts growing over time for each region?

M <- tapply(sales$recipt
            ,list(sales$rep.region, sales$year)
            ,sum)
M

options(scipen = 99)
x <- as.numeric(colnames(M))
plot(x, M[1, ], type = "n", ylim = c(0, max(M))
     ,ylab = "sales"
     ,xlab = ""
     ,bty = "n"
     ,main = "Sales over time")
lines(x, M[1, ], col = "red", lwd = 3 )
lines(x, M[2, ], col = "red4", lwd = 3 )
lines(x, M[3, ], col = "brown3", lwd = 3 )
lines(x, M[4, ], col = "orange", lwd = 3 )
lines(x, M[5, ], col = "gold", lwd = 3 )

legend("bottomleft", legend = rownames(M)
       , lwd = 3
       , bty = "n"
       , col = c("red", "red4", "brown3", "orange", "gold"))
xspline(x, M[1, ], shape = .5, border = "red", lwd = 3)
xspline(x, M[2, ], shape = .5, border = "red4", lwd = 3 )
xspline(x, M[3, ], shape = .5, border = "brown3", lwd = 3 )
xspline(x, M[4, ], shape = .5, border = "orange", lwd = 3 )
xpsline(x, M[5, ], shape = .5, border = "gold", lwd = 3 )




sales <- read.csv("sales.csv"
                  , header = TRUE
                  , stringsAsFactors = FALSE)

reg.by.type <- aggregate(sales$units.sold
                         , list(sales$rep.region, sales$type)
                         , sum)
colnames(reg.by.type) <- c("reg", "type", "units")
reg.by.type

ggplot(reg.by.type) +
        aes(x = reg, y = units, fill = reg) +
        geom_bar(width = 0.95, stat = "identity") +
        coord_polar(theta = "y") +
        ylim(c(0,60000)) +
        xlab("") + ylab("") +
        ggtitle("How to\nmake a curved\nbar chart of units sold by region") +
        geom_text(data = reg.by.type, hjust = 1, size = 7
                  , aes(x = reg, y = 0, label = reg)) +
        theme(legend.position = "none"
              , axis.text.y = element_blank()
              , axis.ticks = element_blank()
              , panel.background = element_blank()) +
        scale_fill_manual(values = 
                                  c("darkslateblue", "dodgerblue4", "deepskyblue4"
                                    , "cornflowerblue", "deepskyblue"))





