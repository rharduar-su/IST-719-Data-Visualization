tweets <- read.csv("/Users/rayannaharduarsingh/Desktop/data/Tweets.csv"
                   , header = TRUE, stringsAsFactors = FALSE)

tweets$created_at[1:3]
# "Fri Jun 24 14:21:28 +0000 2016" "Fri Jun 24 14:22:05 +0000 2016"
# "Fri Jun 24 14:22:27 +0000 2016"

#string part time
#Fri Jun 24 14:21:28 +0000 2016"
my.string.date.format <- "%a %b %d %H:%M:%S +0000 %Y"
?strptime()
strptime(x = tweets$created_at[1:3]
         , format = my.string.date.format)

my.date <- strptime(x = tweets$created_at
         , format = my.string.date.format)
min(my.date)
max(my.date)
range(my.date)
summary(my.date)
max(my.date) - min(my.date)

tmp <- as.Date(my.date)
barplot(table(tmp) main = "Tweets per day")
#lubridate

##############################################
#
# GGPLOT
#
##############################################

install.packages("ggplot2")
library(ggplot2)

file.choose()
sales <- read.csv("/Users/rayannaharduarsingh/Desktop/data/sales.csv",
                  header = TRUE,
                  stringsAsFactors = FALSE)
hist(sales$units.sold)

## gg = grammar of graphics
# ggplot requires data, aesthetic mapping, geometry
ggplot(sales, aes(x = units.sold)) + geom_histogram()

ggplot(sales) + 
  aes(x = units.sold) +
  geom_histogram()

ggplot(sales) +
  aes(x = expenses, y=recipt) +
  geom_point()

p <- ggplot(sales) + aes(x = expenses, y = recipt) + geom_point()
p
class(p)
p$data$rep.region
p$layers

p <- ggplot(sales) + aes(x=expenses, y=recipt)
save(p, file = "myplot.rda")

#aesthetic setting"
p <- ggplot(sales) + 
  aes(x = expenses, y = recipt) +
   geom_point(color = "blue")


#aesthetic mapping"
p <- ggplot(sales) + 
  aes(x = expenses, y = recipt, color= type) +
 geom_point()

# three aesthetic mappings
ggplot(sales) + 
  aes(x = expenses, y = recipt, color= unit.price) +
  geom_point()

ggplot(sales) +
  aes(x = expenses
      , y = recipt
      , color = unit.price
      , size = units.sold
      , shape = type
      , alpha = rep.region) +
  geom_point() +
  ggtitle("TOO MUCH INFORMATION")

sales$unit.price > 14

ggplot(sales) +
  aes(x = expenses, y= recipt, color = unit.price > 14) +
  scale_color_manual(values = c("cadetblue", "blue")) +
  geom_point() +
  geom_rug()

# ~ means "on"

boxplot(sales$recipt ~ sales$rep.region)

my.pred <- predict(lm(sales$recipt ~ sales$expenses))

ggplot(sales) + 
  aes(x = expenses, y = recipt) +
  geom_point() +
  geom_line(aes(y = my.pred), col = "red", lwd = 3)

ggplot(sales) +
  aes(x = expenses, y = recipt) +
  geom_point() +
  geom_smooth()

#hexbin
install.packages("hexbin")
install.packages("RColorBrewer")
library(hexbin)
library(RColorBrewer)

ggplot(sales) + 
  aes(x = expenses, y = recipt) +
  geom_hex(bins = 10) +
  ggtitle("Working with theme and hexbin") +
  theme_minimal() +
  scale_fill_gradientn(colors = brewer.pal(3, "Greens"))
  
  scale_fill_viridis_c()
  sclae_fill_viridis(option = "plasma")
  
ggplot(sales) + 
    aes(x = expenses, y = recipt) +
    geom_hex(bins = 10) +
    ggtitle("Working with theme and hexbin") +
    theme_minimal() +
    scale_fill_gradientn(colors = brewer.pal(6, "YlOrRd"))


#ggextra
install.packages("ggExtra")
library(ggExtra)

p <- ggplot(sales) +
  aes(x = expenses, y = recipt) +
  geom_point(alpha = 0) +
  geom_hex() + 
  scale_fill_gradientn(colors = brewer.pal(6, "Greens")) +
  theme_classic() +
  ggtitle("some text")


ggMarginal(p, type = "histogram"
           , fill = brewer.pal(8, "Greens")[6]
           , color = "white")

#facets
ggplot(sales) +
  aes(x = expenses, y = recipt) +
  geom_point() +
  facet_grid(wine ~ .)

ggplot(sales) +
  aes(x = expenses, y = recipt) +
  geom_point() +
  facet_grid(. ~ wine)

ggplot(sales) +
  aes(x = expenses, y = recipt, color = rep.region) +
  geom_point() +
  facet_grid(wine ~ rep.region) +
  theme_minimal() +
  ggtitle("Facets are POWERFUL")


#boxplots

ggplot(sales) +
  aes(x = units.sold) +
  geom_histogram(bins = 10)

ggplot(sales) +
  aes(x = rep.region, y = recipt) +
  geom_boxplot()

df.year <- aggregate(sales$units.sold, list(year = sales$year), sum)

ggplot(df.year) +
  aes(x = year, y = x) +
  geom_line() +
  ylim(c(0, 40000)) +
  theme_minimal()

df.year <- aggregate(sales$units.sold, list(year = sales$year, reg = sales$rep.region), sum)

ggplot(df.year) +
  aes(x = year, y = x, col = reg) +
  geom_line() +
  ylim(c(0, 10000)) +
  theme_minimal()

ggplot(sales) + 
  aes(x = rep.region, fill = type) +
  geom_bar(position = "dodge")

units.reg.by.type <- aggregate(sales$units.sold
                               , list (reg = sales$rep.region
                                       , type = sales$type)
                               , sum)

colnames(units.reg.by.type) [3] <- "units"

ggplot(units.reg.by.type) + 
  aes(x = reg, y = units, fill = type) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("deeppink4", "lightgoldenrod")) +
  theme_classic() +
  ggtitle("Setting Identity: bars = units sold")







