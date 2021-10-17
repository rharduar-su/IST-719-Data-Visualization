# Author: Rayanna Harduarsingh
# HW 5: Visually Describe a Dataset
# RAMEN RATINGS

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
#loading files
my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"

ramen <- read.csv(paste0(my.dir, "ramenratings.csv")
                , header = TRUE
                , stringsAsFactors = FALSE)
View(ramen)
str(ramen)
is.na(ramen)

#Trying different plots

#Which style of ramen is more popular?

#by average rating
style <- tapply(ramen$Stars,list(ramen$Style),mean)
style

barplot(ramen$Style
        , main="Average Rating of Ramen Styles"
        , ylab="Rating"
        , xlab="Style"
        , col=c("burlywood","darkolivegreen1", "lightpink1","lightcyan1", "lightgoldenrod1", "lavender")
        , beside = TRUE
        , ylim = c(0,5)
        , horiz = TRUE)

#by occurence in data set(more accurate)

ggplot(ramen, aes(x=ramen$Style)) + geom_bar() + labs(x="Style", y="Number of Occurences", title = "Popular Ramen Styles")

#Top ramen countries

#getting an overview of how much each country appears in dataset to determine as popular ramen countries. #some countries have low counts so they are not needed and was removed
country <- table(ramen$Country)
country <- data.frame(country)
country

#plotting
ggplot(country, aes(reorder(Var1, -Freq), Freq)) + geom_col() + labs(x="Country", y="Count", title="Top Ramen Countries")


#Top Ramen Brands
brands <- aggregate(ramen$Stars,list(ramen$Brand),mean)
brands
colnames(brands) <- c("Brand", "Average Rating")
brands <- data.frame(brands)
#too many brands so we are subsetting the data 
brands <- subset(brands, brands$Average.Rating > 3 )
brands
top_n(brands, n=15)

#too much brands to plot and by taking the top 15, they all have the same rating so not 
#much of a huge variation. Maybe instead of mean, we can try sum of ramen.

brands <- table(ramen$Brand)
brands <-data.frame(brands)
brands
colnames(brands) <- c("Brand", "Frequency")
topbrands <- top_n(brands, n=15)
topbrands
ggplot(topbrands, aes(reorder(Brand, -Frequency), Frequency)) + geom_col() + labs(x="Brand", y="Frequency", title="Leading Ramen Brands")

#multidimensional plot

variety <- tapply(ramen$Stars,list(ramen$Dummy, ramen$Brand),mean)
variety <- data.frame(variety)

par(mfrow=c(5,3))
#Nongshim
barplot(variety$Nongshim, beside = TRUE, col = c("red" , "green"), main = "Nongshim Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Nissin
barplot(variety$Nissin, beside = TRUE, col = c("red" , "green"), main = "Nissin Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Maruchan
barplot(variety$Maruchan, beside = TRUE, col = c("red" , "green"), main = "Maruchan Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Paldo
barplot(variety$Maruchan, beside = TRUE, col = c("red" , "green"), main = "Paldo Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Myojo
barplot(variety$Myojo, beside = TRUE, col = c("red" , "green"), main = "Myojo Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Mama
barplot(variety$Mama, beside = TRUE, col = c("red" , "green"), main = "Mama Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#Indomie
barplot(variety$Indomie, beside = TRUE, col = c("red" , "green"), main = "Indomie Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Samyang Foods
barplot(variety$Samyang.Foods, beside = TRUE, col = c("red" , "green"), main = "Samyang Foods Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))

#Ottogi
barplot(variety$Ottogi, beside = TRUE, col = c("red" , "green"), main = "Ottogi Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#lucky me
barplot(variety$Lucky.Me., beside = TRUE, col = c("red" , "green"), main = "Lucky me! Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#vina acecook
barplot(variety$Vina.Acecook, beside = TRUE, col = c("red" , "green"), main = "Vina Acecook Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#vifon
barplot(variety$Vifon, beside = TRUE, col = c("red" , "green"), main = "Vifon Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#mamee
barplot(variety$Mamee, beside = TRUE, col = c("red" , "green"), main = "Mamee Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#maggi
barplot(variety$Maggi, beside = TRUE, col = c("red" , "green"), main = "Maggi Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))
#masterkong
barplot(variety$Master.Kong, beside = TRUE, col = c("red" , "green"), main = "Master.Kong Ratings")
legend("topleft", legend = rownames(variety)
       , lwd = 3
       , bty = "n"
       , col = c("red", "green"))









       