# Author: Rayanna Harduarsingh
# HW#2: Working with Data


#PART 1: VT Plots

#reading hot dogs csv file
hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",
           sep=",",header=TRUE)

#Viewing the hot dogs data set to make sure everything was loaded in 
View(hotdogs)

#checking hot dogs eaten column
hotdogs$Dogs.eaten

#Bar chart Fig 4-11

#basic bar plot of hot dogs eaten
barplot(hotdogs$Dogs.eaten)

#using names.arg to specify the names of each bar
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year)

#adding other arguments(axis labels, borders, colors)
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, 
        col = "red", 
        border = NA,
        xlab = "Year", 
        ylab = "Hot dogs and buns (HDB) eaten")

#shading bars to different colors
fill_colors <- c()
for(i in 1:length(hotdogs$Country) ) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors,"#821122")
  }else{
    fill_colors<-c(fill_colors,"#cccccc")
  }
}
  
#putting fill_colors into barplot
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, 
        col = fill_colors,
        border = NA,
        xlab = "Year", 
        ylab = "Hot dogs and buns (HDB) eaten")

fill_colors <- c()
for ( i in 1:length(hotdogs$New.record)) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else{
    fill_colors <- c(fill_colors, "#cccccc")
  }
}


barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, 
        col = fill_colors,
        border = NA,
        space = 0.3,
        xlab = "Year", 
        ylab = "Hot dogs and buns (HDB) eaten")

#adding a title
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, 
        col = fill_colors,
        border = NA,
        space = 0.3,
        main = "Nathan's Hot Dog Eating Contest Results, 1980-2010",
        xlab = "Year", 
        ylab = "Hot dogs and buns (HDB) eaten")

#Stacked bar chart- Fig 4-22

#loading new data set into new variable
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
                    sep=",",header=TRUE)

#getting overview of hot dog places
hot_dog_places

#changing header names (to get rid of X)
#using quotes to specify it is a string
names(hot_dog_places) <- c("2000", "2001", "2002","2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")

#converting data set into a matrix
hot_dog_places <- as.matrix(hot_dog_places)

hot_dog_places

barplot(hot_dog_places,
        border = NA,
        space = 0.25,
        ylim = c(0,200),
        xlab = "Year",
        ylab = "Hot dogs and buns (HBDs) eaten",
        main = "Hot Dog Eating Contest Results, 1980-2010")


#Scatterplot- Fig 4-28

#loading and reading new csv dataset into new variable
subscribers<- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
           sep=",",header=TRUE)

subscribers

#looking at first five rows of data
subscribers[1:5,]

#creating basic scatter plot
plot(subscribers$Subscribers)

#specifying point type and setting range of vertical axis
plot(subscribers$Subscribers, type = "p", ylim = c(0, 30000))

#combining vertical kines and points
plot(subscribers$Subscribers, type = "h", ylim = c(0, 30000),
     xlab = "Day",
     ylab = "Subscribers")
points(subscribers$Subscribers, pch=19, col="black")


#Time series- Fig 4-34

#loading and reading new data set
population <-
  read.csv("http://datasets.flowingdata.com/world-population.csv",
           sep=",",header=TRUE)
population

plot(population$Year,population$Population,type="l",
      ylim=c(0, 7000000000),xlab="Year",ylab="Population")

#Step chart- Fig 4-43

#loading and reading new data 
postage<-read.csv("http://datasets.flowingdata.com/us-postage.csv",sep=",",header=TRUE)

#creating basic step chart
plot(postage$Year, postage$Price, type = "s")
#s stands for step

#specifying main title and axis labels
plot(postage$Year, postage$Price, type = "s",
     main = "US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab = "Year", ylab = "Postage Rate (Dollars)")


#PART 2: Simple Distributions (art data)
my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"
list.files(my.dir)
art <- read.csv(paste0(my.dir, "art.csv")
                 , header = TRUE
                 , stringsAsFactors = FALSE)
View(art)
summary(art)

#1. What is the distribution of total.sale for the whole dataset? Provide two different plots that show two different ways of showing distribution.
par(mfrow = c(2,2))
#histogram
hist(art$total.sale, main = "Total Sales of Paper",
    xlab = "Total Sales",
    border = 1.5,
    col = "salmon",
    plot = TRUE
)

#density plot

d <- density(art$total.sale)
plot(d, main = "Total Sales of Paper")
polygon(d, col = "paleturquoise1")


#2. Next we want to compare the distributions of subsets of total.sales. Use a third type of distribution plot (different than what you used for the question above) for both of these plots.
#What is the distribution of the totals sales for drawing paper?
#What is the distribution of the totals sales for watercolor paper?

#Box Plot

drawing <- art[art$paper == "drawing", ]
watercolor <- art[art$paper == "watercolor", ]
#subsetting data set into two groups
#View(drawing)
#View(watercolor)
boxplot(drawing$total.sale, ylab = "Total Sales of Drawing Paper", pch = 8, col = "khaki1",main = "Drawing Paper")
boxplot(watercolor$total.sale, ylab= "Total Sales of Watercolor Paper",pch = 8, col = "palegreen", main = "Watercolor Paper")


