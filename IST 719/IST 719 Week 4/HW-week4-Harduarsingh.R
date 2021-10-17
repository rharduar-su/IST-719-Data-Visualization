# Author: Rayanna Harduarsingh
# Week 4 HW: Using Illustrator

hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",
                    sep=",",header=TRUE)

hotdogs

# Bar chart Fig 4-5

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

#Stacked Bar Chart Fig 4-21
names(hot_dog_places)<- c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
hot_dog_matrix<- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border = NA
        , space = 0.25
        , ylim = c(0,200)
        , xlab = "Year"
        , ylab = "Hot dogs and buns (HDBs) eaten"
        , main = "Hot Dog Eating Contest Results, 1980-2010")

#Scatterplot: Fig 4-25
plot(subscribers$Subscribers, type = "p", ylim = c(0, 30000)
     ,xlab = "Day"
     ,ylab = "Subscribers")


#Time series: Fig 4-40

population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep =",", header= TRUE)
head(population)
plot(population$Year, population$Population, type="l"
     , ylim = c(0, 7000000000), xlab="Year", ylab = "Population")

#Step chart: Fig 4-42

postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header = TRUE)
head(postage)
plot(postage$Year, postage$Price, type = "s"
     , main = "US Postage Rates for Letters, First Ounce, 1991-2010"
     , xlab = "Year", ylab = "Postage Rate (Dollars)")


#LOESS Curve Fig: 4-47

unemployment<- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv"
                        , sep=",")
unemployment[1:10,]

plot(1:length(unemployment$Value), unemployment$Value)

scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span =0.5)
