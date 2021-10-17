#Student Alcohol Consumption Final Poster
#Class: IST 719 M001
#Author: Rayanna Harduarsingh
#Date: May 2nd, 2021

alcohol <- read.csv("/Users/rayannaharduarsingh/Downloads/archive/student-por.csv", header = TRUE, stringsAsFactors = FALSE)
View(alcohol)
alcohol$drinking <- alcohol$Dalc + alcohol$Walc

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

#Data Preparation
#getting rid of people over 21
alcohol <- alcohol[-c(280,408,414),]


#OVERALL STORY
#How does underage drinking affect students?

#Key Question
#Does underage drinking affect a student's academic performance?


#gender

drinking.by.gender <- aggregate(alcohol$drinking
                         , list(alcohol$sex)
                         , sum)

colnames(drinking.by.gender) <- c("Gender", "Drinking")

drinking.by.gender

#layered donut
ggplot(drinking.by.gender) +
  aes(x = Gender, y = Drinking, fill = Gender) +
  geom_bar(width = 0.95, stat = "identity") +
  coord_polar(theta = "y") +
  ylim(c(0,1500)) +
  xlab("") + ylab("") +
  ggtitle("How to\nmake a curved\nbar chart of units sold by region") +
  geom_text(data = drinking.by.gender, hjust = 1, size = 7
            , aes(x = Gender, y = 0, label = Gender)) +
  theme(legend.position = "none"
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()
        , panel.background = element_blank()) +
  scale_fill_manual(values = 
                      c("blue4", "firebrick3"))



#alcohol consumption by age
#how many times does the average student drink per week?

drinking.by.age <- tapply(alcohol$drinking
                          , list(alcohol$age)
                          , mean)

barplot(drinking.by.age
        , main="Average Alcoholic Consumption per Week"
        , ylab="Age"
        , xlab="Number of Times"
        , col=c("red2", "gold", "blue4", "darkgreen", "saddlebrown", "navyblue")
        , beside = TRUE
        , xlim = c(0,5)
        , horiz = TRUE
        , las = 2)

alcohol$age <-as.numeric(alcohol$age)
hist(alcohol$age)


#drinking vs absences
  
#converting drinking to factor levels (easier to classify)

alcohol$drinking <- factor(alcohol$drinking,
                    levels = c(1,2,3,4,5,6,7,8,9,10),
                    labels = c("Very Light", "Very Light", "Light", "Light", "Medium", "Medium", "Heavy", "Heavy", "Very Heavy", "Very Heavy"))                                
                                

ggplot(alcohol, aes(x=drinking, y=absences, fill=drinking))+
  geom_violin()+
  theme_bw()+
  ggtitle("Absences Based on Drinking") +
  scale_fill_manual(values = c("red2", "gold", "blue4", "darkgreen", "saddlebrown")) +
  xlab("Type of Drinker")


boxplot(alcohol$G3 ~ alcohol$drinking, ylab = "Final Exam Mark", xlab = "Alcohol Consumption")

#drinking vs final grade

alcohol$G3 <- factor(alcohol$G3,
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                           labels = c("Low","Low","Low","Low","Low","Low","Low","Low","Low","Average","Average","Average","Average","Average","Average","High", "High","High","High"))
mean(alcohol$G3)
boxplot(alcohol$G3)

#alluvial
install.packages("ggalluvial")
library(ggalluvial)

my.df <- aggregate(alcohol$drinking
                   ,list(w = alcohol$drinking, r = alcohol$G3)
                   ,mean)

ggplot(my.df) + aes(axis1 = r, axis2 = w) +
  geom_alluvium(aes(fill = w)) +
  geom_stratum(alpha = .1, color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  ggtitle("Final Grade Based on Alcohol Consumption")

theme_void() +
  scale_fill_manual(values = "red2", "gold", "blue4", "darkgreen", "saddlebrown")

#studytime
install.packages("ggpubr")
library(ggpubr)
ggscatter(alcohol, x = "drinking", y = "studytime", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Alcohol Consumption", ylab = "Hours of study")

d <- density(alcohol$studytime, alcohol$drinking)
plot(d)

plot(alcohol$drinking, alcohol$studytime )



