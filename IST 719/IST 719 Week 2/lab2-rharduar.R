# author: Rayanna Harduarsingh
# Lab 2: working with files and exploring data
#

my.dir <- "/Users/rayannaharduarsingh/Desktop/data/"
#setting directory
#line must have / and must have an end /

list.files(my.dir)
#list all the files
#cannot do capital L

tips <- read.csv(paste0(my.dir, "tips.csv")
                        , header = TRUE
                        , stringsAsFactors = FALSE)
#reading csv file

plot(tips$total_bill)

paste("rayanna", "harduarsingh")

paste0("rayanna", "harduarsingh")
#paste0 doesn't have a space between names

paste("rayanna", "harduarsingh", sep = "-")
#sep separates string with a character you specify

paste("rayanna", "harduarsingh", sep = "$")

my.var <- paste(c("wrt", "wDt", "abt"), 7, sep = "_")
my.var <- paste(c("wrt", "wDt", "abt"), 7:9, sep = "_")
my.var <- paste(c("wrt", "wDt", "abt"), 1:2, sep = "_")

my.title <- paste("Rayanna's", "Scatter", 12)
plot(tips$total_bill, tips$tip, main = my.title)

tips$size
colnames(tips)
#access column names
#rows/observation

tips[ ,3]
tips$tip
tips[ ,"tip"]
#finding third column data

tips$tip[1]
#shows first data row in fsecond column

dim(tips)
#shows dimensions

fix(tips)
#figure out how to work on mac 

View(tips)
#view entire data set in another tab

str(tips)
#shows structure/brief overview

length(tips$tip)
#counts how many observations in a vector

summary(tips)
#basic stats for different variables


sales <- read.table(paste0(my.dir, "Wine.txt")
                    , header = TRUE
                    , sep = "\t"
                    , stringsAsFactors = FALSE)

#stringsAsFactors = FALSE in new versions of R
#stringsAsFactors = TRUE on older version

sales$rep.region
plot(sales$expenses, sales$recipt)


par(mfrow = c(3,2))
hist(sales$recipt)
#distributions tell us the shape of the data

boxplot(sales$recipt)

plot(sales$recipt)

plot(sort(sales$recipt))

d <- density(sales$recipt)

plot(d)

tips$tip

par(mfrow = c(1,1))
#getting plots in one screen to compare side by side
plot(tips$tip)


#do people tip more at lunch or dinner?

lunch <- tips[tips$time == "Lunch", ]
dinner <- tips[tips$time == "Dinner", ]
#subsetting data set into two groups
View(lunch)

par(mfrow = c(1,2))
boxplot(lunch$tip, ylim = c(0,11), main = "Lunch")
boxplot(dinner$tip, ylim = c(0,11), main = "Dinner")


par(mfrow = c(2,1))
hist(lunch$tip, xlim = c(0,11), main = "Lunch")
hist(dinner$tip, xlim = c(0,11), main = "Dinner")

load(paste0(my.dir, "shooting.Rda"))
View(shootings)

save(d, paste0(my.dir, "jeffsDat.rda"))





                  







