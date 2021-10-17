sales <- read.table(paste0(my.dir, "Wine.txt")
                    , header = TRUE
                    , sep = "\t"
                    , stringsAsFactors = FALSE)

View(sales)

#Write a piece of R code to find the names of the sales representatives that sold the most units of 
#white wine in each region in 2010.

#subsetting the data to only get rows that contain the year 2010 and white wine.
wsales <- subset(sales, sales$year==2010 & sales$type=="white")
View(wsales)

#getting the sum of wine sold by each sales rep sorting by region instead of doing it one by one.
wsalesrep <- tapply(wsales$units.sold
            ,list(wsales$sales.rep, wsales$rep.region)
            ,sum)
wsalesrep

#converting to data frame because they are originally atomic vectors and need to be int
wsalesrep <- data.frame(wsalesrep)

#using rownames to display the actual row name and which.max to find the max value of the column(region). Comma is used to list value in the column.

#Central
rownames(wsalesrep[which.max(wsalesrep$Central),])
#Dannette Saltsman sold the most white wine units in the Central region

#East
rownames(wsalesrep[which.max(wsalesrep$East),])
#Aiko Hamsher sold the most white wine units in the East region

#North
rownames(wsalesrep[which.max(wsalesrep$North),])
#Clement Arias sold the most white wine units in the North region

#South
rownames(wsalesrep[which.max(wsalesrep$South),])
#Jewel Copas sold the most white wine units in the South region

#West
rownames(wsalesrep[which.max(wsalesrep$West),])
#Deloras Mcfetridge sold the most white wine units in the West region
