#Author: Rayanna Harduarsingh
#April 7th, 2021
#IST 719 Lab 9

install.packages("igraph")
install.packages("plotrix")
library(igraph)
library(plotrix)

file.choose()
class.info <- read.csv(paste0("/Users/rayannaharduarsingh/Desktop/IST 719 Week 9/classinformation.csv")
                       , header = TRUE
                       , stringsAsFactors =FALSE)
class.network <- read.csv(paste0("/Users/rayannaharduarsingh/Desktop/IST 719 Week 9/classnetwork.csv")
                       , header = TRUE
                       , stringsAsFactors = FALSE)
colnames(class.network)
class.network$X

#removing extra rows after 58
class.network <- class.network[1:58, ]

#putting names in the right order
tmp <- cbind(colnames(class.network)[-1], class.network$X)

#fixing and cleaning up our data to remove spaces and periods
colnames(class.network) <- gsub("\\.", "", colnames(class.network))
                                
M <- as.matrix(class.network[ , -1])
View(M)
dim(M)
rownames(M) <- colnames(M)
#checking for missing values 
#if anything missing returns true 
any(is.na(M))
M[is.na(M)] <- 0
is.na(c(1,2,4,NA,3))

g <- graph_from_adjacency_matrix(M)
plot(g)

class(g)
g

plot.igraph(g, edge.arrow.size = 0, edge.arrow.width =0)
g <- simplify(g)

vcount(g)
ecount(g)
par(mar= c(1,1,1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

#how many links someone has
degree(g)

par(mar = c(5, 10, 4, 1), cex.axis = .7)
barplot(sort(degree(g)), horiz = TRUE, las = 2
        ,main = "total links", col = "blue")
                              
barplot(sort(degree(g, mode = "in")), horiz = TRUE, las = 2
        ,main = "popular", col = "red")

barplot(sort(degree(g, mode = "out")), horiz = TRUE, las = 2
        ,main = "friendly", col = "green")

V(g)$name
V(g)$deg <- degree(g)
V(g)$in.deg <- degree(g, mode = "in")
V(g)$out.deg <- degree(g, mode = "out")
g

par(mar = c(1,1,1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

plot(sort(V(g)$out.deg))
range(V(g)$out.deg)
V(g)$size <- rescale(V(g)$out.deg, c(3,12))

par(mar = c(1,1,1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

par(mfrow = c(3,2))
y <- 1 + sort(V(g)$out.deg)
plot(y, type = "l", lwd = 3, main = "y")

y2 <- sqrt(y)
plot(y2, type = "l", lwd = 3, main = "sqrt(y)")

y3 <- log(y)
plot(y3, type = "l", lwd = 3, main = "log(y)")

y4 <- log10(y)
plot(y3, type = "l", lwd = 3, main = "log10(y)"
     , ylab = "log10(y)")

y5 <- y^2
plot(y5, type = "l", lwd = 3, main = "y^2")

y6 <- y^(1/5)
plot(y6, type = "l", lwd = 3, main = "y^(1/5)")

V(g)
attributes(V(g))
class(g)
names(g)

E(g)$color <- "gold"
V(g)$color <- "cadetblue"
V(g)$size <- 12 * log10(V(g)$out.deg + 1)
par(mar = c(1,1,1,1), mfrow = c(1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

##Word Cloud
install.packages("wordcloud")
library(wordcloud)
  
file.choose()
tweets <- read.csv(paste0("/Users/rayannaharduarsingh/Desktop/data/Tweets.csv")
                         , header = TRUE
                         , stringsAsFactors =FALSE
                         , quote = "\"")
dim(tweets)
colnames(tweets)
tweets$hashtags
  
tweet.tags <- tweets$hashtags[tweets$hashtags != ""]
all.tags <- unlist(strsplit(tweet.tags, "\\|"))
tag.tab <- sort(table(all.tags), decreasing = TRUE)
tag.tab[1:10]
plot(as.numeric(tag.tab), type = "l")
  
tag.words <- names(tag.tab)
tag.freqs <- as.numeric(tag.tab)
cbind(tag.words, tag.freqs)
  
wordcloud(tag.words, tag.freqs, scale = c(4,.5))
  
word.size <- 1 + round(sqrt(tag.freqs), 0)
wordcloud(tag.words, word.size, scale = c(4, .5))
  
myPalFun <- colorRampPalette(c("gold", "red", "green"))
tag.col <- myPalFun(max(word.size))[word.size]
par(bg = "black")
wordcloud(tag.words, word.size, scale = c(4, .5)
            , colors = tag.col, random.color = FALSE
            , ordered.colors = TRUE
            , random.order = FALSE
            , rot.per = 0.5
            , min.freq = 3
            , max.words = Inf)

cbind(tag.freqs, tag.col)[1:10, ]



E(g)$color <- "gold"
V(g)$color <- "cadetblue"
V(g)$size <- 12 * log10(V(g)$out.deg + 1)
par(mar = c(1,1,1,1), mfrow = c(1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

?plot.igraph()
?igraph.plotting()

E(g)$curved <- .5
par(mar = c(1,1,1,1), mfrow = c(1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)
plot(g)

View(class.info)
class.info <- class.info[ , 1:4]

class.info$Name <- gsub(" ", "", class.info$Name)

index <- match(V(g)$name, class.info$Name)

cbind(V(g)$name,class.info$Name[index])

V(g)$country <- class.info$Country[index]
V(g)$class <- class.info$Class[index]

V(g)$color[V(g)$class == "Wed"] <- "brown"
V(g)$color[V(g)$class == "Tues"] <- "forestgreen"

par(mar = c(1,1,1,1), mfrow = c(1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

V(g)$color <- "cadetblue"
V(g)$color[V(g)$country == "China"] <- "brown"
V(g)$color[V(g)$country == "India"] <- "forestgreen"
V(g)$color[V(g)$country == "USA"] <- "blue"
par(mar = c(1,1,1,1), mfrow = c(1,1))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)








                                
                                
