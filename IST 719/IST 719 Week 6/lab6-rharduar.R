#Author: Rayanna Harduarsingh
#Lab 6

x <- 1:24
y <- rnorm(length(x))
A <- runif(n=length(x), min = 2, max = 9)
A <- A + y^2
B <- sample(c("W", "G"), size = length(x), replace = T)

M <- matrix(
  c(1,1,3,
    1,1,3,
    2,2,3),
  nrow = 3, byrow = T
)
M
layout(M)
#shows how everything is going to be layed out 
layout.show(3)

# 1:bottom, 2:left, 3:top, 4:right default 5,4,4,2
par(mar = c(0,4,4,2), bty = "n")
plot(x,y,type = "l", xaxt = "n", ylab = "price", xlab = ""
     , lwd = 3
     , col = "forestgreen")
mtext(text = "Multi-plot layout is cool", side = 3, line = 1, cex = 1.5, adj = 0) #putting text in the margin
par(mar = c(5,4,0,2))
barplot(A, names.arg = 1:24, space = 0, border = NA, col = "tan")
mtext(text = "vol", side = 2, line = 3, cex = .8)

par(mar = c(5,4,4,2))
boxplot(A~B, col = "cadetblue")

#export pdf
pdf(file = "myfilename.pdf", width = 8.5, height = 11)
pie(c(7,12,13), col = c("yellow", "pink", "red"))
dev.off()




