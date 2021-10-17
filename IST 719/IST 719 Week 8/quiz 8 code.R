#i created a sample data frame with the dates provided to test R Code
date.time <- c("2014, Aug, Fri the 16 at 18:40", "2014, Jun, Sat the 24 at 11:51", "2014, Jun, Sun the 25 at 7:22")
my.df <-data.frame(date.time)

View(my.df)

strptime("2014, Aug, Fri the 16 at 18:40", "%Y, %b, %a the %d at %R")
my.df$date.time <- as.POSIXct(strptime(my.df$date.time, "%Y, %b, %a the %d at %R"))

min(my.df$date.time)
max(my.df$date.time)
help("strptime")
help("as.POSIXct")


as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
dev.off()
plot(rnorm(100),col = "#FF7733", pch = 16, cex = 3)
plot(rnorm(100),col = rgb(1,.5,.2), pch = 16, cex = 3)
plot(rnorm(100),col = rgb(.3,.7,1), pch = 16, cex = 3)
plot(rnorm(100),col = "lavenderblush3", pch = 16, cex = 3)

#lightgreen
#144, 238, 144
#dark red
#139, 0, 0
#purple
#171, 130, 255
df2 <-data.frame(replicate(2,sample(0:100, 100, rep=TRUE)))
df2$mode
my.df$mode<-as.factor(my.df$mode)
plot(my.df$x, my.df$y,)

plot(df2$x, df2$y, col=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255))[df2$mode],pch=16)






colnames(df1) <- c("x", "y","mode")
df1 <- data.frame(replicate(2,sample(0:100,100,rep=TRUE)))
df1$mode<- replicate(1,sample(c("T","H","Q"),100,rep=TRUE))
df1$mode<-as.factor(df1$mode)

df1$mode <- col=c(rgb(139,0,0,127.5),rgb(160,32,240,127.5),rgb(144,238,144,127.5))
plot(df1$x,df1$y, pch=16)


points(df1$mode, col=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255)))
plot(df1$x, df1$y, col=c(rgb(139,0,0,127.5),rgb(160,32,240,127.5),rgb(144,238,144,127.5)[df1$mode],pch=16)


     
     
     
     
df2 <- data.frame(replicate(2,sample(0:100,100,rep=TRUE)))
df2$mode<- replicate(1,sample(c("T","H","Q"),100,rep=TRUE))
colnames(df2) <- c("x", "y","mode")
#convert the mode column to factors
df2$mode<-as.factor(df2$mode)
#As we can see, the mode has three Levels:H Q T
#plot the data with legend
plot(df2$x, df2$y,[df2$mode]col=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255)),pch=16)
legend("topright", c("H","Q","T"), cex=1.0, bty="n", fill=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255)))




#lightgreen
#144, 238, 144
#dark red
#139, 0, 0
#purple
#171, 130, 255

plot(my.df$x, my.df$y,pch=16
     ,col=(c(rgb(144,238,144,127.5),rgb(139,0,0,127.5),rgb(171,130,255,127.5))[my.def$mode]])

help("rgb")







