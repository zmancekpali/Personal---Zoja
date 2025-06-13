

howdean$habitat <- as.factor(howdean$habitat)
howdean$type <- as.factor(howdean$type)
howdean$category <- with(howdean, as.factor(habitat: type))

#most basic plot

plot (orders ~ category, data=howdean, ylab="Number of orders", xlab="Sample type")



#a bit more complex

means <- aggregate(orders ~ category, mean, data =howdean)
se<- function(x) {sd(x)/sqrt(length(x))}
ses <- aggregate(orders ~ category, se, data =howdean)



plot(NULL,ylim=c(0,10), xlim=c(0.5, 4.5), xaxt="n", ann=NULL)
title(ylab="Number of orders", xlab="Sample type")

axis(side=1, at=1:4, labels=means$category)

with(means, points(x=category, y=orders, cex=2, lwd=2))

with(howdean, points(x=category, y=orders))


arrows(x0=1:4, y0=means$orders-ses$orders, 
       x1=1:4, y1=means$orders+ses$orders, 
      length=0.05, code=3, angle=90, lwd=2)



#with colour
plot(NULL, ylim=c(0,15), xlim=c(0.5, 4.5), xaxt="n", ann=NULL)
title(ylab="Number of orders", xlab="Sample type")

axis(side=1, at=1:4, labels=c("Gorse Basin", "Gorse Pit", "Grass Basin", "Grass Pit"))

with(means, points(x=category, y=orders, cex=2, lwd=2, col=c(1,2,1,2),pch = 16))

with(howdean, points(x=category, y=orders, col=type))


arrows(x0=1:4, y0=means$orders-ses$orders, 
       x1=1:4, y1=means$orders+ses$orders, 
       length=0.05, code=3, angle=90, lwd=2, col=c(1,2,1,2))


legend("topright", col=howdean$type, legend=c("Basin", "Pit"), pch=1, bty="n")


