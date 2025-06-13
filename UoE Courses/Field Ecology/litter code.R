
#running the tests 

t.test (orders ~ tree, data=litter)

t.test (abundance ~ tree, data=litter)


#getting data ready to plot

litter$tree <- as.factor(litter$tree)


#calculating means and ses for orders

means <- with(litter, tapply(orders, tree, mean))
se <- function(x) { sd(x) / sqrt(length(x))}
ses <- with(litter, tapply(orders, tree, se))

#base plot

plot(NULL, ylim=c(0, 14), xlim= c(0.5,2.5), ann=NULL, xaxt="n")
axis(side =1, at=1:2, labels =names(means))
title(xlab="Tree species", ylab="Number of orders")

#raw points(orders ~ tree, data=litter)
points (orders ~ jitter (as.numeric(tree), 0.5), data=litter)

#mean points
points(means,  cex=2,  lwd=2, pch=16)

#error bars
arrows(x0=1:2, y0=means-ses, x1=1:2, y1=means+ses, length=0.1, angle=90,code=3, lwd=2)

text(x=1, y=13, labels = "n=16")
text(x=2, y=13, labels = "n=17")




#calculating means and ses for abundance

means <- with(litter, tapply(abundance, tree, mean))
se <- function(x) { sd(x) / sqrt(length(x))}
ses <- with(litter, tapply(orders, tree, se))

#base plot

plot(NULL, ylim=c(10, 45), xlim= c(0.5,2.5), ann=NULL, xaxt="n")
axis(side =1, at=1:2, labels =names(means))
title(xlab="Tree species", ylab="Abundance")

#raw points(orders ~ tree, data=litter)
points (abundance ~ jitter (as.numeric(tree), 0.5), data=litter)

#mean points
points(means,  cex=2,  lwd=2, pch=16)

#error bars
arrows(x0=1:2, y0=means-ses, x1=1:2, y1=means+ses, length=0.1, angle=90,code=3, lwd=2)

text(x=1, y=45, labels = "n=16")
text(x=2, y=45, labels = "n=17")




#if need to use non parametric: 

#test:

wilcox.test(orders ~ tree, data=litter)

#plotting


medians <- with(litter, tapply(orders, tree, median))
q1 <- function(x) { quantile(x, 0.25)}
q1s <- with(litter, tapply(orders, tree, q1))

q2 <- function(x) { quantile(x, 0.75)}
q2s <- with(litter, tapply(orders, tree, q2))

plot(NULL, ylim=c(0, 14), xlim= c(0.5,2.5), ann=NULL, xaxt="n")
axis(side =1, at=1:2, labels =names(means))
title(xlab="Tree species", ylab="Number of orders")

points (orders ~ jitter (as.numeric(tree), 0.5), data=litter)
points(medians,  cex=2,  lwd=2)


arrows(x0=1:2, y0=q1s, x1=1:2, y1=q2s, length=0.1, angle=90,code=3, lwd=2)






