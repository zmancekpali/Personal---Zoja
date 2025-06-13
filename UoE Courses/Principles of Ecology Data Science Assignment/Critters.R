#critters dataset - find the mean of both spiders and worms

mean_worms <- mean(critters$worms)
mean_spiders <- mean(critters$spiders)

#now find the distance from each individual data point to the mean
#this creates a more exact picture of the spread of the data

worm_differences <- critters$worms - mean_worms
spiders_differences <- critters$spiders - mean_spiders

#now find the mean of the edited data
mean(worm_differences)
mean(spiders_differences)

#the results are a tiny tiny number > this is because as data falls
#below the mean, it is counted as negative; positive above the mean
#fixing for this:
worm_differences_squared <- worm_differences^2
spiders_differences_squared <- spiders_differences^2

#now, add both of these now positive values together:
sum_worm_differences_squared <- sum(worm_differences_squared)
sum_spiders_differences_squared <- sum(spiders_differences_squared)

#the above equations give the sum of the squared differences from the mean
# sum of squares > in a sequence, at each value calculate the difference between the value and the mean 
#then, square the found distance, and then add all of those values together

#now, measure the sample size (why is it only critters$worms?? - beacuse the length of the dataset is the same for worms and spiders, so use either)
n <- length(critters$worms)

#now, find the variance
var_worms <- sum_worm_differences_squared / (n-1)
var_spiders <- sum_spiders_differences_squared / (n-1)

var_worms
var_spiders

#this doesn't have to be a long process like this every time:
var(critters$worms)
var(critters$spiders)

#variance is measured in: (abundance of worms and spiders per 5m^2 or fainforest floor)^2

#standard deviation by hand
sqrt(var_worms)
sqrt(var_spiders)

#or with R
sd(critters$worms)
sd(critters$spiders)

#sd tels you the spread in our data, in the original units

#What we found, is that both datasets have a similar mean, but a much more varied spread\
# "5m^s of rainforest floor support similar numbers of individuals of worms and spiders (around 96 individuals per 5m^2")
#However, spider abundance is much more variable, with a sd of 6, as opposed to 0.7 for wormss

#show two histograms on the same panel (2 rows, one column)
par(mfrow=c(2,1))

hist(critters$worms, xlim=c(70, 110), xlab = "Worms per 5 m^2", ylab="Frequency", main = NULL)
abline(v=mean(critters$worms), col="red", lty=2, lwd=2)


hist(critters$spiders, xlim=c(70, 110), xlab= "Spiders per 5 m^2", ylab= "Frequency", main=NULL)
abline(v=mean(critters$spiders), col = "blue", lty=2, lwd=2)

#add standard deviation line ***
x <- critters$worms
m<-mean(critters$worms)
std<-sd(critters$worms)
hist(critters$worms, xlim=c(70, 110), xlab = "Worms per 5 m^2", ylab="Frequency", main = NULL)
abline(v=mean(critters$worms), col="red", lty=2, lwd=2)
lines(c(mean(critters$worms)-sd(critters$worms), mean(critters$worms)+sd(critters$worms)), 
c(0.2, 0.2), col="red", lwd=4)
n<- length(critters$worms)
curve(n*dnorm(x, m, std), add=T)

#Dr. Bell's example:
x <- critters$worms
m<-mean(critters$worms)
std<-sd(critters$worms)
hist(critters$worms+rnorm(100,0,0.001), main=NULL, xlab="Worms per 5" ~m^2, xlim=c(70,110), ylim=c(0,35))
abline(v=mean(critters$worms), col="red", lty=2, lwd=2)
lines(c(mean(critters$worms)-sd(critters$worms), mean(critters$worms)+sd(critters$worms)), 
      c(0.2, 0.2), col="red", lwd=4)
n<- length(critters$worms)
curve(n*dnorm(x, m, std), add=T)

#my original code (didnt work)
m <- mean(critters$spiders)
std<- sd(critters$spiders)
hist(critters$spiders, xlim=c(70, 110), xlab= "Spiders per 5 m^2", ylab= "Frequency", main=NULL)
abline(v=mean(critters$spiders), col = "blue", lty=2, lwd=2)
curve(dnorm(x, mean=m, sd=std), add=T)


