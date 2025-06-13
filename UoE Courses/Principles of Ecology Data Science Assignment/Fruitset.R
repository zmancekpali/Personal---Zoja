#fruitset

par(mfrow=c(1,2))

plot(blackberries~soil_pH, ylab="Blackberries per " ~m^2, ylim=c(0,80), 
     data=fruitset)
abline(h=mean(fruitset$blackberries), lty=2, col="blue")

plot(blaeberries~soil_pH, ylab="Blaeberries per " ~m^2, ylim=c(0,80),
     data=fruitset)
abline(h=mean(fruitset$blaeberries), lty=2, col="blue")

par(mar=c(5.1, 6.1, 4.1, 2.1))

#we can see a relationship for baeberries, not for blackberries
#find covariance (how variation in one variable affects another)
cov(fruitset$soil_pH, fruitset$blackberries)
cov(fruitset$soil_pH, fruitset$blaeberries)

