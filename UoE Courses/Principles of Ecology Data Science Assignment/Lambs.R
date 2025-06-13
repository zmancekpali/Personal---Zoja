#lambs
#plot the relationship between population (pop) and weight(wt)
plot(wt~pop, data=lambs, 
     xlab="Total sheep population", ylab="Mean lamb weight (kg)")

#we want to find to what extent the variation in population affects the variation in lamb weight
#aka covariance
cov(lambs$pop, lambs$wt)
#^ higher values of population are associated with lower values of weight
#pop density has a negative effect on lamb weight


#find the line of best fit -> y=mx+b
#b = mean, m=0
plot(wt~pop, data=lambs, 
     xlab="Total sheep population", ylab="Mean lamb weight (kg)")
abline(mean(lambs$wt), 0, col="blue", lty=2) #we are setting this abline up as an intercept and slope - mean and 0)

#unexplained variation = difference between each point and the mean - residuals:
#we know there is lots of unexplained variation 
#the mean does not explain the pattern well
residuals <- lambs$wt - mean(lambs$wt)
total_ss <- sum(residuals^2) #sum of squares
#the total_ss is our null expectation - weight is not affected by population size - each lamb is assigned a weight at random

#line of best fit = must go through a point that represents mean population AND mean weight
plot(wt~pop, data=lambs, 
     xlab="Total sheep population", ylab="Mean lamb weight (kg)")
points(mean(lambs$wt)~mean(lambs$pop), 
       cex = 2, col=2, pch=16) #cex = change size of points, pch= type (16= solid)

#find lines that rotate about that point
m <- seq(-0.04, 0.02, 0.0075) #range of possible slopes
c <- c()
for (i in 1:length(m)) {
  c[i] <- mean(lambs$wt) - m[i]*mean(lambs$pop)
}

#now plot all of these lines
plot(wt~pop, data=lambs)
abline(mean(lambs$wt), 0, col="blue", lty=2)
points(mean(lambs$wt)~mean(lambs$pop), cex=2, col=2, pch=16)

for (i in 1:length(c)) {
  abline(c[i], m[i], lty=2, col=2)
}

m <- seq(-0.04, 0.02, 0.001) #smoother curve

#find sum of squared residuals (error sum of squares)
sse <- c()

for (i in 1:length(m)){
  c <- mean(lambs$wt) - m[i]*mean(lambs$pop)
  residual <- lambs$wt - c - m[i]*lambs$pop
  sse[i] <- sum(residual^2)}

#plot
plot(sse~m, type="l", lty=2, col=2, ylim=c(0, 500),
     ylab="Sum of squared residuas", xlab="Slope")

#which slope gives lowest error of squares
best_m <- m[sse==min(sse)]
#best_m = -0.005
#^the line that passes throguh the point (mean pop, mean wt) and which has a slope of -0.005 leaves the least unexplained variation
#now find the intercept for that line
best_c <- mean(lambs$wt) -best_m*mean(lambs$pop)
#best_c = 15.98399

#or a much easier version:
cov(lambs$pop, lambs$wt)/var(lambs$pop)
#^ this is the slope
#substitute into equation of the line to find intercept
# c = y - mx
mean(lambs$wt) - (cov(lambs$pop, lambs$wt)/var(lambs$pop))*mean(lambs$pop)

#now our line eq is:
#weight = 16 + (-0.005 * pop size)

#store our variables
m<- -0.005138141
c <- 16.05059
residuals_line <- lambs$wt - c - m*lambs$pop #the differece between each point and the regression line
error_ss <- sum(residuals_line^2)
 
#ANOVA time for the error
#find degrees of freedom
error_df <- length(lambs$wt) -2 #here it is two bc of the two line parameters (slope and intercept)
#find mean square
error_ms <- error_ss / error_df

#what do we know?
#a large error_ms says the data are spread out with lots of unexplained variation
#a small error_ms says the data are clustered to the line with little unexplained var.
#small error_ms = the line of regression is good at explaining the data

#ANOVA for the regression line
regression_ss <- total_ss - error_ss
#regression df is 1 - only one slope in each equation of the rotating lines
#mean square
regression_ms <- regression_ss /1
#if regression_ss is small, all points on the reg. line are close to the grand mean
#so the regression line is (nearly) flat = the observed diff. between the reg. line and the mean may have arisen due to chance
#if the regression_ms is big, the line is steep = unlikely that the diff between reg line and grand mean arose by chance

F_ratio <- regression_ms / error_ms
#if the ratio is small, then there is lots of variation around the reg line (most points fall far from it), the slope is close to 0 -> the data are better explained by the grand mean
#if the ratio is large, then there is little variation around the reg line (most points fall close to it), the slope is far from 0 -> the data are better explained by the reg line

1-pf(F_ratio, 1, 27) #the p value 
#the p value falls bellow 0.005, significant
#the data is better explained by the reg line

#you can do all this in one step:
mod <- lm(wt~pop, data=lambs) #lm=linear model, mod=model (call it whatever)
#run ANOVA on your model
anova(mod)

#we can now say: as pop density increases, the autumn weight of lambs declines
#the slope (-0.005kg) tells us that for each additional sheep, the average autumn weight decreases by 5g

#find r^2 = what fraction of the total var is explained by our reg line
r2 <- regression_ss / total_ss
#r^2 = 0.2662124 = 26.6%
#our reg line explains 26.6% of the variation in the lamb weight - way higher than in real life
summary(mod)$r.squared #or just run this

#summary:
plot(wt ~ pop, data = lambs, 
     xlab = "Total sheep population", 
     ylab = "Average weight of autumn lambs (kg)")
mod <- lm (wt ~ pop, data = lambs)

plot(wt ~ pop, data = lambs, 
     xlab = "Total sheep population", 
     ylab = "Average weight of autumn lambs (kg)")
abline(mod, lty= 2, col ="red")

coef(mod) #intercept and slope
summary(mod)$r.squared

