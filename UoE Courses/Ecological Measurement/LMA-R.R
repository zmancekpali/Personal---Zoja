#SUBSETS 
oak <- subset(LMA_raw_data, TID=="Oak")
alder <- subset(LMA_raw_data, TID == "Alder")
rowan <- subset(LMA_raw_data, TID == "Rowan")
birch <- subset (LMA_raw_data, TID == "Birch")

#MEANS
oak_dbh <- mean(oak$Mean_DBH)
oak_age <- mean(oak$age)
oak_lma <- mean(oak$MLMA)
alder_dbh <- mean(alder$Mean_DBH)
alder_age <- mean(alder$age)
alder_lma <- mean(alder$MLMA)
rowan_dbh <- mean(rowan$Mean_DBH)
rowan_age <- mean(rowan$age)
rowan_lma <- mean(rowan$MLMA)
birch_dbh <- mean(birch$Mean_DBH)
birch_age <- mean(birch$age)
birch_lma <- mean(birch$MLMA)

#PLOT
trees <- as.factor(LMA_raw_data$TID)
plot(MLMA~age, col=c("cornflowerblue", "darkorchid2", "gold", "coral2"), 
     data=LMA_raw_data, 
     ylim = c(0, 0.004), 
     xlim = c(15, 120),
     ylab="Mean LMA (g/cm^2)", 
     xlab="Tree age (years)",
     cex=0.8,
     pch=19)
legend("topright", legend=c("Alder", "Birch", "Oak", "Rowan"), pch=19,
       col=c("cornflowerblue", "darkorchid2", "gold", "coral2"))
abline(oak_mod, lty= 2, col ="gold")
abline(alder_mod, lty = 2, col="cornflowerblue")
abline(rowan_mod, lty = 2, col="coral2")
abline(birch_mod, lty = 2, col="darkorchid2")


#LMA STANDARD ERROR
stdev <- sd(LMA_raw_data$MLMA)
st_error <- stdev/sqrt(40)
stdev_oak <- sd(oak$MLMA)
st_error_oak <- stdev_oak/sqrt(10)
stdev_alder <- sd(alder$MLMA)
st_error_alder <- stdev_alder/sqrt(10)
stdev_birch <- sd(birch$MLMA)
st_error_birch <- stdev_birch/sqrt(10)
stdev_rowan <- sd(rowan$MLMA)
st_error_rowan <- stdev_rowan/sqrt(10)

#AREA STANDARD ERRORS
stdev_area <- sd(LMA_raw_data$`Mean area`)
st_error_area <- stdev_area/sqrt(40)

sdA_oak <- sd(oak$`Mean area`)
stE_oak <- sdA_oak/sqrt(10) #3.225782, the typical difference between the
#the sample mean and the true mean is 3 cm^2 
sdA_alder <- sd(alder$`Mean area`)
stE_alder <- sdA_alder/sqrt(10) #6.0810486
sdA_birch <- sd(birch$`Mean area`)
stE_birch <- sdA_birch/sqrt(10) #1.1503203
sdA_rowan <- sd(rowan$`Mean area`)
stE_rowan <- sdA_rowan/sqrt(10) #2.921814

#LINEAR MODELS

#ALDER (WITHIN)
alder_mod <- lm(MLMA~age, data=alder)
anova(alder_mod)
summary(alder_mod)$r.squared
#no effect of age on LMA in alder

#OAK (WITHIN)
oak_mod <- lm(MLMA~age, data=oak)
anova(oak_mod)
summary(oak_mod)$r.squared
#no effect of age on LMA in oak

#ROWAN (WITHIN)
rowan_mod <- lm(MLMA~age, data=rowan)
anova(rowan_mod)
summary(rowan_mod)$r.squared
#no effect of age on LMA in rowan

#BIRCH (WITHIN)
birch_mod <- lm(MLMA~age, data=birch)
anova(birch_mod)
summary(birch_mod)$r.squared
#no effect of age on LMA in birch

#BETWEEN SPECIES
tree_mod<- lm (MLMA ~ age, data=LMA_raw_data)
anova(tree_mod)
summary(tree_mod)$r.squared
#no effect of age on LMA in the trees that we sampled

#MORE PLOTS

