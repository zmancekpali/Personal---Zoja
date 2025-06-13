###Annotated script for Ecological and Environmental Analysis R Statistics
##Any questions, write Kyle Dexter, kyle.dexter@ed.ac.uk

##Exercise 1
#Enter the data 'manually' and create a data frame
A <- c(115,120,135,155,160,170,175,200,205,220)
B <- c(115,145,75,60,95,95,170,105,130,120)
C <- c(155,75,110,145,85,105,140,75,140,110)
height <- c(A,B,C)
fertiliser <- c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C)))
alldata <- data.frame(fertiliser,height)

#build a basic statistical model
(height_lm <- lm(height~fertiliser,data=alldata))

#execute an analysis of variance
anova(height_lm)

#get further information on your statistical model
summary(height_lm)

#extract residuals and see if the follow model assumptions
height_resids <- resid(height_lm)
shapiro.test(height_resids)
bartlett.test(height~fertiliser,data=alldata)

#use diagnostic plots to assess how well model fits data
plot(height_lm)

#use a Tukey's test to determine which groups are signficantly different from each other
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov)

#perform an analysis of variance 'by hand'
total_seedlings <- length(A)+length(B)+length(C)
number_categories <- 3
overall_mean <- (sum(A)+sum(B)+sum(C))/total_seedlings

(SSwithin <- sum((mean(A)-A)^2)+sum((mean(B)-B)^2)+sum((mean(C)-C)^2))
(DFwithin <- total_seedlings -number_categories)
(MSwithin <- SSwithin/DFwithin)

sumA <- length(A)*((mean(A)-overall_mean)^2)
sumB <- length(B)*((mean(B)-overall_mean)^2)
sumC <- length(C)*((mean(C)-overall_mean)^2)
(SSamong <- sumA+sumB+sumC)
(DFamong <- number_categories-1)
(MSamong <- SSamong/DFamong)

(Fstatistic <- MSamong/MSwithin)

pf(Fstatistic, DFamong, DFwithin, lower.tail=FALSE)


##Exercise 2
#read in the soils data; IMPORANT: you need to first set the working directory to where this file is found
soils <- read.csv("Peru_Soil_Data.csv", row.names=1, stringsAsFactors = TRUE)
#if that does not work, you can try the following and browse for the file on your computer
soils <- read.csv(file.choose(), row.names=1,stringsAsFactors=TRUE)

#check out the data you read in
dim(soils)
names(soils)
soils$Soil_pH
soils[,c(4,9)]
head(soils)
summary(soils)

#make a histogram for a variable and make it look presentable
hist(soils$Soil_pH)
hist(soils$Soil_pH,breaks=20)
hist(soils$Soil_pH,breaks=20,col="grey")
hist(soils$Soil_pH,breaks=20,col="grey",xlab="Soil pH",main="")
#add lines to the histogram
abline(v=median(soils$Soil_pH))
abline(v=mean(soils$Soil_pH))

#make boxplots for how continuous variable varies across groups
plot(Soil_pH~Habitat,data=soils)
plot(Potassium~Habitat,data=soils)

#build basic linear models and run analyses of variance
lm_pH <- lm(Soil_pH~Habitat,data=soils)
anova(lm_pH)
lm_K <- lm(Potassium~Habitat,data=soils)
anova(lm_K)

#extract residuals and test if following model assumptions
lm_pH_resids <- resid(lm_pH)
shapiro.test(lm_pH_resids)
bartlett.test(Soil_pH ~Habitat,data=soils)
lm_K_resids <- resid(lm_K)
shapiro.test(lm_K_resids)
bartlett.test(Potassium ~Habitat,data=soils)
#use diagnostic plots to check if models OK
plot(lm_pH)
plot(lm_K)

#log-transform one of the variables to hopefully build a better model
soils$log_K <- log(soils$Potassium)
lm_log_K <- lm(log_K ~Habitat,data=soils)
anova(lm_log_K)
#validate that model
lm_log_K_resids <- resid(lm_K)
shapiro.test(lm_log_K_resids)
bartlett.test(log_K ~Habitat,data=soils)
plot(lm_log_K)


##Exercise 3
#two ways of making scatterplots for two continuous variables
plot(soils$Total_Base_Saturation, soils$Soil_pH)
plot(Soil_pH~Total_Base_Saturation,data=soils)
#make those plots look nicer
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue")
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2)
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2,cex.lab=1.5)
#how to add a trendline to the plot
bestfit <- lm(Soil_pH~Total_Base_Saturation,data=soils)
abline(bestfit)
#make that plot look better
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2,cex.lab=1.5)
abline(bestfit,lty=2,lwd=2,col="red")

#plot related to model we will explore
plot(Soil_pH~Clay, data=soils)
#build model and check it out in a couple of way
pH_vs_Clay <- lm(Soil_pH~Clay,data=soils)
anova(pH_vs_Clay)
summary(pH_vs_Clay)
#check model fit (validate model)
pH_vs_Clay_resids <- resid(pH_vs_Clay)
shapiro.test(pH_vs_Clay_resids)
plot(pH_vs_Clay)
#try and identify outlier on the original plot
plot(Soil_pH~Clay, data=soils)
text(Soil_pH~Clay, data=soils,labels=rownames(soils))
abline(pH_vs_Clay,col="red")
#figure out which row is the outlier and build new model excluding it
rownames(soils)
pH_vs_Clay_new <- lm(Soil_pH~Clay,data=soils[-1,])
anova(pH_vs_Clay_new)
plot(pH_vs_Clay_new)


##Exercise 4
#build model
salt <- lm(Sodium~ Habitat,data=soils)
summary(salt)
plot(salt)
#trying improving model by log-transforming
lm_log_salt <- lm(log(Sodium)~ Habitat,data=soils)
summary(lm_log_salt)
plot(lm_log_salt)

#use non-parametric alternative to ANOVA to test effect of habitat on sodium levels
kruskal.test(Sodium~ Habitat,data=soils)

#check out relationship between these two continuous variables
lm_sand_TBS <- lm(Total_Base_Saturation~Sand, data=soils)
summary(lm_sand_TBS)
plot(lm_sand_TBS)

#use non-parametric alternative to linear regression
cor.test(soils$Sand,soils$Total_Base_Saturation,data=soils, method="spearman")
#follow R's advice :)
cor.test(soils$Sand,soils$Total_Base_Saturation,data=soils, method="spearman",exact=FALSE)


##Exercise 5
#build a model with two explanatory variables
lm_pH_Habitat_TBS <- lm(Soil_pH ~ Habitat + Total_Base_Saturation,data=soils)
anova(lm_pH_Habitat_TBS)
summary(lm_pH_Habitat_TBS)
#build a model allowing those two variables to interact
lm_pH_Habitat_TBS_Interaction <- lm(Soil_pH~Habitat + Total_Base_Saturation + Habitat:Total_Base_Saturation,data=soils)
#same as previous but with easier to implement notation
lm_pH_Habitat_TBS_Interaction <-lm(Soil_pH ~ Habitat*Total_Base_Saturation, data=soils)

#check out that model and make diagnostic plots
anova(lm_pH_Habitat_TBS_Interaction)
summary(lm_pH_Habitat_TBS_Interaction)
plot(lm_pH_Habitat_TBS_Interaction)

#build simpler, 'subset' models
lm_pH_Habitat <- lm(Soil_pH~Habitat,data=soils)
lm_pH_TBS <- lm(Soil_pH~Total_Base_Saturation,data=soils)

#use summary function to get adjusted R2 values for all of these models and compare
summary(lm_pH_Habitat)
summary(lm_pH_TBS)
summary(lm_pH_Habitat_TBS)
summary(lm_pH_Habitat_TBS_Interaction)

#examine sample size for the two habitats in different river basins
summary(soils$River_Basin:soils$Habitat)

#subset the data in a way that allows us to test for interaction of river basin and habitat on soil pH
soils_trim <- soils[soils$River_Basin=="Manu"|soils$River_Basin=="Los_Amigos",]
#alternative subsetting code
soils_trim <- soils[soils$River_Basin %in% c("Manu","Los_Amigos"),]

#build a set of models
lm_P_habitat <- lm(Phosphorus~ Habitat,data= soils_trim)
lm_P_basin <- lm(Phosphorus~ River_Basin,data= soils_trim)
lm_P_habitat_basin <- lm(Phosphorus~ Habitat+River_Basin,data= soils_trim)
lm_P_habitat_basin_interaction <- lm(Phosphorus~ Habitat*River_Basin,data= soils_trim)

#use summary function to get R2 values for those models
summary(lm_P_habitat)
summary(lm_P_basin)
summary(lm_P_habitat_basin)
summary(lm_P_habitat_basin_interaction)

