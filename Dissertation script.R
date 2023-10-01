##%#########################################################################%##
#                                                                             #
#                    Dissertation script - Zoja Manček Páli                   #
#                              Started: 30.9.2023                             #
#                                                                             #
##%#########################################################################%##

#packages
library(dplyr)
library(e1071)
library(MASS)
library(ggfortify)
library(vegan)

#data
trees <- read_excel("trees_final.xlsx", sheet = "analysis - final")
subset_trees <- trees[trees$type %in% c('Native', 'Invasive', 'Naturalised'), ] #excluding the alien group
subset_trees$type <- factor(subset_trees$type , levels=c("Native", "Naturalised", "Invasive")) #rearranging the rows in the dataset to this particular order
swap <- t(subset_trees) #inverts the rows and columns

#initial visualisations
hist(subset_trees$lma)
hist(subset_trees$avg_chl)
hist(subset_trees$ldcm)
hist(subset_trees$A)
hist(subset_trees$E)
hist(subset_trees$GH20)
hist(subset_trees$Dark_resp)



#LMA linear model ----
lma_mod1 <- lm(lma ~ type, data = subset_trees)
bartlett.test(lma ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(lma_mod1)) #non-normally distirbuted

#transform LMA to see if I can model with linear model
boxcox_lma <- boxcox(subset_trees$lma ~ 1) #Find the lambda value for the highest peak on the curve for P
LMA <- subset_trees$lma
lambda_lma <- boxcox_lma$x[which.max(boxcox_lma$y)] #-0.06060606
subset_trees$trans_lma <- (LMA ^ lambda_lma - 1) / lambda_lma


lma_mod_trans <- lm(trans_lma ~ type, data = subset_trees)
bartlett.test(trans_lma ~ type, data = subset_trees) #homoscedascity           
shapiro.test(resid(lma_mod_trans)) #normal
autoplot(lma_mod_trans)
anova(lma_mod_trans) #NS
boxplot(lma ~ type, data = subset_trees, 
        xlab = "Invasion status",
        ylab = expression(LMA ~ (cm^2)))

lma_boxplot <- ggplot(subset_trees, aes(x = type, y = lma)) +
  geom_boxplot(fill = "forestgreen") +
  theme_classic() +
  guides(fill = "none") +
  labs(y=expression(LMA ~ (g/cm^2)), x = "Invasion status") +
  geom_jitter() +
  stat_boxplot(geom = "errorbar",
               width = 0.15)






#LDCM linear model


#chlorophyll linear model ----
chl_mod1 <- lm(avg_chl ~ type, data = subset_trees)
bartlett.test(avg_chl ~ type, data = subset_trees) #heteroscedascity
shapiro.test(resid(chl_mod1)) #non-normally distirbuted

boxcox_chl <- boxcox(subset_trees$avg_chl ~ 1) #Find the lambda value for the highest peak on the curve for P
CHL <- subset_trees$avg_chl
lambda_chl <- boxcox_chl$x[which.max(boxcox_chl$y)] #-0.05050505
subset_trees$trans_chl <- (CHL ^ lambda_chl - 1) / lambda_chl


chl_mod_trans <- lm(trans_chl ~ type, data = subset_trees)
bartlett.test(trans_chl ~ type, data = subset_trees) #heteroscedascity           
shapiro.test(resid(chl_mod_trans)) #non-normal
autoplot(chl_mod_trans) #transformations DID NOT work

boxplot(avg_chl ~ code_two, data = subset_trees, 
        xlab = "Invasion status",
        ylab = expression(LMA ~ (cm^2)))


#photosynthesis linear model ----
a_mod1 <- lm(A ~ type, data = subset_trees)
bartlett.test(A ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(a_mod1)) #normally distirbuted
anova(a_mod1) #NS
boxplot(A ~ code_two, data = subset_trees, 
        xlab = "Invasion status",
        ylab = expression(Photosynthesis ~ (μmol ~m^-2 ~s^-1)))





#glms?? ----
glm(avg_chl ~ type + ever_dec, data = subset_trees)


#mixed effect models?? ----
library(lme4)
model_lma <- lmer(lma ~ type + (1 | ever_dec), data = subset_trees)
model_lma_1 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = subset_trees) 
model_lma_2 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = subset_trees)
model_lma_3 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = subset_trees)
AIC(model_lma_1, model_lma_2, model_lma_3)
#models 2 and 3 fall within 2 AIC scores; so virtually identical fit to the data
#looks like adding DBH does not add anything to the data, so I will not use it

#so for model_lma_2; there is still 12.56 residual std that is not explained by any of these random effects

model_chl_1 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = trees)
model_chl_2 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = trees)
model_chl_1 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = trees)

summary(glm(lma ~ type, data = subset_trees))


#NMDS for LMA - in progress
nmds_data <- data.frame(type = as.factor(swap$type), lma = swap$lma)
