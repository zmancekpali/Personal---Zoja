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
library(multcomp)

#data
trees <- read_excel("trees_final.xlsx", sheet = "analysis - final")
subset_trees <- trees[trees$type %in% c('Native', 'Invasive', 'Naturalised'), ] #excluding the alien group
subset_trees$type <- factor(subset_trees$type , levels=c("Native", "Naturalised", "Invasive")) #rearranging the rows in the dataset to this particular order
subset_trees <- subset_trees %>% mutate(canopy_pos = recode(canopy_pos, "L" = "Lower",
                                                       "U" = "Upper"))

#initial visualisations
hist(subset_trees$lma)
hist(subset_trees$avg_chl)
hist(subset_trees$ldcm)
hist(subset_trees$A)
hist(subset_trees$E)
hist(subset_trees$GH20)
hist(subset_trees$Dark_resp)



#LMA models and boxplot ----
lma_mod1 <- lm(LMA ~ type, data = subset_trees)
autoplot(lma_mod1)
bartlett.test(LMA ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(lma_mod1)) #non-normally distirbuted

#transform LMA to see if I can model with linear model
boxcox_lma <- boxcox(subset_trees$LMA ~ 1) #Find the lambda value for the highest peak on the curve for P
LMA <- subset_trees$LMA
lambda_lma <- boxcox_lma$x[which.max(boxcox_lma$y)] #-0.06060606
subset_trees$trans_LMA <- (LMA ^ (lambda_lma - 1)) / lambda_lma


lma_mod_trans <- lm(trans_LMA ~ type, data = subset_trees)
bartlett.test(trans_LMA ~ type, data = subset_trees) #homoscedascity           
shapiro.test(resid(lma_mod_trans)) #non-normal
autoplot(lma_mod_trans)

lma_KW <- kruskal.test(LMA ~ type, data = subset_trees) #0.0467

(lma_boxplot <- ggboxplot(subset_trees, x = "type", y = "LMA",
                            fill = "chartreuse3",
                            legend = "none",
                            xlab = "Invasion status",
                            add = "jitter",
                            bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 150, label = "Kruskal Wallis, p-value = 0.047", fontface = "bold") +
    labs(y = expression(LMA ~ (g/cm^2))))

ggsave("DISS-lma_boxplot.jpg", lma_boxplot, 
       units = "cm", width = 20, height = 15) 


tukey_result <- TukeyHSD(lma_KW)

#chlorophyll models and boxplot ----
chl_mod1 <- lm(avg_chl ~ type, data = subset_trees)
autoplot(chl_mod1)
bartlett.test(avg_chl ~ type, data = subset_trees) #heteroscedascity
shapiro.test(resid(chl_mod1))#non-normally distirbuted

boxcox_chl <- boxcox(subset_trees$avg_chl ~ 1) #Find the lambda value for the highest peak on the curve for P
CHL <- subset_trees$avg_chl
lambda_chl <- boxcox_chl$x[which.max(boxcox_chl$y)] #-1.1111
subset_trees$trans_chl <- (CHL ^ lambda_chl - 1) / lambda_chl

chl_mod_trans <- lm(trans_chl ~ type, data = subset_trees)
bartlett.test(trans_chl ~ type, data = subset_trees) #heteroscedascity           
shapiro.test(resid(chl_mod_trans)) #non-normal
autoplot(chl_mod_trans) #transformations DID NOT work

kruskal.test(avg_chl ~ type, data = subset_trees) #0.048;  significant

(chl_boxplot <- ggboxplot(subset_trees, x = "type", y = "avg_chl",
                          fill = "chartreuse4",
                          legend = "none",
                          xlab = "Invasion status",
                          add = "jitter",
                          bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 90, label = "Kruskal-Wallis, p-value = 0.048", fontface = "bold") +
    labs(y = expression(Chlorophyll ~ content ~ (SPAD))))

ggsave("DISS-chl_boxplot.jpg", chl_boxplot, 
       units = "cm", width = 20, height = 15) 


#photosynthesis models and boxplot ----
a_mod1 <- lm(A ~ type, data = subset_trees)
autoplot(a_mod1)
bartlett.test(A ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(a_mod1)) #normally distirbuted
anova(a_mod1) #0.04281

(A_boxplot <- ggboxplot(subset_trees, x = "type", y = "A",
                          fill = "darkseagreen",
                          legend = "none",
                          xlab = "Invasion status",
                          add = "jitter",
                          bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 20, label = "ANOVA, p-value = 0.043", fontface = "bold") +
    labs(y = expression(Photosynthesis ~ rate ~ (mmol/m^2/s))))

ggsave("DISS-A_boxplot.jpg", A_boxplot, 
       units = "cm", width = 20, height = 15) 





#LDCM models and boxplot ----
ldcm_mod1 <- lm(LDCM ~ type, data = subset_trees)
autoplot(ldcm_mod1)
bartlett.test(LDCM ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(ldcm_mod1)) #normal
anova(ldcm_mod1)

(ldcm_boxplot <- ggboxplot(subset_trees, x = "type", y = "LDCM",
                        fill = "plum3",
                        legend = "none",
                        xlab = "Invasion status",
                        add = "jitter",
                        bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 0.54, label = "ANOVA, p-value = 0.17", fontface = "bold") +
    labs(y = expression(LDCM ~ (g/g))))

ggsave("DISS-ldcm_boxplot.jpg", ldcm_boxplot, 
       units = "cm", width = 20, height = 15) 

#E models and boxplot ----
e_mod1 <- lm(E ~ type, data = subset_trees)
autoplot(e_mod1)
bartlett.test(E ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(e_mod1)) #normally distributed
anova(e_mod1) #NS

(E_boxplot <- ggboxplot(subset_trees, x = "type", y = "E",
                           fill = "slateblue2",
                           legend = "none",
                           xlab = "Invasion status",
                           add = "jitter",
                           bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 9, label = "ANOVA, p-value = 0.24", fontface = "bold") +
    labs(y = expression(Transpiration ~rate ~ (mmol/m^2/s))))

ggsave("DISS-E_boxplot.jpg", E_boxplot, 
       units = "cm", width = 20, height = 15) 

#GH20 models and boxplot ----
g_mod1 <- lm(GH20 ~ type, data = subset_trees)
autoplot(g_mod1)
bartlett.test(GH20 ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(g_mod1)) #non-normal

boxcox_g <- boxcox(subset_trees$GH20 ~ 1) #Find the lambda value for the highest peak on the curve for P
G <- subset_trees$GH20
lambda_G <- boxcox_g$x[which.max(boxcox_g$y)] #-0.62626
subset_trees$trans_g <- (G ^ (lambda_G - 1)) / lambda_G

g_mod_trans <- lm(trans_g ~ type, data = subset_trees)
autoplot(g_mod_trans)
bartlett.test(trans_g ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(g_mod_trans)) #normal
anova(g_mod_trans) #significant

(g_boxplot <- ggboxplot(subset_trees, x = "type", y = "GH20",
                        fill = "turquoise3",
                        legend = "none",
                        xlab = "Invasion status",
                        add = "jitter",
                        bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 85, label = "ANOVA, p-value = 0.015", fontface = "bold") +
    labs(y = expression(Stomatal ~ conductance ~ (mmol/m^2/s))))

ggsave("DISS-g_boxplot.jpg", g_boxplot, 
       units = "cm", width = 20, height = 15) 



#dark respiration models and boxplot ----
dr_mod1 <- lm(Dark_resp ~ type, data = subset_trees)
autoplot(dr_mod1)
bartlett.test(Dark_resp ~ type, data = subset_trees) #homoscedascity
shapiro.test(resid(dr_mod1)) #non-normal

#can't do a Box-Cox bc the values are negative
kruskal.test(Dark_resp ~ type, data = subset_trees) #NS

(dr_boxplot <- ggboxplot(subset_trees, x = "type", y = "Dark_resp",
                        fill = "steelblue4",
                        legend = "none",
                        xlab = "Invasion status",
                        add = "jitter",
                        bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    annotate("text", x = 2, y = 1.2, label = "Kruskal Wallis, p-value = 0.27", fontface = "bold") +
    labs(y = expression(Dark ~ respiration ~ (mmol/m^2/s))))

ggsave("DISS-dr_boxplot.jpg", dr_boxplot, 
       units = "cm", width = 20, height = 15) 



#additional factors
boxplot(avg_chl ~ canopy_pos, data = subset_trees)
boxplot(lma ~ canopy_pos, data = subset_trees)
boxplot(GH20 ~ canopy_pos, data = subset_trees)

anova(lm(Dark_resp ~ ever_dec, data = subset_trees))
#all except Dark_resp, E, and ldcm are sig. affected by ever_dec



#post-hoc Tukay tests


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
