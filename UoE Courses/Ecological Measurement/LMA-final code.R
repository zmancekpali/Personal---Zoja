tree_age <- LMA$age
lma <- LMA$lma_final
tid <- as.factor(LMA$TID)
plot(lma~tree_age)

two_way_anova <- aov(lma_final ~ age + TID, data = LMA)
summary(two_way_anova)

two_way_plot <- ggplot(LMA, aes(x = TID, y = lma_final)) + geom_point(
  cex = 1.5, pch = 1.0) + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2)

mod <- lm(lma_final~age, data=LMA)
anova(mod)
summary(mod)$r.squared

lma_mod <- lm(lma_final~age*TID, data=LMA)
anova(lma_mod)
summary(lma_mod)$r.squared

lma_species <- lm(lma_final~TID, data=LMA)
anova(lma_species)
summary(lma_species)$r.squared


library(dplyr)
lma_table <- LMA  %>% group_by(TID) %>% summarise (mean_LMA =  mean(lma_final), 
                                                            stdev = sd(lma_final) ,
                                                            se_LMA = sd(lma_final)/sqrt(n()))

#plot showing means + standard error
library(ggplot2)
lma <- ggplot(LMA) + geom_point(aes(TID, lma_final, col=TID), shape=21)

lma <- lma + geom_point(data=LMA, aes(TID, lma_final,col = TID),
                        stat="summary",fun="mean", size = 3)
lma <- lma + geom_errorbar (data = lma_table, 
                            aes(x=TID, 
                                ymin=(mean_LMA-se_LMA), 								
                                ymax=(mean_LMA+se_LMA),
                                col=TID), width=0.1) 
lma <- lma + labs(x="Species", y = "Mean LMA (g/cm^2)")
lma <- lma + theme_bw()


plot(lma_final~age, col=c("cornflowerblue", "darkorchid2", "gold", "coral2"), 
     data=LMA,
     ylab="Mean LMA (g/cm^2)", 
     xlab="Tree age (years)",
     cex=0.8,
     pch=19)
legend("topright", legend=c("A. glutinosa", "B. pendula", "Q. robur", "S. aucuparia"), pch=19,
       col=c("cornflowerblue", "darkorchid2", "gold", "coral2"))

#subsets and linear models
alder <- subset(LMA, TID == "Alder")
alder_mod <- lm(lma_final~age, data=alder)
abline(alder_mod, lty = 2, col="cornflowerblue")

oak <- subset(LMA, TID == "Oak")
oak_mod <- lm(lma_final~age, data=oak)
abline(oak_mod, lty= 2, col ="gold")

rowan <- subset(LMA, TID == "Rowan")
rowan_mod <- lm(lma_final~age, data=rowan)
abline(rowan_mod, lty = 2, col="coral2")

birch <- subset(LMA, TID == "Birch")
birch_mod <- lm(lma_final~age, data=birch)
abline(birch_mod, lty = 2, col="darkorchid2")


#ggpubr
my_data_lma <- data.frame(
  group = tid,
  LMA = c(alder$lma_final, oak$lma_final, rowan$lma_final, birch$lma_final),
  age = c(alder$age, oak$age, rowan$age, birch$age)
) %>% mutate(group = recode(group, "Alder" = "A. glutinosa",
                            "Birch" = "B. pendula",
                            "Oak" = "Q. robur",
                            "Rowan" = "S. aucuparia"))

library(dplyr)
group_by(my_data_lma, group) %>%
  summarise(
    count = n(),
    mean = mean(LMA, na.rm = TRUE),
    sd = sd(LMA, na.rm = TRUE)
  ) 

coef <- coefficients(mod)
intercept <- coef[1]
slope <- coef[2]

mod <- lm(lma_final~age, data=LMA)
anova(mod)
summary(mod)$r.squared

mod1 <- lm(lma_final~Mean_DBH*TID, data=LMA)
anova(mod1)
summary(mod1)$r.squared

alder <- subset(LMA, TID == "Alder")
alder_mod <- lm(lma_final~age, data=alder)
abline(alder_mod, lty = 2, col="cornflowerblue")

coef_alder <- coefficients((alder_mod))
intercept_alder <- coef_alder[1]
slope_alder <- coef_alder[2]

oak <- subset(LMA, TID == "Oak")
oak_mod <- lm(lma_final~age, data=oak)
abline(oak_mod, lty= 2, col ="gold")
coef_oak <- coefficients((oak_mod))
intercept_oak <- coef_oak[1]
slope_oak <- coef_oak[2]


rowan <- subset(LMA, TID == "Rowan")
rowan_mod <- lm(lma_final~age, data=rowan)
abline(rowan_mod, lty = 2, col="coral2")
coef_rowan <- coefficients((rowan_mod))
intercept_rowan <- coef_rowan[1]
slope_rowan <- coef_rowan[2]

birch <- subset(LMA, TID == "Birch")
birch_mod <- lm(lma_final~age, data=birch)
abline(birch_mod, lty = 2, col="darkorchid2")
coef_birch <- coefficients((birch_mod))
intercept_birch <- coef_birch[1]
slope_birch <- coef_birch[2]

lma_species <- lm(lma_final~TID, data=LMA)
anova(lma_species)
summary(lma_species)$r.squared
coef_species <- coefficients(lma_species)
intercept_species <- coef_species[1]
slope_species <- coef_species[2]

my_data <- data.frame (
  group = tid,
  LMA = c(alder$lma_final, birch$lma_final, oak$lma_final, rowan$lma_final),
  age = c(alder$age, birch$age, oak$age, rowan$age)
) %>% mutate(group = recode(group, "Alder" = "A. glutinosa",
                            "Birch" = "B. pendula",
                            "Oak" = "Q. robur",
                            "Rowan" = "S. aucuparis"))

ggscatter(my_data, x = "age", y = "LMA",
          col = c("darkorange", "violetred", "skyblue1", "aquamarine4"),
          ylab = expression("Mean LMA (g/m"^2*")"), #add sub/superscripts
          xlab = "Tree age (years)",
          color = "group",
          legend.title="Species") +
  scale_fill_discrete(palette="PiYG")+
  geom_abline(intercept = intercept, slope = slope, color = "black", linetype = "dashed", size = 0.5) +
  annotate("text", x = 100, y = 68, size = 3,  label=expression("R"^2* " = 0.002"), color = "black") +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  theme(legend.position = c(0.87, 0.9)) +
  theme(legend.text = element_text(face = "italic"))



ggscatter(my_data_lma, x = "age", y = "LMA", 
          color = "group",
          ylab = expression("Mean LMA (g/m"^2*")"), #add sub/superscripts
          xlab = "Tree age (years)",
          legend.title="Species") + 
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) + 
  theme(legend.position = c(0.87, 0.9)) + 
  theme(legend.text = element_text(face = "italic")) +
  
  
  
  geom_abline(intercept = intercept_oak, slope = slope_oak, color = "#00BFC4", linetype = "dashed", size = 0.5) + 
    annotate("text", x = 109, y = 58, size = 3,  label=expression("R"^2* " = 0.014"), color = "#00BFC4") +
    geom_abline(intercept = intercept_rowan, slope = slope_rowan, color = "#C77CFF", linetype = "dashed", size = 0.5) + 
    annotate("text", x = 95, y = 79, size = 3, label=expression("R"^2* " = 0.003"), color = "#C77CFF") +
    geom_abline(intercept = intercept_alder, slope = slope_alder, color = "coral2", linetype = "dashed", size = 0.5) + 
    annotate("text", x = 109, y = 65, size = 3,  label=expression("R"^2* " = 0.014"), color = "coral2") +
    geom_abline(intercept = intercept_birch, slope = slope_birch, color = "#7CAE00", linetype = "dashed", size = 0.5) +
    annotate("text", x = 100, y = 73, size = 3,  label=expression("R"^2* " = 0.002"), color = "black") 