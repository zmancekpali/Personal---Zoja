inga <- read.csv('Inga_traits.csv')

#####
#simple histograms -> without line, for visual estimation only
hist(inga$Leaf_Area, xlab = "Leaf area", main = "Leaf area distribution", prob = TRUE)
hist(inga$Leaf_Area, breaks = 25, xlab = "Leaf area", main = "Leaf area distribution")

#histograms with line; we can see a right skew
x2 <- seq(min(inga$Leaf_Area), max(inga$Leaf_Area), length = 40)
fun <- dnorm(x2, mean = mean(inga$Leaf_Area), 
      sd = sd(inga$Leaf_Area)) #this gives the normal curve
hist(inga$Leaf_Area, prob = TRUE, col = "white", 
     main = "Leaf area distribution", 
     xlab = expression(Leaf~area~(cm^2)))
lines(x2, fun, col = 2, lwd = 2)

hist(inga$Leaf_Area, prob = TRUE, col = "white", 
     main = "Leaf area distribution", 
     breaks = 25, 
     xlab = expression(Leaf~area~(cm^2)))
lines(x2, fun, col = 2, lwd = 2)

#calculate skewedness
library(e1071)
skewness(inga$Leaf_Area) #gives the closeness of the data to a normal distribution
#we get 1.230511 = positive skewness = skewed to the right
kurtosis(inga$Leaf_Area) #gives the extent to which the data is concentrated around the mean
#we get 1.838808

#log-transforming
x3 <- seq(min(log(inga$Leaf_Area)), max(log(inga$Leaf_Area)), length = 40)
fun1 <- dnorm(x3, mean = mean(log(inga$Leaf_Area)), 
             sd = sd(log(inga$Leaf_Area))) #this gives the normal curve
hist(log(inga$Leaf_Area), prob = TRUE, col = "white", 
     main = "Leaf area distribution", 
     breaks = 25, 
     xlab = expression(Log(leaf~area~(cm^2))))
lines(x3, fun1, col = 2, lwd = 2)
skewness(log(inga$Leaf_Area)) #-0.259
kurtosis(log(inga$Leaf_Area)) #-0.523


########
Habitat <- as.factor(my_data$Habitat)
Area <- inga$Leaf_Area
leaf_P <- inga$P_Leaf
gen <- subset(inga, Habitat == "generalist")
flood <- subset(inga, Habitat == "floodplain")
up <- subset(inga, Habitat == "upland")
group <- inga$Habitat

library(dplyr)
my_data <- data.frame(inga) %>% 
  mutate(Habitat = recode(Habitat, "floodplain" = "Floodplain",
                        "generalist" = "Generalist",
                        "upland" = "Upland")) 

boxplot(P_Leaf ~ Habitat, data = inga)

ggboxplot(my_data, x = "Habitat", y = "P_Leaf",            
          fill = "Habitat",
          ylab = expression(P~concentration~(mg/g)),
          legend = "none",
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.15) +
  scale_fill_manual(values = c("skyblue", "orange", "darkgreen")) +
  theme(text = element_text(size=9)) +
  theme(axis.title.x=element_blank())

#####
x4 <- seq(min(inga$P_Leaf, na.rm = TRUE), max(inga$P_Leaf, na.rm = TRUE), length = 40)
fun2 <- dnorm(x4, mean = mean(inga$P_Leaf, na.rm = TRUE), 
              sd = sd(inga$P_Leaf, na.rm = TRUE)) #this gives the normal curve

hist(inga$P_Leaf, breaks = 25, prob = TRUE, col = "white", 
     xlab = "Leaf P concentration (mg/g)", 
     main = "Leaf P concentration distribution")
lines(x4, fun2, col = 2, lwd = 2)
skewness(inga$P_Leaf, na.rm = TRUE) #1.219

ggdensity(inga, x = "P_Leaf", fill = "lightgray", title = "Leaf P") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


#model
mod1 <- lm(P_Leaf ~ Habitat, data = inga, na.action = na.exclude) #excluding the NAs from the model so that observation levels are identical
par(mfrow=c(1,1))
plot(mod1)
library(ggfortify)
autoplot(mod1) #this does the same as plot(mod1)

residuals <- resid(mod1)
shapiro.test(residuals) #p = 0.1193; residuals are normal
ggqqplot(inga$P_Leaf, ylab = "Leaf P Concentration")
bartlett.test(P_Leaf ~ Habitat, data = inga) #p = 0.0058; heteroscedascity; the groups have different variances

###see here for the code for calculating the variances
non_missing <- !is.na(inga$P_Leaf)
residus <- residuals[non_missing]
habs <- inga$Habitat[non_missing]
variance_by_habitat <- tapply(residus, habs, var)

##because we removed the NAs, we also need to update the number of observations now
clean_data <- inga %>% filter(!is.na(P_Leaf))
obs_per_habitat <- clean_data %>% group_by(Habitat) %>% summarize(count = n())


mod2 <- lm(log(P_Leaf) ~ Habitat, data = inga)
resid <- resid(mod2)
shapiro.test(resid) #p = 0.60; normal
bartlett.test(log(P_Leaf) ~ Habitat, data = inga) #0.12; homoscedascity