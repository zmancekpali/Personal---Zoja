##%#########################################################################%##
#                                                                             #
#                    Field course report - Zoja Manček Páli                   #
#                                                                             #
##%#########################################################################%##

#packages
library(readxl)
library(tidyverse)
library(sjmisc)
library(vegan)
library(ggpubr)
library(ggfortify)
library(MASS)
library(lmtest)
library(gridExtra)


#data
bugs <- read_excel("bugs.xlsx", sheet = "raw")
bugs <- bugs %>% mutate(plant_shan = plant_shannons,
                        invert_shan = invShan)

microclimate <- read_excel("bugs.xlsx", sheet = "microclimate")

#preparing data for diversity and NMDS - plants ----
plants <- bugs %>% select(site, transect, transect_ID, quad, heather: lichen_5, 
                          grass_1:rush_3) 
plants$quad <- as.factor(plants$quad)
as.data.frame(colnames(plants))
plants[is.na(plants)] <- 0
plants_diversity <- plants[, c(5:33)] #plants regular dataset for analysis

raw_plant <- bugs[-14,] #this code below removes one datapoint where D is 0 because
#a 0 doesn't work for the NMDS; the above data doesn't remove it
raw_plant$quad <- as.factor(raw_plant$quad)
as.data.frame(colnames(raw_plant))  # see column no.
plant_div <- raw_plant[,c(14:26,29:44)]
plant_div[is.na(plant_div)] <- 0

#preparing data for diversity and NMDS - inverts ----
inverts <- bugs %>% select(site, transect, transect_ID, quad, coleoptera, aranea,
                           hymenoptera, collembola, hemiptera, 
                           diptera, isopoda, acari, opiliones, larvae, psocoptera, 
                           pulmonata, achatinoidea, dictyoptera, dermaptera, julida) 

inverts$quad <- as.factor(inverts$quad)
as.data.frame(colnames(inverts))
inverts_diversity <- inverts[, c(5:20)]
inverts_diversity[is.na(inverts_diversity)] <- 0


#diversity indices ----
plant_shannons <- diversity(plants_diversity) #Shannon's diversity
plants_S <- length(plant_div) #species number
plants_evenness <- plant_shannons/(log(plants_S)) #Pielou's evenness
plants_D <- exp(plant_shannons) #True diversity
plants <- plants %>% mutate(plant_shan = plant_shannons, plant_D = plants_D, plant_J = plants_evenness)

invert_shannons <- diversity(inverts_diversity) #Shannon's diversity
inverts_S <- length(inverts_diversity) #species number
inverts_evenness <- invert_shannons/(log(inverts_S)) #Pielou's evenness
inverts_D <- exp(invert_shannons) #True diversity
inverts <- inverts %>% mutate(inv_shan = invert_shannons, inv_D = inverts_D, inv_J = inverts_evenness)


#plants NMDS ----
set.seed(123)
nmds_plants <- metaMDS(plant_div, distance = "bray", k = 2, trymax=300)
nmds_plants
plot(nmds_plants)
stressplot(nmds_plants)

Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")

names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores <- as.data.frame(scores(nmds_plants)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- as.factor(raw_plant$quad)  #  add the group variable created earlier
head(data.scores)

hull.data <- data.frame()
for(i in 1:length(unique(raw_plant$quad))){
  temp <- data.scores[data.scores$grp == unique(raw_plant$quad)[i], ][chull(data.scores[data.scores$grp ==
                                                                                          unique(raw_plant$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}


(NMDS_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=1) + # add the point markers
    scale_colour_manual("Position", values=Colours) +
    scale_fill_manual("Position", values=Colours) +
    theme_classic() +
    annotate("text", x = 0, y = 2, label = "ANOSIM, p-value > 0.05", fontface = "bold") +
    annotate("text", x = -2.5, y = 2, label = "a)", fontface = "bold", size = 6))

ggsave("plant_nmds.jpg", NMDS_plot, 
       units = "cm", width = 20, height = 15) 


diss_matrix_plants <- vegdist(plant_div, method = "bray")
anosim(diss_matrix_plants, raw_plant$quad, permutations = 9999) #not significant - no sig diff
#between the quad groups



#inverts NMDS ----
set.seed(123)
nmds4 <- metaMDS(inverts_diversity, distance = "bray", k = 2, trymax=300)
nmds4
plot(nmds4)
stressplot(nmds4)

# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")
names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores_4 <- as.data.frame(scores(nmds4)$sites)  # Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores_4$site <- rownames(data.scores_4)  # create a column of site names, from the rownames of data.scores
data.scores_4$grp <- as.factor(inverts$quad)  #  add the group variable created earlier
head(data.scores_4)

hull.data_4 <- data.frame()
for(i in 1:length(unique(inverts$quad))){
  temp_4 <- data.scores_4[data.scores_4$grp == unique(inverts$quad)[i], ][chull(data.scores_4[data.scores_4$grp ==
                                                                                                        unique(inverts$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data_4 <- rbind(hull.data_4, temp_4)
}

(NMDS_plot_4 <- ggplot() +
    geom_polygon(data=hull.data_4,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores_4,aes(x=NMDS1,y=NMDS2,colour=grp),size=1.5) + # add the point markers
    scale_colour_manual("Position", values=Colours) +
    scale_fill_manual("Position", values=Colours) +
    theme_classic() +
    annotate("text", x = 0, y = 1.5, label = "ANOSIM, p-value > 0.05", fontface = "bold") +
    annotate("text", x = -1, y = 1.5, label = "b)", fontface = "bold", size = 6))

ggsave("invert_nmds.jpg", NMDS_plot_4, 
       units = "cm", width = 20, height = 15) 

diss_matrix_inverts <- vegdist(inverts_diversity, method = "bray")
anosim(diss_matrix_inverts, inverts$quad, permutations = 9999) #not significant - no sig diff
#between the quad groups



#plant vs insect div ----
pi <- lm(invert_shannons ~ plant_shannons)
autoplot(pi)
shapiro.test(resid(pi)) #residuals normal
bptest(pi) #vasriances equal
anova(pi) #significant

div_dataset <- data.frame(
  inv_shan = inverts$inv_shan,
  plant_shan = plants$plant_shan,
  quad = bugs$quad
)

(plant_vs_inverts <- ggscatter(div_dataset, x = "plant_shan", y = "inv_shan",
                       col = "dodgerblue",
                       xlab = "Plant H'",
                       ylab = "Invertebrate H'",
                       legend = "none",
                       add = "reg.line",
                       add.params = list(color = "maroon3"),
                       conf.int = TRUE) +# Add confidence interval 
      annotate("text", x = 1, y = 2.1, label = "ANOVA, p-value < 0.05", fontface = "bold"))

ggsave("plants_vs_inverts.png", plant_vs_inverts, 
       units = "cm", width = 20, height = 15) 



#plant Shannon's box plot and model ----
boxplot(plant_shan ~ quad, data = plants)
(plant_shan_plot <- ggboxplot(plants, x = "quad", y = "plant_shan",
          fill = "quad",
          ylab = "Plant H'",
          legend = "none",
          xlab = "Position along the transect",
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
  scale_fill_manual(values = Colours) +
  annotate("text", x = 3, y = 2.0, label = "ANOVA, p-value < 0.05", fontface = "bold") +
  annotate("text", x = 0.8, y = 2.0, label = "b)", fontface = "bold", size = 6))

ggsave("Plant_shan_plot.png", plant_shan_plot, 
       units = "cm", width = 20, height = 15) 

plant_shan_mod <- lm(plant_shan ~ quad, data = plants)
autoplot(plant_shan_mod)
shapiro.test(resid(plant_shan_mod)) #normal
bartlett.test(plant_shan ~ quad, data = plants) #variances equal
anova(plant_shan_mod) #significant (position on the edge is significant)


#plants evenness box plot and model ----
boxplot(plant_J ~ quad, data = plants)

(plant_J_plot <- ggboxplot(plants, x = "quad", y = "plant_J",
            fill = "quad",
            ylab = "Plant J",
            legend = "none",
            xlab = "Position on the transect",
            bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 0.6, label = "ANOVA, p-value < 0.05", fontface = "bold"))

ggsave("plant_J_plot.png", plant_J_plot, 
       units = "cm", width = 20, height = 15) 

plant_J_mod <- lm(plant_J ~ quad, data = plants)
autoplot(plant_J_mod)
shapiro.test(resid(plant_J_mod)) #normal
bartlett.test(plant_J ~ quad, data = plants) #variances equal
anova(plant_J_mod) #significant (position on the edge is significant)


#plants true diversity box plot and models ----
boxplot(plant_D ~ quad, data = plants)

custom_label <- expression(Plant ~ D^1)
(plant_D_plot <- ggboxplot(plants, x = "quad", y = "plant_D",
           fill = "quad",
           legend = "none",
           bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
  scale_fill_manual(values = Colours) +
  annotate("text", x = 3, y = 7, label = "ANOVA, p-value = 0.04", fontface = "bold") +
  labs(y = expression(Plant ~ D^1), x = "Position on the transect"))

ggsave("plant_D_plot.png", plant_D_plot, 
       units = "cm", width = 20, height = 15) 

plant_D_mod <- lm(plant_D ~ quad, data = plants)
autoplot(plant_D_mod)
shapiro.test(resid(plant_D_mod)) #normal
bartlett.test(plant_D ~ quad, data = plants) #variances equal
anova(plant_D_mod) #significant (position on the edge is significant)


#inverts Shannon's box plot and model ----
boxplot(inv_shan ~ quad, data = inverts)

(invert_shan_plot <- ggboxplot(inverts, x = "quad", y = "inv_shan",
           fill = "quad",
           ylab = "Invertebrate H'",
           xlab = "Position along the transect",
           legend = "none",
           bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
   scale_fill_manual(values = Colours) +
   annotate("text", x = 3, y = 2.1, label = "Kruskal Wallis, p-value > 0.05", fontface = "bold") +
   annotate("text", x = 0.8, y = 2.1, label = "b)", fontface = "bold", size = 6))

ggsave("Invert_shan_plot.png", invert_shan_plot, 
       units = "cm", width = 20, height = 15)


invert_shan_trans_plot <- (ggboxplot(inverts, x = "quad", y = "shan_trans",
          fill = "quad",
          ylab = "Box-Cox Transformed Invertebrate H'",
          xlab = "Position along the transect",
          legend = "none",
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
  scale_fill_manual(values = Colours) +
  annotate("text", x = 3, y = 1.0, label = "ANOVA, p-value = 0.26", fontface = "bold"))

ggsave("Invert_shan_plot_trans.png", invert_shan_trans_plot, 
       units = "cm", width = 20, height = 15)

#model - normal:
invert_shan_mod <- lm(inv_shan ~ quad, data = inverts)
autoplot(invert_shan_mod)
shapiro.test(resid(invert_shan_mod)) #non-normal
bartlett.test(inv_shan ~ quad, data = inverts) #variances equal
anova(invert_shan_mod)
kruskal.test(inv_shan ~ quad, data = inverts)

#model - Box-Cox transformation:
boxcox_invshan <- boxcox(inverts$inv_shan ~ 1) #Find the lambda value for the highest peak on the curve
invShan <- inverts$inv_shan
lambda_invShan <- boxcox_invshan$x[which.max(boxcox_invshan$y)]
inverts$shan_trans <- (invShan ^ (lambda_invShan - 1)) / lambda_invShan
invert_shan_mod_trans <- lm(shan_trans ~ quad, data = inverts)
autoplot(invert_shan_mod_trans)
shapiro.test(resid(invert_shan_mod_trans)) #normal but just barely
bartlett.test(inv_shan ~ quad, data = inverts) #variances equal
anova(invert_shan_mod_trans) #insignificant  (position on the edge is not significant)



#inverts evenness box plots and models ----
boxplot(inv_J ~ quad, data = inverts)

(invert_J_plot <- ggboxplot(inverts, x = "quad", y = "inv_J",
                           fill = "quad",
                           ylab = "Invertebrate J",
                           legend = "none",
                           xlab = "Position on the transect",
                           bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 0.8, label = "Kruskal-Wallis, p-value = 0.42", fontface = "bold"))

ggsave("invert_J_plot.png", invert_J_plot, 
       units = "cm", width = 20, height = 15) 

#normal model:
inv_J_mod <- lm(inv_J ~ quad, data = inverts)
autoplot(inv_J_mod)
shapiro.test(resid(inv_J_mod)) #non-normal
bartlett.test(inv_J ~ quad, data = inverts) #variances equal

#Box-Cox trasformed model:
boxcox_invJ <- boxcox(inverts$inv_J ~ 1) #Find the lambda value for the highest peak on the curve
invJ <- inverts$inv_J
lambda_invJ <- boxcox_invJ$x[which.max(boxcox_invJ$y)]
inverts$J_trans <- (invJ ^ (lambda_invJ - 1)) / lambda_invJ
invert_J_mod_trans <- lm(J_trans ~ quad, data = inverts)
autoplot(invert_J_mod_trans)
shapiro.test(resid(invert_J_mod_trans)) #non-normal
bartlett.test(inv_J ~ quad, data = inverts) #variances equal
anova(invert_J_mod_trans) #insignificant  (position on the edge is not significant)

#non-parametric alternative needed:
kruskal.test(inv_J ~ quad, data = inverts) #insignificant

boxplot(inv_J ~ site, data = inverts) #you can see the effect of the ants here = much greater
#variation in evennes in the sites with ants nests present

#inverts true diversity box plots and models ----
boxplot(inv_D ~ quad, data = inverts)

(invert_D_plot <- ggboxplot(inverts, x = "quad", y = "inv_D",
                            fill = "quad",
                            legend = "none",
                            bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 8, label = "Kruskal-Wallis, p-value = 0.42", fontface = "bold") +
    labs(y = expression(Invertebrate ~ D^1), x = "Position on the transect"))

ggsave("invert_D_plot.png", invert_D_plot, 
       units = "cm", width = 20, height = 15) 

inv_D_mod <- lm(inv_D ~ quad, data = inverts)
autoplot(inv_D_mod)
shapiro.test(resid(inv_D_mod)) #non-normal (barely)
bartlett.test(inv_D ~ quad, data = inverts) #variances equal

boxcox_invD <- boxcox(inverts$inv_D ~ 1) #Find the lambda value for the highest peak on the curve
invD <- inverts$inv_D
lambda_invD <- boxcox_invD$x[which.max(boxcox_invD$y)]
inverts$D_trans <- (invD ^ (lambda_invD - 1)) / lambda_invD
invert_D_mod_trans <- lm(D_trans ~ quad, data = inverts)
autoplot(invert_D_mod_trans)
shapiro.test(resid(invert_D_mod_trans)) #non-normal
bartlett.test(inv_D ~ quad, data = inverts) #variances equal

kruskal.test(inv_D ~ quad, data = inverts) #insignificant


#abiotic box plots and models ----
boxplot(par ~ quad, data = bugs)

(par_plot <- ggboxplot(bugs, x = "quad", y = "par",
                            fill = "quad",
                            legend = "none",
                            bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    labs(y = expression(PPFD ~ (µmol/m^2/s)), x = "Position along the transect") +
    annotate("text", x = 3, y = 1500, label = "Kruskal Wallis, p-value < 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 1500, label = "a)", fontface = "bold", size = 6))
    

ggsave("par_plot.png", par_plot, 
       units = "cm", width = 20, height = 15) 

par_mod <- lm(par ~ quad, data = bugs)
autoplot(par_mod)
shapiro.test(resid(par_mod)) #normal
bartlett.test(par ~ quad, data = bugs) #variances not equal
kruskal.test(par ~ quad, data = bugs) #significant

par_plant <- lm(plant_shan ~ par, data = bugs)
autoplot(par_plant)
shapiro.test(resid(par_plant)) #normal
bartlett.test(plant_shan ~ par, data = bugs) #non-equal
anova(par_plant)

(par_plant_plot <- ggscatter(bugs, x = "par", y = "plant_shan",
                       legend = "none",
                       col = "royalblue1",
                       add = "reg.line",
                       add.params = list(color = "red"),
                       conf.int = TRUE)  +
    labs(x = expression(PPFD ~ (µmol/m^2/s)), y = "Plant H'") +
    annotate("text", x = 750, y = 2.5, label = "ANOVA, p-value < 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 2.5, label = "b)", fontface = "bold", size = 6))

ggsave("par_plant_plot.png", par_plant_plot, units = "cm", width = 20, height = 15) 


soil_mod <- lm(soil_moist ~ quad, data = bugs)
autoplot(soil_mod)
shapiro.test(resid(soil_mod)) #non-normal
bartlett.test(soil_moist ~ quad, data = bugs) #variances equal
kruskal.test(soil_moist ~ quad, data = bugs) #NS

(soil_plot <- ggboxplot(bugs, x = "quad", y = "soil_moist",
                       fill = "quad",
                       legend = "none",
                       bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    labs(y = "Soil moisture", x = "Position on the transect") +
    annotate("text", x = 3, y = 11, label = "Kruskal Wallis, p-value = 0.43", fontface = "bold"))


ggsave("soil_plot.png", soil_plot,  units = "cm", width = 20, height = 15) 

soil_plant <- lm(plant_shannons ~ bugs$soil_moist)
autoplot(soil_plant)
shapiro.test(resid(soil_plant)) #normal
bptest(plant_shannons ~ bugs$soil_moist) #non-equal
kruskal.test(plant_shannons ~ bugs$soil_moist) #NS

(soil_plant_plot <- ggscatter(bugs, x = "soil_moist", y = "plant_shan",
                             legend = "none",
                             col = "skyblue1",
                             add = "reg.line",
                             add.params = list(color = "red"),
                             conf.int = TRUE)  +
    labs(x = "Soil moisture", y = "Plant H'") +
    annotate("text", x = 6, y = 2, label = "Kruskal Wallis, p-value = 0.11", fontface = "bold"))

ggsave("soil_plant_plot.jpg", soil_plant_plot, units = "cm", width = 20, height = 15) 


soil_invert <- lm(invert_shannons ~ bugs$soil_moist)
autoplot(soil_invert)
shapiro.test(resid(soil_invert)) #non-normal
bptest(invert_shannons ~ bugs$soil_moist) #non-equal
kruskal.test(invert_shannons ~ bugs$soil_moist) #NS

(soil_invert_plot <- ggscatter(bugs, x = "soil_moist", y = "invert_shan",
                              legend = "none",
                              col = "skyblue1",
                              add = "reg.line",
                              add.params = list(color = "red"),
                              conf.int = TRUE)  +
    labs(x = "Soil moisture", y = "Invertebrate H'") +
    annotate("text", x = 6, y = 2.5, label = "Kruskal Wallis, p-value = 0.15", fontface = "bold"))

ggsave("soil_invert_plot.jpg", soil_invert_plot, units = "cm", width = 20, height = 15) 


#species richness plots ----
plant_s <- lm(plant_richness ~ quad, data = bugs)
autoplot(plant_s)
shapiro.test(resid(plant_s)) #normal
bptest(plant_s) #equal
anova(plant_s) #sig, p = 0.001405

(plant_s <- ggboxplot(bugs, x = "quad", y = "plant_richness",
                              fill = "quad",
                              ylab = "Plant species richness",
                              legend = "none",
                              xlab = "Position along the transect",
                              bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 8, label = "ANOVA, p-value < 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 8, label = "a)", fontface = "bold", size = 6))

ggsave("Plant_S.png", plant_s, 
       units = "cm", width = 20, height = 15) 

invert_s <- lm(invert_richness ~ quad, data = bugs)
autoplot(invert_s)
shapiro.test(resid(invert_s)) #non-normal
bptest(invert_s) #equal
kruskal.test(invert_richness ~ quad, data = bugs) #NS

(invert_s <- ggboxplot(bugs, x = "quad", y = "invert_richness",
                      fill = "quad",
                      ylab = "Invertebrate order richness",
                      legend = "none",
                      xlab = "Position along the transect",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 12, label = "Kruskal Wallis, p-value > 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 12, label = "a)", fontface = "bold", size = 6))

ggsave("Invert_S.png", invert_s, 
       units = "cm", width = 20, height = 15) 




#veg height ----

height_quad <- lm(avg_height ~ quad, data = bugs)
autoplot(height_quad)
shapiro.test(resid(height_quad)) #non-normal
bptest(avg_height ~ quad, data = bugs) #equal
kruskal.test(avg_height ~ quad, data = bugs) #sig; 0.0003746

(height_plot <- ggboxplot(bugs, x = "quad", y = "avg_height",
                          fill = "quad",
                          ylab = "Average plant height (cm)",
                          legend = "none",
                          xlab = "Position along the transect",
                          bxp.errorbar = TRUE, bxp.errorbar.width = 0.1) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 3, y = 85, label = "Kruskal Wallis, p-value < 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 85, label = "a)", fontface = "bold", size = 6))

ggsave("height.png", height_plot, 
       units = "cm", width = 20, height = 15) 


height <- lm(invert_shannons ~ avg_height, data = bugs)
autoplot(height)
shapiro.test(resid(height)) #non-normal
bptest(plant_shannons ~ avg_height, data = bugs) #equal
kruskal.test(invert_shannons ~ avg_height, data = bugs) #NS, 0.4078

(height_invert_plot <- ggscatter(bugs, x = "avg_height", y = "invert_shan",
                      col = "forestgreen",
                      ylab = "Invert H'",
                      legend = "none",
                      xlab = "Average plant height (cm)",
                      add = "reg.line",
                      add.params = list(color = "orange"),
                      conf.int = TRUE) +
    scale_fill_manual(values = Colours) +
    annotate("text", x = 40, y = 3, label = "Kruskal Wallis, p-value > 0.05", fontface = "bold") +
    annotate("text", x = 0.8, y = 3, label = "b)", fontface = "bold", size = 6))

ggsave("height_inverts.png", height_invert_plot, 
       units = "cm", width = 20, height = 15) 



#grids ----

richness_grid <- grid.arrange(arrangeGrob(plant_s, invert_s, 
                                        ncol = 2))
ggsave("richness_grid.jpg", richness_grid, 
       units = "cm", width = 40, height = 15) 

shan_grid <- grid.arrange(arrangeGrob(plant_shan_plot, invert_shan_plot,
                                      ncol = 2))
ggsave("shan_grid.jpg", shan_grid, 
       units = "cm", width = 40, height = 15) 

j_grid <- grid.arrange(arrangeGrob(plant_J_plot, invert_J_plot,
                                      ncol = 2))
ggsave("j_grid.jpg", j_grid, 
       units = "cm", width = 40, height = 15) 

d_grid <- grid.arrange(arrangeGrob(plant_D_plot, invert_D_plot,
                                      ncol = 2))
ggsave("d_grid.jpg", d_grid, 
       units = "cm", width = 40, height = 15) 

nmds_grid <- grid.arrange(arrangeGrob(NMDS_plot, NMDS_plot_4,
                                      ncol = 2))
ggsave("nmds_grid.jpg", nmds_grid, 
       units = "cm", width = 40, height = 15) 

light_plant <- grid.arrange(arrangeGrob(par_plot, par_plant_plot,
                                  ncol = 2))
ggsave("light_plant.jpg", light_plant, 
       units = "cm", width = 40, height = 15) 

abiotics <- grid.arrange(arrangeGrob(par_plot, par_plant_plot, 
                                     ncol = 2))
ggsave("abiotics.jpg", abiotics, 
       units = "cm", width = 40, height = 15) 

height_grid <- grid.arrange(arrangeGrob(height_plot, height_invert_plot, 
                                       ncol = 2))
ggsave("height_grid.jpg", height_grid, 
       units = "cm", width = 40, height = 15)


plant_grid <- grid.arrange(arrangeGrob(plant_s, plant_shan_plot, 
                                          ncol = 2))
ggsave("plant_grid.jpg", plant_grid, 
       units = "cm", width = 40, height = 15) 

invert_grid <- grid.arrange(arrangeGrob(invert_s, invert_shan_plot,
                                      ncol = 2))
ggsave("invert_grid.jpg", invert_grid, 
       units = "cm", width = 40, height = 15) 

