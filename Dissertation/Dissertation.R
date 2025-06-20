##%#########################################################################%##
#                                                                             #
#                    Dissertation script - Zoja Manček Páli                   #
#                              Started: 30.9.2023                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("~/Desktop/Zoja Complete Repository/Dissertation") #sets a new one
getwd() #check that it's worked

#Libraries
library(ape)
library(caret)
library(cowplot)
library(ecodist)
library(ggfortify)
library(ggrepel)
library(goeveg)
library(gridExtra)
library(MASS)
library(nortest)
library(rstatix)
library(sjPlot)
library(tidyverse)
library(vegan)

#Data ----
trees <- read.csv("traits_analysis2.csv")
trees <- trees %>% 
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  mutate(code_two = recode(code_two,
                           "CB" = "C. bullatus",
                           "R. ponticum" = "Invasive")) %>% #recode alien species names
  arrange(code_two = factor(type, levels = c('Native', 'Naturalised', 
                                             'Invasive', 'C. bullatus'))) %>%  #rearranges the categories in this order
  mutate(latin_name = recode(latin_name,
                             "Malus pumila" = "Malus x domestica")) %>% #changing the latin name to match Preston et al. (2002)
  mutate(type = recode(type, RR = "Native", PA = "Naturalised")) %>%  #slight changes to the classifications
  rename("LMA" = "lma") %>% 
  rename("LDMC" = "ldcm") %>% 
  rename("LCC" = "chl") %>% 
  rename("Rleaf" = "Dark_resp")

trees_pos <- trees %>% 
  filter(A >= 0) #removing negative A values (dead leaves)

rows_to_remove <- trees$A < 0
removed_rows <- trees[rows_to_remove, ]

(trees_counts <- trees %>%
    group_by(type) %>%
    summarise(unique_species = n_distinct(code)))
#1 invasive, 12 naturalised, 20 native, 1 alien


nns <- trees %>% 
  filter(type %in% c('Native', 'Naturalised', "Invasive")) %>% #excluding the alien group for initial analysis
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  arrange(type = factor(type, levels = c('Native', 'Naturalised', 'Invasive'))) #rearranges the categories in this order

nns_pos <- nns %>% 
  filter(A >= 0) 

(nns_counts <- nns %>%
    group_by(type) %>%
    summarise(unique_species = n_distinct(code)))
#1 invasive, 12 naturalised, 20 native

traits.palette <- c("Invasive" = "#CD6090", 
                    "Native" = "#698B69",
                    "Naturalised" = "#EEC900", 
                    "C. bullatus" = "#5EA8D9")    #defining 3 colours


cn_trees <- read.csv("cn_analysis.csv")
cn_trees <- cn_trees %>% 
  dplyr::mutate(canopy_pos = recode(canopy_pos, 
                                    "M" = "Lower",
                                    "U" = "Upper"), #recode canopy positions
                code_two = recode(code_two,
                                  "CB" = "C. bullatus", 
                                  "R. ponticum" = "Invasive")) %>% #recode alien species names
  arrange(code_two = factor(type, levels = c('Native', 'Naturalised', 
                                             'Invasive', 'C. bullatus'))) %>% #rearranges the categories in this order
  mutate(c_n = C/N) %>% 
  rename("CN" = "c_n") %>% 
  mutate(type = recode(type, RR = "Native", PA = "Naturalised"))  #slight changes to the classifications

cn_nns <- cn_trees %>% 
  filter(type %in% c('Native', 'Naturalised', "Invasive")) %>% #excluding the alien group for initial analysis
  mutate(canopy_pos = recode(canopy_pos, 
                             "M" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  arrange(type = factor(type, levels = c('Native', 
                                         'Naturalised', 
                                         'Invasive'))) %>%   #rearranges the categories in this order
  mutate(c_n = C/N)

(cn_counts <- cn_trees %>%
    group_by(type) %>%
    summarise(unique_species = n_distinct(code)))

#Mean trait values for each group
(means_trees <- trees_pos %>% 
    group_by(type) %>% 
    summarise(mean_lma = mean(LMA),
              mean_ldmc = mean(LDMC), 
              mean_chl = mean(LCC),
              mean_A = mean(A),
              mean_E = mean(E),
              mean_g = mean(g),
              mean_dr = mean(Rleaf)))

(means_cn_trees <- cn_trees %>% 
    group_by(type) %>% 
    summarise(mean_cn = mean(C/N)))

#Step 1 - NMDS ----
merged_trees_nns <- merge(nns_pos, cn_nns[, c("code", "canopy_pos", "CN")], 
                          by = c("code", "canopy_pos"), 
                          all.x = TRUE)
merged_trees_nns <- unique(merged_trees_nns, by = c("code", "canopy_pos"))

numeric_cols_nns <- colnames(merged_trees_nns)[sapply(merged_trees_nns, 
                                                      is.numeric)] 
numeric_data_nns <- merged_trees_nns[, numeric_cols_nns]
numeric_data_nns <- numeric_data_nns %>% dplyr::select(LCC, LMA, LDMC, A, 
                                                E, g, CN, Rleaf) %>% na.omit()

#finding the lowest stress for up to 6 dimensions:
dimcheckMDS(numeric_data_nns,
            distance = "euclidean",
            k = 6) 
#generally accepted that stress < 0.2 is a fair fit for ordination, so will use 2 dimensions (stress = 0.073) 

#2-dimensional NMDS (with alien) ----
merged_trees <- merge(trees_pos, cn_trees[, c("code", "canopy_pos", "CN")], 
                      by = c("code", "canopy_pos"))

numeric_cols <- colnames(merged_trees)[sapply(merged_trees, is.numeric)] 
numeric_data <- merged_trees[, numeric_cols]
numeric_data <- numeric_data %>% select(LCC, LMA, LDMC, A, E, g, CN, Rleaf)

#finding the lowest stress for up to 6 dimensions:
dimcheckMDS(numeric_data,
            distance = "euclidean",
            k = 6) 

nmds <- metaMDS(numeric_data, distance = "euclidean", k = 2) 
nmds_coords <- as.data.frame(scores(nmds, "sites"))
nmds_coords$type <- merged_trees$type

stressplot(nmds) #non-metric R^2 = 0.995, linear R^2 = 0.979
(stress <- nmds$stress) #0.07331424

hull.data <- data.frame()
for (i in unique(nmds_coords$type)) {
  temp <- nmds_coords[nmds_coords$type == 
                        i, ][chull(nmds_coords[nmds_coords$type == 
                                                 i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(nmds_plot <- ggplot() +
    geom_polygon(data = hull.data[hull.data$type != "Invasive", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data[hull.data$type == "Invasive", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.7) + #add polygons for invasive type
    geom_polygon(data = hull.data[hull.data$type == "Alien", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords, aes(x = NMDS1, y = NMDS2, color = type), 
               size = 3) + # Add points
    scale_color_manual(values = c("Native" = "#698B69", 
                                  "Invasive" = "#CD6090", 
                                  "Naturalised" = "#EEC900", 
                                  "Alien" = "#5EA8D9")) +
    scale_fill_manual(values = c("Native" = "#698B69", 
                                 "Invasive" = "#CD6090", 
                                 "Naturalised" = "#EEC900", 
                                 "Alien" = "#5EA8D9")) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), 
          legend.direction = "vertical", 
          legend.title = element_blank()) +
    annotate("text", label = "a)", x = -66, y = 24, fontface = "bold"))

ggsave("combined_nmds_2d.jpg", nmds_plot, path = "Plots", units = "cm", 
       width = 20, height = 20) 

diss_matrix <- vegdist(numeric_data, method = "euclidean")
anosim(diss_matrix, merged_trees$type, permutations = 9999) 
#significant (1e-04), the three types are significantly different in their traits;
#however, the R value is close to 0 (0.1405), indicating a slight but significant difference between the groups

#drivers of the variation:
en_total = envfit(nmds, numeric_data, permutations = 999, na.rm = TRUE)

plot(nmds)
plot(en_total)
#The arrow(s) point to the direction of most rapid change in the variable (direction of the gradient)
#The length of the arrow(s) is proportional to the correlation between ordination and environmental variable (strength of the gradient)

plot(nmds)
plot(en_total, p.max = 0.05) #can also only plot the significant ones this way


hull.data1 <- data.frame()
for (i in unique(nmds_coords$type)) {
  temp <- nmds_coords[nmds_coords$type == 
                        i, ][chull(nmds_coords[nmds_coords$type == 
                                                 i, c("NMDS1", "NMDS2")]), ]
  hull.data1 <- rbind(hull.data, temp)
}

en_coord_cont_total = as.data.frame(scores(en_total, "vectors")) * 
  ordiArrowMul(en_total)

(drivers_nmds <- ggplot(data = nmds_coords, aes(x = NMDS1, y = NMDS2)) + 
    geom_polygon(data = hull.data1[hull.data1$type != "Invasive", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data1[hull.data1$type == "Invasive", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.7) + #add polygons for invasive type
    geom_polygon(data = hull.data1[hull.data1$type == "Alien", ], 
                 aes(x = NMDS1, y = NMDS2, group = type, fill = type), 
                 alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords, aes(colour = type), size = 3) + 
    scale_color_manual(values = c("Native" = "#698B69", 
                                  "Invasive" = "#CD6090", 
                                  "Naturalised" = "#EEC900", 
                                  "Alien" = "#5EA8D9")) +
    scale_fill_manual(values = c("Native" = "#698B69", 
                                 "Invasive" = "#CD6090", 
                                 "Naturalised" = "#EEC900", 
                                 "Alien" = "#5EA8D9")) +
    theme_classic() +
    theme(axis.title = element_text(size = 10, face = "bold", 
                                    colour = "grey30"), 
          panel.background = element_blank(), 
          panel.border = element_rect(fill = NA, colour = "grey30"), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          legend.key = element_blank(), 
          legend.text = element_text(size = 9, colour = "grey30"),
          legend.position = c(0.13, 0.13), 
          legend.direction = "vertical", 
          legend.title = element_blank()) +
    geom_segment(data = en_coord_cont_total, aes(x = 0, y = 0, 
                                                 xend = NMDS1, 
                                                 yend = NMDS2), 
                 linewidth = 1, alpha = 0.5, colour = "grey30") +
    geom_text(data = en_coord_cont_total, aes(x = NMDS1, y = NMDS2), 
              colour = "black", fontface = "bold", 
              label = row.names(en_coord_cont_total)) +
    labs(colour = "type") + #can't get these arrows to work - idk why
    annotate("text", label = "b)", x = -66, y = 24, fontface = "bold"))
ggsave("drivers_nmds_nns_2d.jpg", drivers_nmds, path = "Plots", units = "cm", 
       width = 20, height = 20)

#importance of each leaf trait:
en_total
#lma and g have high r2 values (close to 1), indicating strong correlations with both NMDS1 and NMDS2.
#This suggests that these traits play a significant role in differentiating between invasive and native species in terms of their leaf characteristics.
#Additionally, the significant p-values (***) indicate that these correlations are statistically robust.
#chl and c_n also show significant correlations with the NMDS axes, but to a lesser extent compared to lma and g (still ***)
#A has a relatively low r2 value and (but is still significant ***), suggesting weaker correlations with the NMDS axes.
#E, LDMC, and DR have very low r^2 values and are insignificant





#Step 2: Species groups comparisons ----
#LMA ----
lma_mod2 <- lm(LMA ~ type, data = trees_pos)
autoplot(lma_mod2)
shapiro.test(resid(lma_mod2)) #residuals not distributed normally
bartlett.test(LMA ~ type, data = trees_pos) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
lma_boxcox2 <- boxcox(LMA ~ 1, data = trees_pos) #the λ is the highest point on the curve
(lma_lambda2 <- lma_boxcox2$x[which.max(lma_boxcox2$y)]) #λ = -0.1414141
trees_pos <- trees_pos %>% mutate(transformed_lma2 = (LMA ^ 
                                                        (lma_lambda2 - 1)) / 
                                    lma_lambda2) #Box-Cox transformation applied in a new column

lma_mod_trans2 <- lm(transformed_lma2 ~ type, data = trees_pos)
autoplot(lma_mod_trans2)
shapiro.test(resid(lma_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_lma2 ~ type, data = trees_pos) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(lma_kruskal2 <- trees_pos %>% kruskal_test(LMA ~ type)) #n = 204; df = 3; p = 0.000019 
(lma_effect2 <- trees_pos %>% kruskal_effsize(LMA ~ type)) #effect size = 0.108 ; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.108 

#Dunn post-hoc test
(dunn_lma2 <- trees_pos %>% dunn_test(LMA ~ type, 
                                      p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 
#alien/native and alien/naturalised differ significantly
#invasive/native and invasive/naturalised differ significantly

(lma_boxplot2 <- ggplot(trees, 
                        aes(x = factor(code_two, levels = 
                                         c('Native', 'Naturalised', 'Invasive', 
                                           'C. bullatus')), #reorders the types 
                            y = LMA, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) +
    labs(x = "\n Invasion status", 
         y = expression("LMA (g cm"^-2*")")) + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")), #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE) +
    annotate("text", label = "e)", x = 0.7, y = 150, fontface = "bold"))

ggsave("lma_boxplot2.jpg", lma_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 


#Chlorophyll ----
chl_mod2 <- lm(LCC ~ type, data = trees_pos)
autoplot(chl_mod2)
shapiro.test(resid(chl_mod2)) #residuals not distributed normally
bartlett.test(LCC ~ type, data = trees_pos) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
chl_boxcox2 <- boxcox(LCC ~ 1, data = trees_pos) #the λ is the highest point on the curve
(chl_lambda2 <- chl_boxcox2$x[which.max(chl_boxcox2$y)]) #λ = -0.1010101
trees_pos <- trees_pos %>% 
  mutate(transformed_chl2 = (LCC ^ (chl_lambda2 - 1)) / chl_lambda2) #Box-Cox transformation applied in a new column

chl_mod_trans2 <- lm(transformed_chl2 ~ type, data = trees_pos)
autoplot(chl_mod_trans2)
shapiro.test(resid(chl_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_chl2 ~ type, data = trees_pos) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(chl_kruskal2 <- trees_pos %>% kruskal_test(LCC ~ type)) #n = 202; df = 3; p = 0.0000146 
(chl_effect2 <- trees_pos %>% kruskal_effsize(LCC ~ type)) #effect size = 0.112; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.112

#Dunn post-hoc test
(dunn_chl2 <- trees_pos %>% dunn_test(LCC ~ type, 
                                      p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 
#alien/native and alien/naturalised differ significantly
#invasive/native and invasive/naturalised differ significantly

(chl_boxplot2 <- ggplot(trees, 
                        aes(x = factor(code_two, levels = 
                                         c('Native', 'Naturalised', 
                                           'Invasive', 'C. bullatus')), #reorders the types 
                            y = LCC, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9"),
                      breaks = c("Native", "Naturalised", 
                                 "Invasive", 
                                 "C. bullatus"),
                      labels = c("Native", "Naturalised", 
                                 "Invasive", 
                                 expression(italic("C. bullatus")))) + #colours each boxplot this particular colour
    labs(x = "", 
         y = expression("LCC (SPAD)"),
         fill = "Invasion status") + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm"),
          legend.position = "none"))

ggsave("chl_boxplot2.jpg", chl_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 


#LDMC ----
ldcm_mod2 <- lm(LDMC ~ type, data = trees_pos)
autoplot(ldcm_mod2)
shapiro.test(resid(ldcm_mod2)) #residuals distributed normally
bartlett.test(LDMC ~ type, data = trees_pos) #homoscedascity
anova(ldcm_mod2) #significant, p = 0.002177; df = 3 

(ldmc_kruskal2 <- trees_pos %>% kruskal_test(LDMC ~ type)) #n = 202; df = 3; p = 0.0000146 
(ldmc_effect2 <- trees_pos %>% kruskal_effsize(LDMC ~ type)) #effect size = 0.112; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.112

#Dunn post-hoc test
(dunn_ldmc2 <- trees_pos %>% dunn_test(LDMC ~ type, 
                                       p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 

(ldcm_boxplot2 <- ggplot(trees, 
                         aes(x = factor(code_two, levels = 
                                          c('Native', 'Naturalised', 
                                            'Invasive', 'C. bullatus')), #reorders the types 
                             y = LDMC, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(paste("LDMC (g" ~ "g"^-1~")"))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("ldcm_boxplot2.jpg", ldcm_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 



#Assimilation rate ----
a_mod2 <- lm(A ~ code_two, data = trees_pos)
autoplot(a_mod2)
shapiro.test(resid(a_mod2)) #residuals distributed normally
bartlett.test(A ~ type, data = trees_pos) #homoscedascity
anova(a_mod2) #p = 0.003221; significant; df = 3

(a_kruskal2 <- trees_pos %>% kruskal_test(A ~ type)) #n = 202; df = 3; p = 0.00134 
(a_effect2 <- trees_pos %>% kruskal_effsize(A ~ type)) #effect size = 0.112; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.112

#Dunn post-hoc test
(dunn_a2 <- trees_pos %>% dunn_test(A ~ type, 
                                    p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 

(a_boxplot2 <- ggplot(trees_pos, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 
                                         'Invasive', 'C. bullatus')), #reorders the types 
                          y = A, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(paste("A (", mu, "mol CO"[2]~"m"^-2*~"s"^-1, ")"))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("a_boxplot2.jpg", a_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 



#Transpiration rate ----
e_mod2 <- lm(E ~ code_two, data = trees_pos)
autoplot(e_mod2)
shapiro.test(resid(e_mod2)) #residuals distributed normally
bartlett.test(E ~ code_two, data = trees_pos) #homoscedascity

(e_kruskal2 <- trees_pos %>% kruskal_test(E ~ type)) #n = 202; df = 3; p = 0.612 
(e_effect2 <- trees_pos %>% kruskal_effsize(E ~ type)) #effect size = -0.006 small magnitude
#report as: small effect size is detected, eta2[H] = -0.006

#Dunn post-hoc test
(dunn_e2 <- trees_pos %>% dunn_test(E ~ type, 
                                    p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 


(e_boxplot2 <- ggplot(trees_pos, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 
                                         'Invasive', 'C. bullatus')), #reorders the types 
                          y = E, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(paste("E (", mu, 
                              "mol H"[2]*"O m"^-2*~"s"^-1, ")"))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("e_boxplot2.jpg", e_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 





#Dark respiration ----
dr_mod2 <- lm(Rleaf ~ code_two, data = trees_pos)
autoplot(dr_mod2)
shapiro.test(resid(dr_mod2)) #residuals not distributed normally
bartlett.test(Rleaf ~ type, data = trees_pos) #heteroscedascity

#Cannot do a Box-Cox transformation because values are negative:
(dr_kruskal2 <- trees_pos %>% kruskal_test(Rleaf ~ type)) #n = 202; df = 3; p = 0.437  
(dr_effect2 <- trees_pos %>% kruskal_effsize(Rleaf ~ type)) #effect size = -0.00142; small magnitude
#report as: small effect size is detected, eta2[H] = -0.00142

y <- expression(paste("R"[leaf], " (", mu, "mol CO"[2], "m"^-2*"s"^-1, ")"))
(dr_boxplot2 <- ggplot(trees_pos, 
                       aes(x = factor(code_two, levels = 
                                        c('Native', 'Naturalised', 
                                          'Invasive', 'C. bullatus')), #reorders the types 
                           y = Rleaf, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = y) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("dr_boxplot2.jpg", dr_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 

#GH2O ----
g_mod2 <- lm(g ~ code_two, data = trees_pos)
autoplot(g_mod2)
shapiro.test(resid(g_mod2)) #residuals not distributed normally
bartlett.test(g ~ code_two, data = trees_pos) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
g_boxcox2 <- boxcox(g ~ 1, data = trees_pos) #the λ is the highest point on the curve
(g_lambda2 <- g_boxcox2$x[which.max(g_boxcox2$y)]) #λ = -0.5454545
trees_pos <- trees_pos %>% 
  mutate(transformed_g2 = (g ^ (g_lambda2 - 1)) / g_lambda2) #Box-Cox transformation applied in a new column

g_mod_trans2 <- lm(transformed_g2 ~ type, data = trees_pos)
autoplot(g_mod_trans2)
shapiro.test(resid(g_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_g2 ~ type, data = trees_pos) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(g_kruskal2 <- trees_pos %>% kruskal_test(g ~ code_two)) #n = 202; df = 3; p = 0.000366
(g_effect2 <- trees_pos %>% kruskal_effsize(g ~ code_two)) #effect size = 0.0777; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.0777

#Dunn post-hoc test
(dunn_g2 <- trees_pos %>% dunn_test(g ~ code_two, 
                                    p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type")) 
#alien/native and alien/naturalised differ significantly from one another
#invasive/naturalised differ significantly from one another

(g_boxplot2 <- ggplot(trees_pos, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 
                                         'Invasive', 'C. bullatus')), #reorders the types 
                          y = g, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(paste("g (", mu, 
                              "mol H"[2]*"O m"^-2*~"s"^-1, ")"))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("g_boxplot2.jpg", g_boxplot2, path = "Plots", units = "cm", 
       width = 25, height = 13) 


#C:N ----
cn_mod2 <- lm(CN ~ type, data = cn_trees)
autoplot(cn_mod2)
shapiro.test(resid(cn_mod2)) #residuals not distributed normally
bartlett.test(CN ~ type, data = cn_trees) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
cn_boxcox2 <- boxcox(CN ~ 1, data = cn_trees) #the λ is the highest point on the curve
(cn_lambda2 <- cn_boxcox2$x[which.max(cn_boxcox2$y)]) #λ = -0.7474747
cn_trees <- cn_trees %>% 
  mutate(transformed_cn2 = (CN ^ (cn_lambda2 - 1)) / cn_lambda2) #Box-Cox transformation applied in a new column

cn_mod_trans2 <- lm(transformed_cn2 ~ type, data = cn_trees)
autoplot(cn_mod_trans2)
shapiro.test(resid(cn_mod_trans2)) #residuals distributed normally
bartlett.test(transformed_cn2 ~ type, data = cn_trees) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(cn_kruskal2 <- cn_trees %>% kruskal_test(CN ~ type)) #n = 99; df = 3; p = 0.00182 
(cn_effect2 <- cn_trees %>% kruskal_effsize(CN ~ type)) #effect size = 0.126 ; moderate magnitude
#report as: moderate effect size is detected, eta2[H] = 0.126 

#Dunn post-hoc test
(dunn_cn2 <- cn_trees %>% dunn_test(CN ~ type, 
                                    p.adjust.method = "bonferroni") %>% 
    add_xy_position(x = "type"))  
#invasive/native differ significantly
#invasive/alien differ significantly

(cn_boxplot2 <- ggplot(cn_trees, 
                       aes(x = factor(code_two, 
                                      levels = c('Native', 'Naturalised', 
                                                 'Invasive', 'C. bullatus')), #reorders the types 
                           y = CN, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", 
                                 "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", 
                                 "C. bullatus" = "#5EA8D9")) + 
    labs(x = "", 
         y = expression("C/N ratio")) + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", 
                                              "plain", "italic")),  #italicise selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("cn_boxplot2.jpg", cn_boxplot2, path = "Plots", units = "cm",
       width = 25, height = 13) 

#Box plots grid ----
#These remove the x-axis tick marks, titles, and labels
a_boxplot2 <- a_boxplot2 + theme(axis.title.x = element_blank(), 
                                 axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())
dr_boxplot2 <- dr_boxplot2 + theme(axis.title.x = element_blank(), 
                                   axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank())
e_boxplot2 <- e_boxplot2 + theme(axis.title.x = element_blank(), 
                                 axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())
g_boxplot2 <- g_boxplot2 + theme(axis.title.x = element_blank(), 
                                 axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())
lma_boxplot2 <- lma_boxplot2 + theme(axis.title.x = element_blank(), 
                                     axis.text.x = element_blank(), 
                                     axis.ticks.x = element_blank())
ldcm_boxplot2 <- ldcm_boxplot2 + theme(axis.title.x = element_blank(), 
                                       axis.text.x = element_blank(), 
                                       axis.ticks.x = element_blank())

(grid1 <- grid.arrange(a_boxplot2, e_boxplot2, g_boxplot2, dr_boxplot2,
                       lma_boxplot2, ldcm_boxplot2, chl_boxplot2, cn_boxplot2, 
                       ncol = 2, widths = c(0.8, 0.8), heights = c(1, 1, 1, 1)))

ggsave("boxplot_grid.jpg", grid1, path = "Plots", units = "cm",
       width = 25, height = 30)

legend <- cowplot::get_legend(chl_boxplot2) #extract legend from previous plot 
#only works if you set the legend.position = "bottom" before running the below code

#run these two also to get no x-axis tick marks:
cn_boxplot2 <- cn_boxplot2 + theme(axis.title.x = element_blank(), 
                                   axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank())
chl_boxplot2 <- chl_boxplot2 + theme(axis.title.x = element_blank(), 
                                     axis.text.x = element_blank(), 
                                     axis.ticks.x = element_blank())
(grid2 <- grid.arrange(e_boxplot2, a_boxplot2, g_boxplot2, dr_boxplot2,
                       ldcm_boxplot2, lma_boxplot2, cn_boxplot2, chl_boxplot2, 
                       ncol = 2, widths = c(1, 1), heights = c(1, 1, 1, 1)))

(grid_final <- cowplot::plot_grid(
  grid2, legend, 
  nrow = 2, rel_heights = c(1, 0.05)))  #Adjust the relative heights as needed

ggsave("boxplot_final_grid.jpg", grid_final, path = "Plots", units = "cm",
       width = 25, height = 32)

#grid for presentation
a_boxplot2 <- a_boxplot2 + theme(axis.title.x = element_blank(), 
                                 axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())
g_boxplot2 <- g_boxplot2 + theme(axis.title.x = element_blank(), 
                                 axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())
lma_boxplot2 <- lma_boxplot2 + theme(axis.title.x = element_blank(), 
                                     axis.text.x = element_blank(), 
                                     axis.ticks.x = element_blank())
ldcm_boxplot2 <- ldcm_boxplot2 + theme(axis.title.x = element_blank(), 
                                       axis.text.x = element_blank(), 
                                       axis.ticks.x = element_blank())

(grid3 <- grid.arrange(a_boxplot2, g_boxplot2, lma_boxplot2,
                       ldcm_boxplot2, cn_boxplot2, chl_boxplot2, 
                       ncol = 2))
ggsave("boxplot_pres_grid.jpg", grid3, path = "Plots", units = "cm",
       width = 25, height = 30)


#Step 3: Pairwise differences ----
merged_trees <- merge(trees_pos, cn_trees[, c("code", "canopy_pos", "CN")], 
                      by = c("code", "canopy_pos"))
sig_traits <- c("LMA", "LDMC", "A", "g", "CN", "LCC")
sub_data <- merged_trees %>%
  filter(type %in% c("Native", "Invasive", "Alien")) %>%
  select(type, one_of(sig_traits))

scaled_data <- scale(sub_data[, sig_traits], center = TRUE, scale = TRUE)

native_means_scaled <- colMeans(scaled_data[sub_data$type == "Native", ])
alien_means_scaled <- colMeans(scaled_data[sub_data$type == "Alien", ]) 
invasive_means_scaled <- colMeans(scaled_data[sub_data$type == "Invasive", ])

(native_alien_diff_scaled <- abs(native_means_scaled - alien_means_scaled))
(invasive_alien_diff_scaled <- abs(invasive_means_scaled - alien_means_scaled))

cbind(native_alien_diff_scaled, invasive_alien_diff_scaled)

wilcox.test(native_alien_diff_scaled, mu = 0) #significant (0.03101)
wilcox.test(invasive_alien_diff_scaled, mu = 0) #significant (0.03101)