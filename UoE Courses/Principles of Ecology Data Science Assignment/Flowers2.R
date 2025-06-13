library(dplyr)

flowers %>% group_by(mix) %>% summarise ( stdev = sd(visits))

flower_table <- flowers  %>%  group_by (mix) %>% summarise (mean_vis =  mean (visits) ,
                                            stdev = sd (visits) , 
                                            se_vis = sd (visits) / sqrt (n () ) )
plot(mean_vis ~as.factor(mix), data=flower_table, ylim=c(0,100), 
     xlab="Flower mix", ylab="Pollinators per 10 minutes")
points(visits~as.factor(mix), data=flowers, col=as.factor(mix))
arrows(x0=1:3, 
       y0=flower_table$mean_vis - flower_table$se_vis, 	x1=1:3, 
       y1=flower_table$mean_vis + flower_table$se_vis,
       code=3, length=0.2, angle=90, col=1:3, lwd=2)

install.packages("ggplot2")
library(ggplot2)

#ggplot makes pretty graphs
#geom = layer; point = scatter plot
#aes = aesthetic; aes= (x variable, y variable, formatting)
p <- ggplot(flowers) + geom_point(aes(mix, visits, col=mix), shape=21)

#add layers
p <- p + geom_point(data=flowers, aes(mix, visits, col=mix), 
                    stat="summary", fun="mean", size=3)

#error bars
p <- p + geom_errorbar(data=flower_table,
                       aes(x=mix,
                      ymin=(mean_vis - se_vis),
                      ymax=(mean_vis + se_vis),
                      col=mix), width=0.1)

p <- p +labs(x="Flower mix", y="Pollinator visits")
p <- p + theme_bw()
                           