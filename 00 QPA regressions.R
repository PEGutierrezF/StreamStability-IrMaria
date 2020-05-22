



#--------------------------------------------
# QPA regression
# 21 May 2020
#PEGF
#--------------------------------------------
#


library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

QPAregression<- read.csv("00 QPAregressions.csv")
QPAregression

QPAregression$variable_f = factor(QPAregression$variable, levels=c('Canopy','Leaf_litter','Shrimp',
                                                                   'Chlorophyll-a','Macroinvertebrates'))

Regression <- ggplot(QPAregression, aes(Time, 
                                     value,
                                     color=variable)) + 
  geom_smooth(se = F, size=5)  + 

  geom_hline(yintercept = 0, color="darkred") +

xlab('Sampling period') + ylab("Change in magnitude") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) +# axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) +
  
  ylim(-3,3) + 
#  annotate("text", x = 5, y = 2.5, label = "Increase",colour = "black", size = 6,angle = 90) +
#  annotate("text", x = 5, y = -2.5, label = "Decrease",colour = "black", size = 6,angle = 90) +
  
 # theme(legend.title=element_blank())+
 # theme(legend.text = element_text(size=20)) +
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +
  
  facet_grid(~ variable_f, scales="free_x")

Regression
  