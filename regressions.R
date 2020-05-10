



#--------------------------------------------
# All variables
# 09 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

regression<- read.csv("regressions.csv")
regression


ggplot(regression, aes(Time, 
                       value, 
                       shape= variable , 
                       col = stream)) + 
 geom_point(size = 5) +
 geom_smooth(method = 'lm', se = T,  aes(color=stream)) + #, alpha = .15, aes(fill = stream)) +


xlab('Sampling period') + ylab("Varible (random units)") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) +# axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) +
  
  geom_hline(yintercept = 0, color="darkred") +

theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
 # facet_grid(stream~ variable)
  # facet_wrap(. ~ variable)

