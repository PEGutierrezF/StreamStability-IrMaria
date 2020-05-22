



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

Regression <- ggplot(regression, aes(Time, 
                       value, 
                       shape= variable , 
                       col = stream)) + 
 geom_point(size = 5) +
 geom_smooth(method = 'lm', se = F,  aes(color=stream)) + #, alpha = .15, aes(fill = stream)) +


xlab('Sampling period') + ylab("Resilience") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) +# axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) +
  
  theme(legend.title = element_text(color = "black", size = 14),  # Legend
    legend.text = element_text(color = "black", size = 12)) +
  
  geom_hline(yintercept = 0, color="darkred") +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  labs(title="", subtitle="LN(Vdis/Vcont)",fill="", caption="") +
  
  ylim(-3.5,3.5) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +

 facet_grid(~ variable)
  # facet_wrap(. ~ variable)
Regression
Regression + ggsave("Regression.jpeg", width=10, height=6,dpi=600)

