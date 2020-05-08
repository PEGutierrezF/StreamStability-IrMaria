



#--------------------------------------------
# Linear model variables post huricanes
# 02 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

variables<- read.csv("variables.csv")
variables


# Lm QPA Leaf Litter ------------------------------------------------------

LeafLitter <- variables %>% select(TimeLeaf,QPALeaflitter,QPBLeaflitter )
LeafLitter <- na.omit(LeafLitter)
LeafLitter

QPALeaf.mod  <- lm(QPALeaflitter ~ TimeLeaf, data=LeafLitter)
summary(QPALeaf.mod)

LeafLitter$QPAresid<- QPALeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)

p1 <- ggplot(LeafLitter,aes(TimeLeaf ,
                y=QPALeaflitter))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Mean litter input rate ("*~g~m^-2~d^-1*")") + # axis x
  
  theme(axis.title.y = element_text(size = 18, angle = 90)) +

  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

  ylim(-3,0) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p1

  # Lm QPB Leaf Litter ------------------------------------------------------

QPBLeaf.mod <- lm(QPBLeaflitter ~ TimeLeaf, data=LeafLitter)
summary(QPBLeaf.mod)

LeafLitter$QPBresid<- QPBLeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)


p2 <- ggplot(LeafLitter,aes(TimeLeaf ,
                           y=QPBLeaflitter))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('Sampling period')+ ylab("Mean litter input rate ("*~g~m^-2~d^-1*")") +
  theme(axis.title.x = element_text(size = 18, angle = 00)) + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

ylim(-3,0) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p2

leaf <- p1 / p2
leaf + ggsave("Leaf.jpeg", width=6, height=10,dpi=600)



# Long-term ---------------------------------------------------------------

# QPA
p3 <- ggplot(variables,aes(TimeLeaf ,
                            y=QPALeaf))+
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPALeaf-QPAsdLeaf   , ymax=QPALeaf+QPAsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.33, xend = 16, yend = 1.33))+
  geom_segment((aes(x = 17, y = 1.33, xend = 41, yend = 1.33)), color="red", linetype="dashed", size=1) + 

  xlab('')+ ylab("Mean litter input rate ("*~g~m^-2~d^-1*")") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,10) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p3

# QPB

p4 <- ggplot(variables,aes(TimeLeaf ,
                           y=QPBLeaf))+
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPBLeaf-QPBsdLeaf   , ymax=QPBLeaf+QPBsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.40, xend = 16, yend = 1.40))+
  geom_segment((aes(x = 17, y = 1.40, xend = 41, yend = 1.40)), color="red", linetype="dashed", size=1) + 

  xlab('Sampling period')+ ylab("Mean litter input rate ("*~g~m^-2~d^-1*")") +
  theme(axis.title.x = element_text(size = 18, angle = 00)) + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,10) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p4



leaf <- p3 / p4
leaf + ggsave("LeafMean.jpeg", width=6, height=10,dpi=600)


