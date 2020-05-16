



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

LeafLitter<- read.csv("01 Leaflitter.csv")
LeafLitter


# Lm QPA Leaf Litter ------------------------------------------------------

LeafLitter <- LeafLitter %>% select(TimeLeaf,QPALeaflitter,QPBLeaflitter )
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
  
  xlab('') + ylab("Residuals") + # axis x
  
  theme(axis.title.y = element_text(size = 18, angle = 90)) +

  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

  ylim(-3,3) +
  
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
  
  xlab('Sampling period')+ ylab("Residuals") +
  theme(axis.title.x = element_text(size = 18, angle = 00)) + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

ylim(-3,3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p2

leaf <- p1 / p2
leaf + ggsave("Leaf.jpeg", width=6, height=10,dpi=600)



# Long-term ---------------------------------------------------------------

LeafLitter<- read.csv("01 Leaflitter.csv")
LeafLitter

# QPA
p3 <- ggplot(LeafLitter,aes(TimeLeaf ,
                            y=QPALeaf ))+
  
  annotate("text", x = 25.8, y = 9.8, label = "? H. Maria",colour = "black", size = 5) +
  annotate(geom = "point", x = 19, y = 9.7,colour = "red",size=3) +
  annotate("text", x = 11.5, y = 6, label = "H. Irma",colour = "black", size = 5) +
  annotate(geom = "rect",xmin=1,xmax=57,ymin=1.136391643,ymax=1.524018959,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPALeaf-QPAsdLeaf   , ymax=QPALeaf+QPAsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.33, xend = 16, yend = 1.33))+
  geom_segment((aes(x = 17, y = 1.33, xend = 57, yend = 1.33)), color="red", linetype="dashed", size=1) + 

  xlab('')+ ylab("Mean litter input rate ("*g~m^-2~d^-1*")") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,10) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p3

# QPB

p4 <- ggplot(LeafLitter,aes(TimeLeaf ,
                           y=QPBLeaf))+
  annotate("text", x = 25.8, y = 9.8, label = "? H. Maria",colour = "black", size = 5) +
  annotate(geom = "point", x = 19, y = 9.7,colour = "red",size=3) +
  annotate("text", x = 11.5, y = 8.5, label = "H. Irma",colour = "black", size = 5) +
  annotate(geom = "rect",xmin=1,xmax=57,ymin=1.042275203,ymax=1.759151731,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPBLeaf-QPBsdLeaf   , ymax=QPBLeaf+QPBsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.40, xend = 16, yend = 1.40))+
  geom_segment((aes(x = 17, y = 1.40, xend = 57, yend = 1.40)), color="red", linetype="dashed", size=1) + 

  xlab('Sampling period')+ ylab("Mean litter input rate ("*g~m^-2~d^-1*")") +
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
leaf + ggsave("LeafMean1.jpeg", width=6, height=10,dpi=600)


