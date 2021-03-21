



#--------------------------------------------
# Linear model variables post huricanes
# 02 May 2020
#PEGF
#--------------------------------------------
#

LeafLitter<- read.csv("data/Leaflitter.csv")
LeafLitter


# Lm QPA Leaf Litter ------------------------------------------------------

LeafLitter <- LeafLitter %>% select(events,QPALeaflitter,QPBLeaflitter )
LeafLitter <- na.omit(LeafLitter)
LeafLitter

QPALeaf.mod  <- lm(QPALeaflitter ~ events, data=LeafLitter)
summary(QPALeaf.mod)

LeafLitter$QPAresid<- QPALeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)

p1 <- ggplot(LeafLitter,aes(events ,
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

QPBLeaf.mod <- lm(QPBLeaflitter ~ events, data=LeafLitter)
summary(QPBLeaf.mod)

LeafLitter$QPBresid<- QPBLeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)


p2 <- ggplot(LeafLitter,aes(events ,
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
leaf
leaf + ggsave("regression Leaf.jpeg", path = "figures", width=6, height=10,dpi=600)



###########################################################################
# Long term ---------------------------------------------------------------
###########################################################################

LeafLitter<- read.csv("data/Leaflitter.csv")
LeafLitter

# QPA
p3 <- ggplot(LeafLitter,aes(TimeLeaf ,
                            y=QPALeaf ))+
  
  annotate("text", x = 26.3, y = 9.8, label = "? H. Maria",colour = "black", size = 5) +
  geom_segment(aes(x=19, xend = 19 , y=9, yend = 10), size=1.5,color = "red", arrow = arrow(length = unit(0.4,"cm")))+
  annotate("text", x = 11.5, y = 6, label = "H. Irma",colour = "black", size = 5) +
  annotate(geom = "rect",xmin=1,xmax=78,ymin=1.136391643,ymax=1.524018959,alpha = 0.2,fill = "green") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPALeaf-QPAsdLeaf   , ymax=QPALeaf+QPAsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.33, xend = 16, yend = 1.33))+
  geom_segment((aes(x = 17, y = 1.33, xend = 78, yend = 1.33)), color="red", linetype="dashed", size=1) + 

  xlab('')+ ylab("") +
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
  annotate("text", x = 26.3, y = 9.8, label = "? H. Maria",colour = "black", size = 5) +
  geom_segment(aes(x=19, xend = 19 , y=9, yend = 10), size=1.5,color = "red", arrow = arrow(length = unit(0.4,"cm")))+
  # annotate(geom = "point", x = 19, y = 9.7,colour = "red",size=3) +
  annotate("text", x = 11.5, y = 8.5, label = "H. Irma",colour = "black", size = 5) +
  annotate(geom = "rect",xmin=1,xmax=78,ymin=1.042275203,ymax=1.759151731,alpha = 0.2,fill = "green") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPBLeaf-QPBsdLeaf   , ymax=QPBLeaf+QPBsdLeaf), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1.40, xend = 16, yend = 1.40))+
  geom_segment((aes(x = 17, y = 1.40, xend = 78, yend = 1.40)), color="red", linetype="dashed", size=1) + 

  xlab('Sampling period')+ ylab("") +
  theme(axis.title.x = element_text(size = 18, angle = 00)) + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,10) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p4

Leaf_long_term <- ggarrange(p3 +rremove("x.text") , p4 , align = "v",
                        labels = c("A", "B"),font.label = list(size = 13,face= "plain",color = "black"),
                        ncol = 1, nrow = 2)


Leaf_long_term. <-annotate_figure(Leaf_long_term,
                  left = text_grob("Mean litter input rate ("*g~m^-2~d^-1*")", rot = 90,
                  color = "Black", face = "bold", size = 18))

Leaf_long_term. + ggsave("Long-term Leaf.jpeg", path = "figures", width=8, height=10,dpi=600)


