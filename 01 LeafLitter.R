




# ---------------------------------------------
# Ecosystem stability - Leaf litter
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#


rm(list=ls())




LeafLitter<- read.csv("data/Leaflitter.csv")
LeafLitter

LeafLitter$date<-as.POSIXct(LeafLitter$date,"%Y-%m-%d",tz = "UTC")

# Lm QPA Leaf Litter ------------------------------------------------------

LeafLitter <- LeafLitter %>% dplyr::select(date,QPALeaflitter,QPBLeaflitter )
LeafLitter <- na.omit(LeafLitter)
LeafLitter



QPALeaf.mod  <- lm(QPALeaflitter ~ date, data=LeafLitter)
summary(QPALeaf.mod)

LeafLitter$QPAresid<- QPALeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)

p1 <- ggplot(LeafLitter,aes(x= date, y=QPALeaflitter))+
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

QPBLeaf.mod <- lm(QPBLeaflitter ~ date, data=LeafLitter)
summary(QPBLeaf.mod)

LeafLitter$QPBresid<- QPBLeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)


p2 <- ggplot(LeafLitter,aes(date, y=QPBLeaflitter))+
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
leaf + ggsave("Regression Leaf litter.jpeg", path = "figures", width=6, height=10,dpi=600)


