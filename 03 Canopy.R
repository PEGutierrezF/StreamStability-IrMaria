



#--------------------------------------------
# Canopy cover
# 08 May 2020
#PEGF
#--------------------------------------------
#

canopycover<- read.csv("data/Canopy.csv")
canopycover

################################################################
# Linear model Canopy QPA --------------------------------------
################################################################

canopy <- canopycover %>% select(TimeCanopy, QPACanopyLog, QPBCanopyLog)
canopy <- na.omit(canopy)
canopy

QPACanopy.mod  <- lm(QPACanopyLog~ TimeCanopy, data=canopy)
summary(QPACanopy.mod)

canopy$QPACanopyresid<- QPACanopy.mod$resid
canopy

1/apply(canopy, 2, sd)

cc1 <- ggplot(canopy, aes(TimeCanopy,
                       y=QPACanopyLog))+
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
cc1



################################################################
# Linear model Canopy QPB --------------------------------------
################################################################

QPBCanopy.mod  <- lm(QPBCanopyLog~ TimeCanopy, data=canopy)
summary(QPBCanopy.mod)

canopy$QPBCanopyresid<- QPBCanopy.mod$resid
canopy

1/apply(canopy, 2, sd)

cc2 <- ggplot(canopy, aes(TimeCanopy,
                          y=QPBCanopyLog))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('Sampling period') + ylab("Residuals") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
cc2

canopyreg <- cc1 / cc2
canopyreg + ggsave("regression Canopy.jpeg", path = "figures", width=6, height=10,dpi=600)


###########################################################################
# Long term ---------------------------------------------------------------
###########################################################################

canopycover<- read.csv("data/Canopy.csv")
canopycover
# QPA
cc3 <- ggplot(canopycover, aes(TimeCanopy,
                       y=QPACanopy))+
  
  annotate(geom = "rect",xmin=1, xmax=42,ymin=6.73,ymax=16.63,alpha = 0.4,fill = "grey") + # Rectangle
  
  geom_point(data = canopycover %>% filter(rank(desc(QPACanopy)) <= 1),color = "red",size = 2) + 
  geom_line() +
  geom_errorbar(aes(ymin=QPACanopy-QPAsdCanopy, ymax=QPACanopy+QPAsdCanopy), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 11.68, xend = 6, yend = 11.68))+ # Line, mean= 11.68
  geom_segment((aes(x = 7, y = 11.68, xend = 42, yend = 11.68)), color="red", linetype="dashed", size=1) +
  
  xlab('')+ ylab("") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,100) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

cc3

# QPB

cc4 <- ggplot(canopycover,aes(TimeCanopy,
                              y=QPBCanopy))+
  
  annotate(geom = "rect",xmin=1,xmax=42,ymin=7.82,ymax=14.40,alpha = 0.4,fill = "grey") +
  
  geom_point() + # Red point
  geom_line() +
  geom_errorbar(aes(ymin=QPBCanopy-QPBsdCanopy, ymax=QPBCanopy+QPBsdCanopy), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 11.11, xend = 6, yend = 11.11))+
  geom_segment((aes(x = 7, y = 11.11, xend = 42, yend = 11.11)), color="red", linetype="dashed", size=1) + 
  
  xlab('Sampling period')+ ylab("") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,100) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

cc4


canopyLong <- ggarrange(cc3 +rremove("x.text") , cc4 , align = "v",
                     labels = c("A", "B"),font.label = list(size = 13,face= "plain",color = "black"),
                     ncol = 1, nrow = 2)


canopyLong. <-annotate_figure(canopyLong,
                           left = text_grob("Canopy openness (%)", rot = 90,
                                            color = "Black", face = "bold", size = 18))

canopyLong. + ggsave("Long-term CanopyCover.jpeg", path = "figures", width=8, height=10,dpi=600)



