



#--------------------------------------------
# Macroinvertebrates
# 27 May 2020
#PEGF
#--------------------------------------------
#




rm(list=ls())




macroinvertebrates <- read.csv("data/all_data.csv")
head(macroinvertebrates)

macroiv <- macroinvertebrates %>% dplyr::select(date_macroiv, QPA_miv, QPB_miv)
macroiv$date_macroiv <- as.POSIXct(macroiv$date_macroiv,"%Y-%m-%d",tz = "UTC")
macroiv <- na.omit(macroiv)


################################################################
# Linear model Macroinvertebrate abundance Prieta A ---------------------
################################################################

QPA.macroinv.mod  <- lm(QPA_miv ~ date_macroiv, data=macroiv)
summary(QPA.macroinv.mod)

macroiv$QPAresid<- QPA.macroinv.mod$resid
head(macroiv)

1/apply(macroiv, 2, sd)


qpa.miv <- ggplot(macroiv, aes(date_macroiv, y=QPA_miv))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Residuals") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
qpa.miv

# LM QPB Macroinvertebrates  ----------------------------------------------


QPB.macroinv.mod  <- lm(QPB_miv ~ date_macroiv, data=macroiv)
summary(QPB.macroinv.mod)

macroiv$QPAresid<- QPB.macroinv.mod$resid
head(macroiv)

1/apply(macroiv, 2, sd)


qpb.miv <- ggplot(macroiv, aes(date_macroiv, y=QPB_miv))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Residuals") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
qpb.miv


plot.miv <- qpa.miv / qpb.miv
plot.miv + ggsave("Regression macroinvertebrate abundance.jpeg",  path = "figures", width=6, height=10,dpi=600)

