



# ---------------------------------------------
# Ecosystem stability - Decapoda Abundance
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




decapod <- read.xlsx("data/data_stability_metrics.xlsx", sheet='decapoda_abundance', detectDates = TRUE)
head(decapod)
decapod <- na.omit(decapod)
head(decapod)

################################################################
# Linear model Decapoda abundance Prieta A ---------------------
################################################################

QPA.shrimp.mod  <- lm(QPA_shrimp ~ date, data=decapod)
summary(QPA.shrimp.mod)

decapod$QPAresid <- QPA.shrimp.mod$resid
decapod

1/apply(decapod, 2, sd)


# Autocorrelation
library(lmtest)
dwt(QPA.shrimp.mod)
dwtest(QPA.shrimp.mod)

qpa.s <- ggplot(decapod, aes(date, y=QPA_shrimp))+
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
qpa.s



################################################################
# Linear model Decapoda abundance Prieta B ---------------------
################################################################


QPB.shrimp.mod <- lm(QPB_shrimp ~ date, data=decapod)
summary(QPB.shrimp.mod)

decapod$QPBresid<- QPB.shrimp.mod$resid
decapod

1/apply(decapod, 2, sd)

qpb.s <- ggplot(decapod, aes(date, y=QPB_shrimp))+
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
qpb.s


plot.shrimp <- qpa.s / qpb.s
plot.shrimp + ggsave("Regression decapod abundance.jpeg",  path = "figures", width=6, height=10,dpi=600)

