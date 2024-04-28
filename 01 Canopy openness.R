



# ---------------------------------------------
# Ecosystem stability - Canopy cover
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls())



canopycover<- read.xlsx("data/data_stability_metrics.xlsx", sheet='canopy', detectDates = TRUE)
head(canopycover)


canopy <- canopycover %>% dplyr::select(date_co, QPA_canopy, QPB_canopy)
canopy$date_co<-as.POSIXct(canopy$date_co,"%Y-%m-%d",tz = "UTC")
canopy <- na.omit(canopy)
head(canopy)

################################################################
# Linear model Canopy QPA --------------------------------------
################################################################
library(car)
QPA.canopy.mod  <- lm(QPA_canopy ~ date_co, data=canopy)
summary(QPA.canopy.mod)

# Temporal stability
residuals <- residuals(QPA.canopy.mod)
1/sd(residuals)

# Autocorrelation
library(lmtest)
dwt(QPA.canopy.mod)
dwtest(QPA.canopy.mod)

res = QPA.canopy.mod$res 
n = length(residuals) 
mod2 = lm(residuals[-n] ~ residuals[-1]) 
summary(mod2)
plot((residuals[-n] ~ residuals[-1])) 


cc1 <- ggplot(canopy, aes(x=date_co, y=QPA_canopy)) +
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

QPB.canopy.mod  <- lm(QPB_canopy~ date_co, data=canopy)
summary(QPB.canopy.mod)

canopy$QPBCanopyresid<- QPB.canopy.mod$resid
canopy

1/apply(canopy, 2, sd)

cc2 <- ggplot(canopy, aes(x=date_co, y=QPB_canopy))+
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
canopyreg + ggsave("Regression Canopy openness.jpeg", path = "figures", width=6, height=10,dpi=600)

