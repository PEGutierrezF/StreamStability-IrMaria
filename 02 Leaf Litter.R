



# ---------------------------------------------
# Ecosystem stability - Leaf litter
# 14 Jun 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




leaflitter <- read.xlsx("data/data_stability_metrics.xlsx", sheet='leaflitter', detectDates = TRUE)
head(leaflitter)


leaflitter$date <- as.POSIXct(leaflitter$date,"%Y-%m-%d",tz = "UTC")
leaflitter <- na.omit(leaflitter)


################################################################
# Linear model Leaf Litter Prieta A ----------------------------
################################################################


QPAleaf.mod  <- lm(QPA_leaflitter ~ date, data=leaflitter)
summary(QPAleaf.mod)
plot(QPA_leaflitter ~ date, data=leaflitter)

leaflitter$QPAresid<- QPAleaf.mod $resid
leaflitter
1/apply(leaflitter, 2, sd) #2 mean apply to columns

# Temporal stability
residuals <- residuals(QPAleaf.mod)
1/sd(residuals)


# Autocorrelation
library(lmtest)
dwt(QPAleaf.mod)
dwtest(QPAleaf.mod)


p1 <- ggplot(leaflitter,aes(x= date, y=QPA_leaflitter)) +
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


################################################################
# Linear model Leaf Litter Prieta B ----------------------------
################################################################

QPBleaf.mod <- lm(QPB_leaflitter ~ date, data=leaflitter)
summary(QPBleaf.mod)

leaflitter$QPBresid<- QPBleaf.mod$resid
leaflitter

1/apply(leaflitter, 2, sd)
# Temporal stability
residuals <- residuals(QPBleaf.mod)
1/sd(residuals)

# Plot --------------------------------------------------------------------

p2 <- ggplot(leaflitter,aes(x= date_ll, y=QPB_leaflitter))+
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


