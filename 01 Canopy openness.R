



# ---------------------------------------------
# Ecosystem stability - Canopy cover
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls())



canopycover <- read.xlsx("data/data_stability_metrics.xlsx", sheet='canopy', detectDates = TRUE)
head(canopycover)


canopy <- canopycover %>% dplyr::select(date_co, QPA_canopy, QPB_canopy)
canopy$date_co <- as.POSIXct(canopy$date_co,"%Y-%m-%d",tz = "UTC")
canopy <- na.omit(canopy)
tail(canopy)

################################################################
# Linear model Canopy QPA --------------------------------------
################################################################
QPA.canopy.mod  <- lm(QPA_canopy ~ date_co, data=canopy)
summary(QPA.canopy.mod)

# Temporal stability
residuals <- residuals(QPA.canopy.mod)
1/sd(residuals)

# Normality
shapiro.test(residuals)
hist(residuals)

# Autocorrelation
dwt(QPA.canopy.mod)
dwtest(QPA.canopy.mod)

# Heterocedasticity
# we fail to reject the null hypothesis (that variance of residuals is constant) 
# and therefore infer that the residuals are homoscedastic. 
lmtest::bptest(QPA.canopy.mod)  # Breusch-Pagan test


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(QPA.canopy.mod, 1)


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


# Temporal stability
residuals <- residuals(QPB.canopy.mod)
1/sd(residuals)

# Normality
shapiro.test(residuals)
hist(residuals)

# Autocorrelation
# P > 0.05: There is no significant evidence of autocorrelation in the residuals of the regression model.
# P < 0.05: There is significant evidence of autocorrelation in the residuals of the regression model.
dwt(QPB.canopy.mod)
dwtest(QPB.canopy.mod)

# Heterocedasticity
# P > 0.05: There is no significant evidence of heteroscedasticity in the residuals of the regression model.
# P < 0.05: There is significant evidence of heteroscedasticity in the residuals of the regression model.
lmtest::bptest(QPB.canopy.mod)  # Breusch-Pagan test


# par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(QPB.canopy.mod, 1)


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

