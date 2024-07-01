



# ---------------------------------------------
# Ecosystem stability - Chlorophyll-a
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




chla <- read.xlsx("data/data_stability_metrics.xlsx", sheet='epilithic_algae', detectDates = TRUE)
head(chla)

chla$date <- as.POSIXct(chla$date,"%Y-%m-%d",tz = "UTC")
chla <- na.omit(chla)

################################################################
# Linear model Chlorophyll-a Prieta A --------------------------
################################################################


QPA.chla.mod  <- lm(QPA_chla~ date, data=chla)
summary(QPA.chla.mod)

# Temporal stability
residuals <- residuals(QPA.chla.mod)
1/sd(residuals)


c1 <- ggplot(chla, aes(date_chla, y=QPA_chla))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
 
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

  ylim(-2,1.5) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
c1



################################################################
# Linear model Chlorophyll-a Prieta B --------------------------
################################################################

QPB.chla.mod  <- lm(QPB_chla~ date, data=chla)
summary(QPB.chla.mod)

# Temporal stability
residuals <- residuals(QPB.chla.mod)
1/sd(residuals)


c2 <- ggplot(chla, aes(date_chla, y=QPB_chla))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('Sampling period') + ylab("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") + 
  theme(axis.title.x = element_text(size = 18, angle = 0)) +# axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y

    ylim(-2,1.5) +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
c2

chlag <- c1 / c2
chlag + ggsave("Regression chlorophyll-a.jpeg",  path = "figures", width=6, height=10,dpi=600)

