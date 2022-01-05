



# ---------------------------------------------
# Ecosystem stability - Chlorophyll-a
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




chlorophyll.a <- read.csv("data/all_data.csv")
head(chlorophyll.a)

chla <- chlorophyll.a %>% dplyr::select(date_chla, QPA_chla, QPB_chla)
chla$date_chla <- as.POSIXct(chla$date_chla,"%Y-%m-%d",tz = "UTC")
chla <- na.omit(chla)


################################################################
# Linear model Chlorophyll-a Prieta A --------------------------
################################################################


QPA.chla.mod  <- lm(QPA_chla~ date_chla, data=chla)
summary(QPA.chla.mod)

chla$QPAChlaresid<- QPA.chla.mod$resid
chla

1/apply(chla, 2, sd)

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


QPB.chla.mod  <- lm(QPB_chla~ date_chla, data=chla)
summary(QPB.chla.mod)

chla$QPB.chla.resid<- QPB.chla.mod$resid
chla

1/apply(chla, 2, sd)

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
chlag + ggsave("Chlorophyll-a.jpeg", width=6, height=10,dpi=600)

