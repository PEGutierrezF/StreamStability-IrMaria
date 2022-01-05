



# ---------------------------------------------
# Ecosystem stability - Shrimps
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




decapod <- read.csv("data/all_data.csv")
head(decapod)

shrimp <- decapod %>% dplyr::select(date_shrimp, QPA_shrimp, QPB_shrimp)
shrimp$date_shrimp <- as.POSIXct(shrimp$date_shrimp,"%Y-%m-%d",tz = "UTC")
shrimp <- na.omit(shrimp)

# Lm QPA Leaf Litter ------------------------------------------------------

QPA.shrimp.mod  <- lm(QPA_shrimp ~ date_shrimp, data=shrimp)
summary(QPA.shrimp.mod)

shrimp$QPAresid<- QPA.shrimp.mod$resid
shrimp

1/apply(shrimp, 2, sd)


qpa.s <- ggplot(shrimp, aes(date_shrimp, y=QPA_shrimp))+
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

# Shrimp QPB --------------------------------------------------------------


QPB.shrimp.mod  <- lm(QPB_shrimp ~ date_shrimp, data=shrimp)
summary(QPB.shrimp.mod)

shrimp$QPBresid<- QPB.shrimp.mod$resid
shrimp

1/apply(shrimp, 2, sd)

qpb.s <- ggplot(shrimp, aes(date_shrimp, y=QPB_shrimp))+
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

