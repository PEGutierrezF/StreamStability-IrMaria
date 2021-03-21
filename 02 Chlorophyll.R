



#--------------------------------------------
# Chlorophyll
# 07 May 2020
#PEGF
#--------------------------------------------
#

chl_a<- read.csv("data/chl_a.csv")
chl_a


# QPA CHLA ----------------------------------------------------------------

chla <- chl_a %>% select(events,QPACHLALog,QPBCHLALog)
chla <- na.omit(chla)

QPAChla.mod  <- lm(QPACHLALog~ events   , data=chla)
summary(QPAChla.mod)

chla$QPAChlaresid<- QPAChla.mod$resid
chla

1/apply(chla, 2, sd)

c1 <- ggplot(chla, aes(events,
                            y=QPACHLALog))+
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



# QPB CHL-A ---------------------------------------------------------------

QPBChla.mod  <- lm(QPBCHLALog~ events   , data=chla)
summary(QPBChla.mod)

chla$QPBChlaresid<- QPBChla.mod$resid
chla

1/apply(chla, 2, sd)

c2 <- ggplot(chla, aes(TimeCHLA,
                       y=QPBCHLALog))+
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
chlag + ggsave("CHLA.jpeg", width=6, height=10,dpi=600)


# Longterm  ---------------------------------------------------------------

chl_a
# QPA
c3 <- ggplot(chl_a,aes(TimeCHLA  ,
                           y=QPAchla))+
  
  annotate(geom = "rect",xmin=1,xmax=23,ymin=707.2232635,ymax=1383.981903,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPAchla-QPAsdCHLA, ymax=QPAchla+QPAsdCHLA), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1045.60, xend = 9, yend = 1045.60))+
  geom_segment((aes(x = 10, y = 1045.60, xend = 23, yend = 1045.60)), color="red", linetype="dashed", size=1) + 
  
  xlab('')+ ylab("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(500,4000) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

c3

# QPB

c4 <- ggplot(chl_a,aes(TimeCHLA  ,
                       y=QPBchla))+
  annotate(geom = "rect",xmin=1,xmax=23,ymin=718.5855469,ymax=2077.039499,alpha = 0.4,fill = "grey") +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPBchla-QPBsdCHLA, ymax=QPBchla+QPBsdCHLA), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 1397.81, xend = 9, yend = 1397.81))+
  geom_segment((aes(x = 10, y = 1397.81, xend = 23, yend = 1397.81)), color="red", linetype="dashed", size=1) + 
  
  xlab('Sampling period')+ ylab("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") +
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,4000) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

c4



LongCHLA <- c3 / c4
LongCHLA + ggsave("CHLAMean.jpeg", width=6, height=10,dpi=600)

