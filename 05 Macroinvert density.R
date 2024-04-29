



# ---------------------------------------------
# Ecosystem stability - Macroinvertebrate density
# 05 Jan 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls())




macroinvertebrates <- read.xlsx("data/data_stability_metrics.xlsx", 
                                sheet='macro_density', detectDates = TRUE)
head(macroinvertebrates)
macroiv <- na.omit(macroinvertebrates)
head(macroiv)


################################################################
# Linear model Macroinvertebrate abundance Prieta A -----------
################################################################

QPA.macroinv.mod  <- lm(QPA_miv ~ date, data=macroiv)
summary(QPA.macroinv.mod)

macroiv$QPAresid<- QPA.macroinv.mod$resid
head(macroiv)

1/apply(macroiv, 2, sd)


# Autocorrelation
dwt(QPA.macroinv.mod)
dwtest(QPA.macroinv.mod)


qpa.miv <- ggplot(macroiv, aes(date, y=QPA_miv))+
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


################################################################
# Linear model Macroinvertebrate abundance Prieta B ------------
################################################################
QPB.macroinv.mod  <- lm(QPB_miv ~ date, data=macroiv)
summary(QPB.macroinv.mod)

macroiv$QPAresid<- QPB.macroinv.mod$resid
head(macroiv)

1/apply(macroiv, 2, sd)


# Autocorrelation
dwt(QPB.macroinv.mod)
dwtest(QPB.macroinv.mod)

# Heterocedasticity
# we fail to reject the null hypothesis (that variance of residuals is constant) 
# and therefore infer that the residuals are homoscedastic. 
lmtest::bptest(QPB.macroinv.mod)  # Breusch-Pagan test


qpb.miv <- ggplot(macroiv, aes(date, y=QPB_miv))+
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

