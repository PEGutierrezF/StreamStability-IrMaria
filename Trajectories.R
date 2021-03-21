



#--------------------------------------------
# QPA regression
# 21 May 2020
#PEGF
#--------------------------------------------
#


QPAregression<- read.csv("data/Trajectories QPA.csv")
head(QPAregression)

QPAregression$date<-as.POSIXct(QPAregression$date,"%Y-%m-%d",tz = "UTC")

variables <- c("Canopy cover", "Chlorophyll-a", "Leaf Litter")
names(variables) <- c("canopy_cover", "Chla", "Leaf_litter")

streams <- c("Prieta A", "Prieta B")
names(streams) <- c("QPA", "QPB")

 ggplot(QPAregression, aes(date,value)) + 
  geom_point() +
  geom_smooth(se = T, size=2) + 
  geom_hline(yintercept = 0, color="darkred") +
  
  xlab('Year') + ylab("Change in magnitude") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) + 
  #  annotate("text", x = 5, y = 2.5, label = "Increase",colour = "black", size = 6,angle = 90) +
  #  annotate("text", x = 5, y = -2.5, label = "Decrease",colour = "black", size = 6,angle = 90) +
  
  #theme(legend.title=element_blank())+
  #theme(legend.text = element_text(size=20)) +
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +
  
  facet_grid(vars(stream), vars(variable),
    labeller = labeller(variable = variables, stream = streams)) +
    theme(
      strip.text.x = element_text(size = 12, color = "black", face = "bold"),
      strip.text.y = element_text(size = 12, color = "black", face = "bold")
    )















QPA_regression<- melt(QPAregression, na.rm = T)
QPA_regression
QPA_regression1<- ddply(QPA_regression, .(variable), mutate, id = seq_along(value))


QPA_regression1$variable_f = factor(QPA_regression1$variable, levels=c('Canopy','Leaf_litter','Shrimp',
                                                                   'Chlorophyll_a','Macroinvertebrates'))

RegressionQPA <- ggplot(QPA_regression1, aes(id, 
                                     value,
                                     color=variable)) + 
  #   annotate("rect", xmin = 0, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "lightblue") +
  
  geom_point() +
  geom_smooth(se = T, size=5)  + 

  geom_hline(yintercept = 0, color="darkred") +

  xlab('Year') + ylab("Change in magnitude") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) + 
#  annotate("text", x = 5, y = 2.5, label = "Increase",colour = "black", size = 6,angle = 90) +
#  annotate("text", x = 5, y = -2.5, label = "Decrease",colour = "black", size = 6,angle = 90) +
  
  #theme(legend.title=element_blank())+
  #theme(legend.text = element_text(size=20)) +
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +
  
  facet_grid(~ variable_f, scales="free_x")

RegressionQPA

RegressionQPA + ggsave("TrajectoriesQPA.jpeg", width=10, height=6,dpi=600)  





# Quebrada Prieta ---------------------------------------------------------




QPB_reg<- read.csv("data/Trajectories QPB.csv")
QPB_reg

QPB_regression<- melt(QPB_reg, na.rm = T)
QPB_regression
QPB_regression1<- ddply(QPB_regression, .(variable), mutate, id = seq_along(value))


QPB_regression1$variable_f = factor(QPB_regression1$variable, 
                                  levels=c('Canopy','Leaf_litter','Shrimp',
                                            'Chlorophyll.a','Macroinvertebrates'))


RegressionQPB <- ggplot(QPB_regression1, aes(id, 
                                           value,
                                           color=variable)) + 
  geom_point() +
  geom_smooth(se = T, size=5)  + 
  
  geom_hline(yintercept = 0, color="darkred") +
  
  xlab('Sampling period (2017-2019)') + ylab("Change in magnitude") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) + 

  theme(legend.position="none")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +
  
  facet_grid(~ variable_f, scales="free_x")

RegressionQPB

RegressionQPB + ggsave("TrajectoriesQPB.jpeg", width=10, height=6,dpi=600)  

