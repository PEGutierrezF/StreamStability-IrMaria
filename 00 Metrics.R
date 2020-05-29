



#--------------------------------------------
# Dot plot
# 09 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

metrics<- read.csv("00 Metrics.csv")
metrics



# Resistance --------------------------------------------------------------

metrics$Variable_f = factor(metrics$Variable, levels=c('Canopy','Leaf','Leaf_Exp','Shrimp', 'Chla','Macroinvt.'))

r1 <- ggplot(metrics, 
      aes(x=Resitance,
          y=Stream))+ 
#          color=Stream,
 #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=5)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
   xlim(-1,1) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_grid(Variable_f~.) +
  theme(strip.text.y = element_text(size = 16, colour = "black", angle = 270)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94"))


r1 + ggsave("Resistance.jpeg", width=6, height=6,dpi=600)


# Resilience --------------------------------------------------------------


metrics$Variable_f = factor(metrics$Variable, levels=c('Canopy','Leaf','Leaf_Exp','Shrimp', 'Chla','Macroinvt.'))

r2 <- ggplot(metrics, 
             aes(x=Resilience,
                 y=Stream))+ 
  #          color=Stream,
  #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=5)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  xlim(-1,1) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_grid(Variable_f~.) +
  theme(strip.text.y = element_text(size = 16, colour = "black", angle = 270)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94"))

r2 + ggsave("Resilience.jpeg", width=6, height=6,dpi=600)


# Recovery ----------------------------------------------------------------

metrics$Variable_f = factor(metrics$Variable, levels=c('Canopy','Leaf','Leaf_Exp','Shrimp', 'Chla','Macroinvt.'))

r3 <- ggplot(metrics, 
             aes(x=Recovery,
                 y=Stream))+ 
  #          color=Stream,
  #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=5)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  xlim(-1,1) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_grid(Variable_f~.) +
  theme(strip.text.y = element_text(size = 16, colour = "black", angle = 270)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94"))

r3

r3 + ggsave("Recovery.jpeg", width=6, height=6,dpi=600)

# Temporal Stability ------------------------------------------------------



metrics$Variable_f = factor(metrics$Variable, levels=c('Canopy','Leaf','Leaf_Exp','Shrimp', 'Chla','Macroinvt.'))


r4 <- ggplot(metrics, 
             aes(x=TemporalStab,
                 y=Stream))+ 
  #          color=Stream,
  #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=5)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  
  theme(axis.title.x = element_text(size = 18, angle = 0)) +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  xlim(-15,15) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_grid(Variable_f~.) +
  theme(strip.text.y = element_text(size = 12, colour = "black", angle = 270)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey"))

r4

metrics <- (r1 + r3) / (r2 + r4) + plot_annotation(tag_levels = 'A')
metrics
metrics + ggsave("Metrics.jpeg", width=6, height=10,dpi=600)



# Relationship between Resistance and Resiliences -------------------------



metrics<- read.csv("00 Metrics.csv")
metrics

r5<- ggplot(metrics, 
             aes(x=Resistance,
                 y=Resilience,
                 color= Stream,
                 group=Variable)) +
geom_point(aes(color=Stream,shape=Variable), size=5) +
geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_hline(aes(yintercept = 0), color='darkgrey') +
  xlim(-1,1) 
r5

cor.test(metrics$Resistance,metrics$Resilience,method = "pearson", exact=F)

# QPA Analisis ------------------------------------------------------------


QPARvR <- metrics %>% select(Stream,Resistance, Resilience) 
QPARvR1 <- QPARvR %>% filter(Stream=="QPA") 
QPARvR1

r6 <- ggplot(QPARvR1, 
            aes(x=Resistance,
                y=Resilience)) +
  geom_point(aes(), size=5) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_smooth(method=lm,se=TRUE,colour="black", size=0.5) +
  geom_hline(aes(yintercept = 0), color='darkgrey') +
  xlim(-1,1) 
r6

