



#--------------------------------------------
# QPA regression
# 21 May 2020
#PEGF
#--------------------------------------------
#


QPAregression<- read.csv("data/Trajectories.csv")
head(QPAregression)


QPAregression$date<-as.POSIXct(QPAregression$date,"%Y-%m-%d",tz = "UTC")

# Reorder names in a new variable
QPAregression$variable_f = factor(QPAregression$variable, 
      levels=c("canopy_cover", "Leaf_litter", "Chla", "Shrimps", "macroinvertebrates"))

levels(QPAregression$variable_f) <- 
  c("textstyle('Canopy openness')", 
    "textstyle('Leaf litter')",
    "textstyle('Chlorophyll-')*italic('a')",
    "textstyle('Shrimps')",
    "atop(NA,atop(textstyle('Macroinvertebrate'),textstyle('density')))")

# Changes names in Facet_grid ---- es una manera buena, pero no la voy a usar --- Habria que labeller(variable_f = variable_f
#variable_f <- c("Canopy openness", "Leaf litter", "Chlorophyll-a","Shrimps","Macroinvertebrates")
#names(variable_f) <- c("canopy_cover", "Leaf_litter", "Chla", "Shrimps", "macroinvertebrates")

streams <- c("Prieta A", "Prieta B")
names(streams) <- c("QPA", "QPB")

# General graph -----------------------------------------------------------

 p<- ggplot(QPAregression, aes(date,value)) + 
  geom_point() +
  geom_smooth(se = T, size=1.7, color= "steelblue3", method = "gam") + 
  geom_hline(yintercept = 0, color="gray20") +
  
  xlab('Year') + ylab("Change in magnitude") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) + 

  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_grid(vars(stream), vars(variable_f),
    labeller = labeller(variable_f = label_parsed, stream = streams)) +
    theme(
      strip.text.x = element_text(size = 10, color = "black"),
      strip.text.y = element_text(size = 10, color = "black"),
      strip.placement = "outside") +
   theme(strip.background=element_rect(color= "black", fill="gray85")) +
   theme(strip.text.x = element_text(margin = margin(0.001,0,0.001,0, "cm"))) +
    
 geom_vline(aes(xintercept=as.POSIXct("2017-09-21")), # Hurricane Maria
            col= "red",linetype=4, alpha=0.9) +
  geom_vline(aes(xintercept=as.POSIXct("2017-09-6")), # Hurricane Irma
             col= "blue",linetype=4, alpha=0.9) 
p

 ggsave("TrajectoriesColors.jpeg",  path = "figures", width=9, height=6,dpi=600)



# Individual strip color  -------------------------------------------------

g <- ggplot_gtable(ggplot_build(p))
stripr <- which(grepl('strip-t', g$layout$name)) # strip-t changes colors
fills <- c("#fec44f","#a1d99b","#a1d99b","#6baed6", "#6baed6")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g) +
ggsave("TrajectoriesC.jpeg", g, path = "figures", width=9, height=6,dpi=600)



# Canopy cover QPA --------------------------------------------------------

ccA <- QPAregression %>%
  filter(stream =="QPA", variable =="canopy_cover") %>%
  mutate(sam_event= seq_along(value))

#ccA$date<-as.POSIXct(ccA$date,"%Y-%m-%d",tz = "UTC")
#descdist(ccA$value, discrete=FALSE, boot=500)

cc.qpA.mod <- gam(value ~s(sam_event), data=ccA, method = "REML")
summary(cc.qpA.mod)

par(mfrow = c(2,2))
gam.check(cc.qpA.mod)


# Canopy cover QPB --------------------------------------------------------

ccB <- QPAregression %>%
  filter(stream =="QPB", variable =="canopy_cover")
ccB$date<-as.POSIXct(ccB$date,"%Y-%m-%d",tz = "UTC")
descdist(ccB$value, discrete=FALSE, boot=500)

cc.qpB.mod <- gam(value ~date, data=ccB, method = "REML")
summary(cc.qpB.mod)


# Leaf litter QPA ---------------------------------------------------------

LLA <- QPAregression %>%
  filter(stream =="QPA", variable =="Leaf_litter")
LLA$date<-as.POSIXct(LLA$date,"%Y-%m-%d",tz = "UTC")
descdist(LLA$value, discrete=FALSE, boot=500)

ll.qpA.mod <- gam(value ~date, data=LLA, method = "REML")
summary(ll.qpA.mod)




