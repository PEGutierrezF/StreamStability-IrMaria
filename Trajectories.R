

#http://environmentalcomputing.net/intro-to-gams/

# ---------------------------------------------
# Trajectories analysis
# 27 Mar 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  


Trajectories<- read.csv("data/Trajectories.csv")
head(Trajectories)


Trajectories$date<-as.POSIXct(Trajectories$date,"%Y-%m-%d",tz = "UTC")

# Reorder names in a new variable
Trajectories$variable_f = factor(Trajectories$variable, 
      levels=c("canopy_cover", "Leaf_litter", "Chla", "Shrimps", "macroinvertebrates"))

levels(Trajectories$variable_f) <- 
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

 p<- ggplot(Trajectories, aes(date,value)) + 
  geom_point() +
  geom_smooth(se = T, size=1.7, color= "steelblue3", method = "gam", formula = y ~s(x)) + 
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

 ggsave("Trajectories.jpeg",  path = "figures", width=9, height=6,dpi=600)



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



###########################################################################
# Generalised additive models (GAMs)
###########################################################################


# Canopy cover QPA --------------------------------------------------------

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

cc.qp_A.mod <- gam(value ~s(date), data=cc_A, method = "REML")
summary(cc.qp_A.mod)

par(mfrow = c(2,2))
gam.check(cc.qpA.mod)


# Canopy cover QPB --------------------------------------------------------

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")
cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))

cc.qp_B.mod <- gam(value ~s(date), data=cc_B, method = "REML")
summary(cc.qp_B.mod)


# Leaf litter QPA ---------------------------------------------------------

LL_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Leaf_litter")

LL_A$date <- as.integer(as.Date(LL_A$date, format = "%Y-%m-%d"))

ll.qp_A.mod <- gam(value ~s(date), data=LL_A, method = "REML")
summary(ll.qp_A.mod)


# Leaf litter QPB ---------------------------------------------------------

LL_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Leaf_litter")

LL_B$date <- as.integer(as.Date(LL_B$date, format = "%Y-%m-%d"))

ll.qp_B.mod <- gam(value ~s(date), data = LL_B, method = "REML")
summary(ll.qp_B.mod)


# Chlorophyll-a QPA -------------------------------------------------------


ch_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Chla")

ch_A$date <- as.integer(as.Date(ch_A$date, format = "%Y-%m-%d"))

ch.qp_A.mod <- gam(value ~s(date), data = ch_A, method = "REML")
summary(ch.qp_A.mod)


# Chlorophyll-a QPB -------------------------------------------------------

ch_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Chla")

ch_B$date <- as.integer(as.Date(ch_B$date, format = "%Y-%m-%d"))

ch.qp_B.mod <- gam(value ~s(date), data = ch_B, method = "REML")
summary(ch.qp_B.mod)


# Shrimps QPA -------------------------------------------------------


shrimps_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Shrimps")

shrimps_A$date <- as.integer(as.Date(shrimps_A$date, format = "%Y-%m-%d"))

shrimps.qp_A.mod <- gam(value ~s(date), data = shrimps_A, method = "REML")
summary(shrimps.qp_A.mod)


# Shrimps QPB -------------------------------------------------------


shrimps_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Shrimps")

shrimps_B$date <- as.integer(as.Date(shrimps_B$date, format = "%Y-%m-%d"))

shrimps.qp_B.mod <- gam(value ~s(date), data = shrimps_B, method = "REML")
summary(shrimps.qp_B.mod)
