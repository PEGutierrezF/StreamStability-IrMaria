



# ---------------------------------------------
# Trajectories analysis
# 19 Jun 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

 

rm(list = ls())

trajectories <- read.xlsx("data/data_trajectories.xlsx", detectDates = TRUE)
head(trajectories)


trajectories$date <- as.POSIXct(trajectories$date,"%Y-%m-%d",tz = "UTC")

# Reorder names in a new variable

variable_new <- c("canopy_cover"= "textstyle('Canopy openness')", 
    "Leaf_litter"="textstyle('Leaf litter')",
    "Chla"="textstyle('Epilithic algae')",  #Chlorophyll-')*italic('a')
    "Shrimps"="atop(NA,atop(textstyle('Decapoda'),textstyle('abundance')))",
    "macroinvertebrates"= "atop(NA,atop(textstyle('Macroinvertebrate'),textstyle('density')))")

trajectories$variable <- factor(trajectories$variable,      # Reordering group factor levels
                         levels = c("canopy_cover", "Leaf_litter", "Chla", "Shrimps", "macroinvertebrates"))

streams_new <- c("QPA"="Prieta A", "QPB"="Prieta B")


# General graph -----------------------------------------------------------

 c <- ggplot(trajectories, aes(date, value)) + 
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_smooth(se = T, size=1.7, color= "gray20", method = "gam", formula = y ~s(x)) + 
  geom_hline(yintercept = 0, color="gray20") +
  
  xlab('Year') + ylab("Change in magnitude (LRR)") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-3,3) + 

  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  geom_vline(aes(xintercept=as.POSIXct("2017-09-21")), # Hurricane Maria
            col= "#e41a1c",linetype=4, alpha=0.9, size = 1) +
  geom_vline(aes(xintercept=as.POSIXct("2017-09-6")), # Hurricane Irma
            col= "#386cb0",linetype=4, alpha=0.9, size = 1) +
  #geom_vline(aes(xintercept=as.POSIXct("2022-03-1")), # Stream FRE
   #        col= "red",linetype=4, alpha=0.9, size = 1) +

 facet_grid(stream ~ variable,
             labeller = labeller(variable = as_labeller(variable_new, label_parsed),
                                 stream  = streams_new)) +
  theme(strip.text.x = element_text(size = 12, color = "black"),
        strip.text.y = element_text(size = 12, color = "black"),
        strip.placement = "outside") +
  theme(strip.background=element_rect(color= "black", fill="gray85")) +
  theme(strip.text.x = element_text(margin = margin(0.001,0,0.001,0, "cm"))) +
  theme(strip.switch.pad.grid = unit('0.5', "cm"))

c

vh_line <- data.frame(
  xintercept = as.POSIXct("2022-03-1"),
  stream = c("QPB", "QPB")
)

p <- c + geom_vline(data = vh_line, aes(xintercept = xintercept),  
               col= "red",linetype=4, alpha=0.9, size = 1) 

#
# p + ggsave("Trajectoriesa.jpeg",  path = "figures", width=9, height=6,dpi=300)


# Individual strip color  -------------------------------------------------

g <- ggplot_gtable(ggplot_build(c))
stripr <- which(grepl('strip-t', g$layout$name)) # strip-t changes colors
fills <- c("#fecc5c","#74c476","#74c476","#74a9cf", "#74a9cf") #colorblind safe
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
 grid.draw(g) + ggsave("SI Appendix, Fig. S2.tiff", g, path = "figures", width=9, height=6,dpi=300)



