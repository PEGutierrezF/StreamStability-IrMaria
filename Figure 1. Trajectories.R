



# ---------------------------------------------
# Trajectories analysis
# 19 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

 

rm(list = ls())

Trajectories<- read.csv("data/Trajectories.csv")
head(Trajectories)


Trajectories$date<-as.POSIXct(Trajectories$date,"%Y-%m-%d",tz = "UTC")

# Reorder names in a new variable

variable_new <- c("canopy_cover"= "textstyle('Canopy openness')", 
    "Leaf_litter"="textstyle('Leaf litter')",
    "Chla"="textstyle('Epilithic algae')",  #Chlorophyll-')*italic('a')
    "Shrimps"="textstyle('Shrimps')",
    "macroinvertebrates"= "atop(NA,atop(textstyle('Macroinvertebrate'),textstyle('density')))")

Trajectories$variable <- factor(Trajectories$variable,      # Reordering group factor levels
                         levels = c("canopy_cover", "Leaf_litter", "Chla", "Shrimps", "macroinvertebrates"))

streams_new <- c("QPA"="Prieta A", "QPB"="Prieta B")

# General graph -----------------------------------------------------------

 p<- ggplot(Trajectories, aes(date, value)) + 
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_smooth(se = T, size=1.7, color= "gray20", method = "gam", formula = y ~s(x)) + 
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
  
  
    
  geom_vline(aes(xintercept=as.POSIXct("2017-09-21")), # Hurricane Maria
            col= "red",linetype=4, alpha=0.9) +
  geom_vline(aes(xintercept=as.POSIXct("2017-09-6")), # Hurricane Irma
             col= "blue",linetype=4, alpha=0.9) +
  
  facet_grid(stream ~ variable,
        labeller = labeller(variable = as_labeller(variable_new, label_parsed),
        stream  = streams_new)) +

  theme(strip.text.x = element_text(size = 10, color = "black"),
    strip.text.y = element_text(size = 10, color = "black"),
    strip.placement = "outside") +
  theme(strip.background=element_rect(color= "black", fill="gray85")) +
  theme(strip.text.x = element_text(margin = margin(0.001,0,0.001,0, "cm"))) 

p

p + geom_richtext(data = labels, aes(label = lab), x = as.POSIXct("2018-12-01"), y = -2.5)

#
p + ggsave("Trajectories.tiff",  path = "figures", width=9, height=6,dpi=300)




# Individual strip color  -------------------------------------------------

g <- ggplot_gtable(ggplot_build(p))
stripr <- which(grepl('strip-t', g$layout$name)) # strip-t changes colors
fills <- c("#fecc5c","#74c476","#74c476","#74a9cf", "#74a9cf") #colorblind safe
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
 grid.draw(g) + ggsave("TrajectoriesD.tiff", g, path = "figures", width=9, height=6,dpi=300)


# Table R-squared 
 
 
# en orden perdo igual data
labels <- data.frame(#variable_new= c("textstyle('Canopy openness')", 
                      #               "textstyle('Leaf litter')",
                       #               "textstyle('Chlorophyll-')*italic('a')",
                        #              "textstyle('Shrimps')",
                         #              "atop(NA,atop(textstyle('Macroinvertebrate'),textstyle('density')))",
                          #           "textstyle('Canopy openness')", 
                           #          "textstyle('Leaf litter')",
                            #         "textstyle('Chlorophyll-')*italic('a')",
                             #        "textstyle('Shrimps')",
                              #       "atop(NA,atop(textstyle('Macroinvertebrate'),textstyle('density')))"),
                                     stream= c("QPA","QPB","QPA","QPB","QPA","QPB","QPA","QPB","QPB","QPB"),
                    lab = paste0("<b>R<sup>2</sup> = ", sprintf("%.2f", c(.59,.51,.14,.16,.41,.51,.37,.28,.55,.60))),
                    code=1:10)

labels


p + geom_richtext(data = labels, aes(x = as.POSIXct("2018-12-01"), y = -2.5, label = lab))






tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}

new <- "R^2"
my_tag <- c(new, "R","R","R","R","R","R","R","R=","R","R")
my_tag

egg::tag_facet(p, 
          x =  as.POSIXct("2017-12-01"), y = -3, 
          vjust = -1, hjust = -0.25,
          open = "", close = "",
          fontface = 3,
          size = 4,
          family = "serif",
          tag_pool = new)



