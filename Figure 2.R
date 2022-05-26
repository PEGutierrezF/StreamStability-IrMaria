



# ---------------------------------------------
# Physicochemical data
# 26 May 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())

df <- "data/physicochemical.xlsx"
excel_sheets(path = df)

df.temp <- read_excel(path = df, sheet = "temperature")
df.temp$date<-as.POSIXct(df.temp$date,"%Y-%m-%d",tz = "UTC")


temp <- ggplot(data=df.temp, aes(x=date, y=temperature, 
                         colour=factor(stream, labels = c("Prieta A", "Prieta B")))) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+ 
  
  # Labels 
  labs(x = "Year", y= "Stream temperature (C)", color='Stream') +
  
  # Vertical line    
  annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
           ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
  annotate("segment", x = as.POSIXct("2017-01-01"), xend = as.POSIXct("2020-09-29"),
           y = 19.9, yend = 19.9, colour = "gray30", size=0.8, linetype=2) +

  
  theme_bw()  +
  theme(legend.position="none") +
  #Quite la leyenda  
  # theme(legend.key.size = unit(0.6, "cm"))+
  # theme(legend.title=element_text(size=14)) + # legend title size
  # theme(legend.text = element_text(color = "black", size = 12))+  #factor name 

  # Axis   
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

temp



# pH ----------------------------------------------------------------------



df.pH <- read_excel(path = df, sheet = "pH")
df.pH$date<-as.POSIXct(df.pH$date,"%Y-%m-%d",tz = "UTC")


pH <- ggplot(data=df.pH, aes(x=date, y=pH., 
                         colour=factor(stream, labels = c("Prieta A", "Prieta B")))) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+ 
  
  # Labels 
  labs(x = "Year", y= "pH", color='Stream') +
  
  # Vertical line    
  annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
           ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
  annotate("segment", x = as.POSIXct("2017-01-01"), xend = as.POSIXct("2020-09-29"),
           y = 6.65, yend = 6.65, colour = "gray30", size=0.8, linetype=2) +
  
  theme_bw()  +
  theme(legend.position="none") +
  #Quite la leyenda  
  # theme(legend.key.size = unit(0.6, "cm"))+
  # theme(legend.title=element_text(size=14)) + # legend title size
  # theme(legend.text = element_text(color = "black", size = 12))+  #factor name 
  
  # Axis   
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y



Fig1 <- (temp) / (pH)
Fig2 <- Fig1 + plot_annotation(tag_levels = 'A') 
Fig2

Fig2 + ggsave("Figure 2.jpg",width = 200, height = 220, units = "mm")
