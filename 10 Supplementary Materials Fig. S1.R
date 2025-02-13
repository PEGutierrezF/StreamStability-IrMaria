



# ---------------------------------------------
# Physicochemical data
# 26 May 2022
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())

install.packages("openxlsx")
library(readxl)

traj_QPA <- read_excel("data/data_trajectories.xlsx", sheet='var_environ')

# traj_QPA <- openxlsx::read.xlsx("data/data_trajectories.xlsx",sheet='var_environ', detectDates = TRUE)

traj_QPA <- traj_QPA %>% 
  dplyr::select(1:11)


data_long <- traj_QPA %>%
  pivot_longer(cols = starts_with("Temp") | starts_with("Cond") | starts_with("Potassium") | starts_with("DOC") | starts_with("Nitrate"),
               names_to = c("variable", "stream"),
               names_pattern = "(.*)_(QPA|QPB)",
               values_to = "Value")

data_long$date_QPA <- as.POSIXct(data_long$date_QPA,"%Y-%m-%d",tz = "UTC")

variable_new <- c("Cond" = "textstyle('Conductivity')~(mu*S~cm^{-1})", 
                  "DOC" = "textstyle('DOC')~(mg*C~L^{-1})",
                  "Nitrate"= "textstyle('Nitrate')~(mg*N~L^{-1})",  #Chlorophyll-')*italic('a')
                  "Potassium"="textstyle('Potassium')~(mg*K~L^{-1})",
                  "Temp"= "textstyle(Temperature)~(degree*C)")


data_long$variable <- factor(data_long$variable,      # Reordering group factor levels
                                levels = c("Temp", "Cond", "DOC", "Nitrate", "Potassium"))

streams_new <- c("QPA"="Prieta A", "QPB"="Prieta B")




stream_env <- ggplot(data_long, aes(x = date_QPA, y = Value)) + 
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_smooth(se = T, size=1.7, color= "gray20", method = "gam", formula = y ~s(x)) + 
  geom_hline(yintercept = 0, color="gray20") +
  
  xlab('Year') + ylab("Change in magnitude (LRR)") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
 # ylim(-3,3) + 
  
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
    geom_vline(aes(xintercept=as.POSIXct("2017-09-21")), # Hurricane Maria
             col= "#e41a1c",linetype=4, alpha=0.9, size = 1) +
  geom_vline(aes(xintercept=as.POSIXct("2017-09-5")), # Hurricane Irma
             col= "#386cb0",linetype=4, alpha=0.9, size = 1) +
  #geom_vline(aes(xintercept=as.POSIXct("2022-03-1")), # Stream FRE
  #        col= "red",linetype=4, alpha=0.9, size = 1) +
  
  theme(strip.text.x = element_text(size = 11, color = "black"),
        strip.text.y = element_text(size = 14, color = "black"),
        strip.placement = "outside") +
  theme(strip.background=element_rect(color= "black", fill="gray85")) +
  theme(strip.text.x = element_text(margin = margin(0.001,0,0.001,0, "cm"))) +
  theme(strip.switch.pad.grid = unit('0.5', "cm")) +
  
  facet_grid(stream ~ variable, 
           labeller = labeller(variable = as_labeller(variable_new, label_parsed),
                               stream  = streams_new)) 
stream_env

ggsave("SI Appendix, Fig. S1.tiff", stream_env, path = "figures", width=9, height=6,dpi=300)



