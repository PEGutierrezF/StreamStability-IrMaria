



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

metrics<- read.csv("metrics.csv")
metrics



# Resistance --------------------------------------------------------------


r1 <- ggplot(metrics, 
      aes(x=Resitance,
          y=Stream))+ 
#          color=Stream,
 #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=3)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  facet_grid(Variable~.)



# Resilience --------------------------------------------------------------


r2 <- ggplot(metrics, 
       aes(x=Resiliences,
           y=Stream))+ 
  #          color=Stream,
  #         group=Variable)) +
  geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_point(size=3)+ #aes(shape=Variable),size=2.7) +#, position=pd, stat="identity") + 
  facet_grid(Variable~.)

r1 + r2


r3<- ggplot(metrics, 
             aes(x=Resitance,
                 y=Resiliences,
                 color= Stream,
                 group=Variable)) +
geom_point(aes(shape=Variable, color=Stream), size=3) +
geom_vline(aes(xintercept = 0), color='darkgrey') +
  geom_hline(aes(yintercept = 0), color='darkgrey')

r3

