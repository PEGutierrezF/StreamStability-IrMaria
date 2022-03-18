



# ---------------------------------------------
# Generalised additive models (GAMs)
# 05 Jan 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------

 


# http://environmentalcomputing.net/intro-to-gams/
# https://stats.stackexchange.com/questions/231399/how-to-determine-the-type-of-spline-in-gam
# https://stats.stackexchange.com/questions/137109/selecting-knots-for-a-gam
# https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package

rm(list=ls())

# s = represent smooth function
# k = knots. 12 month per year or 24 sampling event per year. 
# method = "REML" = REML is not the default method.  So, we need to add
# ?choose.k

# bs= basis spline
# Smooth classes are invoked directly by s terms
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html

# best model: high R-sq, low AIC, low REML

Trajectories<- read.csv("data/Trajectories.csv")
head(Trajectories)


# Canopy cover QPA --------------------------------------------------------
# Normal distribution of value

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

# Check for normality 
shapiro.test(cc_A$value)
hist(cc_A$value)
descdist(cc_A$value, discrete=FALSE, boot=500)

# cc_A$date <- as.numeric(as.Date(cc_A$date, format = "%Y-%m-%d"))
cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

cc.qp_A.mod <- gam(value ~s(date, bs="cr", k=5), data=cc_A, method = "REML") # best model
summary(cc.qp_A.mod)
gam.check(cc.qp_A.mod)

cc.qp_A.mod1 <- gam(value ~s(date, bs="ps", k=5), data=cc_A, method = "REML")
summary(cc.qp_A.mod1)
gam.check(cc.qp_A.mod1)

cc.qp_A.mod2 <- gam(value ~s(date, bs="ts", k=5), data=cc_A, method = "REML")
summary(cc.qp_A.mod2)
gam.check(cc.qp_A.mod2)

AIC(cc.qp_A.mod, cc.qp_A.mod1, cc.qp_A.mod2)




# Canopy cover QPB --------------------------------------------------------
rm(list=ls())

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")

shapiro.test(cc_B$value)
hist(cc_B$value)
descdist(cc_B$value, discrete=FALSE, boot=500)

cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))

# Check Gaussiand vs Scat
cc.qp_B.mod_G <- gam(value ~s(date, bs="cr", k=5), data=cc_B, method = "REML") 
summary(cc.qp_B.mod_G)
par(mfrow = c(2,2))
gam.check(cc.qp_B.mod_G)


cc.qp_B.mod_S <- gam(value ~s(date, bs="cr", k=5), data=cc_B, 
                   family=scat(link="identity"), method = "REML") # best model
summary(cc.qp_B.mod_S)
par(mfrow = c(2,2))
gam.check(cc.qp_B.mod_S)


AIC(cc.qp_B.mod_G,cc.qp_B.mod_S)


# Best family family=scat()
cc.qp_B.mod_S <- gam(value ~s(date, bs="cr", k=5), 
                   family=scat(link="identity"), data=cc_B, method = "REML") # best model
summary(cc.qp_B.mod_S)
par(mfrow = c(2,2))
gam.check(cc.qp_B.mod_S)



cc.qp_B.mod_S1 <- gam(value ~s(date, bs="ps", k=5), 
                    family=scat(link="identity"), data=cc_B, method = "REML")
summary(cc.qp_B.mod_S1)
gam.check(cc.qp_B.mod_S1)


cc.qp_B.mod_S2 <- gam(value ~s(date, bs="ts", k=5), data=cc_B, method = "REML")
summary(cc.qp_B.mod_S2)
gam.check(cc.qp_B.mod_S2)

AIC(cc.qp_B.mod, cc.qp_B.mod_S1, cc.qp_B.mod_S2)
summary(cc.qp_B.mod_S) # best model
summary(cc.qp_B.mod_S1)
summary(cc.qp_B.mod_S2)


# Leaf litter QPA ---------------------------------------------------------

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

LL_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Leaf_litter")

shapiro.test(LL_A$value)
hist(LL_A$value)
descdist(LL_A$value, discrete=FALSE, boot=500)

LL_A$date <- as.integer(as.Date(LL_A$date, format = "%Y-%m-%d"))

ll.qp_A.mod <- gam(value ~s(date, bs="cr", k=10), data=LL_A, method = "REML") # best model
summary(ll.qp_A.mod)
gam.check(ll.qp_A.mod)

ll.qp_A.mod1 <- gam(value ~s(date, bs="ps", k=10), data=LL_A, method = "REML")
summary(ll.qp_A.mod1)
gam.check(ll.qp_A.mod1)

ll.qp_A.mod2 <- gam(value ~s(date, bs="ts", k=10), data=LL_A, method = "REML")
summary(ll.qp_A.mod2)
gam.check(ll.qp_A.mod2)

AIC(ll.qp_A.mod, ll.qp_A.mod1, ll.qp_A.mod2)
anova(ll.qp_A.mod, ll.qp_A.mod1, ll.qp_A.mod2, test="Chisq") # no hay diferencias entre los modelos


# Leaf litter QPB ---------------------------------------------------------

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

LL_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Leaf_litter")

shapiro.test(LL_B$value)
hist(LL_B$value)
descdist(LL_B$value, discrete=FALSE, boot=500)

LL_B$date <- as.integer(as.Date(LL_B$date, format = "%Y-%m-%d"))


# Check Gaussiand vs Scat
ll.qp_B.mod_G <- gam(value ~s(date, bs="cr", k=10), data=LL_B, method = "REML") 
summary(ll.qp_B.mod_G)
par(mfrow = c(2,2))
gam.check(ll.qp_B.mod_G)


ll.qp_B.mod_S <- gam(value ~s(date, bs="cr", k=10), data = LL_B, # best model
                          family=scat(link="identity"), method = "REML") 
summary(ll.qp_B.mod_S)
par(mfrow = c(2,2))
gam.check(ll.qp_B.mod_S)

AIC(ll.qp_B.mod_G,ll.qp_B.mod_S)


# Best family family=scat()
ll.qp_B.mod_S <- gam(value ~s(date, bs="cr", k=10), data = LL_B, 
                     family=scat(link="identity"), method = "REML") 
summary(ll.qp_B.mod_S)

ll.qp_B.mod_S1 <- gam(value ~s(date, bs="ps", k=10), data = LL_B, # best model
                    family=scat(link="identity"), method = "REML")
summary(ll.qp_B.mod_S1)

ll.qp_B.mod_S2 <- gam(value ~s(date, bs="ts", k=10), data = LL_B, 
                    family=scat(link="identity"), method = "REML")
summary(ll.qp_B.mod_S2)


AIC(ll.qp_B.mod_S,ll.qp_B.mod_S1,ll.qp_B.mod_S2)




# Chlorophyll-a QPA -------------------------------------------------------
rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

ch_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Chla")

shapiro.test(ch_A $value)
hist(ch_A $value)
descdist(ch_A $value, discrete=FALSE, boot=500)

ch_A$date <- as.integer(as.Date(ch_A$date, format = "%Y-%m-%d"))

ch.qp_A.mod <- gam(value ~s(date, bs="cr", k=5), data = ch_A, method = "REML") # best model
summary(ch.qp_A.mod)

ch.qp_A.mod1 <- gam(value ~s(date, bs="ps", k=5), data=ch_A, method = "REML") 
summary(ch.qp_A.mod1)
gam.check(ch.qp_A.mod1)

ch.qp_A.mod2 <- gam(value ~s(date, bs="ts", k=5), data=ch_A, method = "REML")
summary(ch.qp_A.mod2)
gam.check(ch.qp_A.mod2)

AIC(ch.qp_A.mod, ch.qp_A.mod1, ch.qp_A.mod2)
anova(ch.qp_A.mod, ch.qp_A.mod1, ch.qp_A.mod2, test="Chisq")


# Anova function has performed an f-test here, 
# and the GAM model is not significantly different that linear regression.



# Chlorophyll-a QPB -------------------------------------------------------
rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

ch_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Chla")

shapiro.test(ch_B $value)
hist(ch_B $value)
descdist(ch_B $value, discrete=FALSE, boot=500)

ch_B$date <- as.integer(as.Date(ch_B$date, format = "%Y-%m-%d"))

ch.qp_B.mod <- gam(value ~s(date, bs="cr", k=5), data=ch_B, method = "REML")  # best model
summary(ch.qp_B.mod)
gam.check(ll.qp_A.mod)

ch.qp_B.mod1 <- gam(value ~s(date, bs="ps", k=5), data=ch_B, method = "REML")
summary(ch.qp_B.mod1)
gam.check(ch.qp_B.mod1)

ch.qp_B.mod2 <- gam(value ~s(date, bs="ts", k=5), data=ch_B, method = "REML")
summary(ch.qp_B.mod2)
gam.check(ch.qp_B.mod2)

AIC(ch.qp_B.mod, ch.qp_B.mod1, ch.qp_B.mod2)
anova(ch.qp_B.mod, ch.qp_B.mod1, ch.qp_B.mod2, test="Chisq")


# Shrimps QPA -------------------------------------------------------
rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")


shrimps_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Shrimps")

shapiro.test(shrimps_A$value)
hist(shrimps_A$value)
descdist(shrimps_A$value, discrete=FALSE, boot=500)

shrimps_A$date <- as.integer(as.Date(shrimps_A$date, format = "%Y-%m-%d"))

shrimps.qp_A.mod <- gam(value ~s(date, bs="cr", k=5), data = shrimps_A, method = "REML") # best model
summary(shrimps.qp_A.mod)

shrimps.qp_A.mod1 <- gam(value ~s(date, bs="ps", k=5), data = shrimps_A, method = "REML") 
summary(shrimps.qp_A.mod1)

shrimps.qp_A.mod2 <- gam(value ~s(date, bs="ts", k=5), data = shrimps_A, method = "REML")
summary(shrimps.qp_A.mod2)


# Shrimps QPB -------------------------------------------------------
rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

shrimps_B <- Trajectories %>%
  filter(stream =="QPB", variable =="Shrimps")

shapiro.test(shrimps_B$value)
hist(shrimps_B$value)
descdist(shrimps_B$value, discrete=FALSE, boot=500)

shrimps_B$date <- as.integer(as.Date(shrimps_B$date, format = "%Y-%m-%d"))

shrimps.qp_B.mod <- gam(value ~s(date, bs="cr", k=5), data = shrimps_B, method = "REML") # best model
summary(shrimps.qp_B.mod)

shrimps.qp_B.mod1 <- gam(value ~s(date, bs="ps", k=5), data = shrimps_B, method = "REML") 
summary(shrimps.qp_B.mod1)

shrimps.qp_B.mod2 <- gam(value ~s(date, bs="ts", k=5), data = shrimps_B, method = "REML")
summary(shrimps.qp_B.mod2)


AIC(shrimps.qp_B.mod,shrimps.qp_B.mod1,shrimps.qp_B.mod2)
