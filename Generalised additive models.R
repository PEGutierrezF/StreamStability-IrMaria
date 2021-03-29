



# ---------------------------------------------
# Generalised additive models (GAMs)
# 28 Mar 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  


#http://environmentalcomputing.net/intro-to-gams/
# https://stats.stackexchange.com/questions/231399/how-to-determine-the-type-of-spline-in-gam
# https://stats.stackexchange.com/questions/137109/selecting-knots-for-a-gam


# k = knots. 12 month per year or 24 sampling event per year. 
# basis spline
# https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package

Trajectories<- read.csv("data/Trajectories.csv")
head(Trajectories)


# Canopy cover QPA --------------------------------------------------------

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

cc.qp_A.mod <- gam(value ~s(date, bs="cr", k=5), data=cc_A, method = "REML") # best smooth
summary(cc.qp_A.mod)
gam.check(cc.qp_A.mod)

cc.qp_A.mod1 <- gam(value ~s(date, bs="ps", k=5), data=cc_A, method = "REML")
summary(cc.qp_A.mod1)
gam.check(cc.qp_A.mod1)

cc.qp_A.mod2 <- gam(value ~s(date, bs="ts", k=5), data=cc_A, method = "REML")
summary(cc.qp_A.mod2)
gam.check(cc.qp_A.mod2)



par(mfrow = c(2,2))


?choose.k

# Canopy cover QPB --------------------------------------------------------

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")
cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))

cc.qp_B.mod <- gam(value ~s(date), data=cc_B, method = "REML")
summary(cc.qp_B.mod)
gam.check(cc.qp_B.mod)

# Leaf litter QPA ---------------------------------------------------------

LL_A <- Trajectories %>%
  filter(stream =="QPA", variable =="Leaf_litter")

LL_A$date <- as.integer(as.Date(LL_A$date, format = "%Y-%m-%d"))

ll.qp_A.mod <- gam(value ~s(date), data=LL_A, method = "REML")
summary(ll.qp_A.mod)
gam.check(ll.qp_A.mod)

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

# https://www.mainard.co.uk/post/why-mgcv-is-awesome/
ch.qp_A.mod1 <- lm(value ~date, data = ch_A)
summary(ch.qp_A.mod1)

AIC(ch.qp_A.mod)
AIC(ch.qp_A.mod1)
anova(ch.qp_A.mod, ch.qp_A.mod1)
# Anova function has performed an f-test here, 
# and the GAM model is not significantly different that linear regression.

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