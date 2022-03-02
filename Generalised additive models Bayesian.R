



# ---------------------------------------------
# Bayesian GAM
# 25 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/

rm(list=ls())

# Note that we use the bf() argument to specify this nonlinear model. 

Trajectories <- read.csv("data/Trajectories.csv")
head(Trajectories)


# Canopy cover QPA --------------------------------------------------------
# Normal distribution of value

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

# Bayesian model

cc.qp_A.Bayes_mod <- brm(bf(value ~ s(date)),
          data = cc_A, family = gaussian(), cores = 1, seed = 14,
          warmup = 28000, iter = 30000, thin = 1, refresh = 0,
          control = list(adapt_delta = 0.99),
          prior = prior(normal(0, 0.1), class = "b"))

summary(cc.qp_A.Bayes_mod)

msms <- marginal_smooths(cc.qp_A.Bayes_mod)
plot(msms)
plot(cc.qp_A.Bayes_mod)
pp_check(cc.qp_A.Bayes_mod)




# Canopy cover QPB --------------------------------------------------------

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")

descdist(cc_B$value, discrete=FALSE, boot=500)

cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))

# Bayesian model
cc.qp_B.Bayes_mod <- brm(bf(value ~ s(date)),
                         data = cc_B, family = gaussian(), cores = 1, seed = 17,
                         iter = 4000, warmup = 2000, thin = 10, refresh = 0,
                         control = list(adapt_delta = 0.99))

summary(cc.qp_B.Bayes_mod)

msms_CC_QPB <- marginal_smooths(cc.qp_B.Bayes_mod)
plot(msms_CC_QPB)
pp_check(cc.qp_B.Bayes_mod)


