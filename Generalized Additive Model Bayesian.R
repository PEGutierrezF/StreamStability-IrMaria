



# ---------------------------------------------
# Bayesian GAM
# 25 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/
# https://tem11010.github.io/regression_brms/

rm(list=ls())



Trajectories <- read.csv("data/Trajectories.csv")
head(Trajectories)



###########################################################################
# Canopy cover Prieta A --------------------------------------------------------
###########################################################################
# Normal distribution of value

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))


############################ Best model -> cc #############################
# Model 1 "cc" ------------------------------------------------------------

priors.cc_A.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                           data = cc_A, family = gaussian())
priors.cc_A.cc

cc.qp_A.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                              data = cc_A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.cc)

summary(cc.qp_A.Bayes.cc)

posterior_summary(cc.qp_A.Bayes.cc)
cc.qp_A.Bayes.cc$fit

prior_summary(cc.qp_A.Bayes.cc)
get_posterior_beliefs_about_hypotheses(cc.qp_A.Bayes.cc)

plot(cc.qp_A.Bayes.cc)

plot(conditional_effects(cc.qp_A.Bayes.cc), points = TRUE)
msms <- conditional_smooths(cc.qp_A.Bayes.cc)
plot(msms)

pp_check(cc.qp_A.Bayes.cc, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.cc, 
          type = "areas",
          prob = 0.95)

# Model 2 "cr" --------------------------------------------------------------


priors.cc_A.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                           data = cc_A, family = gaussian())
priors.cc_A.cr

cc.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                              knots = knots, data = cc_A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.cr)

summary(cc.qp_A.Bayes.cr)
plot(cc.qp_A.Bayes.cr)

plot(conditional_effects(cc.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(cc.qp_A.Bayes.cr)
plot(msms)

pp_check(cc.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 "cs" --------------------------------------------------------------

priors.cc_A.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                           data = cc_A, family = gaussian())
priors.cc_A.cs

cc.qp_A.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                              knots = knots, data = cc_A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.cs)


summary(cc.qp_A.Bayes.cs)
plot(cc.qp_A.Bayes.cs)

cc.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(cc.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(cc.qp_A.Bayes.cs)
plot(msms)

pp_check(cc.qp_A.Bayes.cs, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 "ps" -----------------------------------------------------------------

priors.cc_A.ps = get_prior(value ~ s(date, bs="ps", k=5),
                           data = cc_A, family = gaussian())

cc.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k=5)),
                              data = cc_A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.ps)

summary(cc.qp_A.Bayes.ps)
plot(cc.qp_A.Bayes.ps)
plot(conditional_effects(cc.qp_A.Bayes.ps), points = TRUE)

pp_check(cc.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)

# Model 5 "cp" -----------------------------------------------------------------

priors.cc_A.cp = get_prior(value ~ s(date, bs="cp", k=5),
                           data = cc_A, family = gaussian())

cc.qp_A.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=5)),
                              data = cc_A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.cp)

summary(cc.qp_A.Bayes.cp)
plot(cc.qp_A.Bayes.cp)
plot(conditional_effects(cc.qp_A.Bayes.cp), points = TRUE)

pp_check(cc.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(cc.qp_A.Bayes.cc)
bayes_R2(cc.qp_A.Bayes.cr)
bayes_R2(cc.qp_A.Bayes.cs)
bayes_R2(cc.qp_A.Bayes.ps)
bayes_R2(cc.qp_A.Bayes.cp)


loo.cc <- loo(cc.qp_A.Bayes.cc)
loo.cr <- loo(cc.qp_A.Bayes.cr)
loo.cs <- loo(cc.qp_A.Bayes.cs)
loo.ps <- loo(cc.qp_A.Bayes.ps)
loo.cp <- loo(cc.qp_A.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(cc.qp_A.Bayes.cc, cc.qp_A.Bayes.cr, 
              cc.qp_A.Bayes.cs, cc.qp_A.Bayes.ps, 
              cc.qp_A.Bayes.cp, weights = "loo") #loo




###########################################################################
# Canopy cover Prieta B --------------------------------------------------------
# Normal distribution of value
###########################################################################

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")

cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))


############################ Best model -> cc #############################
# Model 1 Prieta_B "cc" ------------------------------------------------------------

priors.cc_B.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                           data = cc_B, family = gaussian())
priors.cc_B.cc

cc.qp_B.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                              data = cc_B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_B.cc)

summary(cc.qp_B.Bayes.cc)

posterior_summary(cc.qp_B.Bayes.cc)
cc.qp_B.Bayes.cc$fit

prior_summary(cc.qp_B.Bayes.cc)
get_posterior_beliefs_about_hypotheses(cc.qp_B.Bayes.cc)

plot(cc.qp_B.Bayes.cc)

plot(conditional_effects(cc.qp_B.Bayes.cc), points = TRUE)
msms <- conditional_smooths(cc.qp_B.Bayes.cc)
plot(msms)

pp_check(cc.qp_B.Bayes.cc, ndraws = 100)

mcmc_plot(cc.qp_B.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr #############################
# Model 2 Prieta_B "cr" ---------------------------------------------------


priors.cc_B.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                           data = cc_B, family = gaussian())
priors.cc_B.cr

cc.qp_B.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                              data = cc_B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_B.cr)

summary(cc.qp_B.Bayes.cr)
plot(cc.qp_B.Bayes.cr)

plot(conditional_effects(cc.qp_B.Bayes.cr), points = TRUE)
msms <- conditional_smooths(cc.qp_B.Bayes.cr)
plot(msms)

pp_check(cc.qp_B.Bayes.cr, ndraws = 100)

mcmc_plot(cc.qp_B.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Canopy cover Prieta_B "cs" -----------------------------------------------------

priors.cc_B.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                           data = cc_B, family = gaussian())
priors.cc_B.cs

cc.qp_B.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                              data = cc_B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.cc_B.cs)


summary(cc.qp_B.Bayes.cs)
plot(cc.qp_B.Bayes.cs)

cc.qp_B.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(cc.qp_B.Bayes.cs), points = TRUE)
msms <- conditional_smooths(cc.qp_B.Bayes.cs)
plot(msms)

pp_check(cc.qp_B.Bayes.cs, ndraws = 100)

mcmc_plot(cc.qp_B.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Canopy cover Prieta_B "ps" -----------------------------------------------------

priors.cc_B.ps = get_prior(value ~ s(date, bs="ps", k=5),
                           data = cc_B, family = gaussian())

cc.qp_B.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k=5)),
                              data = cc_B, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.cc_B.ps)

summary(cc.qp_B.Bayes.ps)
plot(cc.qp_B.Bayes.ps)
plot(conditional_effects(cc.qp_B.Bayes.ps), points = TRUE)

pp_check(cc.qp_B.Bayes.ps, ndraws = 100)

mcmc_plot(cc.qp_B.Bayes.ps, 
          type = "areas",
          prob = 0.95)

# Model 5 Canopy cover Prieta_B "cp" --------------------------------------------------------

priors.cc_B.cp = get_prior(value ~ s(date, bs="cp", k=5),
                           data = cc_B, family = gaussian())

cc.qp_B.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=5)),
                              data = cc_B, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.cc_B.cp)

summary(cc.qp_B.Bayes.cp)
plot(cc.qp_B.Bayes.cp)
plot(conditional_effects(cc.qp_B.Bayes.cp), points = TRUE)

pp_check(cc.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(cc.qp_B.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(cc.qp_B.Bayes.cc)
bayes_R2(cc.qp_B.Bayes.cr)
bayes_R2(cc.qp_B.Bayes.cs)
bayes_R2(cc.qp_B.Bayes.ps)
bayes_R2(cc.qp_B.Bayes.cp)


loo.cc <- loo(cc.qp_B.Bayes.cc)
loo.cr <- loo(cc.qp_B.Bayes.cr)
loo.cs <- loo(cc.qp_B.Bayes.cs)
loo.ps <- loo(cc.qp_B.Bayes.ps)
loo.cp <- loo(cc.qp_B.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(cc.qp_B.Bayes.cc, cc.qp_B.Bayes.cr, 
              cc.qp_B.Bayes.cs, cc.qp_B.Bayes.ps, 
              cc.qp_B.Bayes.cp, weights = "loo") #loo




