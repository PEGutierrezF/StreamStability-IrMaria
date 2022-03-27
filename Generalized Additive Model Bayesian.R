



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
###########################################################################

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")

cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))


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



###########################################################################
# Leaf litter Prieta A --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

ll.A <- Trajectories %>%
  filter(stream =="QPA", variable =="Leaf_litter")

ll.A$date <- as.integer(as.Date(ll.A$date, format = "%Y-%m-%d"))
descdist(ll.A$value, discrete=FALSE, boot=500)


# Model 1 Leaf litter Prieta A "cc" -----------------------------------------------

priors.ll_A.cc = get_prior(value ~ s(date, bs="cc", k = 8),
                           data = ll.A, family = gaussian())
priors.ll_A.cc

ll.qp_A.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 8)),
                              data = ll.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_A.cc)

summary(ll.qp_A.Bayes.cc)

posterior_summary(ll.qp_A.Bayes.cc)
ll.qp_A.Bayes.cc$fit

prior_summary(ll.qp_A.Bayes.cc)
get_posterior_beliefs_about_hypotheses(ll.qp_A.Bayes.cc)

plot(ll.qp_A.Bayes.cc)

plot(conditional_effects(ll.qp_A.Bayes.cc), points = TRUE)
msms <- conditional_smooths(ll.qp_A.Bayes.cc)
plot(msms)

pp_check(ll.qp_A.Bayes.cc, ndraws = 100)

mcmc_plot(ll.qp_A.Bayes.cc, 
          type = "areas",
          prob = 0.95)


# Model 2 Leaf litter Prieta A "cr" ----------------------------------------

priors.ll_A.cr = get_prior(value ~ s(date, bs="cr", k = 8),
                           data = ll.A, family = gaussian())
priors.ll_A.cr

ll.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 8)),
                              data = ll.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_A.cr)

summary(ll.qp_A.Bayes.cr)
plot(ll.qp_A.Bayes.cr)

plot(conditional_effects(ll.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(ll.qp_A.Bayes.cr)
plot(msms)

pp_check(ll.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(ll.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Leaf litter Prieta A "cs" -----------------------------------------------------

priors.ll_A.cs = get_prior(value ~ s(date, bs="cs", k = 8),
                           data = ll.A, family = gaussian())
priors.ll_A.cs

ll.qp_A.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 8)),
                              data = ll.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_A.cs)


summary(ll.qp_A.Bayes.cs)
plot(ll.qp_A.Bayes.cs)

ll.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(ll.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(ll.qp_A.Bayes.cs)
plot(msms)

pp_check(ll.qp_A.Bayes.cs, ndraws = 100)

mcmc_plot(ll.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Leaf litter Prieta A "ps" -----------------------------------------------------

priors.ll_A.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                           data = ll.A, family = gaussian())

ll.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                              data = ll.A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ll_A.ps)

summary(ll.qp_A.Bayes.ps)
plot(ll.qp_A.Bayes.ps)
plot(conditional_effects(ll.qp_A.Bayes.ps), points = TRUE)

pp_check(ll.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(ll.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cc ##############################################
# Model 5 Leaf litter Prieta A "cp" --------------------------------------------------------

priors.ll_A.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                           data = ll.A, family = gaussian())

ll.qp_A.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                              data = ll.A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ll_A.cp)

summary(ll.qp_A.Bayes.cp)
ll.qp_A.Bayes.cp$fit

plot(ll.qp_A.Bayes.cp)
plot(conditional_effects(ll.qp_A.Bayes.cp), points = TRUE)

pp_check(ll.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(ll.qp_A.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(ll.qp_A.Bayes.cc)
bayes_R2(ll.qp_A.Bayes.cr)
bayes_R2(ll.qp_A.Bayes.cs)
bayes_R2(ll.qp_A.Bayes.ps)
bayes_R2(ll.qp_A.Bayes.cp)


loo.cc <- loo(ll.qp_A.Bayes.cc)
loo.cr <- loo(ll.qp_A.Bayes.cr)
loo.cs <- loo(ll.qp_A.Bayes.cs)
loo.ps <- loo(ll.qp_A.Bayes.ps)
loo.cp <- loo(ll.qp_A.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(ll.qp_A.Bayes.cc, ll.qp_A.Bayes.cr, 
              ll.qp_A.Bayes.cs, ll.qp_A.Bayes.ps, 
              ll.qp_A.Bayes.cp, weights = "loo") #loo


###########################################################################
# Leaf litter Prieta B --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories <- read.csv("data/Trajectories.csv")

ll.B <- Trajectories %>%
  filter(stream =="QPB", variable =="Leaf_litter")

ll.B$date <- as.integer(as.Date(ll.B$date, format = "%Y-%m-%d"))
descdist(ll.B$value, discrete=FALSE, boot=500)



# Model 1 Leaf litter Prieta B "cc" -----------------------------------------------

priors.ll_B.cc = get_prior(value ~ s(date, bs="cc", k = 8),
                           data = ll.B, family = gaussian())
priors.ll_B.cc

ll.qp_B.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 8)),
                              data = ll.B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_B.cc)

summary(ll.qp_B.Bayes.cc)

posterior_summary(ll.qp_B.Bayes.cc)
ll.qp_B.Bayes.cc$fit

prior_summary(ll.qp_B.Bayes.cc)
get_posterior_beliefs_about_hypotheses(ll.qp_B.Bayes.cc)

plot(ll.qp_B.Bayes.cc)

plot(conditional_effects(ll.qp_B.Bayes.cc), points = TRUE)
msms <- conditional_smooths(ll.qp_B.Bayes.cc)
plot(msms)

pp_check(ll.qp_B.Bayes.cc, ndraws = 100)

mcmc_plot(ll.qp_B.Bayes.cc, 
          type = "areas",
          prob = 0.95)


# Model 2 Leaf litter Prieta B "cr" ----------------------------------------


priors.ll_B.cr = get_prior(value ~ s(date, bs="cr", k = 8),
                           data = ll.B, family = gaussian())
priors.ll_B.cr

ll.qp_B.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 8)),
                              data = ll.B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_B.cr)

summary(ll.qp_B.Bayes.cr)
plot(ll.qp_B.Bayes.cr)

plot(conditional_effects(ll.qp_B.Bayes.cr), points = TRUE)
msms <- conditional_smooths(ll.qp_B.Bayes.cr)
plot(msms)

pp_check(ll.qp_B.Bayes.cr, ndraws = 100)

mcmc_plot(ll.qp_B.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Leaf litter Prieta B "cs" -----------------------------------------------------

priors.ll_B.cs = get_prior(value ~ s(date, bs="cs", k = 8),
                           data = ll.B, family = gaussian())
priors.ll_B.cs

ll.qp_B.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 8)),
                              data = ll.B, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ll_B.cs)


summary(ll.qp_B.Bayes.cs)
plot(ll.qp_B.Bayes.cs)

ll.qp_B.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(ll.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(ll.qp_A.Bayes.cs)
plot(msms)

pp_check(ll.qp_B.Bayes.cs, ndraws = 100)

mcmc_plot(ll.qp_B.Bayes.cs, 
          type = "areas",
          prob = 0.95)

############################ Best model -> ps #############################
# Model 4 Leaf litter Prieta B "ps" -----------------------------------------------------

priors.ll_B.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                           data = ll.B, family = gaussian())

ll.qp_B.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                              data = ll.B, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ll_B.ps)

summary(ll.qp_B.Bayes.ps)
plot(ll.qp_B.Bayes.ps)
plot(conditional_effects(ll.qp_B.Bayes.ps), points = TRUE)

pp_check(ll.qp_B.Bayes.ps, ndraws = 100)

mcmc_plot(ll.qp_B.Bayes.ps, 
          type = "areas",
          prob = 0.95)

# Model 5 Leaf litter Prieta B "cp" --------------------------------------------------------

priors.ll_B.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                           data = ll.B, family = gaussian())

ll.qp_B.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                              data = ll.B, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ll_B.cp)

summary(ll.qp_B.Bayes.cp)
plot(ll.qp_B.Bayes.cp)
plot(conditional_effects(ll.qp_B.Bayes.cp), points = TRUE)

pp_check(ll.qp_B.Bayes.cp, ndraws = 100)

mcmc_plot(ll.qp_B.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(ll.qp_B.Bayes.cc)
bayes_R2(ll.qp_B.Bayes.cr)
bayes_R2(ll.qp_B.Bayes.cs)
bayes_R2(ll.qp_B.Bayes.ps)
bayes_R2(ll.qp_B.Bayes.cp)


loo.cc <- loo(ll.qp_B.Bayes.cc)
loo.cr <- loo(ll.qp_B.Bayes.cr)
loo.cs <- loo(ll.qp_B.Bayes.cs)
loo.ps <- loo(ll.qp_B.Bayes.ps)
loo.cp <- loo(ll.qp_B.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(ll.qp_B.Bayes.cc, ll.qp_B.Bayes.cr, 
              ll.qp_B.Bayes.cs, ll.qp_B.Bayes.ps, 
              ll.qp_B.Bayes.cp, weights = "loo") #loo




###########################################################################
# Chlorophyll Prieta A --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

ch.A <- Trajectories %>%
  filter(stream =="QPA", variable =="Chla")

ch.A$date <- as.integer(as.Date(ch.A$date, format = "%Y-%m-%d"))
descdist(ch.A$value, discrete=FALSE, boot=500)
hist(ch.A$value)

# Model 1 Chlorophyll Prieta A "cc" -----------------------------------------------

priors.chl_A.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                           data = ch.A, family = gaussian())

chla.qp_A.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                              data = ch.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.chl_A.cc)

summary(chla.qp_A.Bayes.cc)

posterior_summary(chla.qp_A.Bayes.cc)
chla.qp_A.Bayes.cc$fit

prior_summary(chla.qp_A.Bayes.cc)
get_posterior_beliefs_about_hypotheses(chla.qp_A.Bayes.cc)

plot(chla.qp_A.Bayes.cc)

plot(conditional_effects(chla.qp_A.Bayes.cc), points = TRUE)
msms <- conditional_smooths(chla.qp_A.Bayes.cc)
plot(msms)

pp_check(chla.qp_A.Bayes.cc, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr ##############################################
# Model 2 Chlorophyll Prieta A "cr" ----------------------------------------

priors.chl_A.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                           data = ch.A, family = gaussian())

chla.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                              data = ch.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.chl_A.cr)

summary(chla.qp_A.Bayes.cr)
plot(chla.qp_A.Bayes.cr)

plot(conditional_effects(chla.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(chla.qp_A.Bayes.cr)
plot(msms)

pp_check(chla.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Chlorophyll Prieta A "cs" -----------------------------------------------------

priors.ch_A.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                           data = ch.A, family = gaussian())

chla.qp_A.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                              data = ch.A, family = gaussian(), cores = 1, 
                              seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                              refresh = 0, control = list(adapt_delta = 0.99),
                              prior = priors.ch_A.cs)


summary(chla.qp_A.Bayes.cs)
plot(chla.qp_A.Bayes.cs)

chla.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(chla.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(chla.qp_A.Bayes.cs)
plot(msms)

pp_check(chla.qp_A.Bayes.cs, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Chlorophyll Prieta A "ps" -----------------------------------------------------

priors.ch_A.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                           data = ch.A, family = gaussian())

chla.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                              data = ch.A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ch_A.ps)

summary(chla.qp_A.Bayes.ps)
plot(chla.qp_A.Bayes.ps)
plot(conditional_effects(chla.qp_A.Bayes.ps), points = TRUE)

pp_check(chla.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)


# Model 5 Chlorophyll Prieta A "cp" --------------------------------------------------------

priors.ch_A.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                           data = ch.A, family = gaussian())

chla.qp_A.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                              data = ch.A, family = gaussian(), cores = 1, 
                              seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.ch_A.cp)

summary(chla.qp_A.Bayes.cp)
chla.qp_A.Bayes.cp$fit

plot(ll.qp_A.Bayes.cp)
plot(conditional_effects(chla.qp_A.Bayes.cp), points = TRUE)

pp_check(chla.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(chla.qp_A.Bayes.cc)
bayes_R2(chla.qp_A.Bayes.cr)
bayes_R2(chla.qp_A.Bayes.cs)
bayes_R2(chla.qp_A.Bayes.ps)
bayes_R2(chla.qp_A.Bayes.cp)


loo.cc <- loo(chla.qp_A.Bayes.cc)
loo.cr <- loo(chla.qp_A.Bayes.cr)
loo.cs <- loo(chla.qp_A.Bayes.cs)
loo.ps <- loo(chla.qp_A.Bayes.ps)
loo.cp <- loo(chla.qp_A.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(chla.qp_A.Bayes.cc, chla.qp_A.Bayes.cr, 
              chla.qp_A.Bayes.cs, chla.qp_A.Bayes.ps, 
              chla.qp_A.Bayes.cp, weights = "loo") #loo




###########################################################################
# Chlorophyll Prieta B --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

ch.B <- Trajectories %>%
  filter(stream =="QPB", variable =="Chla")

ch.B$date <- as.integer(as.Date(ch.B$date, format = "%Y-%m-%d"))
descdist(ch.B$value, discrete=FALSE, boot=500)


# Model 1 Chlorophyll Prieta B "cc" -----------------------------------------------

priors.chl_B.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                            data = ch.B, family = gaussian())

chla.qp_B.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                                data = ch.B, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.chl_B.cc)

summary(chla.qp_B.Bayes.cc)

posterior_summary(chla.qp_B.Bayes.cc)
chla.qp_A.Bayes.cc$fit

prior_summary(chla.qp_B.Bayes.cc)
get_posterior_beliefs_about_hypotheses(chla.qp_B.Bayes.cc)

plot(chla.qp_B.Bayes.cc)

plot(conditional_effects(chla.qp_B.Bayes.cc), points = TRUE)
msms <- conditional_smooths(chla.qp_B.Bayes.cc)
plot(msms)

pp_check(chla.qp_B.Bayes.cc, ndraws = 100)

mcmc_plot(chla.qp_B.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr ##############################################
# Model 2 Chlorophyll Prieta B "cr" ----------------------------------------

priors.chl_B.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                            data = ch.B, family = gaussian())

chla.qp_B.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                                data = ch.B, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.chl_B.cr)

summary(chla.qp_B.Bayes.cr)
plot(chla.qp_B.Bayes.cr)

plot(conditional_effects(chla.qp_B.Bayes.cr), points = TRUE)
msms <- conditional_smooths(chla.qp_B.Bayes.cr)
plot(msms)

pp_check(chla.qp_B.Bayes.cr, ndraws = 100)

mcmc_plot(chla.qp_B.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Chlorophyll Prieta B "cs" -----------------------------------------------------

priors.ch_B.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                           data = ch.B, family = gaussian())

chla.qp_B.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                                data = ch.B, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.ch_B.cs)


summary(chla.qp_B.Bayes.cs)
plot(chla.qp_B.Bayes.cs)

chla.qp_B.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(chla.qp_B.Bayes.cs), points = TRUE)
msms <- conditional_smooths(chla.qp_B.Bayes.cs)
plot(msms)

pp_check(chla.qp_B.Bayes.cs, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Chlorophyll Prieta B "ps" -----------------------------------------------------

priors.ch_B.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                           data = ch.B, family = gaussian())

chla.qp_B.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                                data = ch.B, family = gaussian(), cores = 1, 
                                seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                                control = list(adapt_delta = 0.99),
                                prior = priors.ch_B.ps)

summary(chla.qp_B.Bayes.ps)
plot(chla.qp_B.Bayes.ps)
plot(conditional_effects(chla.qp_B.Bayes.ps), points = TRUE)

pp_check(chla.qp_B.Bayes.ps, ndraws = 100)

mcmc_plot(chla.qp_B.Bayes.ps, 
          type = "areas",
          prob = 0.95)

# Model 5 Chlorophyll Prieta B "cp" --------------------------------------------------------

priors.ch_B.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                           data = ch.B, family = gaussian())

chla.qp_B.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                                data = ch.B, family = gaussian(), cores = 1, 
                                seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                                control = list(adapt_delta = 0.99),
                                prior = priors.ch_B.cp)

summary(chla.qp_B.Bayes.cp)
chla.qp_B.Bayes.cp$fit

plot(chla.qp_B.Bayes.cp)
plot(conditional_effects(chla.qp_B.Bayes.cp), points = TRUE)

pp_check(chla.qp_B.Bayes.cp, ndraws = 100)

mcmc_plot(chla.qp_B.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(chla.qp_B.Bayes.cc)
bayes_R2(chla.qp_B.Bayes.cr)
bayes_R2(chla.qp_B.Bayes.cs)
bayes_R2(chla.qp_B.Bayes.ps)
bayes_R2(chla.qp_B.Bayes.cp)


loo.cc <- loo(chla.qp_B.Bayes.cc)
loo.cr <- loo(chla.qp_B.Bayes.cr)
loo.cs <- loo(chla.qp_B.Bayes.cs)
loo.ps <- loo(chla.qp_B.Bayes.ps)
loo.cp <- loo(chla.qp_B.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(chla.qp_B.Bayes.cc, chla.qp_B.Bayes.cr, 
              chla.qp_B.Bayes.cs, chla.qp_B.Bayes.ps, 
              chla.qp_B.Bayes.cp, weights = "loo") #loo



###########################################################################
# Macroinvertebrates Prieta A --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

miv.A <- Trajectories %>%
  filter(stream =="QPA", variable =="macroinvertebrates")

miv.A$date <- as.integer(as.Date(miv.A$date, format = "%Y-%m-%d"))
descdist(miv.A$value, discrete=FALSE, boot=500)
hist(miv.A$value)

# Model 1 Macroinvertebrates Prieta A "cc" -----------------------------------------------

priors.miv_A.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                            data = miv.A, family = gaussian())

miv.qp_A.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                                data = miv.A, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.miv_A.cc)

summary(miv.qp_A.Bayes.cc)

posterior_summary(miv.qp_A.Bayes.cc)
miv.qp_A.Bayes.cc$fit

prior_summary(miv.qp_A.Bayes.cc)
get_posterior_beliefs_about_hypotheses(miv.qp_A.Bayes.cc)

plot(miv.qp_A.Bayes.cc)

plot(conditional_effects(miv.qp_A.Bayes.cc), points = TRUE)
msms <- conditional_smooths(miv.qp_A.Bayes.cc)
plot(msms)

pp_check(miv.qp_A.Bayes.cc, ndraws = 100)

mcmc_plot(miv.qp_A.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr #####################################
# Model 2 Macroinvertebrates Prieta A "cr" ----------------------------------------

priors.miv_A.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                            data = miv.A, family = gaussian())

miv.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                                data = ch.A, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.miv_A.cr)

summary(miv.qp_A.Bayes.cr)
plot(miv.qp_A.Bayes.cr)

plot(conditional_effects(miv.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(miv.qp_A.Bayes.cr)
plot(msms)

pp_check(miv.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(miv.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Macroinvertebrates Prieta A "cs" -----------------------------------------------------

priors.miv_A.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                           data = miv.A, family = gaussian())

miv.qp_A.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                                data = miv.A, family = gaussian(), cores = 1, 
                                seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                                refresh = 0, control = list(adapt_delta = 0.99),
                                prior = priors.miv_A.cs)


summary(miv.qp_A.Bayes.cs)
plot(miv.qp_A.Bayes.cs)

miv.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(miv.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(miv.qp_A.Bayes.cs)
plot(msms)

pp_check(miv.qp_A.Bayes.cs, ndraws = 100)

mcmc_plot(chla.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Macroinvertebrates Prieta A "ps" -----------------------------------------------------

priors.miv_A.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                           data = miv.A, family = gaussian())

miv.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                                data = miv.A, family = gaussian(), cores = 1, 
                                seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                                control = list(adapt_delta = 0.99),
                                prior = priors.miv_A.ps)

summary(miv.qp_A.Bayes.ps)
plot(miv.qp_A.Bayes.ps)
plot(conditional_effects(miv.qp_A.Bayes.ps), points = TRUE)

pp_check(miv.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(miv.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)


# Model 5 Macroinvertebrates Prieta A "cp" --------------------------------------------------------

priors.miv_A.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                           data = miv.A, family = gaussian())

miv.qp_A.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                                data = miv.A, family = gaussian(), cores = 1, 
                                seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                                control = list(adapt_delta = 0.99),
                                prior = priors.miv_A.cp)

summary(miv.qp_A.Bayes.cp)
chla.qp_A.Bayes.cp$fit

plot(miv.qp_A.Bayes.cp)
plot(conditional_effects(miv.qp_A.Bayes.cp), points = TRUE)

pp_check(miv.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(miv.qp_A.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(miv.qp_A.Bayes.cc)
bayes_R2(miv.qp_A.Bayes.cr)
bayes_R2(miv.qp_A.Bayes.cs)
bayes_R2(miv.qp_A.Bayes.ps)
bayes_R2(miv.qp_A.Bayes.cp)


loo.cc <- loo(miv.qp_A.Bayes.cc)
loo.cr <- loo(miv.qp_A.Bayes.cr)
loo.cs <- loo(miv.qp_A.Bayes.cs)
loo.ps <- loo(miv.qp_A.Bayes.ps)
loo.cp <- loo(miv.qp_A.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(miv.qp_A.Bayes.cc, miv.qp_A.Bayes.cr, 
              miv.qp_A.Bayes.cs, miv.qp_A.Bayes.ps, 
              miv.qp_A.Bayes.cp, weights = "loo") #loo




###########################################################################
# Macroinvertebrates Prieta B --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

miv.B <- Trajectories %>%
  filter(stream =="QPB", variable =="macroinvertebrates")

miv.B$date <- as.integer(as.Date(miv.B$date, format = "%Y-%m-%d"))
descdist(miv.B$value, discrete=FALSE, boot=500)
hist(miv.B$value)

# Model 1 Macroinvertebrates Prieta B "cc" -----------------------------------------------

priors.miv_B.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                            data = miv.B, family = gaussian())

miv.qp_B.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                               data = miv.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.miv_B.cc)

summary(miv.qp_B.Bayes.cc)

posterior_summary(miv.qp_B.Bayes.cc)
miv.qp_B.Bayes.cc$fit

prior_summary(miv.qp_B.Bayes.cc)
get_posterior_beliefs_about_hypotheses(miv.qp_B.Bayes.cc)

plot(miv.qp_B.Bayes.cc)

plot(conditional_effects(miv.qp_B.Bayes.cc), points = TRUE)
msms <- conditional_smooths(miv.qp_B.Bayes.cc)
plot(msms)

pp_check(miv.qp_B.Bayes.cc, ndraws = 100)

mcmc_plot(miv.qp_B.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr #####################################
# Model 2 Macroinvertebrates Prieta B "cr" ----------------------------------------

priors.miv_B.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                            data = miv.B, family = gaussian())

miv.qp_B.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                               data = ch.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.miv_B.cr)

summary(miv.qp_B.Bayes.cr)
plot(miv.qp_B.Bayes.cr)

plot(conditional_effects(miv.qp_B.Bayes.cr), points = TRUE)
msms <- conditional_smooths(miv.qp_B.Bayes.cr)
plot(msms)

pp_check(miv.qp_B.Bayes.cr, ndraws = 100)

mcmc_plot(miv.qp_B.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Macroinvertebrates Prieta B "cs" -----------------------------------------------------

priors.miv_A.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                            data = miv.A, family = gaussian())

miv.qp_B.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                               data = miv.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.miv_B.cs)


summary(miv.qp_B.Bayes.cs)
plot(miv.qp_B.Bayes.cs)

miv.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(miv.qp_B.Bayes.cs), points = TRUE)
msms <- conditional_smooths(miv.qp_B.Bayes.cs)
plot(msms)

pp_check(miv.qp_B.Bayes.cs, ndraws = 100)

mcmc_plot(miv.qp_B.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Macroinvertebrates Prieta B "ps" -----------------------------------------------------

priors.miv_B.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                            data = miv.B, family = gaussian())

miv.qp_B.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                               data = miv.B, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.miv_B.ps)

summary(miv.qp_B.Bayes.ps)
plot(miv.qp_B.Bayes.ps)
plot(conditional_effects(miv.qp_B.Bayes.ps), points = TRUE)

pp_check(miv.qp_B.Bayes.ps, ndraws = 100)

mcmc_plot(miv.qp_B.Bayes.ps, 
          type = "areas",
          prob = 0.95)


# Model 5 Macroinvertebrates Prieta B "cp" --------------------------------------------------------

priors.miv_B.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                            data = miv.B, family = gaussian())

miv.qp_B.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                               data = miv.B, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.miv_B.cp)

summary(miv.qp_B.Bayes.cp)
miv.qp_B.Bayes.cp$fit

plot(miv.qp_B.Bayes.cp)
plot(conditional_effects(miv.qp_B.Bayes.cp), points = TRUE)

pp_check(miv.qp_B.Bayes.cp, ndraws = 100)

mcmc_plot(miv.qp_B.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(miv.qp_B.Bayes.cc)
bayes_R2(miv.qp_B.Bayes.cr)
bayes_R2(miv.qp_B.Bayes.cs)
bayes_R2(miv.qp_B.Bayes.ps)
bayes_R2(miv.qp_B.Bayes.cp)


loo.cc <- loo(miv.qp_B.Bayes.cc)
loo.cr <- loo(miv.qp_B.Bayes.cr)
loo.cs <- loo(miv.qp_B.Bayes.cs)
loo.ps <- loo(miv.qp_B.Bayes.ps)
loo.cp <- loo(miv.qp_B.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(miv.qp_B.Bayes.cc, miv.qp_B.Bayes.cr, 
              miv.qp_B.Bayes.cs, miv.qp_B.Bayes.ps, 
              miv.qp_B.Bayes.cp, weights = "loo") #loo



###########################################################################
# Shrimp Abundance Prieta A --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

shr.A <- Trajectories %>%
  filter(stream =="QPA", variable =="Shrimps")

shr.A$date <- as.integer(as.Date(shr.A$date, format = "%Y-%m-%d"))
descdist(shr.A$value, discrete=FALSE, boot=500)
hist(shr.A$value)

# Model 1 Shrimp Prieta A "cc" -----------------------------------------------

priors.shr_A.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                            data = shr.A, family = gaussian())

shr.qp_A.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                               data = shr.A, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.shr_A.cc)

summary(shr.qp_A.Bayes.cc)

posterior_summary(shr.qp_A.Bayes.cc)
shr.qp_A.Bayes.cc$fit

prior_summary(shr.qp_A.Bayes.cc)
get_posterior_beliefs_about_hypotheses(shr.qp_A.Bayes.cc)

plot(shr.qp_A.Bayes.cc)

plot(conditional_effects(shr.qp_A.Bayes.cc), points = TRUE)
msms <- conditional_smooths(shr.qp_A.Bayes.cc)
plot(msms)

pp_check(shr.qp_A.Bayes.cc, ndraws = 100)

mcmc_plot(shr.qp_A.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr #####################################
# Model 2 Shrimp Prieta A "cr" ----------------------------------------

priors.shr_A.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                            data = shr.A, family = gaussian())

shr.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                               data = shr.A, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.shr_A.cr)

summary(shr.qp_A.Bayes.cr)
plot(shr.qp_A.Bayes.cr)

plot(conditional_effects(shr.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(shr.qp_A.Bayes.cr)
plot(msms)

pp_check(shr.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(shr.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Shrimp Prieta A "cs" -----------------------------------------------------

priors.shr_A.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                            data = shr.A, family = gaussian())

shr.qp_A.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                               data = shr.A, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.shr_A.cs)


summary(shr.qp_A.Bayes.cs)
plot(shr.qp_A.Bayes.cs)

shr.qp_A.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(shr.qp_A.Bayes.cs), points = TRUE)
msms <- conditional_smooths(shr.qp_A.Bayes.cs)
plot(msms)

pp_check(shr.qp_A.Bayes.cs, ndraws = 100)

mcmc_plot(shr.qp_A.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Shrimp Prieta A "ps" -----------------------------------------------------

priors.shr_A.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                            data = shr.A, family = gaussian())

shr.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                               data = shr.A, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.shr_A.ps)

summary(shr.qp_A.Bayes.ps)
plot(shr.qp_A.Bayes.ps)
plot(conditional_effects(shr.qp_A.Bayes.ps), points = TRUE)

pp_check(shr.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(shr.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)


# Model 5 Shrimp Prieta A "cp" --------------------------------------------------------

priors.shr_A.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                            data = shr.A, family = gaussian())

shr.qp_A.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                               data = shr.A, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.shr_A.cp)

summary(shr.qp_A.Bayes.cp)
shr.qp_A.Bayes.cp$fit

plot(shr.qp_A.Bayes.cp)
plot(conditional_effects(shr.qp_A.Bayes.cp), points = TRUE)

pp_check(shr.qp_A.Bayes.cp, ndraws = 100)

mcmc_plot(shr.qp_A.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(shr.qp_A.Bayes.cc)
bayes_R2(shr.qp_A.Bayes.cr)
bayes_R2(shr.qp_A.Bayes.cs)
bayes_R2(shr.qp_A.Bayes.ps)
bayes_R2(shr.qp_A.Bayes.cp)


loo.cc <- loo(shr.qp_A.Bayes.cc)
loo.cr <- loo(shr.qp_A.Bayes.cr)
loo.cs <- loo(shr.qp_A.Bayes.cs)
loo.ps <- loo(shr.qp_A.Bayes.ps)
loo.cp <- loo(shr.qp_A.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(shr.qp_A.Bayes.cc, shr.qp_A.Bayes.cr, 
              shr.qp_A.Bayes.cs, shr.qp_A.Bayes.ps, 
              shr.qp_A.Bayes.cp, weights = "loo") #loo





###########################################################################
# Shrimp Abundance Prieta B --------------------------------------------------------
###########################################################################

rm(list=ls())
Trajectories<- read.csv("data/Trajectories.csv")

shr.B <- Trajectories %>%
  filter(stream =="QPB", variable =="Shrimps")

shr.B$date <- as.integer(as.Date(shr.B$date, format = "%Y-%m-%d"))
descdist(shr.B$value, discrete=FALSE, boot=500)
hist(shr.B$value)

# Model 1 Shrimp Prieta B "cc" -----------------------------------------------

priors.shr_B.cc = get_prior(value ~ s(date, bs="cc", k = 5),
                            data = shr.B, family = gaussian())

shr.qp_B.Bayes.cc <- brms::brm(bf(value ~ s(date, bs="cc", k = 5)),
                               data = shr.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.SHR_B.cc)

summary(shr.qp_B.Bayes.cc)

posterior_summary(shr.qp_B.Bayes.cc)
shr.qp_B.Bayes.cc$fit

prior_summary(shr.qp_B.Bayes.cc)
get_posterior_beliefs_about_hypotheses(shr.qp_B.Bayes.cc)

plot(shr.qp_B.Bayes.cc)

plot(conditional_effects(shr.qp_B.Bayes.cc), points = TRUE)
msms <- conditional_smooths(shr.qp_B.Bayes.cc)
plot(msms)

pp_check(shr.qp_B.Bayes.cc, ndraws = 100)

mcmc_plot(shr.qp_B.Bayes.cc, 
          type = "areas",
          prob = 0.95)

############################ Best model -> cr #####################################
# Model 2 Shrimp Prieta B "cr" ----------------------------------------

priors.shr_B.cr = get_prior(value ~ s(date, bs="cr", k = 5),
                            data = shr.B, family = gaussian())

shr.qp_B.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr", k = 5)),
                               data = shr.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.shr_B.cr)

summary(shr.qp_B.Bayes.cr)
plot(shr.qp_B.Bayes.cr)

plot(conditional_effects(shr.qp_B.Bayes.cr), points = TRUE)
msms <- conditional_smooths(shr.qp_B.Bayes.cr)
plot(msms)

pp_check(shr.qp_B.Bayes.cr, ndraws = 100)

mcmc_plot(shr.qp_B.Bayes.cr, 
          type = "areas",
          prob = 0.95)


# Model 3 Shrimp Prieta B "cs" -----------------------------------------------------

priors.shr_B.cs = get_prior(value ~ s(date, bs="cs", k = 5),
                            data = shr.B, family = gaussian())

shr.qp_B.Bayes.cs <- brms::brm(bf(value ~ s(date, bs="cs", k = 5)),
                               data = shr.B, family = gaussian(), cores = 1, 
                               seed = 14, warmup = 8000, iter = 10000, thin = 1, 
                               refresh = 0, control = list(adapt_delta = 0.99),
                               prior = priors.shr_B.cs)


summary(shr.qp_B.Bayes.cs)
plot(shr.qp_B.Bayes.cs)

shr.qp_B.Bayes.cs%>%
  plot(combo = c("hist", "trace"), widths = c(1, 1.5),
       theme = theme_bw(base_size = 16))

plot(conditional_effects(shr.qp_B.Bayes.cs), points = TRUE)
msms <- conditional_smooths(shr.qp_B.Bayes.cs)
plot(msms)

pp_check(shr.qp_B.Bayes.cs, ndraws = 100)

mcmc_plot(shr.qp_B.Bayes.cs, 
          type = "areas",
          prob = 0.95)


# Model 4 Shrimp Prieta B "ps" -----------------------------------------------------

priors.shr_B.ps = get_prior(value ~ s(date, bs="ps", k = 8),
                            data = shr.B, family = gaussian())

shr.qp_B.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k = 8)),
                               data = shr.B, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.shr_B.ps)

summary(shr.qp_B.Bayes.ps)
plot(shr.qp_B.Bayes.ps)
plot(conditional_effects(shr.qp_B.Bayes.ps), points = TRUE)

pp_check(shr.qp_B.Bayes.ps, ndraws = 100)

mcmc_plot(shr.qp_B.Bayes.ps, 
          type = "areas",
          prob = 0.95)


  # Model 5 Shrimp Prieta B "cp" --------------------------------------------------------

priors.shr_B.cp = get_prior(value ~ s(date, bs="cp", k = 8),
                            data = shr.B, family = gaussian())

shr.qp_B.Bayes.cp <- brms::brm(bf(value ~ s(date, bs="cp", k=8)),
                               data = shr.B, family = gaussian(), cores = 1, 
                               seed = 14,warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.shr_B.cp)

summary(shr.qp_B.Bayes.cp)
shr.qp_B.Bayes.cp$fit

plot(shr.qp_B.Bayes.cp)
plot(conditional_effects(shr.qp_B.Bayes.cp), points = TRUE)

pp_check(shr.qp_B.Bayes.cp, ndraws = 100)

mcmc_plot(shr.qp_B.Bayes.cp, 
          type = "areas",
          prob = 0.95)



# Evaluate models ---------------------------------------------------------

bayes_R2(shr.qp_B.Bayes.cc)
bayes_R2(shr.qp_B.Bayes.cr)
bayes_R2(shr.qp_B.Bayes.cs)
bayes_R2(shr.qp_B.Bayes.ps)
bayes_R2(shr.qp_B.Bayes.cp)


loo.cc <- loo(shr.qp_B.Bayes.cc)
loo.cr <- loo(shr.qp_B.Bayes.cr)
loo.cs <- loo(shr.qp_B.Bayes.cs)
loo.ps <- loo(shr.qp_B.Bayes.ps)
loo.cp <- loo(shr.qp_B.Bayes.cp)

loo_compare(loo.cc, loo.cr, loo.cs, loo.ps, loo.cp)  


model_weights(shr.qp_B.Bayes.cc, shr.qp_B.Bayes.cr, 
              shr.qp_B.Bayes.cs, shr.qp_B.Bayes.ps, 
              shr.qp_B.Bayes.cp, weights = "loo") #loo



