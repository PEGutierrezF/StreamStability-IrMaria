










install.packages('semPlot')
library(semPlot)

# cleans global environment
rm(list = ls())

data_pm <- read.xlsx("data/data_SEM.xlsx", detectDates = TRUE)
head(data_pm)
summary(data_pm)

df <- mardia(data_pm)
df$uv.shapiro


mod1 <- '
epilithon ~ canopy + decapod
decapod ~ canopy + epilithon
macroinvertebrates ~ canopy + epilithon + decapod
'

fit <- cfa(mod1, data = data_pm, estimator = "ML")

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle')

