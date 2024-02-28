













# cleans global environment
rm(list = ls())

data_sem <- read.xlsx("data/data_SEM.xlsx", detectDates = TRUE)
head(data_sem)

library(mvnormalTest)
df <- mardia(data_sem)
df$uv.shapiro

mod1 <- '
decapod ~ epilithon
decapod ~ canopy + epilithon

epilithon ~ canopy
epilithon ~ decapod

macroinvertebrates ~ decapod
macroinvertebrates ~ epilithon 
macroinvertebrates ~ canopy + epilithon + decapod'
'

fit1 <- sem(mod1, data=data_sem, std.lv = TRUE, estimator = "MLM")
summary(fit1)

fitMeasures(fit1, c("chisq.scaled", "df.scaled", "pvalue.scaled"))


References
https://bookdown.org/bean_jerry/using_r_for_social_work_research/structural-equation-modeling.html