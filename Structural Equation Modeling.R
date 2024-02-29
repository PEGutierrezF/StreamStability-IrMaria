













# cleans global environment
rm(list = ls())

data_sem <- read.xlsx("data/data_SEM.xlsx", detectDates = TRUE)
head(data_sem)


df <- mardia(data_sem)
df$uv.shapiro

model <- psem(
  
  glm(epilithon ~ macroinvertebrates + decapod + canopy, poisson(link = "log"),
      data = data_sem, na.action = na.exclude),
  
  glm(decapod ~  epilithon + canopy, poisson(link = "log"),
     data = data_sem, na.action = na.exclude),

  glm(macroinvertebrates ~  epilithon + decapod + canopy, poisson(link = "log"),
      data = data_sem, na.action = na.exclude)

  )

summary(model, .progressBar = F)


# -------------------------------------------------------------------------


