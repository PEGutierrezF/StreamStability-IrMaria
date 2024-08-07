



# --------------------------------------------------------
# Long-term ecosystem response: Macroinvertebrate density
# Prieta A
# Date: Fri Jul 19 2024 17:33:45
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




# cleans global environment
rm(list = ls())



# Create a data frame with Decapoda_QPA data (2017-10-05 (after H. Maria, time 0) to 2022-09-01)
macros_QPA <- c(-1.163751591, -1.559647248, -0.530027831, 
                -1.531476371, -1.819158443, -0.003007521, -1.054552299, -0.490022496, -1.286353913,
                -0.138261567, -0.003007521, 0.398433437, -0.255182905, -0.352821375, 0.038296285, 
                0.631207881, -0.414514944, 0.049790664, -0.078042707, -0.084557388, -0.294713744, 
                -0.335871816, -1.037457865, -0.83832919, -0.745547457, -0.335871816, -0.987860924, 
                -0.637658495, -0.509825123, -1.531476371, -0.987860924, 0.457158723, -0.015128882, 
                0.110751364, -1.377325691, -0.758286483, -0.509825123, -1.037457866, 0.378066134, 
                -0.131388688, -0.784261969, -0.758286483, -0.649087191, 0.784902044, -1.504077397, 
                0.264902044, 0.188309599, 0.624863222, 0.534266326, 0.505405556, -0.824535868, 
                0.116113307, -1.202972304, -0.302810954, 0.547965171, 0.348836496, 0.605316467, 
                -1.979501093, -0.033590944, -0.14518201)


event <- seq(1, length(macros_QPA))
data <- data.frame(event, macros_QPA)


hist(macros_QPA)
shapiro.test(macros_QPA)

###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(macros_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared_mod.1 <- summary(mod.1)$r.squared
p_value_mod.1 <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squar:", r_squared_mod.1, "\n")
cat("P-value:", p_value_mod.1, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPA and Trend Line",
       x = "Event",
       y = "Macroinvertebrates QPA") +
  theme_minimal()

mod.1.plot



###########################################################################
# Humped yield curve (mod.2) ----------------------------------------------
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}

# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2 <- nlsLM(macros_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$macros_QPA
mean_observed <- mean(observed_values)
ss_total <- sum((observed_values - mean_observed)^2)
ss_residual <- sum((observed_values - fitted_values)^2)
r_square_mod.2 <- 1 - ss_residual / ss_total
pvalue_mod.2 <- summary(mod.2)$coefficients[4, 4]  # P-value for the 'tau' parameter

# Print R-squared value and p-values
cat("R-squared:", r_square_mod.2, "\n")
cat("P-value:", pvalue_mod.2, "\n")


# Calculate the predicted values from the model
predicted_values <- predict(mod.2, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Macroinvertebrates QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Macoinvertebrates QPA") +
  theme_minimal()

mod.2.plot



###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(macros_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, macros_QPA = predictions), 
            aes(x = event, y = macros_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Macros QPA") +
  theme_minimal()

mod.3.plot



###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Try different starting parameter values
start_params <- list(A = -1, B = 1, C = 0.1, D = median(data$event))

# Fit the model using nls.lm algorithm
mod.4 <- nlsLM(macros_QPA ~ logistic_function(event, A, B, C, D),
               data = data,
               start = start_params)

# Check the summary of the model
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, macros_QPA = predictions), 
            aes(x = event, y = macros_QPA), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Macros QPA") +
  theme_minimal()

mod.4.plot




###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(macros_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
ss_residual <- sum(residuals(mod.5)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Calculate p-values using t-distribution
coefs <- coef(mod.5)
std_errors <- sqrt(diag(vcov(mod.5)))
t_values <- coefs / std_errors
p_values <- 2 * (1 - pt(abs(t_values), df = length(data$event) - length(coefs)))

# Display results
cat("R-squared:", r_squared, "\n")
cat("Parameter estimates:\n",coefs)
cat("Standard errors:\n",std_errors)
cat("t-values:\n", t_values)
cat("p-values:",p_values,"\n")

# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        macros_QPA_pred = predict(mod.5, newdata = data))

library(ggthemes)
# Create a ggplot
mod.5.plot.macros <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = macros_QPA_pred), color = "blue") +
  
  labs(title = expression(Logarithmic~Curve~(italic(y) == a %.% log(italic(x)) + b)),
       x = "Sampling event",
       y = expression(Macroinvertebrate~density~(ind %.% m^{-2} ))) +
  geom_rangeframe() + theme_tufte() +
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray50",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray50",size = 0.5,linetype = 3))

mod.5.plot.macros


###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
# Fit the exponential curve using nlsLM with adjusted starting values
mod.6 <- nlsLM(macros_QPA ~ exponential(event, A, B, C), 
               data = data,
               start = list(A = 1, B = 0.1, C = 0),
               control = nls.lm.control(maxiter = 1000))


# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
# Calculate residual sum of squares
residual_ss <- sum(fit_summary$residuals^2)
# Calculate R-squared value
rsquared <- 1 - residual_ss / total_ss
# Extract p-values
p_values <- fit_summary$coefficients[, "Pr(>|t|)"]
# Print the results
cat("R-squared value:", rsquared, "\n")
cat("P-values for parameters:\n")
print(p_values)


# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Macroinvertebrados QPA") +
  ggtitle("Exponential Curve Fitting") +
  theme_minimal()

mod.6.plot




###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Fit the Gompertz model using nlsLM with adjusted starting values and control settings
mod.7 <- nlsLM(macros_QPA ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 0.1, c = 0.01, d = 0),
               control = nls.lm.control(maxiter = 1000, ftol = 1e-6, ptol = 1e-6))

# Check the summary of the model
summary(mod.7)


rss <- sum(residuals(mod.7)^2)
tss <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Macroinvertebrates QPA") +
  theme_minimal()

mod.7.plot



###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(macros_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(macros_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Macroinvertebrates QPA") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)


###########################################################################
# Function to compute AICc
AICc <- function(fit, return.K = FALSE) {
  n <- length(residuals(fit))
  k <- length(coef(fit)) + 1  # Including the intercept
  aic <- -2 * logLik(fit) + 2 * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  if (return.K) return(k) else return(aicc)
}

n <- length(data$macros_QPA)  # Number of observations

aic_mod.1 <- AICc(mod.1)
aic_mod.2 <- AICc(mod.2)
aic_mod.3 <- AICc(mod.3)
aic_mod.4 <- AICc(mod.4)
aic_mod.5 <- AICc(mod.5)
aic_mod.6 <- AICc(mod.6)
aic_mod.7 <- AICc(mod.7)
aic_mod.8 <- AICc(mod.8)

# Store AICc values in a vector
aic_values <- c(aic_mod.1, aic_mod.2, aic_mod.3, aic_mod.4, aic_mod.5, aic_mod.6, aic_mod.7, aic_mod.8)

# Sort AICc values in ascending order
sorted_indices <- order(aic_values)

# Print sorted AICc values and corresponding model numbers
for (i in sorted_indices) {
  cat("AICc Mod.", i, ":", aic_values[i], "\n")
}
