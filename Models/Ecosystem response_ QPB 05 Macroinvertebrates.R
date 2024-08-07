



# --------------------------------------------------------
# Long-term ecosystem response: Macroinvertebrate density
# Prieta B
# Date: Sun Jul 21 2024 14:34:56
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------



# cleans global environment
rm(list = ls())



# Create a data frame with Decapoda_QPA data (2017-10-05 (after H. Maria, time 0) to 2022-09-01)
macros_QPB <- c(0.416040634, 
                0.759091842, 1.909426685, -1.686774259, -0.173728192, 1.748084406, 
                -0.356049749, -1.608812717, -1.877451187, -1.123304901, 0.010575526, 
                -0.222518356, -0.392417393, -1.138809088, -0.722981193, 0.235697317, 
                0.26298946, -0.915665536, -0.241446366, -0.430157721, -0.722981193, 
                -0.061250208, -2.078816347, -0.993627078, -0.765092678, -0.662963183, 
                -0.061250208, -0.966958831, -0.831784052, -1.154557445, -0.235097138, 
                0.30442665, 0.501400483, 0.086802892, -1.138809088, -1.078184466, 
                0.441832752, -0.294713744, 0.292176462, -0.003007521, -0.033590944, 
                -0.046091107, 0.132028763, 0.309954708, -0.797507196, -0.604135803, 
                -0.561118418, 0.072390496, 0.624797603, 0.708050586, -0.758286483, 
                0.540996501, 0.260283098, -0.262965045, 0.922875611, 0.787638024, 
                0.193272388, -1.020650747, -0.262965045, 0.348836496)



event <- seq(1, length(macros_QPB))
data <- data.frame(event, macros_QPB)


hist(macros_QPB)
shapiro.test(macros_QPB)

###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(macros_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared_mod.1 <- summary(mod.1)$r.squared
p_value_mod.1 <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squar:", r_squared_mod.1, "\n")
cat("P-value:", p_value_mod.1, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPA and Trend Line",
       x = "Event",
       y = "Macroinvertebrates QPB") +
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
mod.2 <- nlsLM(macros_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$macros_QPB
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
mod.2.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Macroinvertebrates QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Macoinvertebrates QPB") +
  theme_minimal()

mod.2.plot



###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(macros_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(macros_QPB), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, macros_QPA = predictions), 
            aes(x = event, y = macros_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Macros QPB") +
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
mod.4 <- nlsLM(macros_QPB ~ logistic_function(event, A, B, C, D),
               data = data,
               start = start_params)

# Check the summary of the model
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$macros_QPB - mean(data$macros_QPB))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(macros_QPB), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, macros_QPB = predictions), 
            aes(x = event, y = macros_QPB), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Macros QPA") +
  theme_minimal()

mod.4.plot




###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(macros_QPB ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$macros_QPA - mean(data$macros_QPB))^2)
ss_residual <- sum(residuals(mod.5)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Calculate p-values using t-distribution
coefs <- coef(mod.5)
std_errors <- sqrt(diag(vcov(mod.5)))
t_values <- coefs / std_errors
p_values <- 2 * (1 - pt(abs(t_values), df = length(data$event) - length(coefs)))

# Display results
cat("R-squared:", r_squared, "\n")
cat("Parameter estimates:\n")
print(coefs)
cat("Standard errors:\n")
print(std_errors)
cat("t-values:\n")
print(t_values)
cat("p-values:","\n")
print(p_values)


# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        macros_QPB_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = macros_QPB_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Macroinvertebrados QPB") +
  theme_minimal()

mod.5.plot




###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(macros_QPB ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$macros_QPB - mean(data$macros_QPB))^2)
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
new_data <- data.frame(event = seq(1, length(macros_QPB), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Macroinvertebrados QPB") +
  ggtitle("Exponential Curve Fitting") +
  theme_minimal()

mod.6.plot




###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Gompertz function
gompertz_asymmetric <- function(x, A, b, c, d) {
  y = A * exp(-b * exp(-c * x)) + d
  return(y)
}

mod.7 <- nlsLM(macros_QPB ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$macros_QPB - mean(data$macros_QPB))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(macros_QPB), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
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
mod.8 <- nls(macros_QPB ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(macros_QPB)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$macros_QPB - mean(data$macros_QPB))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = macros_QPB)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Macroinvertebrates QPB") +
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

n <- length(data$macros_QPB)  # Number of observations

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

