



# ---------------------------------------------
# Long-term ecosystem response: Canopy openness Quebrada Prieta A
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with epilithon_QPA data (2017-01-01 to 2022-09-01)
epilithon_QPA <- c(0.374941114, 0.421008704, 0.059098445, -0.356900918, -0.05326958, 
                   -0.133541362, -0.319898689, -0.340327935, -0.49053223, -0.326974628, 
                   -0.052401445, -0.424571057, -0.270306082, -0.314461617, -0.77198127, 
                   -0.525529712, 0.353432899, 0.012869536, 0.010726568, 0.172358217, 
                   -0.339336835, 0.057037776, -0.309326024, 0.229468207, 0.277150141, 
                   0.242224405, 0.006403858, -0.036876889, -0.097585005, -0.049060763, 
                   -0.655417682, -0.33919034, -0.579209274, -0.067344119, -0.275802251, 
                   -0.106530181, 0.218548616, 0.315346245, -0.104746737, -0.113798581, 
                   -1.216284093, -0.030794936, 0.128182961, 0.296216546, 0.109570674, 
                   0.135445071, 0.163041949, 0.135653819, -0.172758478, -0.042531071, 
                   0.025660758, 0.543062826, 0.494435909, -0.055966918, -0.086641353, 
                   0.009111772, 0.168652927, 0.589891, 0.359543263, 0.249273612, 
                   0.190985156, 0.776090885, 0.219440474, 0.449951267, 0.503635106, 
                   0.160896007, 0.241941622, 0.53811738, 0.148764519)


event <- seq(1, length(epilithon_QPA))
data <- data.frame(event, epilithon_QPA)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(epilithon_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPA and Trend Line",
       x = "Event",
       y = "Canopy QPA") +
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
mod.2 <- nlsLM(epilithon_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$epilithon_QPA
mean_observed <- mean(observed_values)
ss_total <- sum((observed_values - mean_observed)^2)
ss_residual <- sum((observed_values - fitted_values)^2)
r_square_mod.2 <- 1 - ss_residual / ss_total

# Print R-squared value and p-values
cat("R-squared:", r_square_mod.2, "\n")
pvalue <- summary(mod.2)$coefficients[4, 4]  # P-value for the 'tau' parameter
cat("P-value:", pvalue, "\n")


# Calculate the predicted values from the model
predicted_values <- predict(mod.2, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Canopy QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(epilithon_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, epilithon_QPA = predictions), 
            aes(x = event, y = epilithon_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.3.plot


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Define the objective function for optimization
objective_function <- function(params) {
  predicted <- logistic_function(data$event, params[1], params[2], params[3], params[4])
  sum((data$epilithon_QPA - predicted)^2)
}

# Use optim with the BFGS optimizer
initial_params <- c(min(data$epilithon_QPA), max(data$epilithon_QPA), 1, median(data$event))
result <- optim(par = initial_params, fn = objective_function, method = "BFGS")

# Fitted parameter values
fitted_params <- result$par

# Calculate predicted values using fitted parameters
data$predicted <- logistic_function(data$event, fitted_params[1], fitted_params[2], fitted_params[3], fitted_params[4])

# Calculate R-squared
SST <- sum((data$epilithon_QPA - mean(data$epilithon_QPA))^2)
SSE <- sum((data$epilithon_QPA - data$predicted)^2)
rsquared <- 1 - (SSE / SST)

# Print the fitted parameters and R-squared
print("Fitted Parameters:")
print(fitted_params)
print(paste("R-squared:", round(rsquared, 4)))

mod.4.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(aes(x = event, y = predicted), color = "blue") +
  labs(title = "Custom Logistic Regression",
       x = "Event",
       y = "Epilithon QPA") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(epilithon_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$event - mean(data$epilithon_QPA))^2)
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
                        eplitihon_QPA_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = eplitihon_QPA_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Epilithon QPA") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(epilithon_QPA ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$canopy_QPA - mean(data$epilithon_QPA))^2)
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
new_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Epilithon QPA") +
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


mod.7 <- nlsLM(epilithon_QPA ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$epilithon_QPA - mean(data$epilithon_QPA))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Epilithon QPA") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(epilithon_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(epilithon_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPA - mean(data$epilithon_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Epilithon QPA") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)



###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.1), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.1))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.2), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.2))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.3), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.3))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$epilithon_QPA, mean = data$predicted, sd = sqrt(sum((data$epilithon_QPA - data$predicted)^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.5), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.5))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.6), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.6))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.7), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.7))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$epilithon_QPA, mean = fitted(mod.8), sd = sqrt(sum((data$epilithon_QPA - fitted(mod.8))^2) / (length(data$epilithon_QPA) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$epilithon_QPA)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$epilithon_QPA)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$epilithon_QPA)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
num_params <- length(fitted_params)
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * num_params
num_params <- length(fitted_params)
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$epilithon_QPA)) * num_params

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$epilithon_QPA)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$epilithon_QPA)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$epilithon_QPA)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$epilithon_QPA)) * length(coef(mod.8))


# Compare log-likelihoods, AIC, and BIC
cat("Log-Likelihood Mod.1:", log_likelihood_mod.1, "\n")
cat("Log-Likelihood Mod.2:", log_likelihood_mod.2, "\n")
cat("Log-Likelihood Mod.3:", log_likelihood_mod.3, "\n")
cat("Log-Likelihood Mod.4:", log_likelihood_mod.4, "\n")
cat("Log-Likelihood Mod.5:", log_likelihood_mod.5, "\n")
cat("Log-Likelihood Mod.6:", log_likelihood_mod.6, "\n")
cat("Log-Likelihood Mod.7:", log_likelihood_mod.7, "\n")
cat("Log-Likelihood Mod.8:", log_likelihood_mod.8, "\n")

cat("AIC Mod.1:", aic_mod.1, "\n")
cat("AIC Mod.2:", aic_mod.2, "\n")
cat("AIC Mod.3:", aic_mod.3, "\n")
cat("AIC Mod.4:", aic_mod.4, "\n")
cat("AIC Mod.5:", aic_mod.5, "\n")
cat("AIC Mod.6:", aic_mod.6, "\n")
cat("AIC Mod.7:", aic_mod.7, "\n")
cat("AIC Mod.8:", aic_mod.8, "\n")

cat("BIC Mod.1:", bic_mod.1, "\n")
cat("BIC Mod.2:", bic_mod.2, "\n")
cat("BIC Mod.3:", bic_mod.3, "\n")
cat("BIC Mod.4:", bic_mod.4, "\n")
cat("BIC Mod.5:", bic_mod.5, "\n")
cat("BIC Mod.6:", bic_mod.6, "\n")
cat("BIC Mod.7:", bic_mod.7, "\n")
cat("BIC Mod.8:", bic_mod.8, "\n")


