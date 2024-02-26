



# ---------------------------------------------
# Long-term ecosystem response: Canopy openness Quebrada Prieta A
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with canopy_QPA data (2017-01-01 to 2022-09-01)
canopy_QPA <- c(0.336436902, -0.349667996, 0.08348054, 0.194286951, -0.521518253, 
                0.599072942, 2.035709058, 1.727578155, 1.716528319, 1.669203747, 
                1.581599124, 1.645628869, 1.436226353, 1.434460089, 1.479398525, 
                1.391721113, 1.434460089, 1.323650326, 1.294256092, 1.206443161, 
                1.17562524, 1.110170268, 1.026044255, 0.991775799, 1.070264724, 
                1.026930388, 1.036626365, 0.966693202, 0.895547766, 0.759415592, 
                0.631161257, 0.6258842, 0.577094035, 0.681203769, 0.163260678, 
                0.224379493, 0.556040626, 0.491596663, 0.305941265, 0.419465091, 
                0.52140513, 0.25351904, 0.016155257, 0.238118669, 0.137395756, 
                -0.05633391, 0.006844991, 0.152945865, 0.011934061, -0.194014687, 
                -0.05181515, -0.161291931, -0.023368787, -0.01812402, -0.091360533, 
                -0.278572075, -0.028641207, -0.043732484, -0.026001523, -0.203414234, 
                -0.05181515, -0.099839151, -0.318969951, -0.332011037, 0.152945865)

event <- seq(1, length(canopy_QPA))
data <- data.frame(event, canopy_QPA)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(canopy_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
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
mod.2 <- nlsLM(canopy_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
                 data = data, 
                 start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$canopy_QPA
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
mod.2.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
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
mod.3 <- lm(canopy_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), 
            aes(x = event, y = canopy_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.3.plot


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Fit a logistic curve model
mod.4 <- nls(canopy_QPA ~ logistic_function(event, A, B, C, D),
             data = data,
             start = list(A = min(canopy_QPA), B = max(canopy_QPA), C = 1, D = median(event)))

# Get model summary
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$canopy_QPA - mean(data$canopy_QPA))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), aes(x = event, y = canopy_QPA), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(canopy_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$canopy_QPA - mean(data$canopy_QPA))^2)
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
                        canopy_QPA_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = canopy_QPA_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(canopy_QPA ~ exponential(event, A, B, C), 
           data = data,
           start = list(A = 1, B = 0.1, C = 0))

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$canopy_QPA - mean(data$canopy_QPA))^2)
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
new_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "canopy_QPA") +
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


mod.7 <- nlsLM(canopy_QPA ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$canopy_QPA - mean(data$canopy_QPA))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Canopy QPA") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(canopy_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(canopy_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPA - mean(data$canopy_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "canopy_QPA") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)



###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.1), sd = sqrt(sum((data$canopy_QPA - fitted(mod.1))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.2), sd = sqrt(sum((data$canopy_QPA - fitted(mod.2))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.3), sd = sqrt(sum((data$canopy_QPA - fitted(mod.3))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.4), sd = sqrt(sum((data$canopy_QPA - fitted(mod.4))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.5), sd = sqrt(sum((data$canopy_QPA - fitted(mod.5))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.6), sd = sqrt(sum((data$canopy_QPA - fitted(mod.6))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.7), sd = sqrt(sum((data$canopy_QPA - fitted(mod.7))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$canopy_QPA, mean = fitted(mod.8), sd = sqrt(sum((data$canopy_QPA - fitted(mod.8))^2) / (length(data$canopy_QPA) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$canopy_QPA)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$canopy_QPA)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$canopy_QPA)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$canopy_QPA)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$canopy_QPA)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$canopy_QPA)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$canopy_QPA)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$canopy_QPA)) * length(coef(mod.8))


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
cat("AIC Mod.2:", aic_mod.2, "\n") # Humped yield curve  
cat("AIC Mod.3:", aic_mod.3, "\n")
cat("AIC Mod.4:", aic_mod.4, "\n")
cat("AIC Mod.5:", aic_mod.5, "\n")
cat("AIC Mod.6:", aic_mod.6, "\n")
cat("AIC Mod.7:", aic_mod.7, "\n")
cat("AIC Mod.8:", aic_mod.8, "\n")

cat("BIC Mod.1:", bic_mod.1, "\n")
cat("BIC Mod.2:", bic_mod.2, "\n") # Humped yield curve 
cat("BIC Mod.3:", bic_mod.3, "\n")
cat("BIC Mod.4:", bic_mod.4, "\n")
cat("BIC Mod.5:", bic_mod.5, "\n")
cat("BIC Mod.6:", bic_mod.6, "\n")
cat("BIC Mod.7:", bic_mod.7, "\n")
cat("BIC Mod.8:", bic_mod.8, "\n")


