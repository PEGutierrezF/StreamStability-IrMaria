



# ---------------------------------------------
# Long-term ecosystem response: Shrimp abundance Quebrada Prieta A
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with Decapoda_QPA data (2017-10-04 (H. Maria, time 0) to 2022-09-01)
decapoda_QPA <- c(-0.23393578, -0.239203865, -0.086031458, 
                  0.194384455, 0.449264798, 0.357700178, 0.923433147, -0.010068608, 0.383830498, 
                  0.645842869, 0.709639939, -0.010001997, 0.180285861, 0.399394425, 0.269734048, 
                  0.098597297, 0.121989466, 0.408274831, 0.371495165, 0.817862356, 0.500372304, 
                  0.240039901, 0.083373911, 0.354399016, 0.320034799, 0.140367341, -0.254432235, 
                  0.003846263, -0.34321992, -0.081858956, 0.105006467, 0.239319973, 0.408686216, 
                  0.12504962, 0.322919454, 0.718199634, -0.19458057, -0.628874296, -0.699498734, 
                  -0.333033181, -0.643381145, -0.497259411, -0.638556847, -0.771123635, -0.209821714, 
                  -0.777532836, -0.725945658, -0.771792055, -0.762260335, -0.393835734, -0.890604404, 
                  -0.267314505, -0.828057819, -0.435227036, -0.41283978, -0.536717338, -0.534703537, 
                  -0.537231614, -0.883109826, -0.995781911)


event <- seq(1, length(decapoda_QPA))
data <- data.frame(event, decapoda_QPA)




###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(decapoda_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared_mod.1 <- summary(mod.1)$r.squared
p_value_mod.1 <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared_mod.1, "\n")
cat("P-value:", p_value_mod.1, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
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
mod.2 <- nlsLM(decapoda_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$decapoda_QPA
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
mod.2.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Canopy QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Decapoda QPA") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(decapoda_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(decapoda_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, shrimp_QPA = predictions), 
            aes(x = event, y = shrimp_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Decapoda QPA") +
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
mod.4 <- nlsLM(decapoda_QPA ~ logistic_function(event, A, B, C, D),
               data = data,
               start = start_params)

# Check the summary of the model
summary(mod.4)

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals(mod.4)^2))

# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$decapoda_QPA - mean(data$decapoda_QPA))^2)
r_squared_mod.4 <- 1 - (ss_residuals / ss_total)

# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared_mod.4), "\n")



# Extract coefficients and their standard errors
coefficients <- coef(mod.4)
std_errors <- summary(mod.4)$parameters[, "Std. Error"]

# Calculate t-values and p-values
t_values <- coefficients / std_errors
p_values <- 2 * pt(abs(t_values), df = Inf, lower.tail = FALSE)

# Print p-values
cat("p-values:\n")
print(p_values)

# Check the summary of the model
summary_mod_4 <- summary(mod.4)
cat("p-value for coefficient B:", summary_mod_4$coefficients["B", "Pr(>|t|)"], "\n")



# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(decapoda_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot.shrimp <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, shrimp_QPA = predictions), aes(x = event, y = shrimp_QPA), 
            color = "blue") +

  labs( title = expression(Logistic~Curve~(italic(y) == A + frac(B - A, 1 + e^{-C * (italic(x) - D)}))),
        x = "Sampling event",
        y = expression(Decapod~abundance~(ind %.% m^{-2} ))) +
  
  geom_rangeframe() + theme_tufte() +
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray50",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray50",size = 0.5,linetype = 3))

mod.4.plot.shrimp

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(decapoda_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$decapoda_QPA - mean(data$decapoda_QPA))^2)
ss_residual <- sum(residuals(mod.5)^2)
r_squared_mod.5 <- 1 - (ss_residual / ss_total)

# Calculate p-values using t-distribution
coefs <- coef(mod.5)
std_errors <- sqrt(diag(vcov(mod.5)))
t_values <- coefs / std_errors
p_values <- 2 * (1 - pt(abs(t_values), df = length(data$event) - length(coefs)))

# Display results
cat("R-squared:", r_squared_mod.5, "\n")
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
                        decapoda_QPA_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = decapoda_QPA_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Decapoda QPA") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nlsLM(
  decapoda_QPA ~ exponential(event, A, B, C),
  data = data,
  start = list(A = 1, B = 0.1, C = 0)
)

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$decapoda_QPA - mean(data$decapoda_QPA))^2)
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
new_data <- data.frame(event = seq(1, length(decapoda_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Decapoda QPA") +
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

# Fit the nonlinear model using nlsLM
mod.7 <- nlsLM(
  decapoda_QPA ~ gompertz_asymmetric(event, A, b, c, d),
  data = data,
  start = list(A = 1, b = 1, c = 1, d = 0),
  control = nls.lm.control(maxiter = 100, ftol = 1e-6)
)
# Calculate the residual sum of squares (rss)
rss <- sum(residuals(mod.7)^2)
# Calculate the total sum of squares (tss)
tss <- sum((data$decapoda_QPA - mean(data$decapoda_QPA))^2)
# Calculate R-squared value
rsquared_mod.7 <- 1 - (rss / tss)
# Print the R-squared value
cat("R-squared value:", rsquared_mod.7, "\n")
# Print the summary of the model
summary(mod.7)

curve_data <- data.frame(event = seq(1, length(decapoda_QPA), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "blue") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Decapoda QPA") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(decapoda_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(decapoda_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$decapoda_QPA - mean(data$decapoda_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = decapoda_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Decapoda QPA") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)



###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.1), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.1))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.2), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.2))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.3), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.3))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.4), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.4))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.5), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.5))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.6), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.6))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.7), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.7))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$decapoda_QPA, mean = fitted(mod.8), sd = sqrt(sum((data$decapoda_QPA - fitted(mod.8))^2) / (length(data$decapoda_QPA) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$decapoda_QPA)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$decapoda_QPA)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$decapoda_QPA)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$decapoda_QPA)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$decapoda_QPA)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$decapoda_QPA)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$decapoda_QPA)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$decapoda_QPA)) * length(coef(mod.8))


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


