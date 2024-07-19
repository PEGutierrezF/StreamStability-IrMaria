



# ---------------------------------------------
# Long-term ecosystem response: Leaf litter fall Quebrada Prieta A
# 21 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with your leaf litter data (2017-10-30 (H. Maria, time 0) to 2022-09-14)
leaflitter_QPA <- c(
                    -2.565069159, -3.022216147, -2.425299443, -2.356670205, -2.167231853, -2.917151971, 
                    -0.300271157, -2.134096292, -2.839665992, -2.044478086, -1.733085534, -1.624609129, 
                    -1.211743623, -0.799411473, -2.110757766, -1.871576837, -1.200485106, -1.612131249, 
                    -1.764806785, -1.27485795, -1.532273493, -1.261461902, -1.698447453, -1.928908543, 
                    -1.338069713, -1.632515941, -1.562239222, -2.545394251, -1.082064703, -2.733135875, 
                    -1.939825609, -1.43306269, -1.767640986, -0.965536339, -0.391608814, -0.613991123, 
                    -0.508247125, -1.000617123, -1.175868835, -0.824672858, -0.521538434, -1.270373221, 
                    -1.17098708, -1.193741075, -1.037061656, -0.599383721, -0.521492258, -1.92107908, 
                    -1.586354733, -1.464956636, -2.192426795, -0.971256818, -0.302518176, -0.937543182, 
                    -1.670225859, -1.126310067, -1.313000351, -1.194800423, -1.341998189, -0.56209657, 
                    -0.5161628, -0.328599799, -0.585934106, -0.953081472, -0.927344875, 0.523726624, 
                    -0.865303492, 0.023643143, -0.104554204, -1.377159185, -0.871797957, -1.799831283, 
                    -1.171549437, -1.460269437, -1.786864311, -2.056059164, -1.782840537, -1.15055833, 
                    -1.091059192, -0.37511391, -0.444649707, -1.236006878, -0.63063159, -1.001865167, 
                    -0.249679552, -0.161372181, -0.028637124, -0.563554364, -0.8868843, -0.95364459, 
                    -0.482890718, -0.149959004, -0.111404071, -0.771421424, -1.177151156, -1.257598545, 
                    -1.289014428, -1.415630091, -0.319359473, -0.115105525, -1.381377565, -1.145349438, 
                    -0.862815924, 0.173495884, -1.138115783, -0.599902465, -1.417418296, -0.270925982, 
                    -0.135785162, -0.239376617, -0.408619136, -0.525518629, -0.727676895, -0.965580008, 
                    -0.479949827, -0.91494735, -0.322411522, -0.056491592)

event <- seq(1, length(leaflitter_QPA))
data <- data.frame(event, leaflitter_QPA)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(leaflitter_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
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
mod.2 <- nlsLM(leaflitter_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$leaflitter_QPA
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
mod.2.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
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
mod.3 <- lm(leaflitter_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("p_value.mod3:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(leaflitter_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, leaflitter_QPA = predictions), 
            aes(x = event, y = leaflitter_QPA), color = "blue") +
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

# Define the objective function for optimization
objective_function <- function(params, x, y) {
  A <- params[1]
  B <- params[2]
  C <- params[3]
  D <- params[4]
  sum((y - logistic_function(x, A, B, C, D))^2)
}

# Initial guesses for the parameters
initial_params <- c(A = min(leaflitter_QPA), B = max(leaflitter_QPA), C = 1, D = median(event))

# Fit the logistic model using nlsLM
mod.4 <- nlsLM(leaflitter_QPA ~ logistic_function(event, A, B, C, D),
             data = data,
             start = initial_params,
             lower = c(min(leaflitter_QPA), min(leaflitter_QPA), 0, min(event)),
             upper = c(max(leaflitter_QPA), max(leaflitter_QPA), 10, max(event)))


# Extract the fitted parameters
params <- coef(fit)
A <- params['A']
B <- params['B']
C <- params['C']
D <- params['D']

# Print the estimated parameters
cat("Estimated parameters:\n")
cat("A =", A, "\n")
cat("B =", B, "\n")
cat("C =", C, "\n")
cat("D =", D, "\n")

# Get model summary
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$canopy_QPA - mean(data$leaflitter_QPA))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(leaflitter_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, leaflitter_QPA = predictions), 
            aes(x = event, y = leaflitter_QPA), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.4.plot



###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(leaflitter_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$leaflitter_QPA - mean(data$leaflitter_QPA))^2)
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
                        leaflitter_QPA_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot.leaf <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = leaflitter_QPA_pred), color = "blue") +

  labs(title = expression(Logarithmic~Curve~(italic(y) == a %.% log(italic(x)) + b)),
       x = "Sampling event",
       y =  expression(Leaflitter~fall~(g %.% m^{-2} %.% d^{-1}))) +
  geom_rangeframe() + theme_tufte() +
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray50",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray50",size = 0.5,linetype = 3))


mod.5.plot.leaf




###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve using nlsLM with increased iterations
mod.6 <- nlsLM(leaflitter_QPA ~ exponential(event, A, B, C),
               data = data,
               start = list(A = 1, B = 0.1, C = 0),
               lower = c(-Inf, -Inf, -Inf),
               upper = c(Inf, Inf, Inf),
               control = nls.lm.control(maxiter = 200)) # Increase maximum iterations

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$leaflitter_QPA - mean(data$leaflitter_QPA))^2)
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
new_data <- data.frame(event = seq(1, length(leaflitter_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
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


mod.7 <- nlsLM(leaflitter_QPA ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$leaflitter_QPA - mean(data$leaflitter_QPA))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(leaflitter_QPA), length.out = 100))
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
mod.8 <- nls(leaflitter_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(leaflitter_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$leaflitter_QPA - mean(data$leaflitter_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = leaflitter_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "canopy_QPA") +
  theme_minimal()

mod.8.plot




###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.1), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.1))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.2), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.2))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.3), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.3))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.4), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.4))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.5), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.5))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.6), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.6))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.7), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.7))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$leaflitter_QPA, mean = fitted(mod.8), sd = sqrt(sum((data$leaflitter_QPA - fitted(mod.8))^2) / (length(data$leaflitter_QPA) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$leaflitter_QPA)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$leaflitter_QPA)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$leaflitter_QPA)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$leaflitter_QPA)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$leaflitter_QPA)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$leaflitter_QPA)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$leaflitter_QPA)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$leaflitter_QPA)) * length(coef(mod.8))


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

# Store AIC values in a vector
aic_values <- c(aic_mod.1, aic_mod.2, aic_mod.3, aic_mod.4, aic_mod.5, aic_mod.6, aic_mod.7, aic_mod.8)
# Sort AIC values in ascending order
sorted_indices <- order(aic_values)
# Print sorted AIC values and corresponding model numbers
for (i in sorted_indices) {
  cat("AIC Mod.", i, ":", aic_values[i], "\n")
}


cat("BIC Mod.1:", bic_mod.1, "\n")
cat("BIC Mod.2:", bic_mod.2, "\n")
cat("BIC Mod.3:", bic_mod.3, "\n")
cat("BIC Mod.4:", bic_mod.4, "\n")
cat("BIC Mod.5:", bic_mod.5, "\n")
cat("BIC Mod.6:", bic_mod.6, "\n")
cat("BIC Mod.7:", bic_mod.7, "\n")
cat("BIC Mod.8:", bic_mod.8, "\n")

