



# ---------------------------------------------
# Figure 3: Long-term ecosystem responses
# 19 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# Canopy openness Prieta A ------------------------------------------------
# Calculate the predicted values from the model
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}
# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2.QPA <- nlsLM(canopy_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

predicted_values <- predict(mod.2.QPA, newdata = data.frame(event = event))
# Create a ggplot
mod.2.plot.QPA <- ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point(color = "black") +
  geom_line(aes(y = predicted_values), color = "blue", linewidth=1) +
  labs(title = "",
       x = "Sampling event",
       y = "Canopy openness") +
  theme_bw()

mod.2.plot.QPA

# Canopy openness Prieta B ------------------------------------------------
# Calculate the predicted values from the model
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}
# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2.QPB <- nlsLM(canopy_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)
predicted_values <- predict(mod.2.QPB, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot.QPB <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point(color = "black") +
  geom_line(aes(y = predicted_values), color = "blue", linewidth=1) +
  labs(title = "",
       x = "Sampling event",
       y = "") +
  theme_bw()

mod.2.plot.QPB


Figure3 <- mod.2.plot.QPA | mod.2.plot.QPB
Figure3 <- Figure3 + plot_annotation(tag_levels = 'A')

# Save the ggplot object as an image file
ggsave("Figure3.jpg", plot = Figure3, path = "figures", width = 9, height = 6, dpi = 300)


