



# ---------------------------------------------
# Long-term ecosystem response: Parabola Curve Quadratic function
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())


# data
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
data_QPA <- data.frame(event, canopy_QPA)

# Fit a quadratic function to the data using lm():
quadratic_model <- lm(canopy_QPA ~ poly(event, 2, raw = TRUE), data = data)

# Get R-squared value
r_squared <- summary(quadratic_model)$r.squared
cat("R-squared:", r_squared, "\n")

# Get p-values for coefficients
p_values <- summary(quadratic_model)$coefficients[, 4]
cat("P-values for coefficients:\n")
print(p_values)

# Create a ggplot
ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "blue") +
  labs(title = "Parabolic Curve",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title



###########################################################################
# Quebrada Prieta B -------------------------------------------------------
###########################################################################


# Shrimp Abundance --------------------------------------------------------
# Given data
shrimp_QPB <- c(4.696673669, 7.102844975, 2.92230371, 4.775812621, 3.786680872, 
                5.861103582, 4.841844408, 4.095107269, 8.184963933, 7.032519247, 
                8.948997943, 4.558330701, 7.366546849, 6.695518125, 9.333305943, 
                14.62520908, 8.463158621, 12.83836099, 15.93527468, 13.17293781, 
                5.624433334, 5.197004403, 6.152430647, 7.169357883, 5.89132428, 
                8.384487872, 8.557024587, 8.147560149, 10.10395362, 5.160561525, 
                8.385994999, 6.265705047, 5.676738116, 7.221369143, 6.218530111, 
                5.30139752, 4.76111744, 3.812979913, 4.197876748, 4.487483645, 
                4.804636027, 12.30938817, 6.83983239, 4.741247918, 5.366055518)

event <- seq(1, length(shrimp_QPB))
data <- data.frame(event, shrimp_QPB)

# Fit the quadratic model
quadratic_model <- lm(shrimp_QPB ~ poly(event, 2, raw = TRUE), data = data)

# Fit the quadratic model
quadratic_model <- lm(shrimp_QPB ~ poly(event, 2, raw = TRUE), data = data)

# Get R-squared value
r_squared <- summary(quadratic_model)$r.squared
cat("R-squared:", r_squared, "\n")

# Get p-values for coefficients
p_values <- summary(quadratic_model)$coefficients[, 4]
cat("P-values for coefficients:\n")
print(p_values)

# Create the ggplot plot
ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point(color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "red") +
  labs(title = "Quadratic Curve Visualization",
       x = "Event",
       y = "Shrimp QPB") +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
