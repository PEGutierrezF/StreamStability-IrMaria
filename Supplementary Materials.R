

library(patchwork)
plot <- (mod.4.plot.canopy + mod.5.plot.leaf)/
  (mod.6.plot.algae + mod.4.plot.shrimp) /
  (mod.5.plot.macros + plot_spacer())


#Ecology format
ggsave(file="Figure 1.jpeg", plot, width = 24, height = 30, units = "cm", dpi = 600)




# Canopy ------------------------------------------------------------------
# Create a data frame with canopy_QPA data (2017-10-04 (H. Maria, time 0) to 2022-09-01)
canopy_QPA <- c(2.035709058, 1.727578155, 1.716528319, 1.669203747, 
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
head(data)



# Create a data frame with your canopy_QPB data (2017-09-10 to 2022-09-01)
canopy_QPB <- c(1.913568932, 1.551896958, 1.536349726, 1.490548934,
                1.39148244, 1.294424291, 1.13646499, 1.016906589, 1.246987452,
                1.185334648, 1.109390578, 1.240990432, 1.181352166, 1.195615482,
                1.141460837, 0.782647871, 1.030918963, 0.925352283, 1.106814364,
                1.309983304, 1.233443227, 1.201890013, 1.067352865, 1.085962175,
                0.96675143, 0.881087944, 0.923288298, 1.041071334, 0.774289613,
                0.459874479, 0.852670731, 0.967740061, 0.80962898, 0.85930059,
                0.704782735, 0.823422302, 0.573085869, 0.148375163, 0.631465433,
                0.654706397, 0.614728641, 0.627307423, 0.748787321, 0.398941928,
                0.379558061, 0.34147549, 0.421376001, 0.43325061, 0.404163872,
                0.339625351, 0.449973408, 0.421376001, 0.27459288, 0.305723799,
                0.256646237, 0.161738391, 0.270632479, 0.155079099, 0.388415515
)

event <- seq(1, length(canopy_QPB))
data <- data.frame(event, canopy_QPB)


# Linear model for canopy_QPB
linear_model <- lm(canopy_QPB ~ event, data = data)

# Function for logistic curve
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Fit a logistic curve model
mod.4 <- nls(canopy_QPA ~ logistic_function(event, A, B, C, D),
             data = data,
             start = list(A = min(canopy_QPA), B = max(canopy_QPA), C = 1, D = median(event)))

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Linear model for canopy_QPB
linear_model <- lm(canopy_QPB ~ event, data = data)

library(ggthemes)
  
p <- ggplot(data, aes(x = event)) +
  geom_point(aes(y = canopy_QPA), shape = 16, color = "#ce1256", size = 5) +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), 
            aes(y = canopy_QPA), color = "#ce1256", linewidth=1) +
  
  geom_point(aes(y = canopy_QPB), shape = 16, colour = "#0570b0", size = 5) +
  geom_line(aes(y = linear_model$fitted.values), color = "#0570b0", linewidth=1) +
  
  

  labs(x = "Sampling event",
        y = "Canopy openness (%)") +
  
#  geom_rangeframe() + theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  annotate("text", label = "Logistic curve",
           x = 20,y=0.5,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +

annotate("text", label = "Linear model",
         x = 40,y=1,
         color    = "#0570b0",
         size     = 6, 
         family   = "serif", 
         fontface = "italic") 

p



# Leaflitter fall ---------------------------------------------------------

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


# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        leaflitter_QPA_pred = predict(mod.5, newdata = data))

# Linear model for canopy_QPB
mod.1.QPB <- lm(leaflitter_QPB ~ event, data = data)


# Create a ggplot
p1 <- ggplot(data, aes(x = event)) +
  geom_point(aes(y = leaflitter_QPA), shape = 16, colour = "#ce1256", size = 5) +
  geom_line(data = pred_data, aes(x = event, y = leaflitter_QPA_pred), color = "#ce1256") +
  
  geom_point(aes(y = leaflitter_QPB), shape = 16, colour = "#0570b0", size = 5) +
  geom_line(aes(y = mod.1.QPB$fitted.values), color='#0570b0') +  # Trend line without confidence interval

  
  labs(x = "Sampling event",
       y = expression(Leaflitter~fall~(g %.% m^{-2} %.% d^{-1}))) +
  
    geom_rangeframe() + theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
annotate("text", label = "Logarithmic curve",
           x = 90,y=-2,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Linear model",
           x = 40,y=1,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 
p1

p + p1
