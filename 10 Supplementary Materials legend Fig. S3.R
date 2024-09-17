



# --------------------------------------------------------
# Use to get the legend
# Date: Fri Jul 19 2024 14:22:03
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------
legend_data <- data.frame(
  event = rep(data$event, 2),
  canopy = c(data$canopy_QPA, data$canopy_QPB),
  model = factor(rep(c("Prieta A", "Prieta B"), 
                     each = nrow(data)))
)

px <- ggplot(data = legend_data, aes(x = event, y = canopy, color = model)) +
  geom_point(data = data, aes(y = canopy_QPA, color = "Prieta A"), shape = 16, size = 2) +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), 
            aes(y = canopy_QPA, color = "Prieta A")) +
  
  geom_point(data = data, aes(y = canopy_QPB, color = "Prieta B"), shape = 16, size = 2) +
  geom_line(data = data, aes(y = linear_model$fitted.values, color = "Prieta B")) +
  
  scale_color_manual(values = c("Prieta A" = "#ce1256", "Prieta B" = "#0570b0")) +
  
  labs(x = "Sampling event",
       y = "Canopy openness (%)",
       color = "Stream") +
  
  ggthemes::geom_rangeframe(data = data.frame(event = data$event, y = PrietaA$canopy), aes(y = y)) + 
  ggthemes::theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70", size = 0.5, linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70", size = 0.5, linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add horizontal line at y = 0
  
  annotate("text", label = "Logistic curve",
           x = 15, y = 0.25,
           color = "#ce1256",
           size = 6, 
           family = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Linear model",
           x = 42, y = 1.2,
           color = "#0570b0",
           size = 6, 
           family = "serif", 
           fontface = "italic")

px


# Define the g_legend function
g_legend <- function(a.gplot) {
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Extract the legend
legend <- g_legend(p + theme(legend.position = c(0.25, 0.6)) +
                     theme(legend.key.size = unit(0.6, "cm")) +
                     theme(legend.title = element_text(size = 14)) + 
                     theme(legend.text = element_text(color = "black", size = 12)))


