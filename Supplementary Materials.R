

library(patchwork)
plot <- (mod.4.plot.canopy + mod.5.plot.leaf)/
  (mod.6.plot.algae + mod.4.plot.shrimp) /
  (mod.5.plot.macros + plot_spacer())


#Ecology format
ggsave(file="Figure 1.jpeg", plot, width = 24, height = 30, units = "cm", dpi = 600)
