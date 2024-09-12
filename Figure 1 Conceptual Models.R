library(ggplot2)
library(grid)

install.packages("extrafont")
library(extrafont)
head(fonts(), 100)

# Create a sample data frame
data <- data.frame(x = c(-1, 0, 1, 2, 3, 4, 5), 
                   y = c(0, 0, 0, 0, 0, 0, 0))

p <- ggplot(data, aes(x, y)) +
  geom_blank() +  # Create an empty plot
  scale_y_continuous(limits = c(-2, 2), breaks = 0, labels = "0") +  # Set y-axis limits, show only zero
  scale_x_continuous(limits = c(-1, 5), breaks = -1:5) +  # Set x-axis limits and breaks
 # geom_hline(yintercept = 0, size = 1, color = "black") +  # Highlight the zero line on y-axis with size = 1
  geom_rect(aes(xmin=0, xmax=0.1, ymin=-2, ymax=2), 
            fill="gray90") +
  
  annotate("segment", x = -1, y = 0, xend = 5, yend = 0, 
           linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = -1, size = 1, color = "black") +  # Highlight the -1 line on x-axis with size = 1
  
  theme_minimal() +
  xlab("Years since hurricane") +
  ylab("Changes in Magnitude") +

  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_blank(),  # Remove axis lines
    axis.ticks = element_blank(),  # Remove ticks
    axis.text.y = element_text(size = 12, color = 'black', margin = margin(r = -15)),  # Style for y-axis text
    axis.text.x = element_text(size = 12, color = 'black'),  # Style for x-axis text
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Style for x-axis title
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # Style for y-axis title
    plot.margin = unit(c(1,1, 1, 2), "lines")  # Adjust margins around the plot
  )

p

p1 <- p + geom_segment(aes(x = -0.98, y = 1, xend = 5, yend = 1), 
                 linetype="dotted",color = "gray80", size=0.5)
p2 <- p1 + geom_segment(aes(x = -0.98, y = 0.5, xend = 5, yend = 0.5), 
                 linetype="dotted",color = "gray80", size=0.5)
p3 <- p2 + geom_segment(aes(x = -0.98, y = 1.5, xend = 5, yend = 1.5), 
                 linetype="dotted",color = "gray80", size=0.5)

p4 <- p3 + geom_segment(aes(x = -0.98, y = -1, xend = 5, yend = -1), 
                       linetype="dotted",color = "gray80", size=0.5)
p5 <- p4 + geom_segment(aes(x = -0.98, y = -0.5, xend = 5, yend = -0.5), 
                        linetype="dotted",color = "gray80", size=0.5)
p6 <- p5 + geom_segment(aes(x = -0.98, y = -1.5, xend = 5, yend = -1.5), 
                        linetype="dotted",color = "gray80", size=0.5)

p7 <- p6 + geom_segment(aes(x = -0.5, y = -2, xend = -0.5, yend = 2), 
                        linetype="dotted",color = "gray80", size=0.5)
p8 <- p7 + geom_segment(aes(x = 0, y = -2, xend = 0, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p9 <- p8 + geom_segment(aes(x = 0.5, y = -2, xend = 0.5, yend = 2), 
                        linetype="dotted",color = "gray80", size=0.5)
p10 <- p9 + geom_segment(aes(x = 1, y = -2, xend = 1, yend = 2), 
                        linetype="dotted",color = "gray80", size=0.5)
p11 <- p10 + geom_segment(aes(x = 1.5, y = -2, xend = 1.5, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p12 <- p11 + geom_segment(aes(x = 2, y = -2, xend = 2, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p13 <- p12 + geom_segment(aes(x = 2.5, y = -2, xend = 2.5, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p14 <- p13 + geom_segment(aes(x = 3, y = -2, xend = 3, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p15 <- p14 + geom_segment(aes(x = 3.5, y = -2, xend = 3.5, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p16 <- p15 + geom_segment(aes(x = 4, y = -2, xend = 4, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p17 <- p16 + geom_segment(aes(x = 4.5, y = -2, xend = 4.5, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p18 <- p17 + geom_segment(aes(x = 5, y = -2, xend = 5, yend = 2), 
                      linetype="dotted",color = "gray80", size=0.5)
p18


# Add rotated text using grid.text() with the same font family
grid.text("Increase", x = unit(0.11, "npc"), y = unit(0.75, "npc"), 
          rot = 90, gp = gpar(fontsize = 14, fontfamily = "Arial"))
grid.text("Decrease", x = unit(0.11, "npc"), y = unit(0.35, "npc"), 
          rot = 90, gp = gpar(fontsize = 14, fontfamily = "Arial"))



# File path for saving
file_path <- "figures/Figure_1_Models_with_text.tiff"

# Open the TIFF device
tiff(file_path, 
     width = 10, 
     height = 8, 
     units = "in", 
     res = 300, 
     compression = "lzw")

# Print the plot
print(p18)

# Add rotated text using grid.text()
grid.text("Increase", 
          x = unit(0.08, "npc"), 
          y = unit(0.75, "npc"), 
          rot = 90, 
          gp = gpar(fontsize = 14, fontfamily = "Arial"))

grid.text("Decrease", 
          x = unit(0.08, "npc"), 
          y = unit(0.30, "npc"), 
          rot = 90, 
          gp = gpar(fontsize = 14, fontfamily = "Arial"))

# Close the TIFF device to save the file
dev.off()
