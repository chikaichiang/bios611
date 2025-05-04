library(tidyverse)
library(patchwork)  # For combining plots
library(viridis)

# Read data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")

# Create individual plots with consistent styling
create_plot <- function(variable, title) {
  df <- data.frame(table(as.integer(d1[[variable]])))
  names(df) <- c("group", "count")
  
  ggplot(df, aes(x = group, y = count, fill = group)) +
    geom_bar(stat = "identity", width = 0.6, alpha = 0.9, color = NA) +
    scale_fill_viridis(discrete = TRUE, option = "rocket", begin = 0.3, end = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = "Group", y = "Count") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Generate plots
p1 <- create_plot("Smokes", "Smoking Status")
p2 <- create_plot("Hormonal.Contraceptives", "Hormonal Contraceptives")
p3 <- create_plot("IUD", "IUD Usage")

# Combine plots
combined_plot <- p1 + p2 + p3 + 
  plot_layout(nrow = 1) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

# Save high-quality output
ggsave("figures/smoke_hc_iud_bar.png", 
       plot = combined_plot,
       width = 9, 
       height = 4,
       dpi = 300,
       bg = "white")

