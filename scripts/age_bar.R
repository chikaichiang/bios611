library(tidyverse)
library(viridis)  # For colorblind-friendly palettes

# Read and prepare data
d1 <- read_csv("Data/risk_factors.csv") %>%
  mutate(Age = factor(Age, levels = sort(unique(Age))))

# Create color palette based on age groups
age_levels <- length(levels(d1$Age))
color_palette <- viridis(age_levels, option = "plasma", direction = -1)

# Create plot with color variation
age_plot <- ggplot(d1, aes(x = Age, fill = Age)) + 
  geom_bar(color = NA, width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = color_palette, guide = "none") +  # Hide legend
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Participant Age Distribution",
    subtitle = "Color gradient indicates age group progression",
    x = "Age Group (years)",
    y = "Number of Participants",
    caption = "Data source: risk_factors.csv"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.caption = element_text(size = 9, color = "gray50", margin = margin(t = 10))
  )

# Save plot
ggsave("figures/age_bar.png", 
       plot = age_plot,
       width = 8, 
       height = 5,
       dpi = 300,
       bg = "white")

message("Successfully created figures/age_bar.png with color gradient")

