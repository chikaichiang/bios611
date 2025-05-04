library(tidyverse)
library(viridis)

# Read and prepare data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
d1$CervicalCancer <- d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer <- factor(d1$CervicalCancer, levels = c("0","1","2","3","4"))
d1$First.sexual.intercourse <- as.integer(d1$First.sexual.intercourse)
d1 <- d1[which(!is.na(d1$First.sexual.intercourse)), ]

# Create polished density plot
ggplot(d1, aes(x = First.sexual.intercourse, fill = CervicalCancer)) +
  geom_density(alpha = 0.7, color = NA, linewidth = 0.3) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",  # Consistent with other plots
    direction = -1,
    begin = 0.1,
    end = 0.9,
    name = "Test Positivity",
    labels = c("Negative (0)", "Low (1)", "Moderate (2)", "High (3)", "Severe (4)")
  ) +
  scale_x_continuous(
    limits = c(min(d1$First.sexual.intercourse), max(d1$First.sexual.intercourse)),
    breaks = seq(10, 30, by = 5)  # Adjusted for typical age ranges
  ) +
  labs(
    title = "Age at First Sexual Intercourse and Cervical Cancer Risk",
    subtitle = "Density distribution by test positivity level",
    x = "Age at First Sexual Intercourse",
    y = "Density",
    caption = "Data source: risk_factors.csv"
  ) +
  facet_grid(
    CervicalCancer ~ .,
    labeller = labeller(CervicalCancer = c(
      "0" = "Negative (0)",
      "1" = "Low Positivity (1)",
      "2" = "Moderate Positivity (2)",
      "3" = "High Positivity (3)",
      "4" = "Severe Positivity (4)"
    ))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    axis.title = element_text(size = 12),
    strip.text.y = element_text(angle = 0, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.4, "lines"),
    panel.grid.minor = element_blank()
  )

# Save high-quality output
ggsave("figures/sex_time_cervical_cancer_density.png",
       width = 8,
       height = 10,
       dpi = 300,
       bg = "white")

