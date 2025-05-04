library(tidyverse)
library(viridis)

# Read and prepare data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
d1$CervicalCancer <- d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer <- factor(d1$CervicalCancer, levels = c("0","1","2","3","4"))
d1$Hormonal.Contraceptives..years. <- as.numeric(d1$Hormonal.Contraceptives..years.)
d1 <- d1[which(!is.na(d1$Hormonal.Contraceptives..years.)),]

# Create polished density plot
ggplot(d1, aes(x = Hormonal.Contraceptives..years., fill = CervicalCancer)) +
  geom_density(alpha = 0.7, color = NA, linewidth = 0.3) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",
    direction = -1,
    begin = 0.1,
    end = 0.9,
    name = "Cancer Test Positivity",
    labels = c("0 (Negative)", "1", "2", "3", "4 (Highest)")
  ) +
  scale_x_continuous(limits = c(0, max(d1$Hormonal.Contraceptives..years., na.rm = TRUE))) +
  labs(
    title = "Hormonal Contraceptive Duration and Cervical Cancer Risk",
    subtitle = "Density distribution by test positivity level",
    x = "Years of Hormonal Contraceptive Use",
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
      "4" = "Highest Positivity (4)"
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
ggsave("figures/hc_cervical_cancer_density.png",
       width = 8,
       height = 10,
       dpi = 300,
       bg = "white")

