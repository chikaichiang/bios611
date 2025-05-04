library(tidyverse)
library(viridis)

# Read and prepare data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
subset_first_last_diag <- d1[, c('STDs..Time.since.first.diagnosis', 'STDs..Time.since.last.diagnosis')]
subset_first_last_diag$STDs..Time.since.first.diagnosis <- as.numeric(subset_first_last_diag$STDs..Time.since.first.diagnosis)
subset_first_last_diag$STDs..Time.since.last.diagnosis <- as.numeric(subset_first_last_diag$STDs..Time.since.last.diagnosis)
subset_first_last_diag1 <- subset_first_last_diag[which(subset_first_last_diag$STDs..Time.since.first.diagnosis != 0 & 
                                                          !is.na(subset_first_last_diag$STDs..Time.since.first.diagnosis)), ]

# Create polished overlapping histogram
ggplot(subset_first_last_diag1) +
  geom_histogram(
    aes(x = STDs..Time.since.first.diagnosis, fill = "First Diagnosis"),
    binwidth = 1, alpha = 0.7, position = "identity"
  ) +
  geom_histogram(
    aes(x = STDs..Time.since.last.diagnosis, fill = "Last Diagnosis"),
    binwidth = 1, alpha = 0.7, position = "identity"
  ) +
  scale_fill_manual(values = c(
    "First Diagnosis" = "#1f77b4",  # Nice blue
    "Last Diagnosis" = "#ff7f0e"    # Complementary orange
  )) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(
    title = "Time Since First vs Last STD Diagnosis",
    x = "Years Since Diagnosis",
    y = "Count of Participants",
    fill = "Diagnosis Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

# Save high-quality output
ggsave("figures/first_last_diag_hist.png",
       width = 8,
       height = 5,
       dpi = 300,
       bg = "white")

