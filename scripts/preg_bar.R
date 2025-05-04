library(tidyverse)
library(viridis)

# Read data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
preg <- table(as.integer(d1$Num.of.pregnancies), useNA = "ifany")

# Convert to dataframe for ggplot
preg_df <- data.frame(
  pregnancies = as.numeric(names(preg)),
  count = as.numeric(preg)
)

# Create plot with age_bar matching style
ggplot(preg_df, aes(x = factor(pregnancies), y = count, fill = factor(pregnancies))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9, color = NA) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",  # Changed to match age_bar
    direction = -1,
    begin = 0.2,
    end = 0.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Number of Pregnancies",
    y = "Count of Participants",
    title = "Distribution of Pregnancy Counts"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save plot with identical specs to age_bar
ggsave("figures/preg_bar.png", 
       width = 8,  # Matches age_bar width
       height = 5, # Matches age_bar height
       dpi = 300,
       bg = "white")

