library(tidyverse)
library(viridis)

# Read data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
cervical_cancer_test <- d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
levels(cervical_cancer_test) <- c(0, 1, 2, 3, 4)
cerv <- table(cervical_cancer_test)

# Convert to dataframe for ggplot
cerv_df <- data.frame(
  test_result = names(cerv),
  count = as.numeric(cerv)
)

# Create plot with matching age_bar coloring
ggplot(cerv_df, aes(x = test_result, y = count, fill = test_result)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9, color = NA) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",  # Matching age_bar palette
    direction = -1,     # Darker colors for higher values
    begin = 0.2,
    end = 0.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Test Positivity Count (0-4)",
    y = "Number of Participants",
    title = "Cervical Cancer Test Results"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save with matching specs to age_bar
ggsave("figures/cerv_cancer_result_hist.png",
       width = 8,  # Matching age_bar width
       height = 5, # Matching age_bar height
       dpi = 300,
       bg = "white")
