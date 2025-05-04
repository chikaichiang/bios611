library(tidyverse)
library(viridis)

# Read data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
std_nums <- table(as.integer(d1$STDs..number.), useNA = "ifany")

# Convert to dataframe for ggplot
std_df <- data.frame(
  disease_num = as.numeric(names(std_nums)),
  count = as.numeric(std_nums)
)

# Create plot with matching age_bar style
ggplot(std_df, aes(x = factor(disease_num), y = count, fill = factor(disease_num))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9, color = NA) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",  # Matching age_bar palette
    direction = -1,     # Reverse direction (higher numbers = darker)
    begin = 0.2,
    end = 0.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Number of STDs",
    y = "Count of Participants",
    title = "Distribution of STD Counts"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.caption = element_text(size = 9, color = "gray50")
  )

# Save with identical specs to age_bar
ggsave("figures/std_no_hist.png",
       width = 8,  # Matching age_bar width
       height = 5, # Matching age_bar height
       dpi = 300,
       bg = "white")

