library(tidyverse)
library(viridis)

# Read data (keeping your original variable names)
d1 <- read.csv("Data/risk_factors.csv")
no_sex_partner <- table(as.integer(d1$Number.of.sexual.partners), useNA = "ifany")

# Convert to dataframe for ggplot
partner_df <- data.frame(
  partners = as.numeric(names(no_sex_partner)),
  count = as.numeric(no_sex_partner)
)

# Create plot with improved coloring
ggplot(partner_df, aes(x = factor(partners), y = count, fill = factor(partners))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9, color = NA) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "plasma",
    direction = -1,
    begin = 0.2,
    end = 0.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Number of Sexual Partners",
    y = "Count of Participants",
    title = "Distribution of Sexual Partners"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot (using your original dimensions but higher quality)
ggsave("figures/no_sex_partners.png", 
       width = 6, height = 3.5, 
       dpi = 300, bg = "white")

