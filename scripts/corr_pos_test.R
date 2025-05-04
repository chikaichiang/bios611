library(tidyverse)
library(reshape2)
library(viridis)

# Read and process data (keeping original variable names)
d1 <- read.csv("Data/risk_factors.csv")
pos_test <- d1[which(d1$Hinselmann!=0|d1$Schiller!=0|d1$Citology!=0|d1$Biopsy!=0),]
pos_test <- pos_test[, c(33:36)]
corr <- round(cor(pos_test), 4)

# Correlation matrix processing (original functions kept)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(corr)
upper_tri <- get_upper_tri(corr)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Enhanced heatmap plot
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    limits = c(-1, 1),
    name = "Pearson Correlation",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(5, "cm"),
      frame.colour = "black"
    )
  ) +
  geom_text(
    aes(label = value),
    color = "white",
    size = 4.5,
    fontface = "bold"
  ) +
  coord_fixed() +
  labs(
    title = "Correlation Between Positive Test Results",
    caption = "Data source: risk_factors.csv"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 12
    ),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray50",
      hjust = 1
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Save high-quality output
ggsave("figures/corr_pos_test.png",
       plot = ggheatmap,
       width = 8,
       height = 7,
       dpi = 300,
       bg = "white")

