library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)
library(viridis)

# Read and prepare data
d1 <- read.csv("Data/risk_factors.csv")
d1_std <- d1[, c(12:25)]
d1_std$STDs <- as.integer(d1_std$STDs)
d1_std1 <- d1_std[which(!is.na(d1_std$STDs) & d1_std$STDs != 0), ]

# Create frequency table
std_freq <- data.frame(
  `STD Disease` = c("Condylomatosis", "Vaginal Condylomatosis", 
                    "Vulvo-Perineal Condylomatosis", "Syphilis",
                    "Pelvic Inflammatory Disease", "Genital Herpes",
                    "Molluscum Contagiosum", "HIV", "Hepatitis B", 
                    "HPV", "Condylomatosis (Vulvo+Other)"),
  Count = c(44, 4, 43, 18, 1, 1, 1, 18, 1, 2, 43),
  check.names = FALSE
)

# Create table grob
table_grob <- tableGrob(
  std_freq,
  theme = ttheme_minimal(
    core = list(
      bg_params = list(fill = viridis(11, alpha = 0.2), col = "gray30"),
      fg_params = list(hjust = 0, x = 0.05)
    ),
    colhead = list(
      bg_params = list(fill = "#2D708EFF", col = "white"),
      fg_params = list(col = "white", fontface = "bold")
    ),
    base_size = 10,
    padding = unit(c(8, 4), "mm")
  )
)

# Add title and caption
title <- textGrob("STD Frequency Distribution", 
                  gp = gpar(fontsize = 14, fontface = "bold"),
                  vjust = 0.5)
footer <- textGrob("Data source: risk_factors.csv", 
                   gp = gpar(fontsize = 9, col = "gray40"),
                   vjust = 1)

# Combine elements - CORRECTED VERSION
final_table <- gtable_add_rows(
  table_grob, 
  heights = grobHeight(title) + unit(4, "mm"),
  pos = 0
)
final_table <- gtable_add_grob(
  final_table,
  list(title),
  t = 1, l = 1, r = ncol(table_grob)
)
final_table <- gtable_add_rows(
  final_table,
  heights = grobHeight(footer) + unit(2, "mm")
)
final_table <- gtable_add_grob(
  final_table,
  list(footer),
  t = nrow(final_table), l = 1, r = ncol(final_table)
)

# Save output
ggsave("figures/disease_table.png", 
       plot = final_table,
       width = 8, 
       height = 5,
       dpi = 300)
