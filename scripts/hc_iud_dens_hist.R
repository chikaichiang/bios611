library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)
options(warn=-1)
theme_set(theme_bw())
d1<-read.csv("Data/risk_factors.csv")
subset_birth_treat<-d1[ , c('Hormonal.Contraceptives', 'Hormonal.Contraceptives..years.','IUD','IUD..years.')]
subset_birth_treat$Hormonal.Contraceptives<-as.integer(subset_birth_treat$Hormonal.Contraceptives)
subset_birth_treat$IUD<-as.integer(subset_birth_treat$IUD)
subset_birth_treat$Hormonal.Contraceptives..years.<-as.numeric(subset_birth_treat$Hormonal.Contraceptives..years.)
subset_birth_treat$IUD..years.<-as.numeric(subset_birth_treat$IUD..years.)
subset_birth_treat1 <- subset_birth_treat[which((subset_birth_treat$Hormonal.Contraceptives!=0 |
                                                  subset_birth_treat$IUD!=0) & 
                                                  (!is.na(subset_birth_treat$Hormonal.Contraceptives) | 
                                                  !is.na(subset_birth_treat$IUD))),]
subset_birth_hc <- subset_birth_treat1[which(subset_birth_treat1$Hormonal.Contraceptives!=0 &
                                                                               !is.na(subset_birth_treat1$Hormonal.Contraceptives..years.)),]
subset_birth_iud <- subset_birth_treat1[which(subset_birth_treat1$IUD!=0 &
                                               !is.na(subset_birth_treat1$IUD..years.)),]
dev.new()
png(file="figures/hc_iud_dens_hist.png",
    width=600, height=350)
g1<-ggplot(subset_birth_hc, aes(x = Hormonal.Contraceptives..years.)) +                           
  geom_density(alpha = 0.1, fill = "red") +
  labs(title = "Dens of Years having HC", x = "Years")
g2<-ggplot(subset_birth_hc, aes(x = Hormonal.Contraceptives..years.)) + geom_histogram() +
  labs(title = "Hist of Years having HC", x = "Years")
g3<-ggplot(subset_birth_iud, aes(x = IUD..years.)) +                           
  geom_density(alpha = 0.1, fill = "red") +
  labs(title = "Dens of Years having IUD", x = "Years")
g4<-ggplot(subset_birth_iud, aes(x = IUD..years.)) + geom_histogram() +
  labs(title = "Hist of Years having IUD", x = "Years")
g1+g2+g3+g4
dev.off()
