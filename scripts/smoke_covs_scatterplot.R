library(tidyr)
library(ggplot2)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
subset_smoke<-d1[ , c('Smokes..years.', 'Smokes..packs.year.')]
subset_smoke$Smokes..years.<-as.double(subset_smoke$Smokes..years.)
subset_smoke$Smokes..packs.year.<-as.double(subset_smoke$Smokes..packs.year.)
subset_smoke1 <- subset_smoke[which(subset_smoke$Smokes..years.!=0 & 
                                  !is.na(subset_smoke$Smokes..years.)),]
dev.new()
png(file="figures/smoke_covs_scatter.png",
    width=600, height=350)
ggplot(subset_smoke1, aes(x=Smokes..years., y=Smokes..packs.year.)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm, color="black") +
  labs(title="Years of Smoke against Smoke Pack per Year Scatterplot",
       x="Years", y = "Smoke Pack/Year")
dev.off()