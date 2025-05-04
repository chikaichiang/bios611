library(tidyverse)
library(tidyr)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
first_sexual_intercourse<-table(as.integer(d1$First.sexual.intercourse),useNA = "ifany")
dev.new()
png(file="figures/first_sexual_inter_bar.png",
    width=600, height=350)
b <- barplot(first_sexual_intercourse, beside=TRUE, ylim=c(0, max(first_sexual_intercourse) + 15), 
             main="Age of First Sexual Intercourse", xlab = "Age",col=2:3, border=0)
text(b, first_sexual_intercourse + 5, first_sexual_intercourse, font=2, col=2:3)
dev.off()
