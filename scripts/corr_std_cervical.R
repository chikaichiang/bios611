library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$STDs<-as.integer(d1$STDs)
d1<-d1[ ,c(13,14,16,17,18,19,20,21,23,24,25,26,33,34,35,36)]
colnames(d1)<-c("STD_Cnt","Condy","VC","VPC","Shypilis","PID","GH","MC","HIV","HB","HPV","No_Diag","Hinselmann","Schiller","Citology","Biopsy")
d1 <- d1 %>% mutate_if(is.character, as.numeric)
d1<- d1 %>% drop_na()
corr <- round(cor(na.omit(d1)), 2)
melted_cormat <- melt(corr)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(corr)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggsave("figures/corr_std_cervical.png")

