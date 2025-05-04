library(tidyverse)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(gam)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer <- d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy

d1$CervicalCancer <- factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
                           
control <- rfeControl(functions = rfFuncs, method = "repeatedcv",repeats = 5, number = 10)

x <- d1 %>%
     select(Age, Number.of.sexual.partners, First.sexual.intercourse, Num.of.pregnancies, 
            Smokes..years.,Smokes..packs.year.,Hormonal.Contraceptives..years.,IUD..years.,
            STDs..number.,STDs..Time.since.first.diagnosis,STDs..Time.since.last.diagnosis,
            Dx.Cancer,Dx.CIN) %>% as.data.frame()

x <- x %>% mutate_if(is.character, as.numeric)

y <- d1$CervicalCancer

x<-x %>% mutate_each(funs(replace(., is.na(.), -1)))

set.seed(2023)

inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain,]
x_test  <- x[-inTrain,]
y_train <- y[ inTrain]
y_test  <- y[-inTrain]
result_rfe1 <- rfe(x = x_train, 
                  y = y_train, 
                  sizes = c(1:13),
                  rfeControl = control)
varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:8],
                          importance = varImp(result_rfe1)[1:8, 1])
ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")
ggsave("figures/feature_selection1.png")