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
  select(STDs.condylomatosis,STDs.cervical.condylomatosis,STDs.vaginal.condylomatosis,
         STDs.vulvo.perineal.condylomatosis,STDs.syphilis,STDs.pelvic.inflammatory.disease,STDs.genital.herpes,
         STDs.molluscum.contagiosum,STDs.AIDS,STDs.HIV,STDs.Hepatitis.B,STDs.HPV) %>% as.data.frame()
colnames(x)<-c("Condy","C.Condy","V.Condy","VP.Condy","Syphilis","PID","GH","MC","AIDS","HIV","HB","HPV")
  
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
                   sizes = c(1:12),
                   rfeControl = control)
varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:3],
                          importance = varImp(result_rfe1)[1:3, 1])
ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")
ggsave("figures/feature_selection2.png")
