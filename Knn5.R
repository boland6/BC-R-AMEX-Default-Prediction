library(class)
library(mosaic)


Traindata <- read.csv("train_data.csv")


predictors <- Traindata[, -which(colnames(Traindata) == "target")]
target <- Traindata$target


v_knn3_train <- knn(predictors, predictors, target, k = 5)


accuracy <- mean(~(target == v_knn3_train), data = Traindata)
print(accuracy)

raw_counts <- tally(target ~ v_knn3_train, data = Traindata) %>% addmargins()
print(raw_counts)

percentage_table <- tally(target ~ v_knn3_train, data = Traindata) %>% 
  prop.table(margin = 1) %>% round(2)
print(percentage_table)
