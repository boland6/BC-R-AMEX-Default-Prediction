
library(class)
library(mosaic)


Traindata <- read.csv("train_data.csv")


predictors <- Traindata[, -which(colnames(Traindata) == "target")]
target <- Traindata$target


v_knn3_train <- knn(predictors, predictors, target, k = 3)


accuracy <- mean(~(target == v_knn3_train), data = Traindata)
print(accuracy)

raw_counts <- tally(target ~ v_knn3_train, data = Traindata) %>% addmargins()
print(raw_counts)

percentage_table <- tally(target ~ v_knn3_train, data = Traindata) %>% 
  prop.table(margin = 1) %>% round(2)
print(percentage_table)




Testdata <- read.csv("test_data.csv")

test_predictors <- Testdata[, -which(colnames(Testdata) == "target")]
test_target <- Testdata$target

v_knn3_test <- knn(train = predictors, test = test_predictors, cl = target, k = 3)


accuracy_test <- mean(~(test_target == v_knn3_test), data = Testdata)
print(accuracy_test)


raw_counts_test <- tally(test_target ~ v_knn3_test, data = Testdata) %>% addmargins()
print(raw_counts_test)


percentage_table_test <- tally(test_target ~ v_knn3_test, data = Testdata) %>% 
  prop.table(margin = 1) %>% round(2)
print(percentage_table_test)
