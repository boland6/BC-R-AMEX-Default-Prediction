
library(e1071)
library(mosaic)


Traindata <- read.csv("train_data.csv") 

ml_svm <- svm(target ~ ., data = Traindata, type = "C-classification", cost = 10, scale = FALSE)

v_preds_svm_train <- predict(ml_svm, Traindata)

accuracy <- mean(~(target == v_preds_svm_train), data = Traindata)
print(accuracy)

raw_counts <- tally(target ~ v_preds_svm_train, data = Traindata) %>% addmargins() #mosaic
print(raw_counts)

percentage_table <- tally(target ~ v_preds_svm_train, data = Traindata) %>% 
  prop.table(margin = 1) %>% round(2) #mosaic
print(percentage_table)


Testdata <- read.csv("test_data.csv") 

v_preds_svm_test <- predict(ml_svm, Testdata)


accuracy_test <- mean(~(target == v_preds_svm_test), data = Testdata)

print(accuracy_test)

raw_counts_test <- tally(target ~ v_preds_svm_test, data = Testdata) %>% addmargins()#mosaic
print(raw_counts_test)

percentage_table_test <- tally(target ~ v_preds_svm_test, data = Testdata) %>% 
  prop.table(margin = 1) %>% round(2)#mosaic
print(percentage_table_test)

