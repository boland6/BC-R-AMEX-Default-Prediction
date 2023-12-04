#Setting working directory
setwd("~/BC-R-AMEX-Default-Prediction")


############################################################################################
#######################Libraries###########################################################
#Packages and libraries
library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)
library(data.table)
library(mosaic) # #Functions for common statistical tasks.
library(stats)
library(openxlsx)

############################################################################################


############################################################################################
#######################Data Loading###########################################################
#Load the master data file.
#Ingesting data from AMEX
#This contains the labels for the training data for the entire data set
            #also removes ID variable
d_AMEX_train <- read.csv("train.csv") %>% select(-customer_ID)
d_AMEX_test <-read.csv("test.csv") %>% select(-customer_ID)


################################################################################################
################################################################################################
########################////////Model Training////////##########################################
#####################			Stepwise Regression		      ###################################
################################################################################################
################################################################################################


#####AUTOMATED MODEL TRAINING AND TUNING#####
#Build a stepwise logistic regression model
#Make a model with no independent variables.
ml_lognull <- glm(target ~ 1, data=d_AMEX_train, family=binomial) #base R

#Make a model with all available independent variables.
ml_logall <- glm(target ~., data=d_AMEX_train, family=binomial) #base R

#Create a model using the stepwise procedure.
ml_logstep <- step(ml_lognull, scope=formula(ml_logall)) #base R

#Display the main results.  
summary(ml_logstep) #base R


# Capture the output of the summary
reg_output <- capture.output(ml_logstep)

# Write the output to a text file
writeLines(reg_output, "regression_model_summary.txt")


################################################################################################
################################################################################################
########################Evaluate Fit of the stepwise model (Training Data)######################
################################################################################################
################################################################################################

#Evaluate Fit of the stepwise model 
#Create vector of predictions
#round(): round estimated probabilities to get 0 or 1.
v_logstep_train_preds <- predict(ml_logstep, d_AMEX_train, type="response") %>% round() #dplyr

#Compute/display the Percentage Correct in the training data to evaluate fit.
reg_percentage_correct_train <- mean(~(target == v_logstep_train_preds), data=d_AMEX_train) #mosaic
print(reg_percentage_correct_train)

#Create/display Classification Tables for the training data.
#Classification Table of raw counts.
classification_table_counts_train <- tally(target ~ v_logstep_train_preds, data=d_AMEX_train) %>% addmargins() #mosaic
print(classification_table_counts_train)

#Classification Table of percentages of training data.
classification_table_percentages_train <- tally(target ~ v_logstep_train_preds, data=d_AMEX_train) %>% 
  prop.table(margin=1) %>% round(2) #mosaic 
print(classification_table_percentages_train)

# Save the combined result into one worksheet
# Create a new Excel workbook
wb_train <- createWorkbook()

# Add sheets and write data to each sheet
addWorksheet(wb_train, "Accuracy")
writeData(wb_train, "Accuracy", data.frame(Accuracy = reg_percentage_correct_train))

addWorksheet(wb_train, "Classification Table Counts")
writeData(wb_train, "Classification Table Counts", classification_table_counts_train)

addWorksheet(wb_train, "Classi Table Percentages")
writeData(wb_train, "Classi Table Percentages", classification_table_percentages_train)

# Save the workbook
saveWorkbook(wb_train, "log_reg_accuracy_training.xlsx", overwrite = TRUE)



################################################################################################
################################################################################################
########################Evaluate Fit of the stepwise model (Test Data)######################
################################################################################################
################################################################################################

#Evaluate Fit of the stepwise model 
#Create vector of predictions
#round(): round estimated probabilities to get 0 or 1.
v_logstep_test_preds <- predict(ml_logstep, d_AMEX_test, type="response") %>% round() #dplyr

#Compute/display the Percentage Correct in the training data to evaluate fit.
reg_percentage_correct_test <- mean(~(target == v_logstep_test_preds), data=d_AMEX_test) #mosaic
print(reg_percentage_correct_test)

#Create/display Classification Tables for the training data.
#Classification Table of raw counts.
classification_table_counts_test <- tally(target ~ v_logstep_test_preds, data=d_AMEX_test) %>% addmargins() #mosaic
print(classification_table_counts_test)

#Classification Table of percentages of training data.
classification_table_percentages_test <- tally(target ~ v_logstep_test_preds, data=d_AMEX_test) %>% 
  prop.table(margin=1) %>% round(2) #mosaic 
print(classification_table_percentages_test)


# Save the combined result into one worksheet
# Create a new Excel workbook
wb_test <- createWorkbook()

# Add sheets and write data to each sheet
addWorksheet(wb_test, "Accuracy")
writeData(wb_test, "Accuracy", data.frame(Accuracy = reg_percentage_correct_test))

addWorksheet(wb_test, "Classification Table Counts")
writeData(wb_test, "Classification Table Counts", classification_table_counts_test)

addWorksheet(wb_test, "Classi Table Percentages")
writeData(wb_test, "Classi Table Percentages", classification_table_percentages_test)

# Save the workbook
saveWorkbook(wb_test, "log_reg_accuracy_test.xlsx", overwrite = TRUE)
