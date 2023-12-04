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
library(rpart)
library(rpart.plot)

############################################################################################


############################################################################################
#######################Data Loading###########################################################
#Load the master data file.
#Ingesting data from AMEX
#This contains the labels for the training data for the entire data set
df <- read.csv("train.csv")
############################################################################################


############################################################################################
#######################DATA PROFILING###################################################

# List all columns and features of the dataframe
column_names <- colnames(df)
print(column_names)

#Check for NAs
sum(is.na(df))

#Check for duplicates
sum(duplicated(df))



############################################################################################
#######################DATA PREPROCESSING###################################################

#Examine categorical columns

cate_columns <- c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_63', 'D_64', 'D_66', 'D_68')
pk_column <- c('customer_ID')

# Function to calculate the mode (most frequent value)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing values
for (col in names(df)) {
  # Skip the primary key column
  if (col == pk_column) {
    next
  }
  
  # Check if column is numeric
  if (is.numeric(df[[col]])) {
    # Replace NA with the mean
    mean_value <- mean(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- mean_value
  } 
  # Impute categorical columns
  else if (col %in% cate_columns) {
    # Replace NA with the mode or a separate category
    mode_value <- getmode(df[[col]][!is.na(df[[col]])])
    if (is.na(mode_value)) { mode_value <- 'Missing' }  # In case the mode is NA
    df[[col]][is.na(df[[col]])] <- mode_value
  }
  # If not numeric or categorical, potentially do something else or nothing
  else {
    #handle other types of data or skip
  }
}


############################################################################################
############################################################################################
############################################################################################



#Create dummy variable for categorical data

#Create dummy variable (also removes ID variable)
df_2 <-dummy_cols(df, select_columns = cate_columns, 
                          remove_first_dummy = TRUE, 
                          remove_selected_columns = TRUE) %>% select (-'customer_ID', -'S_2')


set.seed(23) # for reproducibility

# Calculate the number of rows that will be in the training set
training_size <- floor(0.80 * nrow(df_2))

# Randomly sample row indices for the training set
training_indices <- sample(seq_len(nrow(df_2)), size = training_size)

# Create a new column 'partition' and assign 'train' or 'test'
df_3 <- df_2 %>%
  mutate(partition = if_else(row_number() %in% training_indices, 'train', 'test'))

# View the first few rows of the modified DataFrame
head(df_3)


#Create data frame for training and test data from partitioned data

#Divide by partitions
df_train <-filter(df_3,partition == "train") %>% select (-'partition')
df_test <-filter(df_3,partition == "test") %>% select (-'partition')


################################################################################################
################################################################################################
########################////////Model Training////////##########################################
#####################			Regression Tree	      ###################################
################################################################################################
################################################################################################


#AUTOMATED TRAINING AND TUNING

#Create dataframe to store results
d_tree_output <- data.frame()

#Run loop for tree with a range of complexity parameters from 0.02 to 0.2
for (num in seq(0.02,0.2,0.02)) {
  
  #Build tree model with different complexity parameters
  ml_tree <- rpart(target ~., data=df_train, 
                   method="class", cp= num)
  
  #Evaluate Fit of the tree with cp = num
  #Create vector of predictions
  v_tree_train_preds <- predict(ml_tree, df_train,type="class") #dplyr
  #Compute/display the Percentage Correct in training data to evaluate fit.
  percentage_correct <- mean(~(target == v_tree_train_preds), data=df_train) #mosaic
  
  #Create output to add results to data frame
  v_output = c(num, percentage_correct)#base
  #Add results to data frame
  d_tree_output = rbind(d_tree_output, v_output)#base
}    

#Name the columns to make the data frame more legible when viewing
colnames(d_tree_output) <- c("cp", "Training %Correct")#base

#View data frame to see which complexity parameter performed best for both training and test

#MODEL TRAINING
#Create a regression tree to predict Count using cp = 0.02
tree02 <- rpart(target ~., data = df_train, method = "class", cp = 0.02)
# ~. means any and all rows  
# anova means classification, not prediction
rpart.plot(tree02, roundint = FALSE, nn = TRUE, extra = 4)


#MODEL TUNING
#Make predictions of training data with tree03
pred_tree02_train <- predict(tree02, df_train,type="class")

#Compute/display the Percentage Correct in training data to evaluate fit.
mean(~(target == pred_tree02_train), data=df_train) #mosaic
#Create/display Classification Tables for the training data.
#Classification Table of raw counts.
tally(target ~ pred_tree02_train, data=df_train) %>% addmargins() #mosaic
#Classification Table of percentages of training data.
tally(target ~ pred_tree02_train, data=df_train) %>% 
  prop.table(margin=1) %>% round(2) #mosaic


################################################################################################
################################################################################################
########################////////Evaluate Fit of the Regression Tree model ////////##########################################
################################################################################################
################################################################################################

#MODEL TESTING

#Evaluate accuracy of the cp=0.02 model
#Create vector of predictions in test data
v_tree02_test_preds <- predict(tree02, df_test,type="class") #dplyr
#Compute/display the Percentage Correct in test data to evaluate accuracy.
mean(~(target == v_tree02_test_preds), data=df_test) #mosaic
#Create/display Classification Tables for test data.
#Classification Table of raw counts.
tally(target ~ v_tree02_test_preds, data=df_test) %>% addmargins() #mosaic
#Classification Table of percentages of test data.
tally(target ~ v_tree02_test_preds, data=df_test) %>% 
  prop.table(margin=1) %>% round(2) #mosaic

