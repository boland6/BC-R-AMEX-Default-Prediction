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


#MODEL TRAINING
#Create a regression tree to predict Count using cp = 0.03
tree03 <- rpart(target ~., data = df_train, method = "anova", cp = 0.03)
# ~. means any and all rows  
# anova means classification, not prediction
rpart.plot(tree03, roundint = FALSE, nn = TRUE, extra = 1)


#MODEL TUNING
#Make predictions of training data with tree03
pred_tree03_train <- predict(tree03, df_train)
#Calculate root mean-squared error (RMSE) of tree03 in training data
RMSE(pred_tree03_train, df_train$target)


#AUTOMATED TRAINING AND TUNING

#Create dataframe to store results
tree_results <- data.frame()

#run loop to train models with different cp
for (num in seq(0.0025, 0.0500, 0.0025)) {
  
  #Create a regression tree to predict Count using cp = num
  tree <- rpart(target ~., data = df_train, method = "anova", cp = num)
  
  #Make predictions of training data with tree
  pred_tree_train <- predict(tree, df_train)
  #Calculate root mean-squared error (RMSE) of tree03 in training data
  tree_rmse <- RMSE(pred_tree_train, df_train$target)
  
  #Create vector of tree output
  tree_output <- c(num, tree_rmse);
  #Add results to data frame, rbind is what's used to add to the data frame
  tree_results <- rbind(tree_results, tree_output)
  
}

#Add descriptive headers to results of the dataframe
colnames(tree_results) <- c("cp", "RMSE")



################################################################################################
################################################################################################
########################////////Evaluate Fit of the Regression Tree model ////////##########################################
################################################################################################
################################################################################################

#MODEL TESTING
#Make predictions of test data with tree03
pred_tree03_test <- predict(tree03, df_test)
#Calculate root mean-squared error (RMSE) of tree03 in test data
RMSE(pred_tree03_test, df_test$target)

test_tree03 <- rpart(target ~., data = df_test, method = "anova", cp = 0.03)
rpart.plot(test_tree03, roundint = FALSE, nn = TRUE, extra = 1)
