#Setting working directory
setwd("~/home/workspaces/mba/MachineLearning_BZAN8067/Final_Project/BC-R-AMEX-Default-Prediction")


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
library(nnet) #Algorithms for creating neural network models.

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


#Partitioning the variables
#partition <- sample(c("train","test"), size = nrow(df_2), replace = TRUE, prob = c(0.8,0.2))
#df_3 <- mutate(df_2, partition)

set.seed(1) # for reproducibility

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
#####################			Neural Network      	      ###################################
################################################################################################
################################################################################################


#AUTOMATED MODEL TRAINING AND TUNING
#Use a loop to determine best number of nodes based on % correct
#Store the number of nodes and % correct in a new data frame.
#Create an empty data frame to store results from loop.
d_nn_output = data.frame()

#Run loop for a neural network with the number of hidden nodes ranging from 2 to 19
for (num in 2:10) {
  
  #Set the seed for the first model to ensure consistent results.
  set.seed(1) #base R
  
  #Create a Neural Network with num hidden nodes.
  ml_nn <- nnet(target~., data = df_train, 
                size= num,linout=F,decay=0.05,MaxNWts=10000) #nnet
  
  #Evaluate fit of the model with num hidden layer nodes 
  #Create vector of predictions
  #round(): round estimated probabilities to get 0 or 1.
  v_nn_train_preds <- predict(ml_nn, df_train) %>% round() #dplyr
  #Compute/display the Percentage Correct in training data to evaluate fit.
  percentage_correct <- mean(~(target == v_nn_train_preds), data=df_train) #mosaic
  
  #Create output to add results to data frame
  v_output = c(num, percentage_correct)#base
  #Add results to data frame
  d_nn_output = rbind(d_nn_output, v_output)#base
}    

#Name the columns to make the data frame more legible when viewing
colnames(d_nn_output) <- c("Number of Hidden Nodes", "Percentage Correct")#base

#View data frame to see which number of n performed best for the training set

#The model with the 9 hidden nodes performed best, so store that model
#Set the seed to ensure consistent results.
set.seed(1) #base R
ml_nn9 <- nnet(target~., data = df_train, 
                size=9,linout=F,decay=0.05,MaxNWts=10000) #nnet

#Create vector of predictions
v_pred_nn9_train_preds <- predict(ml_nn9, df_train) %>% round() #dplyr
#Compute/display the Percentage Correct in training data to evaluate fit.
mean(~(target == v_pred_nn9_train_preds), data=df_train) #mosaic
#Create/display Classification Tables for the training data.
#Classification Table of raw counts.
tally(target ~ v_pred_nn9_train_preds, data=df_train) %>% addmargins() #mosaic
#Classification Table of percentages of training data.
tally(target ~ v_pred_nn9_train_preds, data=df_train) %>% 
  prop.table(margin=1) %>% round(2) #mosaic      


################################################################################################
################################################################################################
########################////////Evaluate Fit of the neural network model ////////###############
################################################################################################
################################################################################################

#MODEL TESTING

#Evaluate accuracy of 9-node model
#Create vector of predictions in test data
#round(): round estimated probabilities to get 0 or 1.
v_nn9_test_preds <- predict(ml_nn9, df_test) %>% round() #dplyr
#Compute/display the Percentage Correct in the test data to evaluate accuracy.
mean(~(target == v_nn9_test_preds), data=df_test) #mosaic
#Create/display Classification Tables for the test data.
#Classification Table of raw counts.
tally(target ~ v_nn9_test_preds, data=df_test) %>% addmargins() #mosaic
#Classification Table of percentages of test data.
tally(target ~ v_nn9_test_preds, data=df_test) %>% 
  prop.table(margin=1) %>% round(2) #mosaic     


