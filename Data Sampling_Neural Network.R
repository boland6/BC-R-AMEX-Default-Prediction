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
#####################			Neural Network      	      ###################################
################################################################################################
################################################################################################

#Set the seed for the first model to ensure consistent results.
set.seed(1) #base R
#Create a Neural Network with 3 hidden nodes.
# maxit and MaxNWts are needed to be defined because the default values are too low for a dataset this large
ml_nn3 <- nnet(target~., data = df_train, 
               size=3, linout=T, decay=1, maxit=1000, MaxNWts=2000) #nnet


#MODEL TUNING

#Evaluate fit of the model with 3 hidden layer nodes 
#Get predictions from the model for the training data.
d_pred_nn3_train <- predict(ml_nn3, df_train)  #base R
#Calculate the RMSE for the predicted vs. actual values in training data.
RMSE(d_pred_nn3_train, df_train$target) #caret

#AUTOMATED MODEL TRAINING AND TUNING
#Use a loop to figure out the best number of hidden nodes based on RMSE
#The loop will automatically store the number of hidden nodes and RMSE to a new data frame.
#Create an empty data frame to store results from loop.
d_nn_output = data.frame() #base R

#Run loop for a neural network with the number of hidden nodes ranging from 2 to 19
for (num in 1:6) {
  
  #Set the seed for the first model to ensure consistent results.
  set.seed(1) #base R
  
  #Create a Neural Network with num hidden nodes.
  # maxit and MaxNWts are needed to be defined because the default values are too low for a dataset this large
  ml_nn <- nnet(target~., data = df_train, 
                size = num, linout=T, decay=1, maxit=1000, MaxNWts=10000) #nnet
  
  #Evaluate fit of the model with num hidden layer nodes 
  #Get predictions from the model for the training data.
  d_pred_nn_train <- predict(ml_nn, df_train)  #base R
  #Calculate the RMSE for the predicted vs. actual values in training data.
  RMSE_train <- RMSE(d_pred_nn_train, df_train$target) #caret
  
  #Create output to add results to data frame
  v_output = c(num, RMSE_train)#base
  #Add results to data frame
  d_nn_output = rbind(d_nn_output, v_output)#base
}    

#Name the columns to make the data frame more legible when viewing
colnames(d_nn_output) <- c("Number of Hidden Nodes", "RMSE_train")#base

#View data frame to see which number of n performed best for the training set


################################################################################################
################################################################################################
########################////////Evaluate Fit of the neural network model ////////###############
################################################################################################
################################################################################################

#The model with the 6 hidden nodes performed best, so store that model
#Set the seed to ensure consistent results.
set.seed(1) #base R
ml_nn6 <- nnet(target~., data = df_train, 
                size = 6, linout=T, decay=1, maxit=1000, MaxNWts=10000) #nnet

#Evaluate accuracy of 14-node model
#Get predictions from the model for the test data.
d_pred_nn6_test <- predict(ml_nn6, df_test)  #base R
#Calculate the RMSE for the predicted vs. actual values in test data.
RMSE(d_pred_nn6_test, df_test$target) #caret
