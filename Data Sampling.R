#Setting working directory
setwd("~/BC-R-AMEX-Default-Prediction")

#Packages and libraries
library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)
library(data.table)

#Ingesting data from AMEX
  #This contains the labels for the training data for the entire data set
df <- read.csv("train.csv")

# List all columns and features of the dataframe
column_names <- colnames(df)
print(column_names)

############################################################################################
#######################Data Check###########################################################

#Check for NAs
sum(is.na(df))

#Check for duplicates
sum(duplicated(df))

############################################################################################
#######################Data CLeanup###########################################################
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
                          remove_selected_columns = TRUE) %>% select (-'customer_ID')


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
#####################			Stepwise Regression		      ###################################
################################################################################################
################################################################################################


#####AUTOMATED MODEL TRAINING AND TUNING#####
#Run a stepwise regression

#Create model with no independent variables
nullmodel <- glm(target ~ 1, data = df_train)
summary(nullmodel)

#Create model with all the independent variables
allmodel <- glm(target ~., data = df_train)
summary(allmodel)



#Calculate the stepwise equation (Create Model)
Regstep <- step(nullmodel, scope = formula(allmodel))
summary(Regstep)

# Capture the output of the summary
reg_output <- capture.output(Regstep)

# Write the output to a text file
writeLines(reg_output, "regression_model_summary.txt")



#########Get Predictions######

#Get predictions against training data using created model
pred_Regstep_train <- predict(Regstep, df_train)
#Get predictions against testing data using created model
pred_Regstep_test <- predict(Regstep, df_test)





