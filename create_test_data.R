#The purpose of this R script is to create a common test dataset from the original large dataset


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###########################################Setting working directory###############################################
setwd("~/BC-R-AMEX-Default-Prediction")



###########################################Load Packages###########################################################

library(data.table)


###########################################Defining File Path######################################################

# Define the file path to original large training data
large_train_data_path <- "train_data(large).csv"

#Define the file path to original large training data labels
large_train_labels_path <- "train_labels.csv"


###########################################Defining Key Variables######################################################

#Setting the desired testing datasize (1750 is approximately a 80train/20test split)
testing_dataSize <- 1750

# Set a random seed for reproducibility
set.seed(23)

###########################################Create Training Data From Large Dataset################################

# Read the file as a data.table
large_train_data <- fread(large_train_data_path)

# Determine the total number of rows
total_testingdata_rows <- nrow(large_train_data)

# Generate a random sample of row indices 
sample_indices <- sample(2:total_testingdata_rows, testing_dataSize, replace = FALSE)

# Subset the data.table
selected_test_data <- large_train_data[sample_indices, ]

#read column of selected test data
names(selected_test_data)


###########################################################################

