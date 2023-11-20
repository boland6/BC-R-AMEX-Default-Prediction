#Setting working directory
setwd("~/Downloads/amex-default-prediction")

#Packages and libraries
library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)

#Ingesting data from AMEX
  #This contains the labels for the training data for the entire dataset
df_labels <- read.csv("train_labels.csv")


# Function to sample a large file without reading into memory

sample_training_data <- function(file_path, sample_size_mb, chunk_size = 1000, seed = 23) {
  
  # Set the seed for reproducibility
  set.seed(seed)
  
  # Estimate the number of lines in the file
  initial_read <- fread(file_path, nrows = chunk_size)
  file_size <- file.size(file_path)
  lines_estimate <- nrow(initial_read) / (file_size / 15e+9) * file_size
  
  # Determine the number of lines to sample based on the desired sample size
  avg_line_size <- file_size / lines_estimate
  num_lines_to_sample <- (sample_size_mb * 1e+6) / avg_line_size
  
  # Create a vector of line numbers to sample
  line_numbers_to_sample <- sort(sample(lines_estimate, num_lines_to_sample))
  
  # Read the sampled lines
  sampled_data <- rbindlist(lapply(line_numbers_to_sample, function(ln) fread(file_path, skip = ln - 1, nrows = 1)))
  
  # Return the sampled data
  return(sampled_data)
}



