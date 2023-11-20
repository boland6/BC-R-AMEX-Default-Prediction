#Setting working directory
setwd("~/Downloads/amex-default-prediction")

#Packages and libraries
library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)


#Ingesting data from AMEX
df < -read.csv("train_data.csv")
df_labels <- read.csv("train_labels.csv")

# Estimate the size of the full dataset in memory
full_size_df <- object.size(df)
full_size_df_labels <- object.size(df_labels)



