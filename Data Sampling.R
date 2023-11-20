#Setting working directory
setwd("~/Downloads/amex-default-prediction")

#Packages and libraries
library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)
library(data.table)

#Ingesting data from AMEX
  #This contains the labels for the training data for the entire dataset
df_labels <- read.csv("train_labels.csv")





