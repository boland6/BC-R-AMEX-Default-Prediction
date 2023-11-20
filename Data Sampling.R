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
