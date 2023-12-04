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

############################################################################################


############################################################################################
#######################Data Loading###########################################################
#Load the master data file.
#Ingesting data from AMEX
#This contains the labels for the training data for the entire data set
d_AMEX_train <- read.csv("train.csv") %>% select(-customer_ID)
d_AMEX_test <-read.csv("test.csv") %>% select(-customer_ID)
############################################################################################
########################////////Remove ID////////##########################################



################################################################################################
################################################################################################
########################////////Model Training////////##########################################
#####################			Stepwise Regression		      ###################################
################################################################################################
################################################################################################


#####AUTOMATED MODEL TRAINING AND TUNING#####
#Build a stepwise logistic regression model
#Make a model with no independent variables.
ml_lognull <- glm(target ~ 1, data=d_AMEX_train, family=binomial) #base R

#Make a model with all available independent variables.
ml_logall <- glm(target ~., data=d_AMEX_train, family=binomial) #base R

#Create a model using the stepwise procedure.
ml_logstep <- step(ml_lognull, scope=formula(ml_logall)) #base R

#Display the main results.  
summary(ml_logstep) #base R

