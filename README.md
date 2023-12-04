# BC-R-AMEX-Default-Prediction
**R-Based Credit Default Prediction Model**


**A Machine Learning Project utilizing Kaggle AMEX anonymized dataset to predict defaults. **

Project Source:
https://www.kaggle.com/competitions/amex-default-prediction/data

**Project Description**
This project develops a predictive model using R to estimate the probability of credit card payment defaults. Utilizing the American Express Default Prediction dataset from Kaggle, the model aims to forecast whether a customer will fail to pay back their credit card balance. The dataset includes anonymized and normalized customer profile features from each statement date, categorized into Delinquency (D_), Spend (S_), Payment (P_), Balance (B_), and Risk (R_*) variables. The goal is to predict the likelihood of a future payment default for each customer.

**Data Source**
The dataset is sourced from the American Express Default Prediction Kaggle Competition. It spans 18 months of customer data and classifies a default event as non-payment within 120 days post the latest statement date. Notably, certain features like 'B_30', 'B_38', 'D_114', etc., are categorical.

**Technologies Used**
R language
R packages
  -dplyr

**Files**
train_data.csv - training data with multiple statement dates per customer_ID
train_labels.csv - target label for each customer_ID

**Model Overview**
A detailed description of the predictive model(s) used, including any specific algorithms or techniques.

**Results and Evaluation**
Performance metrics and evaluation results of the model.

**Contributor**
This project is done as a class project at Boston College Carroll School of Management, MBA Graduate Programs- 
BZAN8067.01 Fall 2023 Machine Learning for Business Intelligence (Prof. VanderWerf)

  Team Member
    -Austin Barrett
    -Zishu Yuan
    -Patrick Boland

# Training Data Creation
Training data (train.csv) is provided by AMEX on Kaggle, and derived from https://www.kaggle.com/competitions/amex-default-prediction/data

However, the data contain about 190 features with 458K unique customer values. This will entail significant processing time for training models. In the interest of efficiency, we selected a subset of the training data. 

There is a a fork of the Kaggle project, American Express - Default Prediction (small), that contains a smaller subset of the data (Note: This Kaggle project page is in Korean) -
https://www.kaggle.com/competitions/amex-default-prediction-small

This subset contains 7000 obs from the original training data. Separately, the outcome have been joined into the training data by the forking author.

# Test Data Creation - create_test_data.R

*Script - create_test_data.R*

**Script Purpose**

The script's primary objective is to generate a test dataset from an existing, larger training dataset. This smaller dataset is intended for testing and validation purposes in the context of predicting American Express defaults. This allows us to utilize the same testing dataframe for consistency.

**Libraries**

The script uses data.table and dplyr libraries.

**Key Variables**


* **Test Data Size**: A specific size for the test dataset is set (1750 rows), targeting an 80-20 train-test split ratio. This size is chosen to provide a substantial yet manageable subset of the original dataset for testing purposes.

* **Random Seed**: A random seed of 23 is set to ensure reproducibility. This means that the random selection of data rows will be consistent across different script executions, given the same seed value.

**Test Data Creation**

**Reading and Sampling**: The script reads the large training dataset using fread from data.table. Then randomly selects a subset of rows based on the predetermined test data size mentioned above.

**Data Subsetting**: The sampled rows are extracted to form the test dataset.

**Label Integration**

**Join Operation**: Using dplyr, a left join is performed between the test dataset and the labels dataset. This merge is based on customer_ID, adding relevant outcome information to each row of the test dataset.

**Output Generation**
The script writes the test dataset, now appended with labels, to a CSV file. This file serves as the output, ready for use in further testing or model validation processes.





    


