#LOAD THE NECESSARY PACKAGES
library(dplyr) #Functions for editing data frames.
library(haven) #Lets R recognize other datafile types besides csv.
library(mosaic) # #Functions for common statistical tasks.
library(e1071) #Functions for Naive Bayes and Support Vector Machines.
library(fastDummies) #Function for creating dummy columns
library(openxlsx) #Package to write xlsx file

############################################################################################
#######################Data Loading###########################################################
#Load the master data file.
#Ingesting data from AMEX
#This contains the labels for the training data for the entire data set
#also removes ID variable
d_AMEX_train <- read.csv("train.csv") %>% select(-customer_ID)
d_AMEX_test <-read.csv("test.csv") %>% select(-customer_ID)


############################################################################################
#######################Functions###########################################################

#Functions to categorize columns based on conditions
column_cate_stats <- function(data) {
  total_rows <- nrow(data)
  stats <- data.frame(
    Total_Occurrences = sapply(data, length),
    Num_NA = sapply(data, function(x) sum(is.na(x))),
    Percent_NA = numeric(ncol(data))
  )
  
  stats$Percent_NA <- round((stats$Num_NA / total_rows) * 100, 4)
  
  # Set column type
  stats$Column_Type <- sapply(names(data), function(column_name) {
    column <- data[[column_name]]
    
    # Exception for the row name 'total_num_obs'
    if (column_name == "total_num_obs") {
      return("Scale Variable")
    }
    # Check if the column contains only NA values
    else if (all(is.na(column))) {
      return("Only NA")
    }
    # Check if the column contains non-numbers
    else if (any(!is.na(column) & !is.numeric(column))) {
      return("Non-Number")
    }
    # Check if the column is binary (contains only 0 and 1)
    else if (all(column[!is.na(column)] %in% c(0, 1))) {
      return("Binary")
    }
    # Check if the column is a whole number (integer)
    else if (all(floor(column[!is.na(column)]) == column[!is.na(column)])) {
      return("Categorical - Numeral")
    }
    # Check if the column is a scale variable (numeric but not whole number)
    else if (is.numeric(column)) {
      return("Scale Variable")
    }
    # For all other cases
    else {
      return("Other")
    }
  })
  
  # Set row names as column names from the original data
  row.names(stats) <- names(data)
  
  return(stats)
}

# Function to transform a scale variable into a binary variable for Naive Bayes
transform_to_binary <- function(column) {
  median_value <- median(column, na.rm = TRUE)  # Compute median, excluding NA values
  return(as.integer(column >= median_value))    # Convert to binary based on the median
}

#Function to check if all variable is binary for Naive Bayes
check_binary <- function(train_df, test_df) {
  # Function to check if all values in a vector are binary
  is_binary <- function(x) {
    all(x %in% c(0, 1))
  }
  
  # Check for train data frame
  train_non_binary <- sapply(train_df, is_binary)
  if (all(train_non_binary)) {
    cat("All variables in the train data frame are binary.\n")
  } else {
    cat("The train data frame has non-binary variables.\n")
    print(names(train_df)[!train_non_binary])
  }
  
  # Check for test data frame
  test_non_binary <- sapply(test_df, is_binary)
  if (all(test_non_binary)) {
    cat("All variables in the test data frame are binary.\n")
  } else {
    cat("The test data frame has non-binary variables.\n")
    print(names(test_df)[!test_non_binary])
  }
}

############################################################################################
#######################Convert Scale Variables###########################################################

  #Use previous defined function to calculate and categorize column based on whether they are binary or scale
  column_categorizations <- column_cate_stats(d_AMEX_train)
  
  
  #Define Categorical Column
  categorical_Column <- rownames(column_categorizations[column_categorizations$Column_Type == "Categorical - Numeral", ])
  #Define Scale Column
  scale_Column <- rownames(column_categorizations[column_categorizations$Column_Type == "Scale Variable", ])


  #Need to create dummy variable 
  #for categorical columns because the categorical column is current
  #defined by whole number digits 1,2,3 etc. This can imply scale. Thus we need to make dummy for those
        #training data
        d_AMEX_train_dummie <- dummy_cols(d_AMEX_train, 
                                              select_columns = categorical_Column, 
                                              remove_first_dummy = TRUE, 
                                              remove_selected_columns = TRUE)
        
        #test data
        d_AMEX_test_dummie <- dummy_cols(d_AMEX_test, 
                                          select_columns = categorical_Column, 
                                          remove_first_dummy = TRUE, 
                                          remove_selected_columns = TRUE)
  
  
  
  
  
  #Transform Scale variable into Binary variable using median using the created function
  for (col_name in scale_Column) {
    d_AMEX_train_dummie[[col_name]] <- transform_to_binary(d_AMEX_train_dummie[[col_name]])
    d_AMEX_test_dummie[[col_name]] <- transform_to_binary(d_AMEX_test_dummie[[col_name]])
    
  }
        
        
  #Verify that transformation is complete
        #compute the variables inside the dataframe using categorization function prior to 
        #checks to verify the output
        #Plese note that total_num_obs was made an exception because it is scale but also whole number
        column_categorizations_train <- column_cate_stats(d_AMEX_train_dummie)
        column_categorizations_test <- column_cate_stats(d_AMEX_test_dummie)
        
        #check function to verify its all binary
        check_binary(d_AMEX_train_dummie, d_AMEX_test_dummie)

        
############################################################################################
#######################Model Training###########################################################
        
        #Create a Naive Bayes model.
        ml_nb <- naiveBayes(target~. ,data=d_AMEX_train_dummie)
        
############################################################################################
#######################MODEL TUNING (Train data)#############################################
        
    #Evaluate Fit of the Naive Bayes model to the training data
        
        #Create a vector of predictions
        v_nb_train_preds <- predict(ml_nb, d_AMEX_train_dummie,type="class") #dplyr
        
        #Compute/display the Percentage Correct in the training data to evaluate fit.
        NaiveBayes_percentage_correct_train <- mean(~(target == v_nb_train_preds), data=d_AMEX_train_dummie) #mosaic
        print(NaiveBayes_percentage_correct_train)
        
        #Create/display Classification Tables for the training data.
        #Classification Table of raw counts.
        NaiveBayes_class_table_counts_train <- tally(target ~ v_nb_train_preds, data=d_AMEX_train_dummie) %>% addmargins() #mosaic
        print(NaiveBayes_class_table_counts_train)
        
        #Classification Table of percentages of training data.
        NaiveBayes_class_table_percentage_train <-tally(target ~ v_nb_train_preds, data=d_AMEX_train_dummie) %>% 
          prop.table(margin=1) %>% round(2) #mosaic
        print(NaiveBayes_class_table_percentage_train)

        
    #Save Results to Excel
        # Create a new Excel workbook
        wb_Naive_train <- createWorkbook()
        
        # Add sheets and write data to each sheet
        addWorksheet(wb_Naive_train, "Accuracy")
        writeData(wb_Naive_train, "Accuracy", data.frame(Accuracy = NaiveBayes_percentage_correct_train))
        
        addWorksheet(wb_Naive_train, "Naive Bayes Accuracy Counts")
        writeData(wb_Naive_train, "Naive Bayes Accuracy Counts", NaiveBayes_class_table_counts_train)
        
        addWorksheet(wb_Naive_train, "Naive Bayes Accuracy Percent")
        writeData(wb_Naive_train, "Naive Bayes Accuracy Percent", NaiveBayes_class_table_percentage_train)
        
        # Save the workbook
        saveWorkbook(wb_Naive_train, "NaiveBayes_accuracy_training.xlsx", overwrite = TRUE)


        
############################################################################################
#######################MODEL TUNING (Testing data)#############################################       
                
        #Create a vector of predictions
        v_nb_test_preds <- predict(ml_nb, d_AMEX_test_dummie,type="class") #dplyr
        
        #Compute/display the Percentage Correct in the training data to evaluate fit.
        NaiveBayes_percentage_correct_test <- mean(~(target == v_nb_test_preds), data=d_AMEX_test_dummie) #mosaic
        print(NaiveBayes_percentage_correct_test)
        
        #Create/display Classification Tables for the training data.
        #Classification Table of raw counts.
        NaiveBayes_class_table_counts_test <- tally(target ~ v_nb_test_preds, data=d_AMEX_test_dummie) %>% addmargins() #mosaic
        print(NaiveBayes_class_table_counts_test)
        
        #Classification Table of percentages of training data.
        NaiveBayes_class_table_percentage_test <-tally(target ~ v_nb_test_preds, data=d_AMEX_test_dummie) %>% 
          prop.table(margin=1) %>% round(2) #mosaic
        print(NaiveBayes_class_table_percentage_test)
        
        
        #Save Results to Excel
        # Create a new Excel workbook
        wb_Naive_test <- createWorkbook()
        
        # Add sheets and write data to each sheet
        addWorksheet(wb_Naive_test, "Accuracy")
        writeData(wb_Naive_test, "Accuracy", data.frame(Accuracy = NaiveBayes_percentage_correct_test))
        
        addWorksheet(wb_Naive_test, "Naive Bayes Accuracy Counts")
        writeData(wb_Naive_test, "Naive Bayes Accuracy Counts", NaiveBayes_class_table_counts_test)
        
        addWorksheet(wb_Naive_test, "Naive Bayes Accuracy Percent")
        writeData(wb_Naive_test, "Naive Bayes Accuracy Percent", NaiveBayes_class_table_percentage_test)
        
        # Save the workbook
        saveWorkbook(wb_Naive_test, "NaiveBayes_accuracy_testing.xlsx", overwrite = TRUE)
        