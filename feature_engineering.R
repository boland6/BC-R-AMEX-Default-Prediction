################################################################################################################
#################################Loading packages##############################################################
library(data.table)
library(ggplot2)
library(dplyr)
library(fastDummies)

########################################################################################################
#################################Functions##############################################################

calculate_NA_stats <- function(data) {
  total_rows <- nrow(data)
  stats <- data.frame(
    Total_Occurrences = sapply(data, length),
    Num_NA = sapply(data, function(x) sum(is.na(x))),
    Percent_NA = numeric(ncol(data))
  )
  
  stats$Percent_NA <- round((stats$Num_NA / total_rows) * 100,4)
  
  # Set column type
  stats$Column_Type <- sapply(names(data), function(column_name) {
    column <- data[[column_name]]
    
    # Check if the column contains only NA values
    if (all(is.na(column))) {
      return("Only NA")
    }
    # Check if the column contains non-numbers
    else if (any(!is.na(column) & !is.numeric(column))) {
      return("Non-Number")
    }
    # Check if all non-NA values are whole numbers under 40
    else if (all(column[!is.na(column)] == floor(column[!is.na(column)])) && all(column[!is.na(column)] < 40)) {
      return("Whole Number (Categorization)")
    }
    # Check if the column contains numbers with decimals
    else if (any(column[!is.na(column)] != floor(column[!is.na(column)]))) {
      return("Numerical")
    }
    # Existing conditions for categorical, date, ID, outcome, or other
    else if (column_name %in% cate_columns) {
      return("AMEX Defined Categorical")
    }
    else if (column_name %in% date_columns) {
      return("Date")
    }
    else if (column_name %in% id_column) {
      return("ID")
    }
    else if (column_name %in% outcome_column) {
      return("Outcome")
    }
    else {
      return("Other")
    }
  })
  
  # Set row names as column names from the original data
  row.names(stats) <- names(data)
  
  return(stats)
}

#Functions to categorize columns based on conditions
categorize_columns <- function(dataframe) {
  result <- vector("list", length = ncol(dataframe))
  names(result) <- names(dataframe)
  
  for (i in seq_along(dataframe)) {
    column <- dataframe[[i]]
    column_name <- names(dataframe)[i]
    
    # Check if the column contains only NA values
    if (all(is.na(column))) {
      result[[i]] <- paste("Only NA")
    }
    # Check if the column contains non-numbers
    else if (any(!is.na(column) & !is.numeric(column))) {
      result[[i]] <- paste("Non-Number")
    }
    # Check if all non-NA values are whole numbers under 40
    else if (all(column[!is.na(column)] == floor(column[!is.na(column)])) && all(column[!is.na(column)] < 40)) {
      result[[i]] <- paste("Whole Number (Categorization)")
    }
    # Check if the column contains numbers with decimals
    else if (any(column[!is.na(column)] != floor(column[!is.na(column)]))) {
      result[[i]] <- paste("Numerical")
    }
    else {
      result[[i]] <- paste("does not match any category")
    }
  }
  
  return(result)
}

# Function to calculate the mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to calculate the mean
getMean <- function(v) {
  mean(na.omit(v))
}

#function to analyze the distribution of a specific column in a dataframe
analyze_target_variable <- function(data, column_name) {
  # Check the structure of the data frame
  str(data)
  
  # Summary statistics of the specified column
  summary(data[[column_name]])
  
  # Frequency table of the specified column
  freq_table <- table(data[[column_name]])
  
  tryCatch(
    {
      # Create a histogram to visualize the distribution of the specified column
      hist_plot <- ggplot(data = data, aes(x = data[[column_name]])) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black") +
        labs(title = paste("Histogram of", column_name, "Variable"),
             x = column_name, y = "Frequency")
      
      # Create a boxplot to visualize the spread of the specified column
      box_plot <- ggplot(data = data, aes(y = data[[column_name]])) +
        geom_boxplot(fill = "green", color = "black") +
        labs(title = paste("Boxplot of", column_name, "Variable"), y = column_name)
      
      # Create a density plot to visualize the distribution of the specified column
      density_plot <- ggplot(data = data, aes(x = data[[column_name]])) +
        geom_density(fill = "purple", color = "black") +
        labs(title = paste("Density Plot of", column_name, "Variable"),
             x = column_name, y = "Density")
      
      # Create a pie chart to visualize the distribution of the specified column
      pie_chart <- ggplot(data = data, aes(x = factor(1), fill = factor(data[[column_name]]))) +
        geom_bar() +
        coord_polar(theta = "y") +
        labs(title = paste("Pie Chart of", column_name, "Variable"))
      
      # Print all the plots
      print(hist_plot)
      print(box_plot)
      print(density_plot)
      print(pie_chart)
    },
    error = function(e) {
      cat("An error occurred while plotting:", conditionMessage(e), "\n")
    }
  )
  
  # Return the frequency table
  return(freq_table)
}

########################################################################################################
#################################Assign Column Type#####################################################

cate_columns <- c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_63', 'D_64', 'D_66', 'D_68')
date_columns <- c('S_2')
id_column <- c('customer_ID')
outcome_column <-c('target')

########################################################################################################
#################################Reading in data########################################################

#reading in large training data using fread
df <- fread("train_data(large).csv")
df_labels <- fread("train_labels.csv")


########################################################################################################
#################################Create Column that captures temporal aspect############################

# Create the total_num_occurance column that contains the number of occurrence of that customer_ID
df[, total_num_obs := .N, by = customer_ID]

# Sort the data by customer_ID and S_2 (date), and then create the n_num_obs column
df[order(customer_ID, S_2), n_num_obs := seq_len(.N), by = customer_ID]


            ######Graph plotting to visualize and ensure data manipulated correctly

            #plot the distribution of total number of occurrences
            ggplot(df, aes(x = total_num_obs)) + 
              geom_histogram(binwidth = 1, fill = "blue", color = "black") +
              labs(title = "Distribution of Total Number of Occurrences", 
                   x = "Total Number of Occurrences", 
                   y = "Frequency") +
              theme_minimal()

            ggplot(df, aes(x = n_num_obs)) + 
              geom_histogram(binwidth = 1, fill = "blue", color = "black") +
              labs(title = "Distribution of Total Number of Occurrences", 
                   x = "Total Number of Occurrences", 
                   y = "Frequency") +
              theme_minimal()
        
        
########################################################################################################
#################################Joining outcomes into data#############################################

        
#Perform lookup and left_join to pull in the 'target' outcome for the created test data
        df_2 <- df %>%
          left_join(df_labels, by = "customer_ID")
        
        #read column of updated test data
        names(df_2)
        


        
########################################################################################################
#################################Examining data for a single customer#############################################        
# Filter the DataFrame for rows where customer_ID contains the specific substring
            single_df <- df_2[grepl("009b4f146ac20c9e528e23137b3fbef84856f327124ade", customer_ID)]

  
  
########################################################################################################
#################################Examining NA Values####################################################
  # Calculate stats
  column_stats <- calculate_NA_stats(df_2)
        
        
########################################################################################################
################Filter dataset to only latest occurrence of each customer#####################################
        
        # Filter to get the latest observation for each customer (for the best data for each customer)
        latest_data <- df_2[,.SD[which.max(n_num_obs)], by = customer_ID]
        
        
        # Calculate stats to examine na values
        column_stats_latest <- calculate_NA_stats(latest_data)
        
        
        
        #Categorize column using function based on data contained in the column on the latest data
        column_categorizations <- categorize_columns(latest_data)
        
        
########################################################################################################    
################NA Value Engineering####################################################################   
        
        #Assigning the following to 0 because the variable in the dataset appears to be whole number
        #potential categorical
        #and 0 is not a existing variable in the column
        #so this allows for capturing NA as a dimension in the model
        latest_data$D_87[is.na(latest_data$D_87)] <- 0
        latest_data$D_66[is.na(latest_data$D_66)] <- 0
        latest_data$D_68[is.na(latest_data$D_68)] <- 0
        latest_data$D_117[is.na(latest_data$D_117)] <- 0
        latest_data$B_38[is.na(latest_data$B_38)] <- 0
        
        #examine the na values after the above functions
        # Calculate stats to examine na values
        column_stats_latest <- calculate_NA_stats(latest_data)
        
        # Identifying rows
        halfNAMissing_Column <- rownames(column_stats_latest[column_stats_latest$Percent_NA > 50, ])
        wholenumber_Column <- rownames(column_stats_latest[column_stats_latest$Column_Type == "Whole Number (Categorization)", ])

        # Replace NA with 0 in columns specified by halfNAMissing_Column in latest_data
        for(col in halfNAMissing_Column) {
          if(col %in% colnames(latest_data)) {
            latest_data[[col]][is.na(latest_data[[col]])] <- 0
          }
        }
        
        # Replace NA with the mode in columns of wholenumber_Column in latest_data
        for(col in wholenumber_Column) {
          if(col %in% colnames(latest_data)) {
            mode_value <- getMode(latest_data[[col]])
            latest_data[[col]][is.na(latest_data[[col]])] <- mode_value
          }
        }
        
        # Calculate stats to examine na values
        column_stats_latest <- calculate_NA_stats(latest_data)
        
        #Numerical columns 
        numerical_NA_rownames <- rownames(column_stats_latest[column_stats_latest$Column_Type == "Numerical" & column_stats_latest$Percent_NA > 0, ])
        
        for(col in numerical_NA_rownames) {
          if(col %in% colnames(latest_data)) {
            mean_value <- getMean(latest_data[[col]])
            latest_data[[col]][is.na(latest_data[[col]])] <- mean_value
          }
        }
        
        # Calculate stats to examine na values
        column_stats_latest <- calculate_NA_stats(latest_data)


########################################################################################################
#################################Drop Date Column and Duplicate column####################################################     
          #n_num_obs should match number of obs since we only selected the last record of the customer
        latest_data_2 <- latest_data %>% select(-n_num_obs, -S_2)
        
        #analyst the distribution of target in the updated df
        analyze_target_variable(latest_data_2,"target")

########################################################################################################
#################################Create dummy columns for categorical variable####################################################     
        
        #Recalculate column stats to obtain latest info
        column_stats_latest <- calculate_NA_stats(latest_data_2)
        #calculate rows that is non-number based on the value inside the columns
        nonnumber_rownames <- rownames(column_stats_latest[column_stats_latest$Column_Type == "Non-Number" & rownames(column_stats_latest) != "customer_ID", ])
        print(nonnumber_rownames)
        
        #Look at the unique values inside those columns to verify
        for (col_name in nonnumber_rownames) {
          # Check if the column name exists in latest_data_2
          if (col_name %in% names(latest_data_2)) {
            # Extract unique values from the column in latest_data_2
            unique_values <- unique(latest_data_2[[col_name]])
            
            # Print the column name and its unique values
            cat("Unique values in column", col_name, ":\n")
            print(unique_values)
            cat("\n") # Just for better readability
          } else {
            cat("Column", col_name, "does not exist in latest_data_2\n")
          }
        }

        #Create dummy variables
        latest_data_2_dummies <- dummy_cols(latest_data_2, 
                                            select_columns = nonnumber_rownames, 
                                            remove_first_dummy = TRUE, 
                                            remove_selected_columns = TRUE)

        #verify        
        #Recalculate column stats to obtain latest info
        column_stats_latest_dummy <- calculate_NA_stats(latest_data_2_dummies)
        
########################################################################################################
#################################sub_sample the large data to 10000##########################
        # Set the seed for reproducibility
        set.seed(23)
        
        sampling_size <- 10000
        
        # Assuming latest_data_2_dummies is a data frame
        # Randomly select 10000 rows from the dataframe
        sampled_data <- latest_data_2_dummies[sample(nrow(latest_data_2_dummies), sampling_size), ]

        
########################################################################################################
#################################Partitioning the variables to training and test##########################
        
        # Calculate the number of rows that will be in the training set
        training_size <- floor(0.80 * nrow(sampled_data))
        
        # Randomly sample row indices for the training set
        training_indices <- sample(seq_len(nrow(sampled_data)), size = training_size)
        
        # Create a new column 'partition' and assign 'train' or 'test'
        sampled_data_2 <- sampled_data %>%
          mutate(partition = if_else(row_number() %in% training_indices, 'train', 'test'))
        
        # View the first few rows of the modified DataFrame
        head(sampled_data_2)
        
        
        
        #partition into training and test
        df_train <-filter(sampled_data_2,partition == "train") %>% select (-'partition')
        df_test <-filter(sampled_data_2,partition == "test") %>% select (-'partition')

########################################################################################################
#################################Write the partitioned data into csv##########################
        
        #Write test data
        write.csv(df_test, "test.csv",row.names = FALSE)
        
        #Write training data
        write.csv(df_train, "train.csv",row.names = FALSE)
        
        