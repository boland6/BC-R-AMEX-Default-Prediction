################################################################################################################
#################################Loading packages##############################################################
library(data.table)
library(ggplot2)
library(dplyr)


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
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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
        
        # Filter to get the latest observation for each customer
        latest_data <- df_2[,.SD[which.max(n_num_obs)], by = customer_ID]
        
        
        # Calculate stats to examine na values
        column_stats_latest <- calculate_NA_stats(latest_data)
        
        
        #Categorize column using function based on data contained in the column on the latest data
        column_categorizations <- categorize_columns(latest_data)
        
        
########################################################################################################    
################NA Value Engineering####################################################################   
        latest_data$D_87[is.na(latest_data$D_87)] <- 0
        latest_data$D_66[is.na(latest_data$D_66)] <- 0
        latest_data$D_68[is.na(latest_data$D_68)] <- 0
        
        

  