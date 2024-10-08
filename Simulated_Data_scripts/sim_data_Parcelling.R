# Required Libraries
library(ggplot2)
library(dplyr)
library(MASS) # for mvrnorm
library(pROC)
source("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\functions.R")


# Set the seed for reproducibility
set.seed(123)

# Define parameters
n_test <- 100000   # Number of observations in the test dataset
n_train <- 10000   # Number of observations in each training dataset
n_replicates <- 10 # Number of training datasets
d <- 8             # Number of dimensions

cut_off_values <- seq(0, 0.9, by = 0.02)


# Covariance matrix (2 * identity matrix)
cov_matrix <- 2 * diag(d)

# Generation of train and test datasets using generate_function (for train and test separately) described in "functions.R" script
train_datasets <- generate_train_data(n_train, n_replicates, d, cov_matrix)
test_data<- generate_test_data(n_test, d, cov_matrix)




X_test <- test_data$X_test
Y_test <- test_data$Y_test






#Train a logistics regression on the whole dataset in order to add the probability column which will be used
# to create a non_financed clients based on cutoffs
# Loop through all train datasets
for (i in 1:n_replicates) {
  model_complete <- glm(Y_train ~ ., data = train_datasets[[i]], family = binomial)
  train_datasets[[i]]$pred_prob_complete <- predict(model_complete, type = "response")
} 


#Use create_financed_non_financed function imported from functions script
# This function will create financed and non_financed list of datasets based on cutoffs 
# The same function will be used and repeated in preprocessing for all strategies

result <- create_financed_non_financed_datasets(train_datasets, cut_off_values, n_replicates)
financed_datasets <- result$financed_datasets
non_financed_datasets <- result$non_financed_datasets

#non_financed_datasets[["cutoff_0.3"]][[1]]

# Create a list of logistic regression models trained just on financed clients dataset 
# This list contains all values from cutoff_values vector and for every cutoff it contains 20 train datasets
# The logistic regression is trained on eery dataset for every cutoff
# This approach will be followed and repeated for all strategies

financed_models_list <- create_models_list(financed_datasets, cut_off_values, n_replicates)




# Assign null values to the Y_train variable for non-financed datasets
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    if (nrow(non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]) > 0) {
      non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]$Y_train <- NA
    }
  }
}





#Creating 10 bins
bins <- seq(0, 1, by = 0.1)


# Assign predicted probabilities and scorebands to non-financed datasets using trained logistic regression on financed clients
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]][[i]]
    
    # Financed dataset predictions assinging column probability and scoreband (using cut to discretize)
    if (nrow(financed) > 0) {
      financed$probability <- predict(financed_model, newdata = financed, type = "response")
      financed$scoreband <- cut(financed$probability, bins, labels = FALSE, include.lowest = TRUE)
    }
    
    # Non_Financed dataset predictions assinging column probability and scoreband (using cut to discretize)
    if (nrow(non_financed) > 0) {
      non_financed$probability <- predict(financed_model, newdata = non_financed, type = "response")
      non_financed$scoreband <- cut(non_financed$probability, bins, labels = FALSE, include.lowest = TRUE)
    }
    
    #Appending them to the final list
    non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed
    financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- financed
  }
}




#Prudence factor 
alpha = 0.15
 
# Calculate funded counts for each cutoff and replicate
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]

    funded_counts <- financed %>%
      group_by(scoreband) %>%
      summarise(
        n_funded = n(),
        n_Bad = sum(Y_train == 0, na.rm = TRUE),
        n_Good = sum(Y_train == 1, na.rm = TRUE),
        Ratio = ifelse(n_funded >0 ,n_Bad / n_funded, NA),
        Ratio_alpha = ifelse(n_funded > 0, n_Bad * alpha / n_funded, NA),  # Calculate Ratio_alpha
        .groups = 'drop'
      )

    # Join funded_counts to the financed dataset
    financed_with_counts <- financed %>%
      left_join(funded_counts, by = "scoreband")

    # Update the financed_datasets list
    financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- financed_with_counts

    # Check and assign the same Ratio and Ratio_alpha to non_financed datasets
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
      
    # If scoreband exists 
    if ("scoreband" %in% colnames(non_financed)) {
      # Assign the Ratio and Ratio_alpha to every scoreband in non_financed
      non_financed_with_ratio <- non_financed %>%
        left_join(funded_counts %>% dplyr::select(scoreband, Ratio, Ratio_alpha), by = "scoreband")

      # Update the non_financed_datasets list
      non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed_with_ratio
    } else {
      cat("No scoreband column in non_financed dataset for cutoff", cutoff, "replicate", i, "\n")
    }
  }
}


non_financed_datasets['cutoff_0.5']










for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    if (nrow(non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]) > 0) {

      # Aassign values based on the Ratio_alpha (ratio multiplied by prudence factor)
      non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]$Y_train <-
        sapply(non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]$Ratio_alpha,
               function(x) stats::rbinom(1, 1, 1 - x ))
    }
  }
}

non_financed_datasets['cutoff_0.3']







# Drop Ratio, Ratio_alpha, probability, and scoreband from non_financed_datasets
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    
    if (cutoff == 0) {
      # Skip dropping columns for non_financed_datasets when cutoff is 0
      financed <- financed %>%
        dplyr::select(-c(n_funded, n_Bad, n_Good, Ratio, Ratio_alpha, probability, scoreband))
    } else {
      # drop these columns that will be not used for classification in that way we have dropped them in order to be able to use
      # effectively create_models_list function
      non_financed <- non_financed %>%
        dplyr::select(-c(Ratio, Ratio_alpha, probability, scoreband))
      financed <- financed %>%
        dplyr::select(-c(n_funded, n_Bad, n_Good, Ratio, Ratio_alpha, probability, scoreband))
    }
    
    non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed
    financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- financed
  }
}







# Initialize list to store combined datasets
combined_datasets <- list()

# Combine financed and non-financed datasets for each cutoff and replicate
for (cutoff in cut_off_values) {
  combined <- lapply(1:n_replicates, function(i) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    
    #just add financed and non_financed in one single dataframe (Adding rows)
    rbind(financed, non_financed)
  })
  combined_datasets[[paste0("cutoff_", cutoff)]] <- combined
}


#combined_datasets['cutoff_0.9']



#check for null values


# cutoff_value <- 0.8
# 
# # Initialize list to store NA counts for each replicate
# na_counts_list <- list()
# 
# # Loop through replicates
# for (replicate_index in 1:n_replicates) {
#   # Select the combined dataset for the specific cutoff and replicate
#   combined_dataset <- combined_datasets[[paste0("cutoff_", cutoff_value)]][[replicate_index]]
#   
#   # Count the number of NA values in each column
#   na_counts <- sapply(combined_dataset, function(x) sum(is.na(x)))
#   
#   # Store the results in the list
#   na_counts_list[[paste0("replicate_", replicate_index)]] <- na_counts
# }
# 
# # Print the results
# print(na_counts_list)
# 



# Create parcelling models list where we can use create_models_list function from function.R script
# This is allowed as all columns different from X_train. were dropped so we can use syntax Y_train ~

parcelling_models_list <- create_models_list(combined_datasets, cut_off_values, n_replicates)



#saveRDS(parcelling_models_list, file = "C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\parcelling_models_list.rds")

###############################################  Plotting #####################################################

gini_results_parcelling <- calculate_gini(parcelling_models_list, X_test, Y_test, n_replicates)
gini_data_parcelling <- prepare_gini_data(gini_results_parcelling,'Parcelling', n_replicates)






# Create the plot
ggplot(gini_data_parcelling, aes(x = cutoff, y = gini, color = model_type, shape = model_type)) +
  stat_summary(fun = mean, geom = "point", size = 2) +  # Increase point size
  stat_summary(fun = mean, geom = "line") +   # Increase line width
  scale_shape_manual(values = c(16, 17, 18, 19)) +  # Use different shapes for points
  labs(
    title = "Comparison of Reject Inference Methods with a Well-Specified Model",
    x = "Cut-off value",
    y = "Gini on Test Set",
    color = "Model Type",
    shape = "Model Type"
  ) +
   theme_classic()+
   scale_y_continuous(limits = c(0.77, 0.85), breaks = seq(0.77, 0.85, 0.01))+
   scale_x_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, 0.2))



