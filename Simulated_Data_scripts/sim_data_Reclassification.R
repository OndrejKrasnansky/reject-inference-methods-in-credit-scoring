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


#Assing X_test and Y_test

X_test <- test_data$X_test
Y_test <- test_data$Y_test



#Train a logistics regression on the whole dataset in order to add the probability column which will be used
# to create a non_financed clients based on cutoffs

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



# Create a list of logistic regression models trained just on financed clients dataset 
# This list contains all values from cutoff_values vector and for every cutoff it contains 20 train datasets
# The logistic regression is trained on eery dataset for every cutoff
# This approach will be followed and repeated for all strategies
financed_models_list <- create_models_list(financed_datasets, cut_off_values, n_replicates)




# Assign pred_Y trained on financed models to Non-financed datasets and round to binary outcomes
for (cutoff in cut_off_values) {
  # We are using nested loop in order to be able to iterate on every cutoff and every datasets inside of cutoffs
  for (i in 1:n_replicates) {
    financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]][[i]]
    
    if (!is.null(financed_model)) {
      non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
      # predict Y using trained logistic regression models on financed datasets and create a new column Y_pred
      if (nrow(non_financed) > 0) {
        non_financed$pred_Y <- predict(financed_model, newdata = non_financed, type = "response") # here is probability
        non_financed$pred_Y <- ifelse(non_financed$pred_Y >= 0.5, 1, 0) # to change probability to binary 0 or 1
        non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed
      }
    }
  }
}




# Drop the first original column 'Y_train' from non-financed datasets and rename 'pred_Y' to 'Y_train' in order to
# be able to combine financed and non_financed datasets 
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    if (nrow(non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]) > 0) {
      non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] %>%
        dplyr::select(-Y_train) %>%
        rename(Y_train = pred_Y)
    }
  }
}



# Initialize list to store combined datasets
combined_datasets <- list()

# Combine financed and modified non-financed datasets
for (cutoff in cut_off_values) {
  combined <- lapply(1:n_replicates, function(i) {   #lapply use combine for every replicate and combine rows 
    rbind(financed_datasets[[paste0("cutoff_", cutoff)]][[i]],
          non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]])
  })
  combined_datasets[[paste0("cutoff_", cutoff)]] <- combined
}


#combined_datasets['cutoff_0.3']

# We have combined financed and non financed (where Y was predicted using financed models) to train reclassified models

reclassified_models_list <- create_models_list(financed_datasets, cut_off_values, n_replicates)




#Saving models in order to be able to plot them with facility in sim_data_plot_ALL.R
# This will be done for parcelling and augmentation too

#saveRDS(reclassified_models_list, file = "C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\reclassified_models_list.rds")
#saveRDS(financed_models_list, file = "C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\financed_models_list.rds")
 

###############################################  Plotting #####################################################


# Calculate Gini for reclassified and financed models
gini_results_reclassified <- calculate_gini(reclassified_models_list, X_test, Y_test, n_replicates)
gini_results_financed <- calculate_gini(financed_models_list, X_test, Y_test, n_replicates)

#Prepare gini results in order to be possible to plot them 
gini_data_reclassified <- prepare_gini_data(gini_results_reclassified, "Reclassified", n_replicates)
gini_data_financed <- prepare_gini_data(gini_results_financed, "Financed", n_replicates)

# Combine both gini_data_reclassified and gini_data_financed 
gini_data_combined <- rbind(gini_data_reclassified, gini_data_financed)
# Exclude observations with missig data
gini_data_combined <- na.omit(gini_data_combined)








# Create the plot where Y-axis is the gini performance on test st and x-axis is a cut_off_value

ggplot(gini_data_combined, aes(x = cutoff, y = gini, color = model_type, shape = model_type)) +
  stat_summary(fun = mean, geom = "point", size = 1.3) +                           # Mean points
  labs(
    title = "Comparison of Reject Inference Methods with a Well-Specified Model",
    x = "Cut-off value",
    y = "Gini on Test Set",
    color = "Model Type",
    shape = "Model Type"
  ) +
  theme_classic() +ylim(0.77,0.85)





