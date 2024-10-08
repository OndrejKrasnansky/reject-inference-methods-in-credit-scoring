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


################################# Until this part is for every model the same ####################################


# Assign null values (NA) to the Y_train variable for non-financed datasets
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    if (nrow(non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]) > 0) {
      non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]$Y_train <- NA
    }
  }
}



#Create a vecotor of 10 bins from 0 to 1 (this will create bins 1,2,3,4,6,7,8,9,10)

bins <- seq(0, 1, by = 0.1)


# Assign predicted probabilities and scorebands to non-financed datasets

for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]][[i]]
    
    # Financed dataset predictions
    if (nrow(financed) > 0) {
      financed$probability <- predict(financed_model, newdata = financed, type = "response")
      financed$scoreband <- cut(financed$probability, bins, labels = FALSE, include.lowest = TRUE)
    }
    
    # Non-financed dataset predictions
    if (nrow(non_financed) > 0) {
      non_financed$probability <- predict(financed_model, newdata = non_financed, type = "response")
      non_financed$scoreband <- cut(non_financed$probability, bins, labels = FALSE, include.lowest = TRUE)
    }
    
    non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- non_financed
    financed_datasets[[paste0("cutoff_", cutoff)]][[i]] <- financed
  }
}






# Initialize an empty list to store combined datasets
# combined_datasets <- list()
# 
# # Combine financed and non-financed datasets for each cutoff and replicate
# for (cutoff in cut_off_values) {
#   for (i in 1:n_replicates) {
#     financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
#     non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
#     
#     # Add z column and combine datasets
#     combined <- bind_rows(
#       financed %>% mutate(z = "f"),
#       non_financed %>% mutate(z = "nf")
#     )
#     
#     # Store combined dataset in list
#     combined_datasets[[paste0("cutoff_", cutoff)]] <- combined
#   }
# }

# Initialize list to store combined datasets
combined_datasets <- list()

# Combine financed and non-financed datasets for each cutoff and replicate
for (cutoff in cut_off_values) {
  combined <- lapply(1:n_replicates, function(i) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    combined <- bind_rows(
      financed %>% mutate(z = "f"), # just creating a new column indicating f as financed nf otherwise
      non_financed %>% mutate(z = "nf")
    )
  })
  combined_datasets[[paste0("cutoff_", cutoff)]] <- combined
}





# Initialize lists to store results
funded_counts_list <- list()
combined_counts_list <- list()


# Calculate funded counts (number of financed clients ) for each cutoff and replicate
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]][[i]]
    funded_counts <- financed %>%
      group_by(scoreband) %>%
      summarise(n_funded = n())
    
    funded_counts_list[[paste0("cutoff_", cutoff)]][[i]] <- funded_counts
  }
}

# Calculate total number of clients for each scoreband using dplyr for each cutoff and replicate
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    total <- combined_datasets[[paste0("cutoff_", cutoff)]][[i]]
    combined_counts <- total %>%
      group_by(scoreband) %>%
      summarise(n_total = n())
    
    combined_counts_list[[paste0("cutoff_", cutoff)]][[i]] <- combined_counts
  }
}

# Initialize lists to store financing probabilities
financing_probabilities_list <- list()

# Calculate financing probabilities for each cutoff and replicate
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    funded_counts <- funded_counts_list[[paste0("cutoff_", cutoff)]][[i]]
    combined_counts <- combined_counts_list[[paste0("cutoff_", cutoff)]][[i]]
    
    financing_probabilities <- funded_counts %>%
      dplyr::left_join(combined_counts, by = "scoreband") %>% 
      dplyr::mutate(financing_probability = n_funded / n_total) # ratio of number of financed clients for every scoreband divided by total number
    
    financing_probabilities_list[[paste0("cutoff_", cutoff)]][[i]] <- financing_probabilities
  }
}



augmentation_datasets <- list()

# Assign weights and create augmentation datasets for each cutoff and replicate
for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    total <- combined_datasets[[paste0("cutoff_", cutoff)]][[i]]
    financing_probabilities <- financing_probabilities_list[[paste0("cutoff_", cutoff)]][[i]]
    
    # Merge total with financing probabilities
    total <- total %>%
      left_join(financing_probabilities %>% dplyr::select(scoreband, financing_probability), by = "scoreband")
    
    # Add weight column based on z and financing_probability
    total <- total %>%
      mutate(weight = ifelse(z == "f", 1 / financing_probability, 1)) %>%
      dplyr::select(-financing_probability)
    
    total$weight <- round(total$weight)
    
    # Store dataset with weights
    augmentation_datasets[[paste0("cutoff_", cutoff)]][[i]] <- total
    
  }
}




#Create a final list of augentation models for every cutoff
# We are using just logistics regression on financed_clients using augmentation_datasets which contains weight column
# Weight column is used to do a weighted prediction 

augmentation_models_list <- list()

for (cutoff in cut_off_values) {
  for (i in 1:n_replicates) {
    augmentation_dataset <- augmentation_datasets[[paste0("cutoff_", cutoff)]][[i]]


     model_augmentation <- glm(Y_train ~ X_train.1 + X_train.2 + X_train.3 + X_train.4 +
                              X_train.5 + X_train.6 + X_train.7 + X_train.8,
                              data = augmentation_dataset %>% filter(z == "f"),
                              family = binomial, weights = weight)

    augmentation_models_list[[paste0("cutoff_", cutoff)]][[i]] <- model_augmentation
    }
  }

 #augmentation_models_list['cutoff_0.3']


 
 
 
 
#saveRDS(augmentation_models_list, file = "C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\augmentation_models_list.rds")


###############################################  Plotting #####################################################



# Calculate Gini for augmentation and financed models
gini_results_augmentation <- calculate_gini(augmentation_models_list, X_test, Y_test, n_replicates)
gini_results_financed <- calculate_gini(financed_models_list, X_test, Y_test, n_replicates)

#Prepare gini results in order to be possible to plot them 
gini_data_augmentation <- prepare_gini_data(gini_results_augmentation, "augmentation", n_replicates)
gini_data_financed <- prepare_gini_data(gini_results_financed, "Financed", n_replicates)

# Combine both datasets
gini_data_combined <- rbind(gini_data_augmentation, gini_data_financed)
gini_data_combined <- na.omit(gini_data_combined)





# Create the plot
ggplot(gini_data_combined, aes(x = cutoff, y = gini, color = model_type, shape = model_type)) +
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







