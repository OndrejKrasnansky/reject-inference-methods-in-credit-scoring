

# Function to generate test dataset
generate_test_data <- function(n_test, d, cov_matrix) {
  Y_test <- rbinom(n_test, size = 1, prob = 0.5)
  X_test <- mvrnorm(n_test, mu = rep(0, d), Sigma = cov_matrix) + matrix(Y_test, n_test, d)  #Multivariate normal distribution from 0 to d using cov matrix which has to be adjusted on dependence of Y
  return(list(X_test = X_test, Y_test = Y_test))
}


generate_train_data <- function(n_train, n_replicates, d, cov_matrix) {
  # This function generate a list of train datasets
  # n_train      -> number of instances in the train set
  # n_replicates -> is a number of datasets we want to create inside the list
  # d            -> number of dimensions
  # cov_matrix   -> is the cov_matrix needed to calculate sigma (variance)
  # returns      -> list of dataframes
  
  train_datasets <- list()
  
  for (i in 1:n_replicates) {
    Y_train <- rbinom(n_train, size = 1, prob = 0.5)
    X_train <- mvrnorm(n_train, mu = rep(0, d), Sigma = cov_matrix) + matrix(Y_train, n_train, d)
    train_datasets[[i]] <- data.frame(Y_train = Y_train, X_train = X_train)
  }
  
  return(train_datasets)
}

#####################################################

create_financed_non_financed_datasets <- function(train_datasets, cut_off_values = seq(0, 0.9, by = 0.02), n_replicates) {
  #This function was made in order to avoid constatly repeating the divison by cutoffs for every model
  # It serves to create a non financed_dataset based on predicted probability on the full dataset 
  #
  # train_datasets -> The list of n_replicates datasets
  # cut_off_values -> The vector which defines cutoff values for which we are creating datasets
  # n_replicates -> Number of datasets
  # returns -> list of financed and non_financed_datates (splitted) for every cuttoff value
  
  
  # Initialize lists to store subsets
  financed_datasets <- list()
  non_financed_datasets <- list()
  
  # Categorize based on cutoffs and create subsets
  for (cutoff in cut_off_values) {
    financed <- list()
    non_financed <- list()
    
    for (i in 1:n_replicates) {
      # Filter financed and non-financed datasets
      financed[[i]] <- train_datasets[[i]] %>%
        filter(pred_prob_complete >= cutoff) %>%
        dplyr::select(-pred_prob_complete)
      
      non_financed[[i]] <- train_datasets[[i]] %>%
        filter(pred_prob_complete < cutoff) %>%
        dplyr::select(-pred_prob_complete)
    }
    
    # Store the subsets in the lists with the cutoff as the key
    financed_datasets[[paste0("cutoff_", cutoff)]] <- financed
    non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
  }
  
  return(list(financed_datasets = financed_datasets, non_financed_datasets = non_financed_datasets))
}






create_models_list <- function(datasets, cut_off_values = seq(0, 0.9, by = 0.02), n_replicates) {
  #Function that creates a list of logistics regression models applicable for a list of datasets
  # This function will be used just for Reclassification,Financed and Parcelling models
  #
  # datasets       -> input is a list of datasets
  # cut_off_values -> vector of cut_off_off_values fixed from 0 to 0.09 stepped by 0.02 this serves to iterate throug every cutoff in a list
  # n_replicates   -> is the number of datasets contained in an input list

  models_list <- list()

  for (index_cut in cut_off_values) { #iteration through the length of a cutoff_values vector
    models <- list()
    for (i in 1:n_replicates) { # iterate through all all simulated training datasets
      data <- datasets[[paste0("cutoff_",index_cut)]][[i]]
      model <- glm(Y_train ~ ., data = data, family = binomial)
      models[[i]] <- model
    }
    models_list[[paste0("cutoff_", index_cut)]] <- models
  }
  return(models_list)
}











# Function to calculate gini coefficients for every model 

calculate_gini <- function(models_list, X_test, Y_test, n_replicates = 10) {
  gini_results <- list()
  for (cutoff in cut_off_values) {
    models <- models_list[[paste0("cutoff_", cutoff)]]
    cutoff_gini <- numeric(n_replicates)
    for (i in 1:n_replicates) {
      model <- models[[i]]
      if (!is.null(model)) {
        test_prob <- predict(model, newdata = data.frame(X_train = X_test), type = "response")
        roc_curve <- roc(Y_test, test_prob)
        auc_value <- auc(roc_curve)
        gini_value <- 2 * auc_value - 1
        cutoff_gini[i] <- gini_value
      } else {
        cutoff_gini[i] <- NA
      }
    }
    gini_results[[paste0("cutoff_", cutoff)]] <- cutoff_gini
  }
  return(gini_results)
}




# Prepare and  restructures the Gini coefficients into a data frame suitable for plotting

prepare_gini_data <- function(gini_results, model_type, n_replicates = 10) {
  do.call(rbind, lapply(names(gini_results), function(cutoff_name) {
    data.frame(
      cutoff = as.numeric(sub("cutoff_", "", cutoff_name)),
      replicate = 1:n_replicates,
      gini = gini_results[[cutoff_name]],
      model_type = model_type
    )
  }))
}









