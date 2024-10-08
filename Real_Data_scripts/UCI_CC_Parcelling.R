# Load necessary libraries
library(caTools)
library(dplyr)
library(caret)
library(pROC)

# Load the data
UCI_Credit_Card <- read.csv("C:\\Users\\ondre\\Documents\\statistics_scripts\\Real_Data_scripts\\UCI_Credit_Card.csv") %>%
  rename(Default = default.payment.next.month) %>% dplyr::select(-ID)

# Convert the target variable to a factor
UCI_Credit_Card$Default <- factor(UCI_Credit_Card$Default, levels = c(0, 1), labels = c("Good", "Bad"))

# Set seed for reproducibility
set.seed(123)

# Split the dataset into train and test
sample <- sample.split(UCI_Credit_Card$Default, SplitRatio = 0.8)
train_data <- subset(UCI_Credit_Card, sample == TRUE)
test_data <- subset(UCI_Credit_Card, sample == FALSE)

# Fit the logistic regression model
model_for_split <- glm(Default ~ ., data = train_data, family = binomial)

# Predict probabilities for train data
train_data$pred_prob_split <- predict(model_for_split, type = "response")

# Define cutoff values
cut_off_values <- seq(0, 0.4, by = 0.02)

# Initialize lists to store datasets
financed_datasets <- list()
non_financed_datasets <- list()

# Categorize based on cutoffs
for (cutoff in cut_off_values) {
  financed <- train_data %>% filter(pred_prob_split >= cutoff) %>% dplyr::select(-pred_prob_split)
  non_financed <- train_data %>% filter(pred_prob_split < cutoff) %>% dplyr::select(-pred_prob_split)
  
  financed_datasets[[paste0("cutoff_", cutoff)]] <- financed
  non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
}

# Train models on financed datasets using 5-cross_validation
train_control <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
financed_models_list <- list()

for (cutoff in cut_off_values) {
  if (nrow(financed_datasets[[paste0("cutoff_", cutoff)]]) > 0) {
    model <- train(Default ~ ., 
                   data = financed_datasets[[paste0("cutoff_", cutoff)]],
                   trControl = train_control,
                   method = "glm",
                   family = "binomial",
                   metric = "ROC")
    financed_models_list[[paste0("cutoff_", cutoff)]] <- model
  }
}

# Assign predicted probabilities to datasets using 10 bins
bins <- seq(0, 1, by = 0.1)

for (cutoff in cut_off_values) {
  financed <- financed_datasets[[paste0("cutoff_", cutoff)]]
  non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
  financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]]
  
  if (nrow(financed) > 0) {
    financed$probability <- predict(financed_model, newdata = financed, type = "prob")[, "Bad"]
    financed$scoreband <- cut(financed$probability, bins, labels = FALSE, include.lowest = TRUE)
  }
  
  if (nrow(non_financed) > 0) {
    non_financed$probability <- predict(financed_model, newdata = non_financed, type = "prob")[, "Bad"]
    non_financed$scoreband <- cut(non_financed$probability, bins, labels = FALSE, include.lowest = TRUE)
  }
  
  non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
  financed_datasets[[paste0("cutoff_", cutoff)]] <- financed
}

alpha = 0.15

# Calculate funded counts for each cutoff and replicate
for (cutoff in cut_off_values) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]]
    
    funded_counts <- financed %>%
      group_by(scoreband) %>%
      summarise(
        n_funded = n(),
        n_Bad = sum(Default == "Bad", na.rm = TRUE),
        n_Good = sum(Default == "Good", na.rm = TRUE),
        Ratio = ifelse(n_funded > 0, n_Bad / n_funded, NA),
        Ratio_alpha = ifelse(n_funded > 0, n_Bad * alpha / n_funded, NA),  # Calculate Ratio_alpha
        .groups = 'drop'
      )
    
    # Join funded_counts to the financed dataset
    financed_with_counts <- financed %>%
      left_join(funded_counts, by = "scoreband")
    
    # Update the financed_datasets list
    financed_datasets[[paste0("cutoff_", cutoff)]] <- financed_with_counts
    
    # Check and assign the same Ratio and Ratio_alpha to non_financed datasets
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
    
    if ("scoreband" %in% colnames(non_financed)) {
      # Assign the Ratio and Ratio_alpha to every scoreband in non_financed
      non_financed_with_ratio <- non_financed %>%
        left_join(funded_counts %>% dplyr::select(scoreband, Ratio, Ratio_alpha), by = "scoreband")
      
      # Update the non_financed_datasets list
      non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed_with_ratio
    } else {
      cat("No scoreband column in non_financed dataset for cutoff", cutoff)
    }
}



# Replace NA with 1 in the Ratio_alpha column for non_financed_datasets
for (cutoff in cut_off_values) {
  non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
  
  if (!is.null(non_financed) && "Ratio_alpha" %in% colnames(non_financed)) {
    non_financed <- non_financed %>%
      mutate(Ratio_alpha = ifelse(is.na(Ratio_alpha), 1, Ratio_alpha))
    
    # Update the non_financed_datasets list
    non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
  }
}




for (cutoff in cut_off_values) {
    if (nrow(non_financed_datasets[[paste0("cutoff_", cutoff)]]) > 0) {
      
      # Assign values based on the Ratio_alpha (ratio multiplied by prudence factor)
      non_financed_datasets[[paste0("cutoff_", cutoff)]]$Default <- 
        sapply(non_financed_datasets[[paste0("cutoff_", cutoff)]]$Ratio_alpha, 
               function(x) stats::rbinom(1, 1, 1 - x ))
      # and reconvert Default to a factor
      non_financed_datasets[[paste0("cutoff_", cutoff)]]$Default <- 
        factor(non_financed_datasets[[paste0("cutoff_", cutoff)]]$Default, levels = c(0, 1), labels = c("Good", "Bad"))
  }
}

# Initialize an empty list to store combined datasets
combined_datasets <- list()

# Combine financed and non-financed datasets for each cutoff and replicate
for (cutoff in cut_off_values) {
    financed <- financed_datasets[[paste0("cutoff_", cutoff)]]
    non_financed <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
    
    # Add z column and combine datasets
    combined <- bind_rows(
      financed %>% mutate(z = "f"),
      non_financed %>% mutate(z = "nf")
    )
    
    # Store combined dataset in list
    combined_datasets[[paste0("cutoff_", cutoff)]] <- combined
}


# Initialize the list to store parcelling models
parcelling_models_list <- list()

# Check for missing values in the combined datasets
for (cutoff in cut_off_values) {
  dataset <- combined_datasets[[paste0("cutoff_", cutoff)]]
  
  
  
  # Check if the dataset has enough rows to train the model
  if (nrow(dataset) > 10) {  # Adjust the threshold as needed
    # Train the model using caret's train function
    model_parcelling <- train(
      Default ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
        PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
        BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
        PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
      data = dataset,
      method = "glm",
      family = "binomial",
      trControl = train_control  # Use the previously defined trainControl
    )
    
  # Store the model in the list
  parcelling_models_list[[paste0("cutoff_", cutoff)]] <- model_parcelling
  }}

saveRDS(parcelling_models_list, file = "parcelling_models_real.rds")

#parcelling_models_list <- readRDS("parcelling_models_list_20.rds")

#####PLOTTING############
parcelling_predictions <- data.frame(cutoff = numeric(), gini = numeric())

for (cutoff in cut_off_values) {
  model_parcelling <- parcelling_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_parcelling <- predict(model_parcelling, newdata = test_data, type = "prob")[, 2]
  
  gini_parcelling <- 2 * auc(roc(response = test_data$Default, predictor = test_data$pred_parcelling)) - 1
  
  parcelling_predictions <- rbind(parcelling_predictions, data.frame(cutoff = cutoff, gini = gini_parcelling))
}

financed_predictions <- data.frame(cutoff = numeric(), gini = numeric())

for (cutoff in cut_off_values) {
  financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_financed <- predict(financed_model, newdata = test_data, type = "prob")[, 2]
  
  gini_financed <- 2 * auc(roc(response = test_data$Default, predictor = test_data$pred_financed)) - 1
  
  financed_predictions <- rbind(financed_predictions, data.frame(cutoff = cutoff, gini = gini_financed))
}

# Combine the results
gini_values_combined <- bind_rows(
  parcelling_predictions %>% mutate(model_type = "Parcelling"),
  financed_predictions %>% mutate(model_type = "Financed")
)

# Plot the Gini coefficients
ggplot(gini_values_combined, aes(x = cutoff, y = gini, color = model_type, group = model_type)) +
  geom_line() +
  geom_point() +
  labs(title = "UCI_Credit_Card",
       x = "Cutoff Value",
       y = "Gini on test set",
       color = "Model Type") +
  theme_classic() 







