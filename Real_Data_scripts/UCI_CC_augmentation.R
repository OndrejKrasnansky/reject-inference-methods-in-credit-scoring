# Load necessary libraries
library(caTools)
library(dplyr)
library(caret)
library(pROC)

# Load the data
UCI_Credit_Card <- read.csv("C:\\Users\\ondre\\Documents\\statistics_scripts\\Real_Data_scripts\\UCI_Credit_Card.csv") %>%
  rename(Default = default.payment.next.month) %>%
  select(-ID)

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
  financed <- train_data %>% filter(pred_prob_split >= cutoff) %>% select(-pred_prob_split)
  non_financed <- train_data %>% filter(pred_prob_split < cutoff) %>% select(-pred_prob_split)
  
  financed_datasets[[paste0("cutoff_", cutoff)]] <- financed
  non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
}

# Train models on financed datasets
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

# Assign predicted probabilities to datasets
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

# Initialize lists to store results
funded_counts_list <- list()
combined_counts_list <- list()

# Calculate funded counts for each cutoff and replicate
for (cutoff in cut_off_values) {
  financed <- financed_datasets[[paste0("cutoff_", cutoff)]]
  funded_counts <- financed %>%
    group_by(scoreband) %>%
    summarise(n_funded = n())
  
  funded_counts_list[[paste0("cutoff_", cutoff)]] <- funded_counts
}

# Calculate total counts for each cutoff and replicate
for (cutoff in cut_off_values) {
  total <- combined_datasets[[paste0("cutoff_", cutoff)]]
  combined_counts <- total %>%
    group_by(scoreband) %>%
    summarise(n_total = n())
  
  combined_counts_list[[paste0("cutoff_", cutoff)]] <- combined_counts
}

# Initialize lists to store financing probabilities
financing_probabilities_list <- list()

# Calculate financing probabilities for each cutoff and replicate
for (cutoff in cut_off_values) {
  funded_counts <- funded_counts_list[[paste0("cutoff_", cutoff)]]
  combined_counts <- combined_counts_list[[paste0("cutoff_", cutoff)]]
  
  financing_probabilities <- funded_counts %>%
    dplyr::left_join(combined_counts, by = "scoreband") %>%
    dplyr::mutate(financing_probability = n_funded / n_total)
  
  financing_probabilities_list[[paste0("cutoff_", cutoff)]] <- financing_probabilities
}

# Initialize lists to store datasets with weights
augmentation_datasets <- list()

# Assign weights and create augmentation datasets for each cutoff and replicate
for (cutoff in cut_off_values) {
  total <- combined_datasets[[paste0("cutoff_", cutoff)]]
  financing_probabilities <- financing_probabilities_list[[paste0("cutoff_", cutoff)]]
  
  # Merge total with financing probabilities
  total <- total %>%
    left_join(financing_probabilities %>% dplyr::select(scoreband, financing_probability), by = "scoreband")
  
  # Add weight column based on z and financing_probability
  total <- total %>%
    mutate(weight = ifelse(z == "f", 1 / financing_probability, 1)) %>%
    dplyr::select(-financing_probability)
  
  total$weight <- round(total$weight)
  
  # Store dataset with weights
  augmentation_datasets[[paste0("cutoff_", cutoff)]] <- total
  
}

augmentation_datasets["cutoff_0.3"]
names(augmentation_datasets[["cutoff_0.3"]])

# Initialize the list to store augmentation models
augmentation_models_list <- list()

for (cutoff in cut_off_values) {
  augmentation_dataset <- augmentation_datasets[[paste0("cutoff_", cutoff)]]
  
  # Train the model using caret's train function
  model_augmentation <- train(
    Default ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
      PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
      BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
      PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
    data = augmentation_dataset %>% filter(z == "f"),
    method = "glm",
    family = "binomial",
    trControl = train_control,  # Use the previously defined trainControl
    weights = weight
  )
  
  # Store the model in the list
  augmentation_models_list[[paste0("cutoff_", cutoff)]] <- model_augmentation
}

saveRDS(augmentation_models_list, file = "augmentation_models_real.rds")


###########DA QUI NON FUNZIONAAAAAAAA##############################


augmentation_predictions <- data.frame(cutoff = numeric(), gini = numeric())

for (cutoff in cut_off_values) {
  model_augmentation <- augmentation_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_augmentation <- predict(model_augmentation, newdata = test_data, type = "prob")[, "Bad"]
  
  gini_augmentation <- 2 * auc(roc(response = test_data$Default, predictor = test_data$pred_augmentation)) - 1
  
  augmentation_predictions <- rbind(augmentation_predictions, data.frame(cutoff = cutoff, gini = gini_augmentation))
}

financed_predictions <- data.frame(cutoff = numeric(), gini = numeric())

for (cutoff in cut_off_values) {
  financed_model <- financed_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_financed <- predict(financed_model, newdata = test_data, type = "prob")[, "Bad"]
  
  gini_financed <- 2 * auc(roc(response = test_data$Default, predictor = test_data$pred_financed)) - 1
  
  financed_predictions <- rbind(financed_predictions, data.frame(cutoff = cutoff, gini = gini_financed))
}

# Combine the results
gini_values_combined <- bind_rows(
  augmentation_predictions %>% mutate(model_type = "Augmentation"),
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







