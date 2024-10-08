# Load necessary libraries
library(caTools)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(pROC)


# Set seed for reproducibility
set.seed(123)



# Load the data
UCI_Credit_Card <- read.csv("C:\\Users\\ondre\\Documents\\statistics_scripts\\Real_Data_scripts\\UCI_Credit_Card.csv")

# Rename the target column
UCI_Credit_Card <- UCI_Credit_Card %>% 
  rename(Default = default.payment.next.month)

# Convert the target variable to a factor
UCI_Credit_Card$Default <- factor(UCI_Credit_Card$Default, levels = c(0, 1), labels = c("Good", "Bad"))

UCI_Credit_Card <- UCI_Credit_Card %>% dplyr::select(-ID)


# Split the dataset into train and test
sample <- sample.split(UCI_Credit_Card$Default, SplitRatio = 0.8)
train_data <- subset(UCI_Credit_Card, sample == TRUE)
test_data <- subset(UCI_Credit_Card, sample == FALSE)

# Separate features and target variable for training data
X_train <- train_data %>% dplyr::select(-Default)
Y_train <- train_data$Default

# Separate features and target variable for testing data
X_test <- test_data %>% dplyr::select(-Default)
Y_test <- test_data$Default

# Fit the full logistic regression model
model_for_split <- glm(Default ~ ., data = train_data, family = binomial)

# Predict probabilities using the full logistic regression model
train_data$pred_prob_split <- predict(model_for_split, type = "response")

# Define cutoff values
cut_off_values <- seq(0, 0.4, by = 0.02)

# Initialize lists to store subsets
financed_datasets <- list()
non_financed_datasets <- list()

# Categorize based on cutoffs and create subsets
for (cutoff in cut_off_values) {
  financed <- train_data %>%
    filter(pred_prob_split >= cutoff) %>%
    dplyr::select(-pred_prob_split)
  
  non_financed <- train_data %>%
    filter(pred_prob_split < cutoff) %>%
    dplyr::select(-pred_prob_split)
  
  financed_datasets[[paste0("cutoff_", cutoff)]] <- financed
  non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed
}

# Create trainControl object for cross-validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)

# Initialize list to store models for financed data
models_financed <- list()

# Fit models on financed datasets
for (cutoff in cut_off_values) {
  model <- train(form = Default ~ ., 
                 data = financed_datasets[[paste0("cutoff_", cutoff)]],
                 trControl = train_control,
                 method = "glm",
                 family = "binomial",
                 metric = "ROC")
  models_financed[[paste0("cutoff_", cutoff)]] <- model
}



# Loop through cutoffs to predict Y for each non-financed dataset
for (cutoff in cut_off_values) {
  # Get the corresponding model for the financed dataset
  model <- models_financed[[paste0("cutoff_", cutoff)]]
  
  # Get the corresponding non-financed dataset
  non_financed_data <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
  
  # Ensure the model only predicts on the relevant features (excluding target variable)
  if (nrow(non_financed_data) > 0) {
    # Predict probabilities using the fitted model
    non_financed_data$pred_Y_prob <- predict(model, newdata = non_financed_data, type = "prob")[, "Bad"]
    
    # Convert probabilities to binary outcomes
    non_financed_data$pred_Y <- ifelse(non_financed_data$pred_Y_prob >= 0.5, "Bad", "Good")
    
    # Store the updated non-financed dataset with predictions
    non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed_data
  }
}

# Drop the original column 'Default' from non-financed datasets and rename 'pred_Y' to 'Default'
for (cutoff in cut_off_values) {
  # Get the non-financed dataset for the current cutoff
  non_financed_data <- non_financed_datasets[[paste0("cutoff_", cutoff)]]
  
  if (nrow(non_financed_data) > 0) {
    non_financed_datasets[[paste0("cutoff_", cutoff)]] <- non_financed_data %>%
      dplyr::select(-Default, -pred_Y_prob) %>%  # Drop the original 'Default' column
      rename(Default = pred_Y)  # Rename 'pred_Y' to 'Default'
  }
}

# Initialize list to store combined datasets
combined_datasets <- list()

# Combine financed and modified non-financed datasets
for (cutoff in cut_off_values) {
  # Combine financed and non-financed datasets for the current cutoff
  combined_datasets[[paste0("cutoff_", cutoff)]] <- rbind(
    financed_datasets[[paste0("cutoff_", cutoff)]],
    non_financed_datasets[[paste0("cutoff_", cutoff)]]
  )
}

# Fit models on combined datasets
models_reclassified <- list()
for (cutoff in cut_off_values) {
  model <- train(form = Default ~ ., 
                 data = combined_datasets[[paste0("cutoff_", cutoff)]],
                 trControl = train_control,
                 method = "glm",
                 family = "binomial",
                 metric = "ROC")
  models_reclassified[[paste0("cutoff_", cutoff)]] <- model
}


# Loading of models
parcelling_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Real_Data_scripts\\parcelling_models_real.rds")
augmentation_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Real_Data_scripts\\augmentation_models_real.rds")

# # Initialize Gini values data frames
gini_values_financed <- data.frame(cutoff = cut_off_values, gini = numeric(length(cut_off_values)))
gini_values_reclassified <- data.frame(cutoff = cut_off_values, gini = numeric(length(cut_off_values)))
gini_values_augmentation <- data.frame(cutoff = cut_off_values, gini = numeric(length(cut_off_values)))
gini_values_parcelling <- data.frame(cutoff = cut_off_values, gini = numeric(length(cut_off_values)))

for (i in seq_along(cut_off_values)) {
  cutoff <- cut_off_values[i]
  
  # For financed models
  model_financed <- models_financed[[paste0("cutoff_", cutoff)]]
  test_data$pred_prob_test <- predict(model_financed, newdata = test_data, type = "prob")[, "Bad"]
  roc_obj_financed <- roc(test_data$Default, test_data$pred_prob_test)
  auc_value_financed <- auc(roc_obj_financed)
  gini_value_financed <- 2 * auc_value_financed - 1
  gini_values_financed$gini[i] <- gini_value_financed
  
  # For reclassified models
  model_reclassified <- models_reclassified[[paste0("cutoff_", cutoff)]]
  test_data$pred_prob_test <- predict(model_reclassified, newdata = test_data, type = "prob")[, "Bad"]
  roc_obj_reclassified <- roc(test_data$Default, test_data$pred_prob_test)
  auc_value_reclassified <- auc(roc_obj_reclassified)
  gini_value_reclassified <- 2 * auc_value_reclassified - 1
  gini_values_reclassified$gini[i] <- gini_value_reclassified
  
  # For augmentation models
  model_augmentation <- augmentation_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_prob_test <- predict(model_augmentation, newdata = test_data, type = "prob")[, "Bad"]
  roc_obj_augmentation <- roc(test_data$Default, test_data$pred_prob_test)
  auc_value_augmentation <- auc(roc_obj_augmentation)
  gini_value_augmentation <- 2 * auc_value_augmentation - 1
  gini_values_augmentation$gini[i] <- gini_value_augmentation
  
  # For parcelling models
  model_parcelling <- parcelling_models_list[[paste0("cutoff_", cutoff)]]
  test_data$pred_prob_test <- predict(model_parcelling, newdata = test_data, type = "prob")[, "Bad"]
  roc_obj_parcelling <- roc(test_data$Default, test_data$pred_prob_test)
  auc_value_parcelling <- auc(roc_obj_parcelling)
  gini_value_parcelling <- 2 * auc_value_parcelling - 1
  gini_values_parcelling$gini[i] <- gini_value_parcelling
}

# Combine Gini values into a single data frame for plotting
gini_values_combined <- data.frame(
  cutoff = rep(cut_off_values, 4),
  gini = c(gini_values_financed$gini, gini_values_reclassified$gini, gini_values_augmentation$gini, gini_values_parcelling$gini),
  model_type = rep(c("Financed", "Reclassified", "Augmentation", "Parcelling"), each = length(cut_off_values))
)



# Define the model types and corresponding shapes and colors
model_types <- c('Financed','Augmentation', 'Reclassification', 'Parcelling')
shapes <- c(17, 15, 8, 7)  # Corresponding shapes for each model type
colors <- c("green","black", "magenta", "red")  # Corresponding colors for each model type

ggplot(gini_values_combined, aes(x = cutoff, y = gini, color = model_type, shape = model_type)) +
  stat_summary(fun = mean, geom = "point", size = 2) +  # Increase point size
  stat_summary(fun = mean, geom = "line") +   # Increase line width
  scale_shape_manual(values = shapes) +  # Use different shapes for points
  scale_color_manual(values = colors) +  # Use different colors for lines
  labs(
    title = "Comparison of Reject Inference Methods on Real UCI Credit Card dataset",
    x = "Cut-off value",
    y = "Gini on Test Set",
    color = "Model Type",
    shape = "Model Type"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.15, 0.15),  # Position inside the plot area (x, y) in [0,1] coordinates
    legend.background = element_rect(fill = "white", color = "black"),  # Add white background and black border to the legend
    legend.title = element_blank(),
    legend.text = element_text(size = 10),  # Adjust legend text size as needed
    axis.title = element_text(size = 12),   # Adjust axis titles size
    axis.text = element_text(size = 10)     # Adjust axis labels size
  )

