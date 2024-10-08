# Load necessary libraries
library(dplyr)
library(tidyr)
library(PMCMRplus)
library(reshape2)
library(rstatix)
library(scmamp)
library(MASS) # for mvrnorm
library(pROC)
source("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\functions.R")

set.seed(123)

# Define parameters
n_test <- 100000   
n_train <- 10000   
n_replicates <- 10
d <- 8             
cut_off_values <- seq(0, 0.9, by = 0.02)


# Covariance matrix (2 * identity matrix)
cov_matrix <- 2 * diag(d)


train_datasets <- generate_train_data(n_train, n_replicates, d, cov_matrix)
test_data<- generate_test_data(n_test, d, cov_matrix)


X_test <- test_data$X_test
Y_test <- test_data$Y_test



# As scmamp is not already supported we had to download it from github 

# if (!require("devtools")) {
#   install.packages("devtools")
# }
# 
# devtools::install_github("b0rxa/scmamp")


# We have to import our models and calculate its corresponding Gini which was precedently used just for plotting
# In this script we are doing various statistical test on these calculated gini values

reclassified_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\reclassified_models_list.rds")
financed_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\financed_models_list.rds")
augmentation_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\augmentation_models_list.rds")
parcelling_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\parcelling_models_list.rds")



# Call the calculate_gini function 
gini_results_augmentation <- calculate_gini(augmentation_models_list, X_test, Y_test, n_replicates)
gini_results_financed <- calculate_gini(financed_models_list, X_test, Y_test, n_replicates)
gini_results_reclassified <- calculate_gini(reclassified_models_list, X_test, Y_test, n_replicates)
gini_results_parcelling <- calculate_gini(parcelling_models_list, X_test, Y_test, n_replicates)




# Prepare the data (more info in functions)

gini_data_augmentation <- prepare_gini_data(gini_results_augmentation, "Augmentation", n_replicates)
gini_data_financed <- prepare_gini_data(gini_results_financed, "Financed", n_replicates)
gini_data_reclassified <- prepare_gini_data(gini_results_reclassified, "Reclassified", n_replicates)
gini_data_parcelling <- prepare_gini_data(gini_results_parcelling,'Parcelling', n_replicates)






# Rename columns in each data frame before merging to avoid duplication names 
# ( as previously our columns were named just gini for every model)
gini_data_augmentation <- gini_data_augmentation %>% rename(Augmented_gini = gini, Augmented_model_type = model_type)
gini_data_parcelling <- gini_data_parcelling %>% rename(Parcelling_gini = gini, Parcelling_model_type = model_type)
gini_data_reclassified <- gini_data_reclassified %>% rename(Reclassified_gini = gini, Reclassified_model_type = model_type)
gini_data_financed <- gini_data_financed %>% rename(Financed_gini = gini, Financed_model_type = model_type)




#######################################Statistical Tests############################################################

# Function to perform Shapiro-Wilk test and print the result
shapiro_and_print <- function(colonna) {
  shapiro_test <- shapiro.test(colonna)
  print(shapiro_test)
}

# Merge data frames sequentially
merged_df <- merge(gini_data_augmentation, gini_data_parcelling, by = c("cutoff", "replicate"), all = TRUE)
merged_df <- merge(merged_df, gini_data_reclassified, by = c("cutoff", "replicate"), all = TRUE)
merged_df <- merge(merged_df, gini_data_financed, by = c("cutoff", "replicate"), all = TRUE)

# Remove rows with NA
# Filter retains only those rows where complete.cases(.) is TRUE so rows with missing values will be dropped

merged_df_gini_complete <- merged_df %>% filter(complete.cases(.))

# Perform Shapiro-Wilk test and print results for columns of interest
columns_to_verify <- c("Financed_gini", "Reclassified_gini", "Augmented_gini", "Parcelling_gini")
for (column in columns_to_verify) {
  shapiro_and_print(merged_df_gini_complete[[column]])
}

required_columns <- c("Financed_gini", "Reclassified_gini", "Augmented_gini", "Parcelling_gini")

# Subset the dataset to include only the specified columns
df_restricted <- merged_df_gini_complete[, required_columns]

# Non-parametric test of equality of means: Friedman Test (general distribution)
friedmanTest(df_restricted)

# Post-hoc Nemenyi test (all-pairs)
test = nemenyiTest(df_restricted, alpha=0.05)
plotCD(df_restricted, alpha=0.05, cex=1) # critical difference plot
test # critical difference 
test$diff.matrix # difference in mean ranks
abs(test$diff.matrix) > test$statistic # significant tests







