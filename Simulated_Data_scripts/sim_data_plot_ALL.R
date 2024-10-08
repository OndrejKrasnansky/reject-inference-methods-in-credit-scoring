
# Required Libraries
library(ggplot2)
library(dplyr)
library(MASS) 
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






reclassified_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\reclassified_models_list.rds")
financed_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\financed_models_list.rds")
augmentation_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\augmentation_models_list.rds")
parcelling_models_list <- readRDS("C:\\Users\\ondre\\Documents\\statistics_scripts\\Simulated_Data_SCRIPTS\\parcelling_models_list.rds")






# Call the calculate_gini function to see the debug output
gini_results_augmentation <- calculate_gini(augmentation_models_list, X_test, Y_test, n_replicates)
gini_results_financed <- calculate_gini(financed_models_list, X_test, Y_test, n_replicates)
gini_results_reclassified <- calculate_gini(reclassified_models_list, X_test, Y_test, n_replicates)
gini_results_parcelling <- calculate_gini(parcelling_models_list, X_test, Y_test, n_replicates)




# Prepare the data (more info in functions)

gini_data_augmentation <- prepare_gini_data(gini_results_augmentation, "Augmentation", n_replicates)
gini_data_financed <- prepare_gini_data(gini_results_financed, "Financed", n_replicates)
gini_data_reclassified <- prepare_gini_data(gini_results_reclassified, "Reclassified", n_replicates)
gini_data_parcelling <- prepare_gini_data(gini_results_parcelling,'Parcelling', n_replicates)


# Combine both datasets
gini_data_combined <- rbind(gini_data_financed, gini_data_augmentation, gini_data_reclassified, gini_data_parcelling)
gini_data_combined <- na.omit(gini_data_combined)





# Define the model types and corresponding shapes and colors
model_types <- c('Financed','Augmentation', 'Reclassification', 'Parcelling')
shapes <- c(17, 15, 8, 7)  # Corresponding shapes for each model type
colors <- c("green","black", "magenta", "red")  # Corresponding colors for each model type

ggplot(gini_data_combined, aes(x = cutoff, y = gini, color = model_type, shape = model_type)) +
  stat_summary(fun = mean, geom = "point", size = 2) +  # Increase point size
  stat_summary(fun = mean, geom = "line") +   # Increase line width
  scale_shape_manual(values = shapes) +  # Use different shapes for points
  scale_color_manual(values = colors) +  # Use different colors for lines
  labs(
    title = "Comparison of Reject Inference Methods with a Well-Specified Model",
    x = "Cut-off value",
    y = "Gini on Test Set",
    color = "Model Type",
    shape = "Model Type"
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0.77, 0.85), breaks = seq(0.77, 0.85, 0.01)) +
  scale_x_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, 0.2)) +
  theme(
    legend.position = c(0.15, 0.54),  # Position inside the plot area (x, y) in [0,1] coordinates
    legend.background = element_rect(fill = "white", color = "black"),  # Add white background and black border to the legend
    legend.title = element_blank(),
    legend.text = element_text(size = 10),  # Adjust legend text size as needed
    axis.title = element_text(size = 12),   # Adjust axis titles size
    axis.text = element_text(size = 10)     # Adjust axis labels size
  )



