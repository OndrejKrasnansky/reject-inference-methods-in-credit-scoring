# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)


# Set the seed for reproducibility
set.seed(123)

# Number of samples 
n <- 10000

# Generate Y from a Bernoulli(0.5) distribution
Y <- rbinom(n, size = 1, prob = 0.5)

# Generate X from a Normal distribution based on Y
X <- rnorm(n, mean = Y, sd = 1)

# Combine into a data frame
data <- data.frame(X=X, Y = Y)

# Learn logistic regression model on the full dataset 
#(on the whole population which will be used to cutoff to create non financed and financed clients)
# Y as a dependet variable X as independet ( familiy binomial to indicate that is logistic regression )
model_full <- glm(Y ~ X, data = data, family = binomial)

# Predict probabilities using the full logistic regression model and create a column pred_prob_full
# "Response" gives us a predicted probability for a succes (1) for each observation 
data$pred_prob_full <- predict(model_full, type = "response")

data

# Identify non-financed clients (predicted probability < 0.3) and drop the previously created column using dplyr
non_financed <- data %>% filter(pred_prob_full < 0.3) %>% dplyr::select(-pred_prob_full)

# Identify financed clients (predicted probability >= 0.3)
financed <- data %>% filter(pred_prob_full >= 0.3) %>% dplyr::select(-pred_prob_full) 



# Fit logistic regression model just on financed clients
model_financed <- glm(Y ~ X, data = financed, family = binomial)

# Predict Y for non-financed clients using model_financed and create Y_pred column which will be continuosly renamed as Y
non_financed$pred_Y <- predict(model_financed, newdata = non_financed, type = "response")

# Round predicted probabilities to get binary predictions (0 or 1)
non_financed$pred_Y <- ifelse(non_financed$pred_Y >= 0.5, 1, 0)

# Drop the first column 'Y' from non_financed dataframe
non_financed <- non_financed %>% dplyr::select(-Y)

# Rename the 'pred_Y' column to 'Y'
non_financed <- non_financed %>% rename(Y = pred_Y)

# Combine financed and non_financed dataframes by adding rows
combined_df <- rbind(financed, non_financed)

# Print the combined dataframe just to check 
print(combined_df)


# Train the final reclassified model on combined dataframe from financed and nonfinanced dataset with its Y predicted
model_reclassified <- glm(Y ~ X, data = combined_df, family = binomial)

# Add predicted probabilities to the combined_df dataframe 
# This is used just for plotting as we need Y-axis as probability 
combined_df <- combined_df %>%
  mutate(prob_full = predict(model_full, newdata = combined_df, type = "response"),
         prob_financed = predict(model_financed, newdata = combined_df, type = "response"),
         prob_reclassified = predict(model_reclassified, newdata = combined_df, type = "response"))




# Define the model types and corresponding shapes and colors

model_types <- c('Oracle', 'Financed', 'Reclassified')
colors <- c("orange", "blue", "red")  # Corresponding colors for each model type

ggplot(combined_df, aes(x = X)) +
  geom_line(aes(y = prob_full, color = 'Oracle'), linetype = "dashed", size = 1) +
  geom_line(aes(y = prob_financed, color = 'Financed'), linetype = "dotted", size = 1.2) +
  geom_line(aes(y = prob_reclassified, color = 'Reclassified'), linetype = "solid", size = 1) +
  labs(
    title = "",
    x = "Feature x",
    y = expression(p(1 ~ "|" ~ x)),
    color = "Model",
    shape = "Model"
  ) +
  scale_shape_manual(values = shapes) +  # Use different shapes for points
  scale_color_manual(values = colors) +  # Use different colors for lines
  theme_classic() +  # theme_classic to use just a white background 
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.6, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 8),  # Adjust legend text size as needed
    legend.title = element_text(size = 10),  # Adjust legend title size as needed
    axis.title = element_text(size = 10),   # Adjust axis titles size
    axis.text = element_text(size = 10)     # Adjust axis labels size 
  )  + scale_x_continuous(limits = c(-2, 4), breaks = seq(-2, 4, 1)) + # x_continuos and y_continous used to imititate exact plot scale from the original paper
  scale_y_continuous(breaks = c(0, 0.4, 0.8)) 









