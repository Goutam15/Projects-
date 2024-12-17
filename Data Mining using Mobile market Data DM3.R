# Load necessary libraries
library(dplyr)
library(readr)
library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(data.table)

# Read and prepare the data
reviews_combined <- bind_rows(
  read.csv('phone_user_review_file_1.csv'),
  read.csv('phone_user_review_file_2.csv'),
  read.csv('phone_user_review_file_3.csv'),
  read.csv('phone_user_review_file_4.csv'),
  read.csv('phone_user_review_file_5.csv'),
  read.csv('phone_user_review_file_6.csv')
)

# Sample data to manage memory effectively
set.seed(123)
reviews_sample <- sample_n(reviews_combined, size = 5000)

# Convert 'score' to a binary outcome based on the average score
average_score <- mean(reviews_sample$score, na.rm = TRUE)
reviews_sample <- mutate(reviews_sample, positive_review = as.factor(score > average_score))

# Remove rows with missing values in the relevant columns
reviews_sample <- reviews_sample %>%
  filter(!is.na(score) & !is.na(country) & !is.na(source) & !is.na(product) & !is.na(positive_review))

# Fit the models
logistic_model <- glm(positive_review ~ ., data = reviews_sample, family = binomial())
rf_model <- randomForest(positive_review ~ ., data = reviews_sample, ntree = 100, na.action = na.omit)

# Analyze the models
summary(logistic_model)
importance(rf_model)

# ROC curve for logistic model
roc_curve <- roc(reviews_sample$positive_review, predict(logistic_model, type = "response", newdata = reviews_sample))
plot(roc_curve)

# Save the models
saveRDS(logistic_model, "logistic_model.rds")
saveRDS(rf_model, "rf_model.rds")

# Fit the Random Forest model
rf_model <- randomForest(positive_review ~ score + country + source + product, data = reviews_sample, ntree = 100)


# Fit the Random Forest model
rf_model <- randomForest(positive_review ~ score + country + source + product, data = reviews_sample, ntree = 100)

# Create a model summary table
rf_model_summary <- data.frame(
  Type = c("No. of Trees", "No. of Variables Tried at Each Split"),
  Value = c(rf_model$ntree, rf_model$mtry)
)
print(rf_model_summary)

# Predict outcomes using the Random Forest model
rf_predictions <- predict(rf_model, newdata = reviews_sample, type = "class")

# Generate a confusion matrix using caret
conf_matrix <- confusionMatrix(data = rf_predictions, reference = reviews_sample$positive_review)
print(conf_matrix)

# Extract overall model statistics (e.g., Accuracy, Kappa)
statistics_table <- data.frame(
  Metric = c("Accuracy", "Kappa"),
  Value = c(conf_matrix$overall['Accuracy'], conf_matrix$overall['Kappa'])
)
print(statistics_table)

# Extract and plot variable importance
importance_data <- importance(rf_model)
importance_df <- data.frame(
  Variable = rownames(importance_data),
  MeanDecreaseGini = importance_data[, "MeanDecreaseGini"]
)

# Plotting Variable Importance using ggplot2
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini, fill = Variable)) +
  geom_col() +
  coord_flip() +
  labs(title = "Variable Importance (Mean Decrease Gini)", x = "Variables", y = "Mean Decrease Gini") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("RF_Variable_Importance.png", width = 8, height = 6)

# ROC curve for the Random Forest model using probability predictions
rf_prob_predictions <- predict(rf_model, newdata = reviews_sample, type = "prob")[,2]
roc_data_rf <- roc(reviews_sample$positive_review, rf_prob_predictions)

# Plotting the ROC Curve using ggplot2
roc_plot_rf <- ggplot(data = data.frame(specificity = 1 - roc_data_rf$specificities, sensitivity = roc_data_rf$sensitivities),
                      aes(x = specificity, y = sensitivity)) +
  geom_line(color = "red") +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve for Random Forest Model", x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal()

# Display and save the ROC plot
print(roc_plot_rf)
ggsave("ROC_Curve_RF_Model.png", plot = roc_plot_rf, width = 8, height = 6)








