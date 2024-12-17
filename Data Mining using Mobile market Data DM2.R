#install.packages("rpart")
#install.packages('MASS')
library(dplyr)
library(MASS)

# Load and combine datasets
df1 <- read.csv('phone_user_review_file_1.csv')
df2 <- read.csv('phone_user_review_file_2.csv')
df3 <- read.csv('phone_user_review_file_3.csv')
df4 <- read.csv('phone_user_review_file_4.csv')
df5 <- read.csv('phone_user_review_file_5.csv')
df6 <- read.csv('phone_user_review_file_6.csv')
merged_df <- rbind(df1, df2, df3, df4, df5, df6)

# Step 1: Calculate the median of the 'score' column, excluding NA values
median_score <- median(merged_df$score, na.rm = TRUE)

# Step 2: Impute NA values with the median
merged_df$score[is.na(merged_df$score)] <- median_score

# Step 3: Verify the imputation
na_count_score_after <- sum(is.na(merged_df$score))

# Print the results to confirm no NA values remain
print(paste("NA count in 'score' after imputation:", na_count_score_after))
print(paste("Median value used for imputation:", median_score))


# Preprocessing Data
merged_df <- merged_df %>%
  filter(!is.na(extract)) %>%
  mutate(
    Sentiment = case_when(
      score <= 3 ~ 0,
      score > 3 & score < 7 ~ 1,
      score >= 7 ~ 2
    )
  )

# Convert the date column to Date object
merged_df$date <- as.Date(merged_df$date, format = "%m/%d/%Y")

# Check the updated format
head(merged_df$date)

# Now you can proceed to format it as desired
merged_df$date <- format(merged_df$date, "%Y-%m-%d")

# Check the summary statistics
summary(merged_df$date)

typeof(merged_df$date)

# Remove rows with NA values in the date column
merged_df <- merged_df[!is.na(merged_df$date), ]


library(dplyr)

# Assuming 'merged_df' is the name of your data frame
filtered_df <- suppressWarnings(merged_df %>%
                                  filter(grepl("^[a-zA-Z0-9 ]+$", source) & grepl("^[a-zA-Z0-9 ]+$", product))
)

nrow(filtered_df)

library(dplyr)


subset_df <- filtered_df %>%
  select(lang, country, source, product, date, score)

library(dplyr)
#install.packages('caret')
library(caret)

subset_df$date <- as.Date(subset_df$date, format = "%Y-%m-%d")

# Convert date to year
subset_df$year <- as.integer(format(subset_df$date, "%Y"))

# Create a variable for the decade
subset_df$decade <- cut(subset_df$year, 
                        breaks = c(1970, seq(1980, 2020, by = 10)), 
                        labels = c("1970-1979", paste(seq(1980, 2010, by = 10), "-", seq(1989, 2019, by = 10))))

library(dplyr)
library(tidyr)

# Function to perform one-hot encoding
perform_one_hot_encoding <- function(data) {
  # Perform one-hot encoding for each column
  encoded_data <- data %>%
    mutate_if(is.character, factor) %>%
    group_by(decade) %>%
    mutate_at(vars(lang, country, source, product), list(~ as.integer(factor(.)))) %>%
    ungroup()
  
  return(encoded_data)
}

# Subset the data by decades
decades <- unique(subset_df$decade)

# Perform one-hot encoding for each decade
encoded_dfs <- lapply(decades, function(decade) {
  subset_decade <- subset_df %>% filter(decade == decade)
  encoded_subset <- perform_one_hot_encoding(subset_decade)
  return(encoded_subset)
})

# Combine the encoded dataframes for each decade
final_encoded_df <- bind_rows(encoded_dfs)

final_df <- cbind(subset_df, final_encoded_df)

head(final_encoded_df)



final_encoded_df <- final_encoded_df %>%
  mutate(
    Sentiment = case_when(
      score <= 5 ~ 0,
      score >= 5 ~ 1
    )
  )




# Select the columns for correlation calculation
selected_columns <- final_encoded_df[, c("lang", "country", "source", "product", "Sentiment", "year")]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)


# Load the corrplot package
library(corrplot)

# Create a correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Load necessary libraries
library(glmnet)

# Prepare data for logistic regression
# Assuming 'final_encoded_df' contains the data with encoded features and 'score' is the target variable


LogReg_final_encoded_df <- final_encoded_df %>%
  select(lang, country,source, year, product,Sentiment)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(LogReg_final_encoded_df), 0.8 * nrow(LogReg_final_encoded_df))
train_data <- LogReg_final_encoded_df[train_index, ]
test_data <- LogReg_final_encoded_df[-train_index, ]

#install.packages("nnet")
#library(nnet)

# Train logistic regression model
logit_model <- glm(Sentiment ~ ., data = train_data,family = binomial())
summary(logit_model)

logit_model$aic

log_modelstep <-stepAIC(logit_model)


library(gt)
library(broom)

# Assuming your logistic regression model is stored as `logit_model`
# Tidy the model's summary
tidy_log_model <- broom::tidy(logit_model, conf.int = TRUE)

# Create a gt table from the tidy summary
gt_table <- tidy_log_model %>%
  gt() %>%
  cols_label(
    term = "Variable",
    estimate = "Coefficient",
    std.error = "Std. Error",
    statistic = "z value",
    p.value = "P-value"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value),
    decimals = 4
  ) %>%
  tab_header(
    title = "Logistic Regression Model Summary",
    subtitle = "Model coefficients and statistics"
  ) %>%
  tab_spanner(
    label = "Model Metrics",
    columns = vars(estimate, std.error, statistic, p.value)
  ) %>%
  tab_footnote(
    footnote = "Significance codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
    locations = cells_title(groups = "title")
  )

# Print the table
print(gt_table)


# Predict probabilities for the test data
predicted_probs <- predict(logit_model, newdata = test_data, type = "response")

# Determine class labels based on a threshold (default is 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)


# Ensure both are factors and have the same levels
predicted_classes_factor <- factor(predicted_classes, levels = c(0, 1))
actual_classes_factor <- factor(test_data$Sentiment, levels = c(0, 1))

cm <- confusionMatrix(predicted_classes_factor, actual_classes_factor)
print(cm)

# Convert the matrix to a data frame for compatibility with gt
cm_df <- as.data.frame(cm)

# Adding the Prediction and Reference labels correctly
cm_df$Prediction <- rownames(cm)
rownames(cm_df) <- NULL

# Ensure that no column names are duplicated
cm_df <- rename(cm_df, Reference0 = `0`, Reference1 = `1`)

# Now, pivot the dataframe to a long format
cm_df <- tidyr::pivot_longer(cm_df, 
                             cols = c("Reference0", "Reference1"), 
                             names_to = "Reference", 
                             values_to = "Count",
                             names_prefix = "Reference")

# The 'names_prefix' strips 'Reference' from the start of the column names to ensure the names are unique during pivot

# Create a gt table to display the results more elegantly
library(gt)

summary_table <- gt(cm_df) %>%
  tab_header(
    title = "Confusion Matrix and Statistics"
  ) %>%
  cols_label(
    Prediction = "Prediction",
    Reference = "Reference",
    Count = "Count"
  ) %>%
  fmt_number(
    columns = vars(Count),
    decimals = 0
  ) %>%
  tab_footnote(
    footnote = paste0("Accuracy: ", accuracy, ", Sensitivity: ", sensitivity,
                      ", Specificity: ", specificity, ", Pos Pred Value: ",
                      ifelse(is.nan(pos_pred_value), "NaN", pos_pred_value),
                      ", Neg Pred Value: ", neg_pred_value, 
                      ", Balanced Accuracy: ", balanced_accuracy)
  )

# Print the gt table
print(summary_table)





# Ensure actual_classes is a factor with the correct levels
#actual_classes_factor <- factor(actual_classes, levels = c(0, 1))

# Create ROC object
roc_obj <- roc(response = actual_classes_factor, predictor = predicted_probs)

# Plot ROC curve
plot(roc_obj, main = "ROC Curve", col = "#1c61b6", lwd = 2)


# Calculate AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "#1c61b6", lwd = 2)

# Add AUC to the plot
text(x=0.6, y=0.2, labels=paste("AUC:", formatC(auc_value, format="f", digits=2)), col="black")


####Decion Tress########


# Fit a decision tree model
dt_model <- rpart(Sentiment ~ ., data = train_data, method = "class")

# Print the summary of the model
print(summary(dt_model))

print(names(cp_df))


# Assuming 'cp_table' comes from the summary of a model, not directly from 'printcp()' as it was in the sample code
# Convert the CP table from the decision tree model summary to a data frame
cp_df <- as.data.frame(summary(dt_model)$cp)
# Load the necessary libraries
library(gt)
library(dplyr)

# Use the exact column names from cp_df in the cols_label function
gt_summary <- cp_df %>%
  gt() %>%
  tab_header(
    title = "Decision Tree Model Summary",
    subtitle = "Model statistics including complexity parameter, splits, and errors"
  ) %>%
  cols_label(
    CP = "Complexity Parameter",
    nsplit = "Number of Splits",
    `rel error` = "Relative Error",  # Correct usage of backticks for column names with spaces
    xerror = "Cross-validation Error",
    xstd = "Standard Deviation of Cross-validation Error"
  )

# Print the summary table
print(gt_summary)






# Make predictions
dt_predictions <- predict(dt_model, test_data, type = "class")

# Print unique values in the original Sentiment column to confirm it contains valid categorical data
print(unique(test_data$Sentiment))
# Print a few of the raw predictions to check their output
print(head(dt_predictions))



# Load caret for evaluation
library(caret)

# Confusion Matrix
dt_cm <- confusionMatrix(dt_predictions_factor, actual_classes_factor)
print(dt_cm)

# Define the levels present in both the predictions and the actual data
common_levels <- c("0", "1")  # Levels present in the data and predicted

# Recreate factors with the common levels
dt_predictions_factor <- factor(dt_predictions, levels = common_levels)
actual_classes_factor <- factor(test_data$Sentiment, levels = common_levels)

# Check levels to ensure they are set correctly
print(levels(dt_predictions_factor))
print(levels(actual_classes_factor))

# Recompute the confusion matrix with the corrected factors
dt_cm <- confusionMatrix(dt_predictions_factor, actual_classes_factor)
print(dt_cm)


library(caret)
library(gt)
library(broom)
library(dplyr)

# Assuming 'dt_predictions' and 'actual_classes_factor' are available and the confusionMatrix has been calculated
dt_predictions <- predict(dt_model, test_data, type = "class")
actual_classes_factor <- factor(actual_classes, levels = c(0, 1))
dt_cm <- confusionMatrix(factor(dt_predictions, levels = c(0, 1)), actual_classes_factor)

# Extract values from the confusion matrix
conf_matrix <- as.matrix(dt_cm$table)
accuracy <- dt_cm$overall['Accuracy']
sensitivity <- dt_cm$byClass['Sensitivity']
specificity <- dt_cm$byClass['Specificity']
pos_pred_value <- dt_cm$byClass['Positive Predictive Value']
neg_pred_value <- dt_cm$byClass['Negative Predictive Value']
balanced_accuracy <- dt_cm$byClass['Balanced Accuracy']

# Create a tidy data frame of the confusion matrix
tidy_data <- tibble::tibble(
  Reference = rep(colnames(conf_matrix), each = nrow(conf_matrix)),
  Prediction = rep(rownames(conf_matrix), times = ncol(conf_matrix)),
  Value = as.vector(conf_matrix)
)

# Use broom to tidy the confusion matrix
conf_tidy <- tidy(conf_matrix)

# Create a gt summary table
summary_table <- tidy_data %>%
  gt() %>%
  tab_header(
    title = "Confusion Matrix and Statistics"
  ) %>%
  fmt_number(
    columns = vars(Value),
    decimals = 0
  ) %>%
  tab_spanner(
    label = "Confusion Matrix",
    columns = vars(Reference, Prediction)
  ) %>%
  tab_footnote(
    footnote = paste("Accuracy:", accuracy, "Sensitivity:", sensitivity, "Specificity:", specificity,
                     "Pos Pred Value:", pos_pred_value, "Neg Pred Value:", neg_pred_value,
                     "Balanced Accuracy:", balanced_accuracy)
  )

# Show the summary table
print(summary_table)

# Calculate AUC
library(pROC)
dt_roc <- roc(actual_classes_factor, as.numeric(dt_predictions_factor))
dt_auc <- auc(dt_roc)
print(paste("AUC:", dt_auc))

# Plot ROC Curve
plot(dt_roc, main = "Decision Tree ROC Curve", col = "#1c61b6", lwd = 2)

rpart.plot(dt_model, extra = 101, type = 4, fallen.leaves = TRUE)

##################################Q2######################################################

# Load necessary libraries
library(glmnet)

# Prepare data for logistic regression
# Assuming 'final_encoded_df' contains the data with encoded features and 'score' is the target variable


LRQ2 <- final_encoded_df %>%
  select(country,score,Sentiment)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index2 <- sample(1:nrow(LRQ2), 0.8 * nrow(LRQ2))
train_data2 <- LRQ2[train_index, ]
test_data2 <- LRQ2[-train_index, ]

#install.packages("nnet")
#library(nnet)

# Train logistic regression model
logit_model2 <- glm(Sentiment ~ ., data = train_data2,family = binomial())
summary(logit_model2)

# Predict probabilities for the test data
predicted_probs2 <- predict(logit_model2, newdata = test_data2, type = "response")

# Determine class labels based on a threshold (default is 0.5)
predicted_classes2 <- ifelse(predicted_probs2 > 0.5, 1, 0)
# Ensure both are factors and have the same levels
predicted_classes_factor2 <- factor(predicted_classes2, levels = c(0, 1))
actual_classes_factor2 <- factor(test_data2$Sentiment, levels = c(0, 1))

cm <- confusionMatrix(predicted_classes_factor2, actual_classes_factor2)
print(cm)


# Load necessary libraries
library(pROC)

log_pred_probs2 <- predict(logit_model2, test_data2, type = "response")

# Creating the ROC curve
roc_curve <- roc(test_data2$Sentiment, log_pred_probs2)

# Plotting the ROC curve
plot(roc_curve, main="ROC Curve for Logistic Regression", col="#1c61b6", lwd=2)
abline(a=0, b=1, lty=2, col="red")  # Adding a reference line

# Adding AUC (Area Under the Curve) to the plot
auc(roc_curve)  # This function calculates the AUC
print(paste("AUC: ", auc(roc_curve)))  # Printing the AUC value

# Optionally, adding AUC to the plot
text(x=0.6, y=0.2, labels=paste("AUC:", formatC(auc(roc_curve), format="f", digits=2)), col="#1c61b6")












# Fit a decision tree model
dt_model2 <- rpart(Sentiment ~ ., data = train_data2, method = "class")

# Print the summary of the model
print(summary(dt_model2))

# Make predictions
dt_predictions2 <- predict(dt_model2, test_data2, type = "class")

# Print unique values in the original Sentiment column to confirm it contains valid categorical data
print(unique(test_data2$Sentiment))
# Print a few of the raw predictions to check their output
print(head(dt_predictions))



# Load caret for evaluation
library(caret)



# Define the levels present in both the predictions and the actual data
common_levels <- c("0", "1")  # Levels present in the data and predicted

# Recreate factors with the common levels
dt_predictions_factor2 <- factor(dt_predictions2, levels = common_levels)
actual_classes_factor2 <- factor(test_data2$Sentiment, levels = common_levels)

# Confusion Matrix
dt_cm2 <- confusionMatrix(dt_predictions_factor2, actual_classes_factor2)
print(dt_cm2)


# Calculate AUC
library(pROC)
dt_roc2 <- roc(actual_classes_factor2, as.numeric(dt_predictions_factor2))
dt_auc2 <- auc(dt_roc2)
print(paste("AUC:", dt_auc2))

# Plot ROC Curve
plot(dt_roc2, main = "Decision Tree ROC Curve", col = "#1c61b6", lwd = 2)

