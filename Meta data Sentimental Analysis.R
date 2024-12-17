# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(readr)      # For reading CSV files

# Load the dataset
tweets <- read_csv("Downloads/archive (12)/auspol2019.csv")

# View the first few rows of the dataset and the structure
head(tweets)
str(tweets)

colnames(tweets)

# Load necessary libraries
library(dplyr)
library(lubridate)
library(stringr)
library(caret)
library(pROC)

# Step 1: Preprocessing Data
tweets <- tweets %>%
  mutate(
    hour = lubridate::hour(lubridate::ymd_hms(created_at)),
    day_of_week = lubridate::wday(lubridate::ymd_hms(created_at)),
    tweet_length = nchar(full_text),
    hashtags_count = str_count(full_text, "#\\w+"),
    mentions_count = str_count(full_text, "@\\w+"),
    urls_count = str_count(full_text, "http[s]?://\\S+"),
    retweet_count_log = log1p(retweet_count)  # Using log of retweet counts as a proxy for engagement
  )

# Step 2: Create binary target variable based on a threshold K for likes
K <- 3  # Set threshold value K
tweets <- mutate(tweets, liked_more_than_K = as.integer(favorite_count > K))

# Step 3: Feature Engineering
# Additional feature based on user description length (if applicable)
tweets <- mutate(tweets, user_description_length = nchar(as.character(user_description)))

# Step 4: Prepare Data for Modeling
# Split data into training and testing sets
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(tweets$liked_more_than_K, p = 0.8, list = TRUE)
train_data <- tweets[training_indices[[1]], ]
test_data <- tweets[-training_indices[[1]], ]

# Step 5: Build Logistic Regression Model
model <- glm(liked_more_than_K ~ hour + day_of_week + tweet_length + hashtags_count + 
               mentions_count + urls_count + retweet_count_log + user_description_length, 
             data = train_data, family = binomial())

# Step 6: Model Evaluation
# Predict on test set
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate model using confusion matrix and AUC
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$liked_more_than_K)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Compute ROC and AUC
roc_result <- roc(response = test_data$liked_more_than_K, predictor = predictions)
auc_value <- auc(roc_result)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("AUC:", auc_value, "\n")

# Assuming necessary libraries are loaded, including sentimentr for sentiment analysis

# Check the output structure of sentiment analysis
print(head(sentiment_results))
str(sentiment_results)


# Aggregate sentiment scores by element_id assuming it directly maps to tweets
# Calculating average sentiment score per tweet
aggregated_sentiment <- sentiment_results %>%
  group_by(element_id) %>%
  summarise(
    avg_sentiment = mean(ave_sentiment, na.rm = TRUE),  # Average sentiment
    sentiment_valence = sign(mean(ave_sentiment, na.rm = TRUE))  # Overall sentiment valence
  )

# Check column names in both data frames
print(names(tweets))
print(names(sentiment_results))

# Add an element_id based on row number to tweets
tweets <- tweets %>% 
  mutate(element_id = row_number())

# Now perform the join
tweets <- left_join(tweets, sentiment_results, by = "element_id")

# Check results to ensure the join was successful
print(head(tweets))


# Assuming 'element_id' in sentiment_results maps directly to a column in tweets that identifies tweets
# This column might be 'id' or another unique identifier per tweet
tweets <- left_join(tweets, aggregated_sentiment, by = "element_id")

# Verify the join and view results
print(head(tweets))

# Prepare data for modeling including sentiment scores
set.seed(123)
train_indices <- createDataPartition(tweets$liked_more_than_K, p = 0.8, list = TRUE)
train_data <- tweets[train_indices[[1]], ]
test_data <- tweets[-train_indices[[1]], ]

# Build logistic regression model including sentiment features
model <- glm(liked_more_than_K ~ hour + day_of_week + tweet_length + hashtags_count + 
               mentions_count + urls_count + avg_sentiment + sentiment_valence, 
             data = train_data, family = binomial())

# Predict and evaluate the model
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$liked_more_than_K)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
roc_result <- roc(response = test_data$liked_more_than_K, predictor = predictions)
auc_value <- auc(roc_result)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("AUC:", auc_value, "\n")

library(pROC)

# Build the model using tweet metadata

model_tweet_metadata <- glm(liked_more_than_K ~ hour + day_of_week + tweet_length + 
                              hashtags_count + mentions_count + urls_count,
                            data = train_data, family = binomial())

# Generate predictions
predictions_tweet_metadata <- predict(model_tweet_metadata, newdata = test_data, type = "response")

# Calculate ROC and AUC for the tweet metadata model
roc_tweet_metadata <- roc(test_data$liked_more_than_K, predictions_tweet_metadata)
auc_tweet_metadata <- auc(roc_tweet_metadata)

# Plot the ROC curve
plot(roc_tweet_metadata, main = "ROC Curve for Tweet Metadata", col = "blue")
abline(a = 0, b = 1, col = "red", lty = 2)  # Reference line
legend("bottomright", legend = paste("AUC =", round(auc_tweet_metadata, 3)), col = "blue", lwd = 2)

# Build the model using user metadata

model_user_metadata <- glm(liked_more_than_K ~ user_description_length,
                           data = train_data, family = binomial())

# Generate predictions
predictions_user_metadata <- predict(model_user_metadata, newdata = test_data, type = "response")

# Calculate ROC and AUC for the user metadata model
roc_user_metadata <- roc(test_data$liked_more_than_K, predictions_user_metadata)
auc_user_metadata <- auc(roc_user_metadata)

# Plot the ROC curve
plot(roc_user_metadata, main = "ROC Curve for User Metadata", col = "green")
abline(a = 0, b = 1, col = "red", lty = 2)  # Reference line
legend("bottomright", legend = paste("AUC =", round(auc_user_metadata, 3)), col = "green", lwd = 2)


# Build logistic regression model using sentiment scores
model_sentiment <- glm(liked_more_than_K ~ avg_sentiment,
                       data = train_data, family = binomial())

# Generate predictions for the test set using the sentiment model
predictions_sentiment <- predict(model_sentiment, newdata = test_data, type = "response")


# Calculate the ROC curve and AUC for the sentiment score model
roc_result_sentiment <- roc(response = test_data$liked_more_than_K, predictor = predictions_sentiment)
auc_value_sentiment <- auc(roc_result_sentiment)

# Plot the ROC curve
plot(roc_result_sentiment, main = "ROC Curve for Sentiment Scores", col = "#8e44ad")
abline(a = 0, b = 1, col = "red", lty = 2)  # Add a diagonal reference line
legend("bottomright", legend = paste("AUC =", round(auc_value_sentiment, 3)), 
       box.lty = 1, col = "#8e44ad", lwd = 2)


# Compute ROC and AUC
roc_result <- roc(response = test_data$liked_more_than_K, predictor = as.numeric(predictions))
auc_value <- auc(roc_result)

# Plot the ROC curve
plot(roc_result, main = "ROC Curve for Tweet Popularity Model", col = "#2c3e50")
abline(a = 0, b = 1, col = "red", lty = 2)  # Add a reference line
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), box.lty = 1, col = "#2c3e50", lwd = 2)


library(ggplot2)

# Example: Visualize the distribution of tweet lengths
ggplot(tweets, aes(x = tweet_length)) +
  geom_histogram(bins = 30, fill = "#3498db") +
  labs(title = "Distribution of Tweet Lengths", x = "Tweet Length", y = "Frequency")
