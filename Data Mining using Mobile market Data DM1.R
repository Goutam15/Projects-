# Question 1: Can we develop a model to predict customer churn based on their Source, country, and days as customer?

## ---- 1. Libraries ----
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(ggplot2) 
library(Matrix) # For sparse matrices
library(gtsummary)

# Load Dataset
data <- read.csv("Downloads/archive (1)/merged_reviews.csv") 

# Data Exploration and Preprocessing 

data$date <- as.Date(data$date, format = "%m/%d/%Y")

# Create 'days_as_customer'
data$days_as_customer <- as.numeric(Sys.Date() - data$date)

# Handle Missing Values 
# Replace with your chosen strategy (e.g., removal or imputation)
data <- na.omit(data)  

# Encode Categoricals
data$source <- factor(data$source)
data$country <- factor(data$country)

# Create Churn Variable 
data$churn <- ifelse(data$days_as_customer > 6500, TRUE, FALSE) 

## Data Visualization 
ggplot(data, aes(x = days_as_customer)) + geom_histogram(bins = 30)

# Data Splitting 
library(caret)

set.seed(123)
train_index <- createDataPartition(data$churn, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Model Building 

# a. Logistic Regression
logit_model <- glm(churn ~ score + days_as_customer + source + product,
                   data = train_data, family = "binomial")
summary(logit_model)

# b. Decision Tree
library(rpart)

tree_model <- rpart(churn ~ score + days_as_customer + source + product,
                    data = train_data, method = "class")
print(tree_model)
summary(tree_model)

# c. Random Forest

library(Matrix)


## ---- 5. One-Hot Encoding (Base R Approach) ----
model_mat <- sparse.model.matrix(~ 0 + ., data = train_data) 
train_data_dummies <- cbind(train_data[c("score", "days_as_customer")], as.matrix(model_mat))

# Load necessary libraries
library(randomForest)
library(caret) # for createDataPartition and dummyVars

# Load Dataset 
data <- read.csv("Downloads/archive (1)/merged_reviews.csv")

# Data Preprocessing 
# Here include all necessary preprocessing steps like converting factors, handling missing values etc.

# Convert factors to dummy variables
dummies <- dummyVars(" ~ .", data = data)
data_dummies <- predict(dummies, newdata = data)

# Split the data into training and test sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(dummies$churn, p = 0.7, list = FALSE)
train_data_dummies <- dummies[train_index, ]
test_data_dummies <- dummies[-train_index, ]

# Random Forest Model Building 
# Ensure that 'churn' is a factor if it's not already
train_data$churn <- as.factor(train_data$churn)

# Build the Random Forest model
rf_model <- randomForest(churn ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data)), importance = TRUE, na.action = na.omit)

# Print the Random Forest model summary
print(rf_model)

# Print the variable importance
importance(rf_model)

# Predict on test data
test_data_predictions <- predict(rf_model, newdata = test_data_dummies)

# Create a confusion matrix to evaluate the model on test data
confusion_matrix <- table(test_data_dummies$churn, test_data_predictions)

# Print the confusion matrix
print(confusion_matrix)




# Model Evaluation 
# Prepare test_data using the same preprocessing and one-hot encoding
predictions <- predict(rf_model, newdata = test_data_dummies) 

# Calculate evaluation metrics using 'caret' or other packages
library(caret)
confusionMatrix(predictions, test_data$churn)

# Question 2: Are there distinct segments of customers where different factors being the strongest predictors of churn?

# Libraries 
library(dplyr)   # For data manipulation
library(tidyr)   # For potential data reshaping
library(rpart)   # For decision trees
library(rpart.plot)  # For visualizing decision trees
library(cluster) # For clustering analysis
library(randomForest) # For churn prediction within segments

# Load Dataset

data <- read.csv("Downloads/archive (1)/merged_reviews.csv") 

# Data Preparation and Exploration 

train_data$churn <- as.logical(train_data$churn)

# Basic Summary
summary(data)

# Handle Missing Values 

for (col in c("source", "product", "score", "country", "lang", "days_as_customer")) {
  if (anyNA(train_data[[col]])) {
    train_data[[col]] <- impute.mean(train_data[[col]]) 
  }
}

# Encode categorical variables
train_data$source <- factor(train_data$source)
train_data$product <- factor(train_data$product)

summary(train_data)

## ---- 5. Data Splitting ----
table(train_data$churn)
set.seed(123) 
train_index <- createDataPartition(train_data$churn, p = 0.7, list = FALSE, times =1) 





# Basic Sentiment Analysis
train_data$sentiment <- ifelse(train_data$score > 8, "positive", 
                         ifelse(train_data$score < 5, "negative", "neutral"))


str(train_data) 
str(test_data)  
str(data)


# Data Splitting 
table(train_data$churn)
set.seed(123) 
train_index <- createDataPartition(train_data$churn, p = 0.7, list = FALSE, times = 1) 

# Data Splitting 
table(data$sentiment)
set.seed(123) 
train_index <- createDataPartition(data$sentiment, p = 0.7, list = FALSE, times = 1) 

# Feature Engineering (Do it again on the split datasets)

test_data$date <- as.Date(test_data$date)  
test_data$year_joined <- format(test_data$date, "%Y")
test_data$month_joined <- format(test_data$date, "%m") 
test_data$churn<- format(test_data$churn)


str(data)

table(train_data$churn)


sapply(train_data, class)
lapply(train_data, levels)
head(train_data)

model_matrix <- model.matrix(~ -1 + ., data = train_data)
# Replace train_data with the matrix excluding the churn column
train_data_encoded <- cbind(train_data[,-which(names(train_data) == "churn")], model_matrix) 

# Select relevant features
clustering_data <- train_data[, c("source", "product", "score", "country", "lang", "days_as_customer")] 
set.seed(123) 
km_results <- kmeans(clustering_data, centers = 4) # Adjust the number of clusters
train_data$cluster <- km_results$cluster

churn_tree <- rpart(churn ~ source + product + score + country + lang + days_as_customer,
                    data = train_data, method = "class")
rpart.plot(churn_tree)  


# Segmentation  

# a. Decision Tree for Visualization
churn_tree <- rpart(churn ~ source + product + score + country + lang + days_as_customer,
                    data = train_data, method = "class")
rpart.plot(churn_tree)  


