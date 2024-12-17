library(tidyverse)  # For data manipulation and visualization
library(caret)      # For modeling
library(randomForest)  # For Random Forest model
library(rpart)         # For Decision Trees
library(rpart.plot)    # For plotting Decision Trees

# Load the dataset (make sure the path is correct for your "MeTooHate.csv" file)
data <- read.csv("Downloads/MeTooHate.csv")

# View the first few rows of the dataset
head(data)

# Exploratory Data Analysis (EDA)

# Basic summary of the dataset
summary(data)

# Exploring data structure 
str(data)

# Load the necessary library
library(gtsummary)

# Assuming 'data' is your dataset
# Exploring the structure of the dataset
str(data)

# Creating a summary table
# If 'data' is a dataframe or tibble, you can create a table using tbl_summary()
table <- tbl_summary(data)


# Create a new variable for tweet length
data$tweet_length <- nchar(data$text)  # Assuming 'text' column has tweets

# Plotting distributions of numeric variables
hist(data$tweet_length, main="Distribution of Tweet Length", xlab="Number of Characters", col="blue")

print(names(data))

# Corrected boxplot code using 'category' as the grouping variable
boxplot(tweet_length ~ category, data=data, main="Tweet Length by Tweet Type", xlab="Tweet Type", ylab="Length", col=c("red", "green"))


# Count plot for tweet types
ggplot(data, aes(x = category)) + 
  geom_bar(fill = "steelblue") + 
  labs(title = "Count of Tweet Types", x = "Tweet Type", y = "Count") +
  theme_minimal() # Cleaner look

# Correlation plot for numerical variables (if applicable)
numeric_cols <- sapply(data, is.numeric)
correlations <- cor(data[, numeric_cols]) 
corrplot::corrplot(correlations, method = "circle") 

# Data preprocessing

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$category, p=0.8, list=FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Preprocess the data: scaling and centering
preProc <- preProcess(trainData[, c("tweet_length"), drop = FALSE], method = c("center", "scale"))

# Apply the transformations to the training and testing data
trainData <- predict(preProc, trainData)
testData <- predict(preProc, testData)

# Build prediction model 

# Build Logistic Regression model
logisticModel <- glm(category ~ tweet_length, data=trainData, family=binomial())
summary(logisticModel)

# Decision Tree
treeModel <- rpart(category ~ tweet_length, data=trainData, method="class")
rpart.plot(treeModel)

# Random Forest
set.seed(123)
rfModel <- randomForest(category ~ tweet_length, data=trainData, ntree=100)
print(rfModel)

library(pROC)

# First, make predictions with the random forest model on the test data
rfPred <- predict(rfModel, testData, type = "response")

# Plot the ROC curve
plot(roc_obj, main="ROC Curve", col="#1c61b6", lwd=2)

# Calculate the AUC
roc_auc <- auc(roc_obj)
print(paste("AUC:", roc_auc))


# Plot the ROC curve
plot(roc_obj)

# Assuming 'logisticModel' is your fitted logistic regression model
# and 'testData' is your data frame with the test set.

# Generate predictions on the test set
logisticResults <- predict(logisticModel, newdata = testData, type = "response")

# Convert the probabilities to a binary outcome based on a threshold (e.g., 0.5)
logisticResults <- ifelse(logisticResults > 0.5, 1, 0)

# Assuming logisticResults has already been generated and contains prediction probabilities

# Convert the probabilities to binary outcome based on a threshold (e.g., 0.5)
logisticResults <- ifelse(logisticResults > 0.5, 1, 0)

# Ensure testData$category is a factor (if it's not already)
testData$category <- factor(testData$category, levels = unique(testData$category))

# Make sure logisticResults is a factor and uses the same levels as testData$category
logisticResults <- factor(logisticResults, levels = levels(testData$category))

# Now, both logisticResults and testData$category are factors with the same levels
confMat <- confusionMatrix(logisticResults, testData$category)

# Print the confusion matrix to check the results
print(confMat)

# Check the names of the dataframe
print(names(confMat_melted))

library(ggplot2)

# Assuming confMat is your confusion matrix
confMat <- matrix(c(142297, 0, 19137, 0), nrow = 2, byrow = TRUE)

# Create a data frame from the matrix to use in ggplot
confMat_df <- as.data.frame(as.table(confMat))

# Set names for readability
names(confMat_df) <- c("Reference", "Prediction", "Frequency")

# Plot heatmap
heatmap_plot <- ggplot(confMat_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual Class", y = "Predicted Class") +
  theme_minimal()

# Print the plot
print(heatmap_plot)



# Assuming 'category' is a factor with the appropriate levels
# and 'tweet_length' is a numeric variable.

# Build prediction model 

# Logistic Regression
logisticModel <- glm(category ~ tweet_length, data=trainData, family=binomial())
summary(logisticModel)

# Decision Tree
treeModel <- rpart(category ~ tweet_length, data=trainData, method="class")
rpart.plot(treeModel)

# Random Forest
set.seed(123)
rfModel <- randomForest(category ~ tweet_length, data=trainData, ntree=100)
print(rfModel)


# Print the custom table
print(table_custom)
# Load necessary libraries
library(randomForest)  # For Random Forest model
library(caret)         # For modeling and evaluation
library(pROC)          # For ROC curve and AUC

# Ensure 'category' is a factor for classification purposes
trainData$category <- as.factor(trainData$category)
testData$category <- as.factor(testData$category)

# Build the Random Forest model
set.seed(123)  # For reproducibility
rfModel <- randomForest(category ~ tweet_length, data=trainData, ntree=100)

# Make predictions with the Random Forest model on the test data
rfPred <- predict(rfModel, testData, type = "prob")[,2]  # Assuming binary classification: 1

# ROC curve and AUC calculation
roc_obj <- roc(testData$category, rfPred)
plot(roc_obj, main="ROC Curve", col="#1c61b6", lwd=2)
roc_auc <- auc(roc_obj)
print(paste("AUC:", roc_auc))

# Assuming logisticModel is your fitted logistic regression model
# Generate predictions on the test set
logisticResults <- predict(logisticModel, newdata=testData, type="response")

# Convert the probabilities to a binary outcome based on a threshold (e.g., 0.5)
logisticResults <- ifelse(logisticResults > 0.5, 1, 0)

# Ensure predictions and actual categories are factors with the same levels
logisticResults <- factor(logisticResults, levels=c(0,1))
testData$category <- factor(testData$category, levels=c(0,1))

# Compute the confusion matrix
confMat <- confusionMatrix(logisticResults, testData$category)
print(confMat)

# Convert the confusion matrix to a dataframe suitable for plotting
confMat_melted <- as.data.frame(as.table(confMat$table))

# Print column names to ensure correct reference
print(names(confMat_melted))

# Assuming the column names are indeed 'Var1' and 'Var2', here is the corrected plot code:
plot <- ggplot(confMat_melted, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = 'Predicted', y = 'Actual', fill = 'Frequency') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Display the plot
print(plot)
