
# Load necessary libraries
library(sparklyr) # For connecting to Spark
library(dplyr) # For data manipulation
library(readr) # For reading CSV files
library(ggplot2) # For data visualization

# Initialize a Spark session with a compatible version
sc <- spark_connect(master = "local", version = "3.0.0")

# Load the datasets into R
file1 <- read_csv("Downloads/HPI_AT_BDL_ZIP3 (1).csv") # Load ZIP3 dataset
file2 <- read_csv("Downloads/HPI_AT_BDL_ZIP5 (1).csv") # Load ZIP5 dataset

# Display the first few rows of each dataset in R
head(file1)
head(file2)

# Load data into Spark
data1 <- spark_read_csv(sc, name = "data1", path = "Downloads/HPI_AT_BDL_ZIP3 (1).csv", header = TRUE, infer_schema = TRUE) # Load ZIP3 dataset into Spark
data2 <- spark_read_csv(sc, name = "data2", path = "Downloads/HPI_AT_BDL_ZIP5 (1).csv", header = TRUE, infer_schema = TRUE) # Load ZIP5 dataset into Spark

# Display the first few rows of each dataset in Spark
head(data1)
head(data2)

# Convert Spark DataFrames to R DataFrames for visualization
data1_df <- data1 %>% collect() # Convert Spark DataFrame to R DataFrame for ZIP3
data2_df <- data2 %>% collect() # Convert Spark DataFrame to R DataFrame for ZIP5

# Convert HPI column to numeric, handling non-numeric values
data1_df <- data1_df %>%
  mutate(HPI = as.numeric(HPI)) %>% # Convert HPI to numeric
  filter(!is.na(HPI)) # Remove rows with NA HPI

data2_df <- data2_df %>%
  mutate(HPI = as.numeric(HPI)) %>% # Convert HPI to numeric
  filter(!is.na(HPI)) # Remove rows with NA HPI

# Aggregate data1 by year and calculate mean HPI
data1_agg <- data1_df %>% group_by(Year) %>% summarise(mean_HPI = mean(HPI, na.rm = TRUE)) # Aggregate by Year and calculate mean HPI for ZIP3

# Aggregate data2 by year and calculate mean HPI
data2_agg <- data2_df %>% group_by(Year) %>% summarise(mean_HPI = mean(HPI, na.rm = TRUE)) # Aggregate by Year and calculate mean HPI for ZIP5

# Plotting aggregated time series for data1
ggplot(data1_agg, aes(x = Year, y = mean_HPI)) + 
  geom_line() +
  ggtitle("Mean HPI Over Time (ZIP3)") +
  xlab("Year") +
  ylab("Mean HPI")

# Plotting aggregated time series for data2
ggplot(data2_agg, aes(x = Year, y = mean_HPI)) + 
  geom_line() +
  ggtitle("Mean HPI Over Time (ZIP5)") +
  xlab("Year") +
  ylab("Mean HPI")

# Calculate correlation matrix for data1
correlation_matrix_data1 <- cor(data1_df %>% select_if(is.numeric), use = "complete.obs") # Calculate correlation matrix for ZIP3
print(correlation_matrix_data1)

# Calculate correlation matrix for data2
correlation_matrix_data2 <- cor(data2_df %>% select_if(is.numeric), use = "complete.obs") # Calculate correlation matrix for ZIP5
print(correlation_matrix_data2)

### Step 2: Predictive Modeling with Linear Regression

# Prepare the data for modeling
data1_prepared <- data1 %>%
  select(Year, HPI, Annual_Change_) %>%
  filter(!is.na(HPI) & !is.na(Annual_Change_)) # Select relevant columns and remove rows with NA values for ZIP3

data2_prepared <- data2 %>%
  select(Year, HPI, Annual_Change_) %>%
  filter(!is.na(HPI) & !is.na(Annual_Change_)) # Select relevant columns and remove rows with NA values for ZIP5

# Train a linear regression model for data1
model1 <- data1_prepared %>%
  ml_linear_regression(response = "HPI", features = c("Year", "Annual_Change_")) # Train linear regression model for ZIP3

# Print model summary for data1
print("Linear Regression Model Summary for Data1 (ZIP3):")
summary(model1)

# Train a linear regression model for data2
model2 <- data2_prepared %>%
  ml_linear_regression(response = "HPI", features = c("Year", "Annual_Change_")) # Train linear regression model for ZIP5

# Print model summary for data2
print("Linear Regression Model Summary for Data2 (ZIP5):")
summary(model2)