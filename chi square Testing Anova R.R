# Scenario 1: Blood Type Distribution
# Observed frequencies for blood types
observed1 <- c(12, 8, 24, 6) # [A, B, O, AB]

# Expected proportions for blood types
expected_proportions1 <- c(0.20, 0.28, 0.36, 0.16) # [A, B, O, AB]

# Expected frequencies
expected1 <- 50 * expected_proportions1 # Total sample size = 50

# Chi-square test
chi_squared1 <- sum((observed1 - expected1)^2 / expected1)

# Critical value at alpha = 0.10, df = 3
critical_value1 <- qchisq(0.90, df = 3) # 1 - alpha = 0.90

# Output the test statistic and critical value
cat("Scenario 1: Chi-Squared Test Statistic:", chi_squared1, "\n")
cat("Scenario 1: Critical Value:", critical_value1, "\n\n")

# Scenario 2: On-Time Performance by Airlines
# Observed frequencies
observed2 <- c(125, 10, 200 - 125 - 40 - 10, 40) # [On time, NAS delay, Aircraft arriving late, Other]

# Expected proportions
expected_proportions2 <- c(0.708, 0.082, 0.09, 0.12) # [On time, NAS delay, Aircraft arriving late, Other]

# Expected frequencies
expected2 <- 200 * expected_proportions2 # Total sample size = 200

# Chi-square test
chi_squared2 <- sum((observed2 - expected2)^2 / expected2)

# Critical value at alpha = 0.05, df = 3
critical_value2 <- qchisq(0.95, df = 3) # 1 - alpha = 0.95

# Output the test statistic and critical value
cat("Scenario 2: Chi-Squared Test Statistic:", chi_squared2, "\n")
cat("Scenario 2: Critical Value:", critical_value2, "\n")

# Scenario 1: Ethnicity and Movie Admissions
data1 <- matrix(c(724, 335, 174, 107, 370, 292, 152, 140), nrow = 2, byrow = TRUE)
colnames(data1) <- c("Caucasian", "Hispanic", "African American", "Other")
rownames(data1) <- c("2013", "2014")

# Chi-square test for independence
test1 <- chisq.test(data1)

# Output the test statistic and p-value
print(test1)


# Scenario 2: Women in the Military
data2 <- matrix(c(10791, 7816, 932, 11819, 62491, 42750, 9525, 54344), nrow = 4, byrow = TRUE)
colnames(data2) <- c("Officers", "Enlisted")
rownames(data2) <- c("Army", "Navy", "Marine Corps", "Air Force")

# Chi-square test for independence
test2 <- chisq.test(data2)

# Output the test statistic and p-value
print(test2)

# Scenario 3: Sodium Contents of Foods
condiments <- c(270, 130, 230, 180, 80, 70, 200) 
cereals <- c(260, 220, 290, 290, 200, 320, 140)
desserts <- c(100, 180, 250, 250, 300, 360, 300, 160) 

# Combine all data into one vector
sodium_content <- c(condiments, cereals, desserts)

# Create a group vector
group <- factor(rep(c("Condiments", "Cereals", "Desserts"), times = c(length(condiments), length(cereals), length(desserts))))

# ANOVA test
test3 <- aov(sodium_content ~ group)

# Summary of the ANOVA test
summary(test3)

# Data for Sales of Leading Companies
cereal <- c(578, 320, 264, 249, 237)
chocolate <- c(311, 106, 109, 125, 173)
candy <- c(261, 185, 302)
coffee <- c(689)

# Combine all data into one vector
sales <- c(cereal, chocolate, candy, coffee)

# Create a group vector
group <- factor(rep(c("Cereal", "Chocolate", "Candy", "Coffee"), 
                    times = c(length(cereal), length(chocolate), length(candy), length(coffee))))

# One-Way ANOVA test
anova_result <- aov(sales ~ group)

# Summary of the ANOVA test
summary(anova_result)

# Post-hoc analysis (Tukey test)
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.01) {
  TukeyHSD(anova_result)
} else {
  cat("No significant difference found; post-hoc analysis not required.\n")
}

# Data for Per-Pupil Expenditures
eastern_third <- c(4946, 5953, 6202, 7243)
middle_third <- c(6149, 7451, 6000, 6479)
western_third <- c(5282, 8605, 6528, 6911)

# Combine all data into one vector
expenditures <- c(eastern_third, middle_third, western_third)

# Create a group vector
group <- factor(rep(c("Eastern", "Middle", "Western"), 
                    times = c(length(eastern_third), length(middle_third), length(western_third))))

# One-Way ANOVA test
anova_result <- aov(expenditures ~ group)

# Summary of the ANOVA test
summary(anova_result)

# Post-hoc analysis (Tukey test)
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(anova_result)
} else {
  cat("No significant difference found; post-hoc analysis not required.\n")
}

# Data for Increasing Plant Growth
# Grow-light 1
growth_light1_foodA <- c(9.2, 9.4, 8.9)
growth_light1_foodB <- c(7.1, 7.2, 8.5)

# Grow-light 2
growth_light2_foodA <- c(8.5, 9.2, 8.9)
growth_light2_foodB <- c(5.5, 5.8, 7.6)

# Combine data into a data frame
growth_data <- data.frame(
  growth = c(growth_light1_foodA, growth_light1_foodB, growth_light2_foodA, growth_light2_foodB),
  light = factor(rep(c("Light1", "Light2"), each = 6)),
  food = factor(rep(c("FoodA", "FoodA", "FoodB", "FoodB"), times = 3))
)

# Two-Way ANOVA
anova_result <- aov(growth ~ light + food + light:food, data = growth_data)

# Summary of the ANOVA test
summary(anova_result)

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# ---------------------------
# Baseball Data Analysis
# ---------------------------

# 1. Import baseball.csv
baseball <- read.csv("~/Downloads/baseball.csv")

# 2. Perform EDA on baseball data
# Descriptive statistics
summary(baseball)

library(ggplot2)
# Visualization (Plotting Wins by Year as an example)
ggplot(baseball, aes(x = Year, y = W)) + 
  geom_line() +
  labs(title = "Wins by Year in Baseball Data", x = "Year", y = "Wins")

# Extract decade from year
baseball$Decade <- baseball$Year - (baseball$Year %% 10)



library(ggplot2)
# Create a wins table by summing the wins by decade
wins <- baseball %>%
  group_by(Decade) %>%
  summarize(wins = sum(W)) %>%
  as_tibble()

# 3. Chi-Square Goodness-of-Fit test for wins by decade
# a. State the hypotheses
# Null Hypothesis: The number of wins is uniformly distributed across decades.
# Alternative Hypothesis: The number of wins is not uniformly distributed across decades.

# b. Find the critical value (alpha = 0.05)
# Using Chi-Square distribution table or R function
df <- nrow(wins) - 1 # degrees of freedom
critical_value <- qchisq(0.95, df)

# c. Compute the test value
observed <- wins$wins
expected <- rep(mean(wins$wins), length(observed))
chi_sq_test_value <- sum((observed - expected)^2 / expected)

# d. Make the decision
decision <- ifelse(chi_sq_test_value > critical_value, "Reject", "Do not reject")

# e. Compare critical value with test value and p-value with significance level
p_value <- pchisq(chi_sq_test_value, df, lower.tail = FALSE)
same_result <- (p_value < 0.05) == (chi_sq_test_value > critical_value)

# ---------------------------
# Crop Data Analysis
# ---------------------------

# 4. Import crop_data.csv
crop_data <- read.csv("~/Downloads/crop_data.csv")

# 5. Two-way ANOVA on crop data
# Convert variables to factors
crop_data$density <- as.factor(crop_data$density)
crop_data$fertilizer <- as.factor(crop_data$fertilizer)
crop_data$block <- as.factor(crop_data$block)

# Two-way ANOVA
anova_result <- aov(yield ~ density * fertilizer, data = crop_data)

# Summary of the ANOVA test
anova_summary <- summary(anova_result)

# Explanation of results
# Null Hypothesis for density: No difference in yield due to density
# Null Hypothesis for fertilizer: No difference in yield due to fertilizer
# Null Hypothesis for interaction: No interaction effect between density and fertilizer on yield
# Alternative Hypotheses oppose the null hypotheses



library(ggplot2)
# Output the results
list(Baseball_EDA_Summary = summary(baseball),
     Baseball_Plot = ggplot(baseball, aes(x = Year, y = W)) + geom_line() + labs(title = "Wins by Year", x = "Year", y = "Wins"),
     Chi_Square_Test_Value = chi_sq_test_value,
     Critical_Value = critical_value,
     Decision = decision,
     Same_Result_Comparison = same_result,
     Crop_ANOVA_Summary = anova_summary
)

