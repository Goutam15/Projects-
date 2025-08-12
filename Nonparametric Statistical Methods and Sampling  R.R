# Load necessary library
library(dplyr)


# The Game Attendance Hypothesis Testing:

# Game attendance data
attendance_data <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573,
                     2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)

# a. State the hypotheses and identify the claim
# H0: Median attendance = 3000
# H1: Median attendance ≠ 3000 (two-sided test)

# b. Find the critical value(s)
alpha <- 0.05
critical_value <- qt(alpha/2, df=length(attendance_data)-1, lower.tail = FALSE)

# c. Compute the test value (Using a one-sample t-test for demonstration purposes, assuming data is normally distributed)
test_result <- t.test(attendance_data, mu = 3000, alternative = "two.sided")

# d. Make the decision
decision <- ifelse(test_result$p.value < alpha, "Reject H0", "Fail to reject H0")

# e. Summarize the results
cat("Test statistic:", test_result$statistic, "\nP-value:", test_result$p.value, 
    "\nDecision:", decision, "\nCritical Value:", critical_value, "\n")

# The Lottery Ticket Sales Hypothesis Testing:

# Lottery ticket sales data setup
number_of_days_less_than_200 <- 15
total_days_sampled <- 40
# Assuming the median is 200, so we test if the actual median is less (one-tailed test)

# a. State the hypotheses and identify the claim
# H0: Median tickets sold per day = 200
# H1: Median tickets sold per day < 200

# b. Perform a binomial test to compute the test value and make the decision
test_result <- binom.test(number_of_days_less_than_200, total_days_sampled, p = 0.5, alternative = "less")

# c. Summarize the results
cat("Number of successes (days with <200 tickets):", number_of_days_less_than_200, 
    "\nTotal days sampled:", total_days_sampled, 
    "\nP-value:", test_result$p.value, 
    "\nDecision: Based on the p-value, ", 
    ifelse(test_result$p.value < 0.05, "reject H0", "fail to reject H0"), 
    "- there is", ifelse(test_result$p.value < 0.05, "sufficient", "insufficient"), 
    "evidence to conclude that the median is below 200 tickets.")



# Lengths of Prison Sentences:

# Data for males and females
males <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Perform Wilcoxon rank sum test
test_result_sentences <- wilcox.test(males, females, alternative = "two.sided")

# Print the result
cat("Lengths of Prison Sentences Test:\n")
print(test_result_sentences)
if(test_result_sentences$p.value < 0.05) {
  cat("Reject the null hypothesis: There is sufficient evidence at alpha = 0.05 to conclude there is a difference in sentence length between genders.\n\n")
} else {
  cat("Fail to reject the null hypothesis: There is not sufficient evidence at alpha = 0.05 to conclude there is a difference in sentence length between genders.\n\n")
}


# Winning Baseball Games:

# Data for NL and AL wins
NL_wins <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL_wins <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Perform Wilcoxon rank sum test
test_result_baseball <- wilcox.test(NL_wins, AL_wins, alternative = "two.sided")

# Print the result
cat("Winning Baseball Games Test:\n")
print(test_result_baseball)
if(test_result_baseball$p.value < 0.05) {
  cat("Reject the null hypothesis: There is sufficient evidence at alpha = 0.05 to conclude there is a difference in the number of wins between the NL and AL Eastern Divisions.\n")
} else {
  cat("Fail to reject the null hypothesis: There is not sufficient evidence at alpha = 0.05 to conclude there is a difference in the number of wins between the NL and AL Eastern Divisions.\n")
}


# Mathematics literacy scores

western_hemisphere <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
eastern_asia <- c(523, 547, 547, 391, 549)

# Perform the Kruskal-Wallis test
test_result <- kruskal.test(list(Western_Hemisphere = western_hemisphere,
                                 Europe = europe,
                                 Eastern_Asia = eastern_asia))

# Print the test result
print(test_result)

# Decision making based on alpha = 0.05
alpha <- 0.05
if (test_result$p.value < alpha) {
  cat("At alpha = 0.05, there is sufficient evidence to reject the null hypothesis. This suggests there is a statistically significant difference in mathematics literacy scores among the groups.\n")
} else {
  cat("At alpha = 0.05, there is not sufficient evidence to reject the null hypothesis. This suggests there is no statistically significant difference in mathematics literacy scores among the groups.\n")
}

# Subway and Rail passengers data

subway <- c(845, 494, 425, 313, 108, 41)
rail <- c(39, 291, 142, 103, 33, 38)

# Perform Spearman rank correlation test
test_result <- cor.test(subway, rail, method = "spearman")

# Print the result
print(test_result)

# Decision based on alpha = 0.05
alpha <- 0.05
if (test_result$p.value < alpha) {
  cat("At alpha = 0.05, there is sufficient evidence to reject the null hypothesis, suggesting a significant association between subway and rail passengers.\n")
} else {
  cat("At alpha = 0.05, there is not sufficient evidence to reject the null hypothesis, suggesting no significant association between subway and rail passengers.\n")
}

# Suggest a reason for the transportation authority to use the results
cat("If a significant association is found, it suggests that patterns in subway usage may be related to commuter rail usage, which could inform infrastructure development, service planning, and integration strategies between these two modes of transportation.")


# Simulation for Prizes in Caramel Corn Boxes:

set.seed(2024)

# Function to simulate buying boxes for all five prizes
simulate_boxes <- function(trials) {
  counts <- numeric(trials)
  for (i in 1:trials) {
    prizes <- numeric(5)
    count <- 0
    while (sum(prizes > 0) < 5) {
      prize <- sample(1:5, 1)
      prizes[prize] <- prizes[prize] + 1
      count <- count + 1
    }
    counts[i] <- count
  }
  return(mean(counts))
}

# Average number of boxes to get all five prizes (500 times)
average_boxes <- simulate_boxes(500)
average_boxes

# Simulation for Lottery Winner:
 
set.seed(2024)

# Function to simulate buying lottery tickets to spell "gold"
simulate_lottery <- function(trials) {
  counts <- numeric(trials)
  for (i in 1:trials) {
    letters <- rep(0, 4) # g, o, l, d
    count <- 0
    while (sum(letters > 0) < 4) {
      ticket <- sample(c('g', 'o', 'l', 'd'), 1, prob = c(0.4, 0.3, 0.2, 0.1))
      if (ticket == 'g') letters[1] <- 1
      if (ticket == 'o') letters[2] <- 1
      if (ticket == 'l') letters[3] <- 1
      if (ticket == 'd') letters[4] <- 1
      count <- count + 1
    }
    counts[i] <- count
  }
  return(mean(counts))
}

# Average number of tickets to win the prize (1000 times)
average_tickets <- simulate_lottery(1000)
average_tickets
