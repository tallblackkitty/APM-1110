#5
percent_images_supplied <- c(15, 20, 25, 40)
percent_relevant_images <- c(50, 60, 80, 85)

weighted_sum_relevant_images <- sum(percent_images_supplied * percent_relevant_images)

overall_percent_relevant_images <- weighted_sum_relevant_images / sum(percent_images_supplied)

cat("Overall percentage of relevant images:", round(overall_percent_relevant_images, 2), "%\n")

#6
#Make a function to toss a fair coin first then answer the question
coin_toss <- function() {
  outcomes <- c("H", "T")
  return(sample(outcomes, size = 1))
}

experiment <- function(n) {
  outcomes <- replicate(n, paste(coin_toss(), coin_toss(), sep = ""))
  event_1 <- length(grep("HH|TT", outcomes))
  event_2 <- length(grep("HH|HT", outcomes))
  event_3 <- length(grep("TH|HH", outcomes))
  intersection_12 <- length(grep("HH", outcomes))
  intersection_13 <- length(grep("HH", outcomes))
  intersection_23 <- length(grep("HH", outcomes))
  intersection_123 <- length(grep("HHH", outcomes))
  probabilities <- c(event_1 / n, event_2 / n, event_3 / n, intersection_12 / n, intersection_13 / n, intersection_23 / n, intersection_123 / n)
  return(probabilities)
}

set.seed(123)
num_trials <- 100000
probabilities <- experiment(num_trials)

cat("Probability of E1:", probabilities[1], "\n")
cat("Probability of E2:", probabilities[2], "\n")
cat("Probability of E3:", probabilities[3], "\n")
cat("Probability of E1 ∩ E2:", probabilities[4], "\n")
cat("Probability of E1 ∩ E3:", probabilities[5], "\n")
cat("Probability of E2 ∩ E3:", probabilities[6], "\n")
cat("Probability of E1 ∩ E2 ∩ E3:", probabilities[7], "\n")

#We have shown that E1 and E2 & E1 and E3 aren't pairwise independent. However, the probability of E2 and E3 are equal, so they are pairwise independent.
#We have also shown that P(E1∩E2∩E3) and P(E1) * P(E2) * P(E3) aren't equal so they are not mutually independent.
