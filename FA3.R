#2
P_A <- 0.3
P_B_given_A <- 0.75
P_notB_given_notA <- 0.95

# (a) 
P_B <- P_A * P_B_given_A + (1 - P_A) * (1 - P_notB_given_notA)

# (b)
P_A_given_B <- P_A * P_B_given_A / P_B

print(paste("Probability that a 1 was received:", round(P_B, 4)))
print(paste("Probability that a 1 was transmitted given that a 1 was received:", round(P_A_given_B, 4)))

#7
programming_percentages <- c(0.10, 0.30, 0.60)
error_rates <- c(0.08, 0.05, 0.01)


overall_error <- sum(programming_percentages * error_rates)

cat("The overall percentage of error is:", round(overall_error * 100, 2), "%\n")

most_likely_person <- which.min(error_rates)
cat("The most likely person to have written a program with an error is:", c("Jane", "Amy", "Ava")[most_likely_person], "\n")
