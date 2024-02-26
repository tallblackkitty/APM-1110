#I.
#probability of success
p <- 0.2

x <- rgeom(1000, prob = p)

mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)

cat("Number of trials required to achieve first success:\n")
cat("Mean (in 2 decimal places): ", formatC(mean_x, digits = 2, format = "f"), "\n", sep = "")
cat("Variance (in 2 decimal places): ", formatC(var_x, digits = 2, format = "f"), "\n", sep = "")
cat("Standard deviation (in 2 decimal places): ", formatC(sd_x, digits = 2, format = "f"), "\n", sep = "")

hist(x, breaks = 20, main = "Histogram of Geometric Distribution", xlab = "Number of Trials", ylab = "Frequency")

#II.
n1 <- 10 
p <- 0.10

prob_more_than_10_percent_1 <- sum(dbinom(11:n1, size = n1, prob = p))
prob_more_than_10_percent_1

n2 <- 10
p <- 0.10

prob_more_than_10_percent_2 <- sum(dbinom(11:n2, size = n2, prob = p))
prob_more_than_10_percent_2
