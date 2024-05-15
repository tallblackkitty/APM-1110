library(MASS)
library(ggplot2)

# Load the CSV file
bitcoin_df <- read.csv("D:/FEU/2ND YR 2ND SEM/PROBABILITY/btc1.csv")

# Clean the Price column by removing commas and convert it to numeric
bitcoin_df$Price <- as.numeric(gsub(",", "", bitcoin_df$Price))

# Calculate returns using the cleaned Price column
returns <- diff(log(bitcoin_df$Price))
bitcoin_df$Return <- c(NA, returns)  # Pad with NA to match the length of the original data frame

# Remove the first row with NA in Return column for fitting the distribution
bitcoin_df <- bitcoin_df[-1, ]

# Fit various distribution functions
bitcoin_fit_norm <- fitdistr(bitcoin_df$Return, "normal")
bitcoin_fit_t <- fitdistr(bitcoin_df$Return, "t", start = list(m = mean(bitcoin_df$Return), s = sd(bitcoin_df$Return), df = 10))

# Plot histograms and fitted distributions
hist(bitcoin_df$Return, freq = FALSE, main = "Bitcoin Returns Distribution", xlab = "Returns", ylab = "Density", ylim = c(0, max(density(bitcoin_df$Return)$y)*1.5))
curve(dnorm(x, mean = bitcoin_fit_norm$estimate[1], sd = bitcoin_fit_norm$estimate[2]), 
      col = "blue", lwd = 2, add = TRUE)
curve(bitcoin_fit_t$estimate[2] * dt((x - bitcoin_fit_t$estimate[1]) / bitcoin_fit_t$estimate[2], df = bitcoin_fit_t$estimate[3]), 
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Normal", "Student's t"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

# Using the Kolmogorov-Smirnov test to assess goodness of fit
ks_norm <- ks.test(bitcoin_df$Return, "pnorm", bitcoin_fit_norm$estimate[1], bitcoin_fit_norm$estimate[2])
ks_t <- ks.test(bitcoin_df$Return, "pt", df = bitcoin_fit_t$estimate[3])

ks_results <- data.frame(Distribution = c("Normal", "Student's t"),
                         p_value = c(ks_norm$p.value, ks_t$p.value))
print(ks_results)

# Plot the ECDF and fitted CDFs for visual comparison
ggplot(bitcoin_df, aes(x = Return)) +
  stat_ecdf(geom = "step", color = "black") +
  stat_function(fun = pnorm, args = list(mean = bitcoin_fit_norm$estimate[1], sd = bitcoin_fit_norm$estimate[2]), 
                col = "blue", size = 1) +
  stat_function(fun = function(x) pt((x - bitcoin_fit_t$estimate[1]) / bitcoin_fit_t$estimate[2], df = bitcoin_fit_t$estimate[3]),
                col = "red", size = 1) +
  labs(title = "ECDF and Fitted CDFs of Bitcoin Returns", x = "Returns", y = "ECDF/CDF") +
  theme_minimal() +
  scale_color_manual(values = c("black", "blue", "red"), 
                     labels = c("Empirical", "Normal", "Student's t")) +
  theme(legend.position = "top")
