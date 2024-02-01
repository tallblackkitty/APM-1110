results <- read.csv("C:/Users/edtech3/Downloads/random1.csv")


results

mean1 <- mean(results$arch1)
mean1
mean2 <- mean(results$arch2)
mean2
mean3 <- mean(results$prog1)
mean3
mean4 <- mean(results$prog2)
mean4

median1 <-median(results$arch1)
median1
median2 <-median(results$arch2)
median2
median3 <-median(results$prog1)
median3
median4 <-median(results$prog2)
median4


sd1 <- sd(results$arch1)
sd1
sd2 <- sd(results$arch2)
sd2
sd3 <- sd(results$prog1)
sd3
sd4 <- sd(results$prog1) 
sd4

library(e1071) 
skew1 <- skewness(results$arch1)
skew1
skew2 <- skewness(results$arch2)
skew2
skew3 <- skewness(results$prog1)
skew3
skew4 <- skewness(results$prog2)
skew4



Females <-  c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43, 44, 75, 78, 80, 81, 83, 83, 85)
Males <- c (48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56,
            42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)

stem(Females)
stem(Males)


data_summary <-list(Female = Females, Male = Males)

boxplot(data_summary, col = c("pink", "lightblue"), main = "Boxplot by Gender", xlab = "Gender", ylab="Final Exam Scores", names = c("Female", "Male"))