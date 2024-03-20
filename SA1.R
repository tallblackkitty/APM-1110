defective_prob <- function(x1, x2, x3, y1, y2, y3) {
  if (is.na(x1) || is.na(x2) || is.na(x3) || is.na(y1) || is.na(y2) || is.na(y3)) {
    print("Please enter numeric values!")
    return(NULL)
  }
  if (!(0.1 <= x1 && x1 <= 0.4 && 0.1 <= x2 && x2 <= 0.4 && 0.1 <= x3 && x3 <= 0.4)) {
    print("Please input values between 0.1 to 0.4!")
    return(NULL)
  }
  if (!(0.01 <= y1 && y1 <= 0.05 && 0.01 <= y2 && y2 <= 0.05 && 0.01 <= y3 && y3 <= 0.05)) {
    print("Please input values between 0.01 to 0.05!")
    return(NULL)
  }
  total_defective_products <- x1 * y1 + x2 * y2 + x3 * y3
  total_products <- x1 + x2 + x3
  prob_defective_prod <- total_defective_products / total_products
  return(prob_defective_prod)
}
x1 <- 0.3
x2 <- 0.2
x3 <- 0.1
y1 <- 0.03
y2 <- 0.04
y3 <- 0.02
#x1 <- as.numeric(readline(prompt = "Enter x1 (0.1 to 0.4): "))
#x2 <- as.numeric(readline(prompt = "Enter x2 (0.1 to 0.4): "))
#x3 <- as.numeric(readline(prompt = "Enter x3 (0.1 to 0.4): "))
#y1 <- as.numeric(readline(prompt = "Enter y1 (0.01 to 0.05): "))
#y2 <- as.numeric(readline(prompt = "Enter y2 (0.01 to 0.05): "))
#y3 <- as.numeric(readline(prompt = "Enter y3 (0.01 to 0.05): "))
prob <- defective_prob(x1, x2, x3, y1, y2, y3)
if (!is.null(prob)) {
  print(sprintf("The probability of a randomly selected product being defective is: %.2f%%", prob * 100))
}
#2

library(shiny)

ui_univariate <- fluidPage(
  titlePanel("Univariate Discrete Random Variable Calculator"),
  sidebarLayout(
    sidebarPanel(
      textInput("values", "Enter values (separated by commas):", ""),
      textInput("probs", "Enter probabilities (separated by commas):", ""),
      actionButton("calculate", "Calculate"),
      verbatimTextOutput("output_mean"),
      verbatimTextOutput("output_variance")
    ),
    mainPanel(
      plotOutput("pdf_plot"),
      plotOutput("cdf_plot")
    )
  )
)

server_univariate <- function(input, output) {
  observeEvent(input$calculate, {
    values <- unlist(strsplit(input$values, ","))
    probs <- unlist(strsplit(input$probs, ","))

    values <- as.numeric(values)
    probs <- as.numeric(probs)

    if (any(probs < 0 | probs > 1) || sum(probs) != 1) {
      output$output_mean <- renderPrint("Invalid probabilities. Probabilities must be in [0, 1] and sum to 1.")
      output$output_variance <- renderPrint("")
      return()
    }

    mean_val <- sum(values * probs)
    variance_val <- sum((values - mean_val)^2 * probs)
    
    pdf_data <- data.frame(x = values, y = probs)
    output$pdf_plot <- renderPlot({
      plot(pdf_data$x, pdf_data$y, type = "h", lwd = 10, col = "blue",
           main = "Probability Mass Function (PMF)", xlab = "Values", ylab = "Probability")
    })
    cdf_data <- data.frame(x = values, y = cumsum(probs))
    output$cdf_plot <- renderPlot({
      plot(cdf_data$x, cdf_data$y, type = "s", lwd = 2, col = "red",
           main = "Cumulative Distribution Function (CDF)", xlab = "Values", ylab = "Cumulative Probability")
    })
    output$output_mean <- renderPrint(paste("Mean:", mean_val))
    output$output_variance <- renderPrint(paste("Variance:", variance_val))
  })
}
ui_bivariate <- fluidPage(
  titlePanel("Bivariate Discrete Random Variable Calculator"),
  sidebarLayout(
    sidebarPanel(
      textInput("values_x", "Enter x values (separated by commas):", ""),
      textInput("values_y", "Enter y values (separated by commas):", ""),
      textInput("probs", "Enter joint probabilities (row-wise, separated by commas):", ""),
      actionButton("calculate", "Calculate"),
      verbatimTextOutput("output_marginal"),
      verbatimTextOutput("output_conditional")
    ),
    mainPanel(
      plotOutput("marginal_pdf_plot"),
      plotOutput("conditional_pdf_plot")
    )
  )
)

server_bivariate <- function(input, output) {
  observeEvent(input$calculate, {
    values_x <- unlist(strsplit(input$values_x, ","))
    values_y <- unlist(strsplit(input$values_y, ","))
    probs <- unlist(strsplit(input$probs, ","))

    values_x <- as.numeric(values_x)
    values_y <- as.numeric(values_y)
    probs <- as.numeric(probs)

    if (any(probs < 0 | probs > 1) || sum(probs) != 1) {
      output$output_marginal <- renderPrint("Invalid probabilities. Probabilities must be in [0, 1] and sum to 1.")
      output$output_conditional <- renderPrint("")
      return()
    }

    marginal_x <- rowSums(matrix(probs, nrow = length(values_x), byrow = TRUE))
    marginal_y <- colSums(matrix(probs, nrow = length(values_x), byrow = TRUE))

    marginal_pdf_data <- data.frame(x = values_x, y = marginal_x)
    output$marginal_pdf_plot <- renderPlot({
      barplot(marginal_pdf_data$y, names.arg = marginal_pdf_data$x,
              main = "Marginal Probability Mass Function (PMF) for X",
              xlab = "Values", ylab = "Probability", col = "blue")
    })

    conditional_y_given_x <- matrix(probs, nrow = length(values_x), byrow = TRUE) / marginal_x
    conditional_x_given_y <- t(t(matrix(probs, nrow = length(values_x), byrow = TRUE)) / marginal_y)

    conditional_pdf_data <- data.frame(x = rep(values_x, each = length(values_y)), 
                                       y = rep(values_y, length(values_x)),
                                       z = c(conditional_y_given_x))
    output$conditional_pdf_plot <- renderPlot({
      contourplot(z ~ x * y, data = conditional_pdf_data,
                  main = "Conditional Probability Mass Function (PMF) for Y given X",
                  xlab = "X Values", ylab = "Y Values", col = rainbow(20))
    })
    output$output_marginal <- renderPrint({
      print("Marginal Distributions:")
      print(paste("X:", marginal_x))
      print(paste("Y:", marginal_y))
    })
    output$output_conditional <- renderPrint({
      print("Conditional Distributions:")
      print("P(Y|X):")
      print(conditional_y_given_x)
      print("P(X|Y):")
      print(conditional_x_given_y)
    })
  })
}
shinyApp(ui = ui_univariate, server = server_univariate)
shinyApp(ui = ui_bivariate, server = server_bivariate)

#3
p <- 0.6
searches <- rgeom(10000, p)
hist(searches, breaks = max(searches), freq = FALSE)
mean_searches <- mean(searches)
var_searches <- var(searches)
cat("Mean:", mean_searches, "\n")
cat("Variance:", var_searches, "\n")

searches_conditional <- searches[searches >= 3]
hist(searches_conditional, breaks = max(searches_conditional), freq = FALSE)
mean_conditional <- mean(searches_conditional)
var_conditional <- var(searches_conditional)
cat("\nMean (conditional):", mean_conditional, "\n")
cat("Variance (conditional):", var_conditional, "\n")

prob_4_given_gt_3 <- sum(searches_conditional == 4) / sum(searches_conditional > 3)
prob_1 <- sum(searches == 1) / length(searches)
cat("\nP(X=4|X>3):", prob_4_given_gt_3, "\n")
cat("P(X=1):", prob_1, "\n")

prob_5_given_gt_3 <- sum(searches_conditional == 5) / sum(searches_conditional > 3)
prob_2 <- sum(searches == 2) / length(searches)
cat("P(X=5|X>3):", prob_5_given_gt_3, "\n")
cat("P(X=2):", prob_2, "\n")




