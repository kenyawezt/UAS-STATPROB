#Nama: Kadek Wahyu Alpha Kusuma Putra/ NIM :2415091012/ Kelas: IKI

# Load necessary libraries
library(ggplot2)
library(car)

# Load dataset
data("mtcars")

# View the first few rows of the dataset (ensure the data is correct)
head(mtcars)

# Linear regression: mpg (fuel efficiency) as the response, wt (car weight) as the predictor
model <- lm(mpg ~ wt, data = mtcars)

# Display regression results
summary(model)

# Assumptions check

# 1. Linearity
linearity_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Linearity Check")

# 2. Homoscedasticity
homoscedasticity_plot <- ggplot(mtcars, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  ggtitle("Homoscedasticity Check") +
  labs(x = "Fitted Values", y = "Residuals")

# 3. Normality of Residuals
qq_plot <- qqPlot(model$residuals, main = "Normality of Residuals")

# Visualization of the regression results
scatter_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Scatter Plot with Regression Line") +
  labs(x = "Car Weight (wt)", y = "Miles per Gallon (mpg)")

# Display plots
print(linearity_plot)
print(homoscedasticity_plot)
print(scatter_plot)

# Interpretation
cat("Interpretation:\n")
cat("Regression equation: mpg =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "* wt\n")
cat("P-value of the slope:", summary(model)$coefficients[2, 4], "\n")
