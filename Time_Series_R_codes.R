# Timeseries Codes

# Loading data from Excel, setting time
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)

# Load the data file
df <- read_excel('/Users/JU/OneDrive/桌面/Regression/project data/New_Employ.xlsx')
head(df)


# Check null variables
na_values <- is.na(df)
print(na_values)
na_counts <- colSums(na_values)
print(na_counts)

# Make sure no duplicates
duplicates <- duplicated(df)
num_duplicates <- sum(duplicates)
print(paste("Number of duplicates:", num_duplicates))


# 1. Preview of data set

# Print structure information of the data frame
str(df)
# Print summary information including data types and number of non-NA values
summary(df)
# For more detailed information about the entire object, including memory usage
object.size(df)
print(object.size(df), units = "auto")


# 2.1 Distribution of data
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Select only numerical columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]

# Create a list to store plots
plot_list <- list()

# Generate a plot for each numerical column
for (col_name in names(df_numeric)) {
  # Using tidy evaluation with the aes() function
  p <- ggplot(df, aes(x = .data[[col_name]])) + 
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.4) +
    geom_density(color = "red", size = 1.5) +
    labs(x = col_name, y = "Density") +
    theme_minimal() +
    ggtitle(paste("Distribution of", col_name)) +
    annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f\nSD: %.2f\nMedian: %.2f\nMin: %.2f\nMax: %.2f", 
                                                       mean(df[[col_name]], na.rm = TRUE), 
                                                       sd(df[[col_name]], na.rm = TRUE), 
                                                       median(df[[col_name]], na.rm = TRUE), 
                                                       min(df[[col_name]], na.rm = TRUE), 
                                                       max(df[[col_name]], na.rm = TRUE)), 
             hjust = 1.1, vjust = 1.1, size = 3.5)
  
  plot_list[[col_name]] <- p
}

# Arrange plots into a grid
grid_plots <- do.call(grid.arrange, c(plot_list, ncol = 3))



# 2.2 Time series plot
library(ggplot2)
library(gridExtra)

# Convert the first column to Date, Adjust format as needed
df$Time <- as.Date(df$Timestamp, format = "%Y-%m-%d")

# Creating the response variable y with time index
y <- df[c("Timestamp", "Monthly Unemployment Rate")]
# Creating the feature matrix X with time index
X <- df[c("Timestamp", "S&P500", "Federal Funds Effective Rate", "Average Egg Price", "Global Price of Aluminium", "Global Price of Olive Oil")]

# further setting name of each
y <- data.frame(Time = y$Timestamp, Monthly_Unemployment = y$`Monthly Unemployment Rate`)
X <- data.frame(Time = X$Timestamp, SP = X$`S&P500`, Federal = X$`Federal Funds Effective Rate`, Egg = X$`Average Egg Price`, Aluminium = X$`Global Price of Aluminium`, Olive = X$`Global Price of Olive Oil`)
print(head(y))
print(head(X))

plots <- list() # create an empty list for plots

p <- ggplot(y, aes(x = Time, y = Monthly_Unemployment)) +
  geom_line() + # Draw the line
  geom_point() + # Add points
  labs(title = "Time Series of Monthly Unemployment Rate", 
       x = "Time", 
       y = "Unemployment") +
  theme_minimal()

plots[["Monthly_Unemployment"]] <- p

# Get all column names except 'Time'
column_names <- names(X)[-which(names(X) == "Time")]

# Loop through each column name and create a plot, storing it in the list
for (col_name in column_names) {
  p <- ggplot(X, aes_string(x = "Time", y = col_name)) +
    geom_line() +  # Add line
    ggtitle(paste("Time Series of", col_name)) +  # Add title
    xlab("Time") +  # Label x-axis
    ylab(col_name) +  # Label y-axis
    theme_minimal()  # Use minimal theme
  
  plots[[col_name]] <- p  # Add the plot to the list
}

do.call(grid.arrange, c(plots, ncol = 2)) 

# Summarize what we can learn form this session
# Use grid.arrange to displace all plots, need to import gridExtra
# [[ ]] for setting/calling name of column
# need to use data.frame to set name for each column




# Boxplot for outliers 

plots <- list()
x_col_names <- names(X)[-which(names(X) == "Time")]

for (col_name in x_col_names) {
  p <- ggplot(X, aes_string(x = factor(0), y = col_name)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col_name)) +
    xlab("") +  # No x-axis label needed
    ylab(col_name) +
    theme_minimal()
  plots[[col_name]] <- p
}

y_col_names <- names(y)[-which(names(y) == "Time")]

for (col_name in y_col_names) {
  p <- ggplot(y, aes_string(x = factor(0), y = col_name)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col_name)) +
    xlab("") +  # No x-axis label needed
    ylab(col_name) +
    theme_minimal()
  plots[[col_name]] <- p
}

plot_layout <- do.call(grid.arrange, c(plots, ncol = 2))  # Adjust ncol to best fit your display

# Summarize what we can learn form this session
# use geom_boxplot for boxplot




# Scatter plot
data_merged <- merge(X, y, by = "Time")
# Get the names of the columns from X to plot against Monthly_Unemployment
x_col_names <- names(X)[-which(names(X) == "Time")]

# Initialize a list to store plots
plots <- list()

for (col_name in x_col_names) {
  p <- ggplot(data_merged, aes_string(x = col_name, y = "Monthly_Unemployment")) +
    geom_point(alpha = 0.4) +  # Use points with some transparency
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line without CI
    ggtitle(paste("Scatter Plot of", col_name, "vs. Monthly Unemployment")) +
    xlab(col_name) + 
    ylab("Monthly Unemployment Rate") +
    theme_minimal()
  plots[[col_name]] <- p
}

# Combine all plots using patchwork
plot_layout <- do.call(grid.arrange, c(plots, ncol = 2))



# Correlation matrix
library(ggcorrplot)

# Merge the data frames by 'Time'
data_merged <- merge(X, y, by = "Time")

# Another way to remove the 'Time' column for correlation analysis
data_numeric <- data_merged[, sapply(data_merged, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(data_numeric, use = "complete.obs")  # 'complete.obs' handles missing data by excluding

# Print the correlation matrix
print(cor_matrix)



# VIF
install.packages("car")
library(car)
vif_values <- vif(lm(Monthly_Unemployment ~ ., data = data_numeric))
print(vif_values)

# What we learn
# if a package is not founded, should: install.packages("name of package")



# Cook's distance
# Fit a linear regression model
model <- lm(Monthly_Unemployment ~ ., data = data_numeric)

# Calculate Cook's distance
cooks_d <- cooks.distance(model)

# Plot Cook's distance
plot(cooks_d, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Index", ylab = "Cook's Distance")
abline(h = 4/length(cooks_d), col = "red", lty = 2)

# What we learn 
# Create a model of regression result using lm
# Use plot() to plot graphs
# use abline to add a straight line to existing plot



# Outlier removal & Data Standardization

# Outlier Removal
n <- nrow(data_numeric)
k <- length(coef(model))
threshold <- 4 / (n - k - 1)

outliers <- which(cooks_d > threshold)
data_cleaned <- data_numeric[-outliers, ]
head(data_cleaned)

# Data Standardization
data_standardized <- scale(data_cleaned)

# Convert back to a data frame, if necessary
data_standardized <- as.data.frame(data_standardized)

# View the summary to check the transformation
head(data_standardized)

# Split Data for model building - back testing
y <- data.frame(Monthly_Unemployment = data_standardized$Monthly_Unemployment)
head(y)
X <- data.frame(SP = data_standardized$SP, Federal = data_standardized$Federal, Egg = data_standardized$Egg, Aluminium=data_standardized$Aluminium)
head(X)

model <- lm(Monthly_Unemployment ~ ., data = data_standardized)



# Residuals VS Fitted Values
# Extract fitted values and residuals
fitted_values <- fitted(model)
residuals <- residuals(model)

# Plot residuals vs fitted values
plot(fitted_values, residuals, main = "Residuals vs Fitted Values Plot",
     xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0 for reference

# What we learn
# can directly get fitted values and residuals with fitted() and residuals() function


# Residual VS Predictors
par(mfrow = c(2, 3))  # Set up a 2x3 grid for the plots
for (i in 2:ncol(X)) {  # Start from the second column (Time is not an independent variable)
  plot(X[, i], residuals, main = paste("Residuals vs", colnames(X)[i]),
       xlab = colnames(X)[i], ylab = "Residuals", pch = 20)
}


# Observed VS Fitted
observed <- data_standardized$Monthly_Unemployment
fitted_values <- fitted(model)

# Plot observed vs fitted values
plot(fitted_values, observed, main = "Observed vs Fitted Values Plot",
     xlab = "Fitted Values", ylab = "Observed Values", pch = 20)
abline(0, 1, col = "red")  # Add a diagonal reference line


# Test for Homoscedasticity (Breusch-Pagan Test)
library(lmtest)
bp_test <- bptest(model)
print(bp_test)

# Test for Autocorrelation (Durbin-waston test)
# lmtest library
dw_test <- dwtest(model)
print(dw_test)


# Test for Normality(Shapiro-Wilk Test)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# Test for Normality(Q-Q plot)
qqnorm(residuals)
qqline(residuals)





# Create training and testing datasets



y_train <- data.frame(Monthly_Unemployment = data_standardized$Monthly_Unemployment)
head(y_train)
X_train <- data.frame(SP = data_standardized$SP, Federal = data_standardized$Federal, Egg = data_standardized$Egg, Aluminium=data_standardized$Aluminium)
head(X_train)



# Model Selection Part:
# Need to include: BP test p-value, DW test statistic, JB test p-value, RMSE, MAE, R square, adjusted R square

# OLS

library(lmtest)
library(Metrics)
library(tseries)

model <- lm(Monthly_Unemployment ~ ., data = cbind(y_train, X_train))

# Breusch-Pagan test for heteroscedasticity
bp_test <- bptest(model)

# Durbin-Watson test for autocorrelation
dw_test <- dwtest(model)

# Jarque-Bera test for normality of residuals
jb_test <- jarque.bera.test(residuals(model))
     
# Calculate RMSE and MAE
predicted <- predict(model, newdata = X_train)  # Assuming X_train includes all predictor variables
rmse_value <- rmse(y_train$Monthly_Unemployment, predicted)
mae_value <- mae(y_train$Monthly_Unemployment, predicted)

# Get R squared and Adjusted R squared from the model summary
summary_stats <- summary(model)
r_squared <- summary_stats$r.squared
adj_r_squared <- summary_stats$adj.r.squared

# Print results
cat("BP Test p-value:", bp_test$p.value, "\n")
cat("DW Test statistic:", dw_test$statistic, "\n")
cat("JB Test p-value:", jb_test$p.value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R squared:", r_squared, "\n")
cat("Adjusted R squared:", adj_r_squared, "\n")



# WLS
residuals <- residuals(model)
weights <- 1 / (residuals^2)

# Fit WLS model using the defined weights
wls_model <- lm(Monthly_Unemployment ~ ., data = cbind(y_train, X_train), weights = weights)

# Breusch-Pagan test for heteroscedasticity
wls_bp_test <- bptest(wls_model)

# Assuming 'wls_model' is your model
residuals_wls <- residuals(wls_model)
# Compute the numerator (sum of squared differences of successive residuals)
dw_numerator <- sum(diff(residuals_wls)^2)

# Compute the denominator (sum of squared residuals)
dw_denominator <- sum(residuals_wls^2)

# Compute the Durbin-Watson statistic
dw_statistic <- dw_numerator / dw_denominator

# Jarque-Bera test for normality of residuals
wls_jb_test <- jarque.bera.test(residuals(wls_model))

# Predict and calculate RMSE and MAE
wls_predicted <- predict(wls_model, newdata = X_train)
wls_rmse_value <- rmse(y_train$Monthly_Unemployment, wls_predicted)
wls_mae_value <- mae(y_train$Monthly_Unemployment, wls_predicted)

# Get R squared and Adjusted R squared from the model summary
wls_summary_stats <- summary(wls_model)
wls_r_squared <- wls_summary_stats$r.squared
wls_adj_r_squared <- wls_summary_stats$adj.r.squared

# Print results
cat("WLS BP Test p-value:", wls_bp_test$p.value, "\n")
cat("Durbin-Watson statistic:", dw_statistic, "\n")
cat("WLS JB Test p-value:", wls_jb_test$p.value, "\n")
cat("WLS RMSE:", wls_rmse_value, "\n")
cat("WLS MAE:", wls_mae_value, "\n")
cat("WLS R squared:", wls_r_squared, "\n")
cat("WLS Adjusted R squared:", wls_adj_r_squared, "\n")


#The error occurs because X_train is a data frame 
#it cannot be directly used in the formula inside the lm() function
#Instead, we need to specify the individual variables from X_train in the formula.


library(nlme)
library(Metrics)  # For RMSE and MAE
library(lmtest)   # For Breusch-Pagan test

# Assuming a simple AR(1) correlation structure for demonstration
rho <- 0.5  # Replace this with an estimated autocorrelation value
n <- length(y_train)

# Create a covariance matrix based on AR(1) structure
cov_matrix <- outer(1:n, 1:n, function(i, j) rho^abs(i-j))

# Combine y_train and X_train into one data frame
trainData <- cbind(y_train, X_train)

# Fit the GLS model with the combined data frame
gls_model <- gls(Monthly_Unemployment ~ SP + Federal + Egg + Aluminium, data = trainData,
                 correlation = corAR1(form = ~ 1, value = rho))


# Summary of the GLS model
summary(gls_model)

# Predictions and evaluation
predictions <- predict(gls_model)
rmse <- rmse(y_train$Monthly_Unemployment, predictions)
mae <- mae(y_train$Monthly_Unemployment, predictions)


# Get residuals
residuals <- residuals(gls_model)

fitted_values <- fitted(gls_model)

# Adjust fitted_values to match the length of residuals
fitted_values_used <- fitted_values[1:length(residuals)]

# Create the data frame for Breusch-Pagan test
bp_data <- data.frame(residuals = residuals, fitted_values = fitted_values_used, X_train)

# Fit a linear model to use in bptest
lm_for_bp <- lm(residuals ~ fitted_values + SP + Federal + Egg + Aluminium, data = bp_data)

# Perform the Breusch-Pagan test
bp_test <- bptest(lm_for_bp)

# Assuming y_train$Monthly_Unemployment is your observed values
observed_values <- y_train$Monthly_Unemployment

# Calculate the pseudo R-squared
pseudo_r_squared <- cor(observed_values, fitted_values_used)^2
# Number of predictors (excluding the intercept)
num_predictors <- length(coef(lm_for_bp)) - 1

# Number of observations
num_obs <- length(observed_values)

# Calculate adjusted R-squared
adjusted_r_squared <- 1 - (1 - pseudo_r_squared) * (num_obs - 1) / (num_obs - num_predictors - 1)

# Load necessary libraries
library(lmtest)
library(tseries)

# Assuming 'residuals' are already extracted from your GLS model
# Durbin-Watson test
dw_test <- dwtest(residuals ~ fitted_values, data = bp_data)

# Jarque-Bera test
jb_test <- jarque.bera.test(residuals)

# Print the Breusch-Pagan test results
cat("Breusch-Pagan Test Statistic:", bp_test$statistic, "\n")
cat("Breusch-Pagan Test p-value:", bp_test$p.value, "\n")

cat("Durbin-Watson statistic:", dw_test$p.value, "\n")
cat("JB Test p-value:", jb_test$p.value, "\n")

# Print evaluation metrics
cat("Root Mean Square Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

cat("GLS R squared:", pseudo_r_squared, "\n")
cat("GLS Adjusted R squared:", adjusted_r_squared, "\n")



# Lasso
# no package for tests, need to manually calculate! 

library(glmnet)

# Prepare the matrix of predictors
X_matrix <- as.matrix(X_train)

# Convert the response to a vector
y_vector <- y_train$Monthly_Unemployment

# Fit Lasso model

lasso_model <- glmnet(X_matrix, y_vector, alpha = 1)  # Alpha = 1 for Lasso

# Cross-validation for lambda selection
cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1)
opt_lambda <- cv_lasso$lambda.min  # Minimum lambda from CV

# Predict using the optimal lambda
lasso_pred <- predict(lasso_model, s = opt_lambda, newx = X_matrix)


# Calculate residuals
lasso_residuals <- y_vector - lasso_pred
# Breusch-Pagan test
lasso_bp_test <- bptest(y_vector ~ lasso_pred, data = data.frame(y_vector, lasso_pred),
                        varformula = ~ lasso_pred, studentize = FALSE)

# Calculate Durbin-Watson statistic manually
dw_numerator <- sum(diff(lasso_residuals)^2)
dw_denominator <- sum(lasso_residuals^2)
dw_statistic <- dw_numerator / dw_denominator

# Jarque-Bera test
lasso_jb_test <- jarque.bera.test(lasso_residuals)

# Calculate RMSE and MAE
lasso_rmse <- sqrt(mean((y_vector - lasso_pred)^2))
lasso_mae <- mean(abs(y_vector - lasso_pred))

# Calculate R squared and Adjusted R squared
lasso_r_squared <- cor(y_vector, lasso_pred)^2
lasso_adj_r_squared <- 1 - (1-lasso_r_squared)*(length(y_vector)-1)/(length(y_vector)-ncol(X_matrix)-1)

# Print results
cat("Lasso BP Test p-value:", lasso_bp_test$p.value, "\n")
cat("Durbin-Watson statistic:", dw_statistic, "\n")
cat("Lasso JB Test p-value:", lasso_jb_test$p.value, "\n")
cat("Lasso RMSE:", lasso_rmse, "\n")
cat("Lasso MAE:", lasso_mae, "\n")
cat("Lasso R squared:", lasso_r_squared, "\n")
cat("Lasso Adjusted R squared:", lasso_adj_r_squared, "\n")






# Ridge
cv_ridge <- cv.glmnet(X_matrix, y_vector, alpha = 0)  # Alpha = 0 for Ridge regression
opt_lambda <- cv_ridge$lambda.min  # Optimal lambda value

# Predict using the optimal lambda
ridge_pred <- predict(cv_ridge, s = opt_lambda, newx = X_matrix)

# Calculate residuals
ridge_residuals <- y_vector - ridge_pred

library(lmtest)
library(tseries)
library(Metrics)

# Breusch-Pagan test
ridge_bp_test <- bptest(y_vector ~ ridge_pred, data = data.frame(y_vector, ridge_pred),
                        varformula = ~ ridge_pred, studentize = FALSE)

# Durbin-Watson statistic
dw_numerator <- sum(diff(ridge_residuals)^2)
dw_denominator <- sum(ridge_residuals^2)
dw_statistic <- dw_numerator / dw_denominator

# Jarque-Bera test for normality of residuals
ridge_jb_test <- jarque.bera.test(ridge_residuals)

# RMSE and MAE
ridge_rmse <- sqrt(mean((y_vector - ridge_pred)^2))
ridge_mae <- mean(abs(y_vector - ridge_pred))

# R squared and Adjusted R squared
ridge_r_squared <- cor(y_vector, ridge_pred)^2
ridge_adj_r_squared <- 1 - (1-ridge_r_squared)*(length(y_vector)-1)/(length(y_vector)-ncol(X_matrix)-1)


# Print results
cat("Ridge BP Test p-value:", ridge_bp_test$p.value, "\n")
cat("Durbin-Watson statistic:", dw_statistic, "\n")
cat("Ridge JB Test p-value:", ridge_jb_test$p.value, "\n")
cat("Ridge RMSE:", ridge_rmse, "\n")
cat("Ridge MAE:", ridge_mae, "\n")
cat("Ridge R squared:", ridge_r_squared, "\n")
cat("Ridge Adjusted R squared:", ridge_adj_r_squared, "\n")



# Elasic Net
# Fit Elastic Net model with cross-validation

cv_elastic <- cv.glmnet(X_matrix, y_vector, alpha = 0.5)  # Alpha = 0.5 for a balance of Ridge and Lasso
opt_lambda <- cv_elastic$lambda.min  # Optimal lambda value

# Predict using the optimal lambda
elastic_pred <- predict(cv_elastic, s = opt_lambda, newx = X_matrix)

# Calculate residuals
elastic_residuals <- y_vector - elastic_pred


# Breusch-Pagan test
elastic_bp_test <- bptest(y_vector ~ elastic_pred, data = data.frame(y_vector, elastic_pred),
                          varformula = ~ elastic_pred, studentize = FALSE)

# Durbin-Watson statistic
dw_numerator <- sum(diff(elastic_residuals)^2)
dw_denominator <- sum(elastic_residuals^2)
dw_statistic <- dw_numerator / dw_denominator

# Jarque-Bera test for normality of residuals
elastic_jb_test <- jarque.bera.test(elastic_residuals)

# RMSE and MAE
elastic_rmse <- sqrt(mean((y_vector - elastic_pred)^2))
elastic_mae <- mean(abs(y_vector - elastic_pred))

# R squared and Adjusted R squared
elastic_r_squared <- cor(y_vector, elastic_pred)^2
elastic_adj_r_squared <- 1 - (1-elastic_r_squared)*(length(y_vector)-1)/(length(y_vector)-ncol(X_matrix)-1)

# Print results
cat("Elastic Net BP Test p-value:", elastic_bp_test$p.value, "\n")
cat("Durbin-Watson statistic:", dw_statistic, "\n")
cat("Elastic Net JB Test p-value:", elastic_jb_test$p.value, "\n")
cat("Elastic Net RMSE:", elastic_rmse, "\n")
cat("Elastic Net MAE:", elastic_mae, "\n")
cat("Elastic Net R squared:", elastic_r_squared, "\n")
cat("Elastic Net Adjusted R squared:", elastic_adj_r_squared, "\n")









# Model Selection & Back Testing


# OLS Back Testing
library(caret)
library(Metrics)  # for calculating RMSE and MAE

# Prepare the dataset by combining X_train with y_train
train_data <- cbind(X_train, y_train)  # Ensure y_train is not already part of X_train

# Set up 10-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(train_data$Monthly_Unemployment, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data)), train_indices)
  
  train_set <- train_data[train_indices, ]
  test_set <- train_data[test_indices, ]
  
  # Fit OLS model
  ols_model <- lm(Monthly_Unemployment ~ ., data = train_set)
  
  # Predict on validation set
  predictions <- predict(ols_model, newdata = test_set)
  
  # Calculate metrics
  mse_value <- mean((test_set$Monthly_Unemployment - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(test_set$Monthly_Unemployment - predictions))
  r2_value <- cor(test_set$Monthly_Unemployment, predictions)^2
  
  n <- length(test_set$Monthly_Unemployment)
  p <- length(coef(ols_model)) - 1  # Adjust for the intercept
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")





# WLS Back Testing
library(caret)
library(Metrics)  # for RMSE and MAE calculations

# Prepare the dataset by combining X_train with y_train
train_data <- cbind(X_train, y_train)  # Ensure y_train is not already part of X_train

# Set up 10-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(train_data$Monthly_Unemployment, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data)), train_indices)
  
  train_set <- train_data[train_indices, ]
  test_set <- train_data[test_indices, ]
  
  # Fit initial OLS model to calculate weights
  initial_model <- lm(Monthly_Unemployment ~ ., data = train_set)
  initial_residuals <- residuals(initial_model)
  weights <- 1 / (initial_residuals^2)
  
  # Fit WLS model using the calculated weights
  wls_model <- lm(Monthly_Unemployment ~ ., data = train_set, weights = weights)
  
  # Predict on validation set
  predictions <- predict(wls_model, newdata = test_set)
  
  # Calculate metrics
  mse_value <- mean((test_set$Monthly_Unemployment - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(test_set$Monthly_Unemployment - predictions))
  r2_value <- cor(test_set$Monthly_Unemployment, predictions)^2
  
  n <- length(test_set$Monthly_Unemployment)
  p <- length(coef(wls_model)) - 1  # Adjust for the intercept
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")




# GLS back testing

library(nlme)
library(caret)
library(Metrics)

# Ensure the data frame is properly formatted
train_data <- cbind(X_train, y_train)  # Assuming y_train is not already part of X_train

# Set up 10-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(train_data$Monthly_Unemployment, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize lists to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data)), train_indices)
  
  train_set <- train_data[train_indices, ]
  test_set <- train_data[test_indices, ]
  
  # Fit GLS model
  gls_model <- gls(Monthly_Unemployment ~ ., data = train_set, correlation = corAR1(form = ~ 1))
  
  # Predict on validation set
  predictions <- predict(gls_model, newdata = test_set)
  
  # Calculate metrics
  mse_value <- mean((test_set$Monthly_Unemployment - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(test_set$Monthly_Unemployment - predictions))
  r2_value <- cor(test_set$Monthly_Unemployment, predictions)^2
  
  n <- length(test_set$Monthly_Unemployment)
  p <- length(coef(gls_model))
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")


# Lasso Back Testing
library(glmnet)
library(caret)
library(Metrics)  # for RMSE and MAE calculations

# Prepare the matrix of predictors and convert the response to a vector
X_matrix <- as.matrix(X_train)
y_vector <- y_train$Monthly_Unemployment

# Set up k-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(y_vector, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(length(y_vector)), train_indices)
  
  X_train_cv <- X_matrix[train_indices, ]
  y_train_cv <- y_vector[train_indices]
  X_test_cv <- X_matrix[test_indices, ]
  y_test_cv <- y_vector[test_indices]
  
  # Fit Lasso model using cross-validation to select the best lambda
  cv_lasso <- cv.glmnet(X_train_cv, y_train_cv, alpha = 1)
  opt_lambda <- cv_lasso$lambda.min
  
  # Predict using the optimal lambda
  predictions <- predict(cv_lasso$glmnet.fit, s = opt_lambda, newx = X_test_cv)
  
  # Calculate metrics
  mse_value <- mean((y_test_cv - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(y_test_cv - predictions))
  r2_value <- cor(y_test_cv, predictions)^2
  n <- length(y_test_cv)
  p <- ncol(X_matrix)  # or use length(cv_lasso$glmnet.fit$beta[opt_lambda])
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")





# Ridge back testing
library(glmnet)
library(caret)
library(Metrics)  # for RMSE and MAE calculations

# Prepare the matrix of predictors and convert the response to a vector
X_matrix <- as.matrix(X_train)
y_vector <- y_train$Monthly_Unemployment

# Set up k-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(y_vector, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(length(y_vector)), train_indices)
  
  X_train_cv <- X_matrix[train_indices, ]
  y_train_cv <- y_vector[train_indices]
  X_test_cv <- X_matrix[test_indices, ]
  y_test_cv <- y_vector[test_indices]
  
  # Fit Ridge model using cross-validation to select the best lambda
  cv_ridge <- cv.glmnet(X_train_cv, y_train_cv, alpha = 0)  # Alpha = 0 for Ridge
  opt_lambda <- cv_ridge$lambda.min
  
  # Predict using the optimal lambda
  predictions <- predict(cv_ridge$glmnet.fit, s = opt_lambda, newx = X_test_cv)
  
  # Calculate metrics
  mse_value <- mean((y_test_cv - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(y_test_cv - predictions))
  r2_value <- cor(y_test_cv, predictions)^2
  n <- length(y_test_cv)
  p <- ncol(X_matrix)  # or use length(cv_ridge$glmnet.fit$beta[opt_lambda])
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")





# Elastic Net back Testing
library(glmnet)
library(caret)
library(Metrics)  # for RMSE and MAE calculations

# Prepare the matrix of predictors and convert the response to a vector
X_matrix <- as.matrix(X_train)
y_vector <- y_train$Monthly_Unemployment

# Set up k-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(y_vector, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store results
results <- list(mse = numeric(), rmse = numeric(), mae = numeric(), r2 = numeric(), adjusted_r2 = numeric())

# Cross-validation loop
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(length(y_vector)), train_indices)
  
  X_train_cv <- X_matrix[train_indices, ]
  y_train_cv <- y_vector[train_indices]
  X_test_cv <- X_matrix[test_indices, ]
  y_test_cv <- y_vector[test_indices]
  
  # Fit Elastic Net model using cross-validation to select the best lambda
  cv_elastic <- cv.glmnet(X_train_cv, y_train_cv, alpha = 0.5)  # Alpha = 0.5
  opt_lambda <- cv_elastic$lambda.min
  
  # Predict using the optimal lambda
  predictions <- predict(cv_elastic$glmnet.fit, s = opt_lambda, newx = X_test_cv)
  
  # Calculate metrics
  mse_value <- mean((y_test_cv - predictions)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(y_test_cv - predictions))
  r2_value <- cor(y_test_cv, predictions)^2
  n <- length(y_test_cv)
  p <- ncol(X_matrix)  # or use length(cv_elastic$glmnet.fit$beta[opt_lambda])
  adjusted_r2_value <- 1 - ((1 - r2_value) * (n - 1) / (n - p - 1))
  
  # Store results
  results$mse <- c(results$mse, mse_value)
  results$rmse <- c(results$rmse, rmse_value)
  results$mae <- c(results$mae, mae_value)
  results$r2 <- c(results$r2, r2_value)
  results$adjusted_r2 <- c(results$adjusted_r2, adjusted_r2_value)
}

# Calculate mean of each metric
mean_mse <- mean(results$mse)
mean_rmse <- mean(results$rmse)
mean_mae <- mean(results$mae)
mean_r2 <- mean(results$r2)
mean_adjusted_r2 <- mean(results$adjusted_r2)

# Print results
cat("Mean MSE:", mean_mse, "\n")
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean R-squared:", mean_r2, "\n")
cat("Mean Adjusted R-squared:", mean_adjusted_r2, "\n")





