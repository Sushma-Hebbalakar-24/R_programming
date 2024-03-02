install.packages("caret")
install.packages("tidyverse")
install.packages("glmnet")
install.packages("xgboost")
install.packages("rpart")
install.packages("e1071")
install.packages("readxl")
install.packages("openxlsx")
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("dbscan")
install.packages("pheatmap")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("ROSE")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("skimr")
install.packages("naivebayes")
install.packages("e1071")
install.packages("naivebayes")
install.packages("kernlab")
install.packages("kernlab")
library(skimr)
library(corrplot)
library(FactoMineR)
library(caret)
library(ROSE)
library(randomForest)
library(e1071)
library(caret)
library(tidyverse)
library(glmnet)
library(xgboost)
library(rpart)
library(e1071)
library(readxl)
library(openxlsx)
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library(factoextra)
library(ggplot2)
library(gridExtra)
library(dbscan)
library(pheatmap) 
library(dplyr)


#file paths for 3 data sets
customer_churn_file_path <- "E:\\NCI\\Data Mining & Machine Learning 1\\Projects\\Churn_Modelling.csv"

#reading the data files
customer_data <- read.csv(customer_churn_file_path, na.strings = c("", "NA", "N/A", "9999"))

#viewing the data
str(customer_data)

# Function to fetch numeric columns
get_numeric_columns <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  return(names(numeric_columns[numeric_columns]))
}

# Function to fetch character columns
get_character_columns <- function(data) {
  character_columns <- sapply(data, is.character)
  return(names(character_columns[character_columns]))
}

customer_data_null_count <- colSums(is.na(customer_data))

# Print the null counts
print(customer_data_null_count)
dim(customer_data)
str(customer_data)

drop <- c("RowNumber", "CustomerId", "Surname")
cd_u <- customer_data[,!(names(customer_data) %in% drop)]

names(cd_u)
skim(cd_u)
summary(cd_u)
str(cd_u)

numeric_columns= get_numeric_columns(cd_u)
character_columns = get_character_columns(cd_u)

numeric_columns

# Visualize bar plots for character columns
for (col in character_columns) {
  p <- ggplot(cd_u, aes(x = .data[[col]])) +
    geom_bar() +
    labs(title = paste("Bar Plot of", col))
  
  print(p)
}

# Visualize histograms for numeric columns
for (col in numeric_columns) {
  p <- ggplot(cd_u, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", col))
  
  print(p)
}

# Create subplots
plots <- list()

for (col in numeric_columns) {
  if (col %in% c("CreditScore", "Age", "Tenure", "NumOfProducts")) {
    # Histograms for numeric columns
    p <- ggplot(cd_u, aes(x = .data[[col]])) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", col))
    
  } else if (col %in% c("Balance", "EstimatedSalary")) {
    # Line plots for numeric columns
    p <- ggplot(cd_u, aes(x = seq_along(.data[[col]]), y = .data[[col]])) +
      geom_line() +
      labs(title = paste("Line Plot of", col))
    
  } else {
    # Count plots for categorical columns
    p <- ggplot(cd_u, aes(x = .data[[col]])) +
      geom_bar() +
      labs(title = paste("Count Plot of", col))
  }
  
  plots[[col]] <- p
}

# Arrange subplots in a grid
grid.arrange(grobs = plots, ncol = 3)

library(ggplot2)

# Function to plot bivariate histogram
plot_bivariate_histogram <- function(data, x_column, exited_column) {
  hist_plot <- ggplot(data, aes(x = .data[[x_column]], fill = factor(.data[[exited_column]]))) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
    labs(title = paste("Bivariate Histogram of", x_column, "and", exited_column),
         x = x_column, y = "Frequency") +
    scale_fill_manual(values = c("blue", "red"))
  
  print(hist_plot)
}

library(ggplot2)

# Function to plot boxplot
plot_boxplot <- function(data, x_column, y_column) {
  boxplot <- ggplot(data, aes(x = .data[[x_column]], y = .data[[y_column]], fill = factor(.data[[y_column]]))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", y_column, "by", x_column),
         x = x_column, y = y_column) +
    scale_fill_manual(values = c("blue", "red"))
  
  print(boxplot)
}


plot_boxplot(cd_u, "Age","Exited")
plot_bivariate_histogram(cd_u, "Age", "Exited")

library(ggplot2)

# Function to plot bivariate histogram and boxplot
plot_bivariate_hist_box <- function(data, x_column, y_column) {
  
  # Bivariate Histogram
  hist_plot <- ggplot(data, aes(x = .data[[x_column]], fill = factor(.data[[y_column]]))) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
    labs(title = paste("Bivariate Histogram of", x_column, "and", y_column),
         x = x_column, y = "Frequency") +
    scale_fill_manual(values = c("blue", "red"))
  
  # Boxplot
  boxplot <- ggplot(data, aes(x = factor(.data[[y_column]]), y = .data[[x_column]], fill = factor(.data[[y_column]]))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", x_column, "by", y_column),
         x = y_column, y = x_column) +
    scale_fill_manual(values = c("blue", "red"))
  
  # Display plots side by side
  grid.arrange(hist_plot, boxplot, nrow = 2)
}

plot_bivariate_hist_box(cd_u, "Age", "Exited")
plot_bivariate_hist_box(cd_u, "CreditScore", "Exited")
plot_bivariate_hist_box(cd_u, "Balance", "Exited")
plot_bivariate_hist_box(cd_u, "EstimatedSalary", "Exited")

library(ggplot2)

plot_categorical <- function(data, feature) {
  
  # Calculate churn rate
  churn_rate <- data %>%
    group_by(.data[[feature]], Exited) %>%
    summarize(count = n()) %>%
    group_by(.data[[feature]]) %>%
    mutate(churn_rate = count / sum(count))
  
  # Create a plot
  p <- ggplot(churn_rate, aes(x = .data[[feature]], y = churn_rate, fill = factor(Exited))) +
    geom_col(position = "dodge") +
    labs(title = paste("Categorical Plot of", feature),
         x = feature, y = "Churn Rate") +
    scale_fill_manual(values = c("blue", "red")) +
    theme_minimal()
  
  print(p)
}

# Example usage:
# Assuming your data frame is named cd_u
# Assuming "Exited" is a binary variable (0 or 1)
plot_categorical(cd_u, "Geography")
plot_categorical(cd_u, "Gender")
plot_categorical(cd_u, "Tenure")
plot_categorical(cd_u, "NumOfProducts")


library(ggplot2)

plot_categorical_columns <- function(data, cat_columns) {
  colors_cat <- c("blue", "red")  # Specify your desired colors
  
  # Create a list to store the plots
  plots <- list()
  
  for (col in cat_columns) {
    # Create a dataframe for the count plot
    count_data <- data %>%
      group_by(.data[[col]], Exited) %>%
      summarize(count = n()) %>%
      mutate(churn_rate = count / sum(count))
    
    # Map values to "No" and "Yes" for specific columns
    if (col %in% c("HasCrCard", "IsActiveMember")) {
      count_data[[col]] <- factor(count_data[[col]], levels = c(0, 1), labels = c("No", "Yes"))
    }
    
    # Create the count plot
    p <- ggplot(count_data, aes(x = .data[[col]], y = churn_rate, fill = factor(Exited))) +
      geom_col(position = "dodge") +
      labs(title = paste("Categorical Plot of", col),
           x = col, y = "Churn Rate") +
      scale_fill_manual(values = colors_cat) +
      theme_minimal()
    
    # Add the plot to the list
    plots[[col]] <- p
  }
  
  # Arrange subplots in a grid
  grid.arrange(grobs = plots, ncol = length(cat_columns))
}

# Assuming "Exited" is a binary variable (0 or 1)
#categorical_columns <- c("HasCrCard", "IsActiveMember")  # Add more columns if needed
plot_categorical_columns(cd_u,"HasCrCard" )
plot_categorical_columns(cd_u, "IsActiveMember")


library(ggplot2)
library(gridExtra)

plot_categorical_columns <- function(data, cat_columns) {
  colors_cat <- c("blue", "red")  # Specify your desired colors
  
  # Create a list to store the plots
  plots_count <- list()
  plots_churn_rate <- list()
  
  for (col in cat_columns) {
    # Create a dataframe for the count plot
    count_data <- data %>%
      group_by(.data[[col]], Exited) %>%
      summarize(count = n()) %>%
      mutate(churn_rate = count / sum(count))
    
    # Map values to "No" and "Yes" for specific columns
    if (col %in% c("HasCrCard", "IsActiveMember")) {
      count_data[[col]] <- factor(count_data[[col]], levels = c(0, 1), labels = c("No", "Yes"))
    }
    
    # Create the count plot
    p_count <- ggplot(count_data, aes(x = .data[[col]], y = count, fill = factor(Exited))) +
      geom_col(position = "dodge") +
      labs(title = paste("Count Plot of", col),
           x = col, y = "Count") +
      scale_fill_manual(values = colors_cat) +
      theme_minimal()
    
    # Create the churn rate plot
    p_churn_rate <- ggplot(count_data, aes(x = .data[[col]], y = churn_rate)) +
      geom_col(position = "dodge") +
      labs(title = paste("Churn Rate Plot of", col),
           x = col, y = "Churn Rate") +
      scale_fill_manual(values = colors_cat) +
      theme_minimal()
    
    # Add the plots to the lists
    plots_count[[col]] <- p_count
    plots_churn_rate[[col]] <- p_churn_rate
  }
  
  # Arrange count and churn rate plots side by side
  grid.arrange(grobs = c(plots_count, plots_churn_rate), ncol = length(cat_columns))
}


categorical_columns <- c("HasCrCard", "IsActiveMember")  # Add more columns if needed
plot_categorical_columns(cd_u, categorical_columns)


plot_categorical_columns(cd_u,"HasCrCard" )
plot_categorical_columns(cd_u, "IsActiveMember")
plot_categorical_columns(cd_u, "Geography")
plot_categorical_columns(cd_u, "Gender")
plot_categorical_columns(cd_u, "Tenure")
plot_categorical_columns(cd_u, "NumOfProducts")

# Function to perform chi-square test for given categorical columns
chi_square_test <- function(data, categorical_columns) {
  chi2_array <- c()
  p_array <- c()
  
  for (column in categorical_columns) {
    # Create a contingency table
    crosstab <- table(data[[column]], data$Exited)
    
    # Perform chi-square test
    chi_result <- chisq.test(crosstab)
    
    # Extract chi-square statistic and p-value
    chi2 <- chi_result$statistic
    p_value <- chi_result$p.value
    
    # Append to arrays
    chi2_array <- c(chi2_array, chi2)
    p_array <- c(p_array, p_value)
  }
  
  # Create a dataframe
  df_chi <- data.frame(
    'Variable' = categorical_columns,
    'Chi_square' = chi2_array,
    'p_value' = p_array
  )
  
  # Sort the dataframe by Chi-square in descending order
  df_chi <- df_chi[order(-df_chi$`Chi_square`), ]
  
  return(df_chi)
}

# Example usage:
# Assuming your data frame is named cd_u
# Assuming "Exited" is a binary variable (0 or 1)
categorical_columns <- c("HasCrCard", "IsActiveMember", "Geography", "Gender", "Tenure", "NumOfProducts")
result <- chi_square_test(cd_u, categorical_columns)
print(result)

drop_cat <- c("Tenure", "HasCrCard")
cd_u <- cd_u[,!(names(cd_u) %in% drop_cat)]
names(cd_u)

library(plyr)

# Convert 'Gender' to numeric using factor
cd_u$Gender <- as.numeric(factor(cd_u$Gender, levels = c("Female", "Male")))

# Example: Map 'Geography' values according to your mapping
cd_u$Geography <- mapvalues(cd_u$Geography, 
                                from = c("Germany", "Spain", "France"), 
                                to = c(1, 0, 3))
# Scaling

# Specify columns to scale
scl_columns <- c("CreditScore", "Age", "Balance")

# Standardize the specified columns
cd_u[, scl_columns] <- scale(cd_u[, scl_columns])
cd_u[["EstimatedSalary"]] <- scale(cd_u[["EstimatedSalary"]])
  
# View the updated data frame
print(head(cd_u))


library(caret)
library(ROSE)

# Set a random seed for reproducibility
set.seed(123)

train_index <- createDataPartition(cd_u$Exited, p = 0.70, list = FALSE)

# Create the training set
train_data <- cd_u[train_index, ]

# Create the testing set
test_data <- cd_u[-train_index, ]

# View the dimensions of the training and testing sets
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

exit_counts <- table(train_data$Exited)
print(exit_counts)

# Find the minimum count (to balance the classes)
min_count <- min(exit_counts)
max_count <- max(exit_counts)

# Perform oversampling to balance the classes
balanced_train_data <- ovun.sample(Exited ~ ., data = train_data, method = "over", N=7000+(max_count-min_count))

# Extract the balanced data frame
balanced_train_data <- balanced_train_data$data

# Print the new counts to verify balancing
new_exit_counts <- table(balanced_train_data$Exited)
print(new_exit_counts)

print(max(min_count * length(unique(train_data$Exited)),sum(exit_counts)))
print(max(min_count,max_count))

# Assuming your data frame is named balanced_train_data
# Assuming "Exited" is the column of interest

# Load necessary packages if not already loaded
# install.packages("e1071")

library(e1071)
library(caTools)
library(caret)

# Convert "Exited" to a factor
balanced_train_data$Exited <- as.factor(balanced_train_data$Exited)
test_data$Exited <- as.factor(test_data$Exited)
length(test_data$Exited)

# Train Naive Bayes model
naive_bayes_model <- naiveBayes(Exited ~ ., data = balanced_train_data)
# Print the Naive Bayes model summary
print(naive_bayes_model)
# Make predictions on the test set
naive_bayes_preds <- predict(naive_bayes_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
naive_bayes_preds <- factor(naive_bayes_preds, levels = levels(test_data$Exited))
#levels(test_data$Exited)
#levels(naive_bayes_preds)
# Print confusion matrix for Naive Bayes
confusionMatrix(naive_bayes_preds, test_data$Exited)
length(naive_bayes_preds)

# Train Logistic Regression model
logistic_regression_model <- glm(Exited ~ ., data = balanced_train_data, family = "binomial")
# Make predictions on the test set
logistic_regression_preds <- predict(logistic_regression_model, test_data, type = "response")
# Convert probabilities to classes (0 or 1)
logistic_regression_preds <- ifelse(logistic_regression_preds > 0.5, 1, 0)
# Convert predicted values to factor with the same levels as test_data$Exited
logistic_regression_preds <- factor(logistic_regression_preds, levels = levels(test_data$Exited))
# Print confusion matrix for Logistic Regression
confusionMatrix(table(logistic_regression_preds, test_data$Exited))
# Print the Logistic Regression model summary
summary(logistic_regression_model)
length(logistic_regression_preds)

predicted_levels<-levels(test_data$Exited)
# Train SVM model
svm_model <- svm(Exited ~ ., data = balanced_train_data, kernel = "polynomial")
# Make predictions on the test set
svm_preds <- predict(svm_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
svm_preds <- factor(svm_preds, levels = levels(test_data$Exited))
# Print confusion matrix for SVM
confusionMatrix(svm_preds, test_data$Exited)
print(levels(balanced_train_data$Exited))
print(levels(svm_preds))
length(svm_preds)

# Load necessary packages if not already loaded
# install.packages("randomForest")

library(randomForest)

# Train Random Forest model
rf_model <- randomForest(Exited ~ ., data = balanced_train_data)
# Make predictions on the test set
rf_preds <- predict(rf_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
rf_preds <- factor(rf_preds, levels = levels(test_data$Exited))
# Print confusion matrix for Random Forest
confusionMatrix(rf_preds, test_data$Exited)
length(rf_preds)

# Combine predictions into a data frame
ensemble_preds <- data.frame(
  naive_bayes = as.numeric(as.character(naive_bayes_preds)),
  logistic_regression = as.numeric(as.character(logistic_regression_preds)),
  svm = as.numeric(as.character(svm_preds)),
  random_forest = as.numeric(as.character(rf_preds))
)

# Create a new column in the data frame to store the majority voted predictions
ensemble_preds$majority_vote <- apply(ensemble_preds, 1, function(row) {
  # Count occurrences of each class in the row
  class_counts <- table(row)
  
  # Find the class with the maximum count
  majority_class <- as.numeric(names(class_counts)[which.max(class_counts)])
  
  return(majority_class)
})
#print(ensemble_preds$majority_vote)
ensemble_preds$majority_vote <- factor(ensemble_preds$majority_vote, levels = levels(test_data$Exited))

# Check the lengths of vectors
length(ensemble_preds$majority_vote)
length(test_data$Exited)

# Compare the levels of the factors
levels(ensemble_preds$majority_vote)
levels(test_data$Exited)

# Print the majority voted predictions
confusionMatrix(ensemble_preds$majority_vote, test_data$Exited)

# Install and load necessary packages if not already installed
# install.packages(c("caret", "e1071", "kernlab", "ROCR"))

library(caret)
library(e1071)
library(kernlab)
library(ROCR)
library(yardstick)


# Set seed for reproducibility
set.seed(123)

# Install and load necessary packages
#install.packages("e1071")
library(e1071)
library(caret)
library(naivebayes)
library(pROC)

# Assuming naive_bayes_preds contains predicted probabilities
roc_nb <- roc(test_data$Exited, as.numeric(naive_bayes_preds), levels = c(0, 1))
roc_lr <- roc(test_data$Exited, as.numeric(logistic_regression_preds), levels = c(0, 1))
roc_svm <- roc(test_data$Exited, as.numeric(svm_preds) - 1, levels = c(0, 1))
roc_rf <- roc(test_data$Exited, as.numeric(rf_preds) - 1, levels = c(0, 1))
roc_ensample <- roc(test_data$Exited, as.numeric(ensemble_preds$majority_vote) - 1, levels = c(0, 1))

# Plot ROC curves
plot(roc_nb, col = "blue", main = "ROC Curves for Different Models", lwd = 2, cex.main = 1.5, cex.lab = 1.3)
lines(roc_lr, col = "red", lwd = 2)
lines(roc_svm, col = "green", lwd = 2)
lines(roc_rf, col = "purple", lwd = 2)
lines(roc_ensample, col = "brown", lwd = 2)

# Add a legend
legend("bottomright", 
       legend = c("Naive Bayes", "Logistic Regression", "SVM", "Random Forest","ROC_ensample"), 
       col = c("blue", "red", "green", "purple","brown"), 
       lwd = 2, 
       cex = 0.8) 

# Assuming roc_nb, roc_lr, roc_svm, roc_rf are already created

# Load the pROC package if not already loaded
# install.packages("pROC")
library(pROC)

# Print AUC values
auc_nb <- auc(roc_nb)
auc_lr <- auc(roc_lr)
auc_svm <- auc(roc_svm)
auc_rf <- auc(roc_rf)
auc_ensample <- auc(roc_ensample)

# Display AUC values
cat("AUC for Naive Bayes:", auc_nb, "\n")
cat("AUC for Logistic Regression:", auc_lr, "\n")
cat("AUC for SVM:", auc_svm, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")
cat("AUC for Ensample:", auc_ensample, "\n")

# AUC values
auc_values <- c(auc_nb, auc_lr, auc_svm, auc_rf, auc_ensample)
# Custom colors for each bar
custom_colors <- c("skyblue", "lightgreen", "salmon", "lightcoral","orange")

# Model names
model_names <- c("Naive Bayes", "Logistic Regression", "SVM", "Random Forest","Ensample")

# Create a data frame
auc_data <- data.frame(Model = model_names, AUC = auc_values)

# Plot the bar plot
barplot(auc_data$AUC, names.arg = auc_data$Model, col = custom_colors, main = "AUC Comparison",
        ylab = "AUC", ylim = c(0, 1), beside = TRUE)

library(caret)

# Create a random forest model
rf_model <- train(
  Exited ~ .,
  data = balanced_train_data,  # Assuming you have balanced data
  method = "rf",  # Random Forest
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross-validation
  preProcess = c("center", "scale")  # Standardize the data
)

# Print the cross-validated results
print(rf_model)

# Access the performance metrics
summary(rf_model)

# Assuming 'actual' and 'predicted' are vectors of actual and predicted values

predicted <- naive_bayes_preds

# Function to calculate MAE and MSE
calculate_mae_mse <- function(actual, predicted) {
  # Convert factors to numeric vectors
  actual <- as.numeric((actual))
  predicted <- as.numeric((predicted))
  
  mae <- mean(abs(actual - predicted))
  mse <- mean((actual - predicted)^2)
  
  return(c(MAE = mae, MSE = mse))
}

# Function to calculate F1-score
calculate_f1_score <- function(actual, predicted) {
  confusion_matrix <- table(actual, predicted)
  actual <- factor(actual)
  predicted <- factor(predicted, levels = levels(actual))
  
  # Calculate precision, recall, and F1-score
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(f1_score)
}

# Calculate F1-score
cat("naive_bayes_preds")
mae_mse_nb <- calculate_mae_mse(test_data$Exited,naive_bayes_preds)
print(mae_mse_nb)
f1_score <- calculate_f1_score(test_data$Exited, naive_bayes_preds)
cat("F1-score:", f1_score, "\n")

cat("logistic_regression_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,logistic_regression_preds)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, logistic_regression_preds)
cat("F1-score:", f1_score, "\n")

cat("svm_preds")
mae_mse_svm <- calculate_mae_mse(test_data$Exited,svm_preds)
print(mae_mse_svm)
f1_score <- calculate_f1_score(test_data$Exited, svm_preds)
cat("F1-score:", f1_score, "\n")

cat("rf_preds")
mae_mse_rf <- calculate_mae_mse(test_data$Exited,rf_preds)
print(mae_mse_rf)
f1_score <- calculate_f1_score(test_data$Exited, rf_preds)
cat("F1-score:", f1_score, "\n")

cat("ensemble_preds")
mae_mse_en <- calculate_mae_mse(test_data$Exited,ensemble_preds$majority_vote)
print(mae_mse_en)
f1_score <- calculate_f1_score(test_data$Exited, ensemble_preds$majority_vote)
cat("F1-score:", f1_score, "\n")

################Unbalanced train_data###########################################
# Convert "Exited" to a factor
train_data$Exited <- as.factor(train_data$Exited)
test_data$Exited <- as.factor(test_data$Exited)
length(test_data$Exited)

# Train Naive Bayes model
naive_bayes_model <- naiveBayes(Exited ~ ., data = train_data)
# Print the Naive Bayes model summary
print(naive_bayes_model)
# Make predictions on the test set
naive_bayes_preds <- predict(naive_bayes_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
naive_bayes_preds <- factor(naive_bayes_preds, levels = levels(test_data$Exited))
#levels(test_data$Exited)
#levels(naive_bayes_preds)
# Print confusion matrix for Naive Bayes
confusionMatrix(naive_bayes_preds, test_data$Exited)
length(naive_bayes_preds)

# Train Logistic Regression model
logistic_regression_model <- glm(Exited ~ ., data = train_data, family = "binomial")
# Make predictions on the test set
logistic_regression_preds <- predict(logistic_regression_model, test_data, type = "response")
# Convert probabilities to classes (0 or 1)
logistic_regression_preds <- ifelse(logistic_regression_preds > 0.5, 1, 0)
# Convert predicted values to factor with the same levels as test_data$Exited
logistic_regression_preds <- factor(logistic_regression_preds, levels = levels(test_data$Exited))
# Print confusion matrix for Logistic Regression
confusionMatrix(table(logistic_regression_preds, test_data$Exited))
# Print the Logistic Regression model summary
summary(logistic_regression_model)
length(logistic_regression_preds)

predicted_levels<-levels(test_data$Exited)
# Train SVM model
svm_model <- svm(Exited ~ ., data = train_data, kernel = "polynomial")
# Make predictions on the test set
svm_preds <- predict(svm_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
svm_preds <- factor(svm_preds, levels = levels(test_data$Exited))
# Print confusion matrix for SVM
confusionMatrix(svm_preds, test_data$Exited)
print(levels(train_data$Exited))
print(levels(svm_preds))
length(svm_preds)

# Load necessary packages if not already loaded
# install.packages("randomForest")

library(randomForest)

# Train Random Forest model
rf_model <- randomForest(Exited ~ ., data = train_data)
# Make predictions on the test set
rf_preds <- predict(rf_model, test_data)
# Convert predicted values to factor with the same levels as test_data$Exited
rf_preds <- factor(rf_preds, levels = levels(test_data$Exited))
# Print confusion matrix for Random Forest
confusionMatrix(rf_preds, test_data$Exited)
length(rf_preds)

# Combine predictions into a data frame
ensemble_preds <- data.frame(
  naive_bayes = as.numeric(as.character(naive_bayes_preds)),
  logistic_regression = as.numeric(as.character(logistic_regression_preds)),
  svm = as.numeric(as.character(svm_preds)),
  random_forest = as.numeric(as.character(rf_preds))
)

# Create a new column in the data frame to store the majority voted predictions
ensemble_preds$majority_vote <- apply(ensemble_preds, 1, function(row) {
  # Count occurrences of each class in the row
  class_counts <- table(row)
  
  # Find the class with the maximum count
  majority_class <- as.numeric(names(class_counts)[which.max(class_counts)])
  
  return(majority_class)
})
#print(ensemble_preds$majority_vote)
ensemble_preds$majority_vote <- factor(ensemble_preds$majority_vote, levels = levels(test_data$Exited))

# Check the lengths of vectors
length(ensemble_preds$majority_vote)
length(test_data$Exited)

# Compare the levels of the factors
levels(ensemble_preds$majority_vote)
levels(test_data$Exited)

# Print the majority voted predictions
confusionMatrix(ensemble_preds$majority_vote, test_data$Exited)

# Install and load necessary packages if not already installed
# install.packages(c("caret", "e1071", "kernlab", "ROCR"))



library(caret)
library(e1071)
library(kernlab)
library(ROCR)
library(yardstick)


# Set seed for reproducibility
set.seed(123)

# Install and load necessary packages
library(e1071)
library(caret)
library(naivebayes)
library(pROC)

# Assuming naive_bayes_preds contains predicted probabilities
roc_nb <- roc(test_data$Exited, as.numeric(naive_bayes_preds), levels = c(0, 1))
roc_lr <- roc(test_data$Exited, as.numeric(logistic_regression_preds), levels = c(0, 1))
roc_svm <- roc(test_data$Exited, as.numeric(svm_preds) - 1, levels = c(0, 1))
roc_rf <- roc(test_data$Exited, as.numeric(rf_preds) - 1, levels = c(0, 1))
roc_ensample <- roc(test_data$Exited, as.numeric(ensemble_preds$majority_vote) - 1, levels = c(0, 1))

# Plot ROC curves
plot(roc_nb, col = "blue", main = "ROC Curves for Different Models", lwd = 2, cex.main = 1.5, cex.lab = 1.3)
lines(roc_lr, col = "red", lwd = 2)
lines(roc_svm, col = "green", lwd = 2)
lines(roc_rf, col = "purple", lwd = 2)
lines(roc_ensample, col = "brown", lwd = 2)

# Add a legend
legend("bottomright", 
       legend = c("Naive Bayes", "Logistic Regression", "SVM", "Random Forest","ROC_ensample"), 
       col = c("blue", "red", "green", "purple","brown"), 
       lwd = 2, 
       cex = 0.8) 

# Assuming roc_nb, roc_lr, roc_svm, roc_rf are already created

# Load the pROC package if not already loaded
# install.packages("pROC")
library(pROC)

# Print AUC values
auc_nb <- auc(roc_nb)
auc_lr <- auc(roc_lr)
auc_svm <- auc(roc_svm)
auc_rf <- auc(roc_rf)
auc_ensample <- auc(roc_ensample)

# Display AUC values
cat("AUC for Naive Bayes:", auc_nb, "\n")
cat("AUC for Logistic Regression:", auc_lr, "\n")
cat("AUC for SVM:", auc_svm, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")
cat("AUC for Ensample:", auc_ensample, "\n")

# AUC values
auc_values <- c(auc_nb, auc_lr, auc_svm, auc_rf, auc_ensample)
# Custom colors for each bar
custom_colors <- c("skyblue", "lightgreen", "salmon", "lightcoral","orange")

# Model names
model_names <- c("Naive Bayes", "Logistic Regression", "SVM", "Random Forest","Ensample")

# Create a data frame
auc_data <- data.frame(Model = model_names, AUC = auc_values)

# Plot the bar plot
barplot(auc_data$AUC, names.arg = auc_data$Model, col = custom_colors, main = "AUC Comparison",
        ylab = "AUC", ylim = c(0, 1), beside = TRUE)


# Access the confusion matrix for each fold
confusion_matrices_rf <- rf_model$results$confusionMatrix

# Print confusion matrices for each fold
print(confusion_matrices_rf)

# Calculate F1-score
cat("naive_bayes_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,naive_bayes_preds)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, naive_bayes_preds)
cat("F1-score:", f1_score, "\n")

cat("logistic_regression_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,logistic_regression_preds)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, logistic_regression_preds)
cat("F1-score:", f1_score, "\n")

cat("svm_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,svm_preds)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, svm_preds)
cat("F1-score:", f1_score, "\n")

cat("rf_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,rf_preds)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, rf_preds)
cat("F1-score:", f1_score, "\n")

cat("ensemble_preds")
mae_mse_lr <- calculate_mae_mse(test_data$Exited,ensemble_preds$majority_vote)
print(mae_mse_lr)
f1_score <- calculate_f1_score(test_data$Exited, ensemble_preds$majority_vot)
cat("F1-score:", f1_score, "\n")

library(caret)

# Create a random forest model
rf_model <- train(
  Exited ~ .,
  data = train_data,  # Assuming you have balanced data
  method = "rf",  # Random Forest
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross-validation
  preProcess = c("center", "scale")  # Standardize the data
)

# Print the cross-validated results
print(rf_model)

# Access the performance metrics
summary(rf_model)






