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
install.packages("MASS")
install.packages("parsnip")
install.packages("yardstick")
install.packages("pROC")
install.packages("caTools") 
install.packages("ROCR") 
install.packages("kknn")
install.packages("gplots")
library(gplots)
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
dry_bean_file_path <- "E:\\NCI\\Data Mining & Machine Learning 1\\Projects\\Dry_bean.xlsx"

#reading the data files
dry_bean_data <- read.xlsx(dry_bean_file_path, na.strings = c("", "NA", "N/A", "9999"))

#viewing the data
str(dry_bean_data)


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

# Get null counts for data
dry_bean_null_count <- colSums(is.na(dry_bean_data))

# Print the null counts
print(dry_bean_null_count)


############################DRY BEAN DATA ###############################
dry_bean_columns = names(dry_bean_data)

library(ggplot2)

# Assuming Drybean_data is your data frame
# and Class is a categorical variable representing the classes

# Unique classes in the "Class" column
unique_classes <- unique(dry_bean_data$Class)

# Generating a color palette with enough colors
color_palette <- scales::hue_pal()(length(unique_classes))

# Melt the data frame for easier plotting
melted_data <- reshape2::melt(dry_bean_data, id.vars = "Class")

# Loop through each feature and create a separate violin plot
for (feature in unique(melted_data$variable)) {
  plot_data <- melted_data[melted_data$variable == feature, ]
  
  # Create a violin plot
  p <- ggplot(plot_data, aes(x = variable, y = value, fill = Class)) +
    geom_violin(scale = "width", trim = FALSE) +
    scale_fill_manual(values = setNames(color_palette, unique_classes)) +
    labs(title = paste("Class-wise Distribution of", feature), x = "Features", y = "Values") +
    theme_minimal()
  
  # Print the plot
  print(p)
}

# Assuming dry_bean_data is your data frame and dry_bean_columns is a vector of column names
n_columns <- length(dry_bean_columns)
n_rows <- ceiling(n_columns / 3)  # Adjust the number of rows as needed

# Set up a multi-plot layout
par(mfrow=c(n_rows, 3), mar=c(3, 3, 1, 1))

for (column in dry_bean_columns) {
  if (is.numeric(dry_bean_data[[column]])) {
    hist(dry_bean_data[[column]], col = 'blue', border = 'white', main = column)
  } else {
    barplot(table(dry_bean_data[[column]]), col = 'blue', border = 'white', main = column)
  }
}

# Reset the plotting layout
par(mfrow=c(1, 1))

# Set up a multi-plot layout
par(mfrow=c(n_rows, 3), mar=c(3, 3,1,1))

for (column in dry_bean_columns) {
  if (is.numeric(dry_bean_data[[column]])) {
    boxplot(dry_bean_data[[column]], data = dry_bean_data,varwidth = TRUE)
  } else {
    barplot(table(dry_bean_data[[column]]), col = 'blue', border = 'white', main = column)
  }
}


# Reset the plotting layout
par(mfrow=c(1, 1))

# Get counts of 'Class' values
cluster_counts <- table(dry_bean_data$Class)

# Display the counts
print(cluster_counts)

# Extract features (excluding the 'Class' column) for clustering
features <- dry_bean_data[, -which(names(dry_bean_data) == "Class")]

# Standardize the features
scaled_features <- scale(features)


# Apply DBSCAN
dbscan_result <- dbscan(scaled_features, eps = 1, minPts = 5)  # Adjust parameters as needed
dbscan_result

# Identify outliers
outliers_mask <- dbscan_result$cluster == 0  # Outliers have cluster ID -1

class_column <- dry_bean_data[['Class']]
concatenated_data <- cbind(scaled_features, class_column)
colnames(concatenated_data) <- c(colnames(scaled_features), 'Class')

#head(concatenated_data)

# Filter out outliers
filtered_data <- concatenated_data[!outliers_mask, ]
#head(filtered_data)

# Display the filtered data
cat("Original data shape:", dim(dry_bean_data), "\n")
cat("Filtered data shape:", dim(filtered_data), "\n")


str(filtered_data)
filtered_data=data.frame(filtered_data)
str(filtered_data)
#head(filtered_data)


# Get data types of each column
column_types <- sapply(filtered_data, class)
# Display the data types
print(column_types)

# Identify columns to convert (excluding 'Class' column)
columns_to_convert <- setdiff(names(filtered_data), "Class")
columns_to_convert

# Convert selected columns from character to numeric
filtered_data[, columns_to_convert] <- lapply(filtered_data[, columns_to_convert], as.numeric)
#head(filtered_data)

# Display the updated dataframe
str(filtered_data)

# Identify numeric columns (excluding 'Class' column)
numeric_columns <- Filter(is.numeric, filtered_data[, setdiff(names(filtered_data), "Class")])

# Perform PCA on numeric columns
pca_result <- prcomp(numeric_columns, scale. = TRUE)

# Print the summary of PCA
summary(pca_result)

# Reset the plotting layout
par(mfrow=c(1, 1))
fviz_eig(pca_result, addlabels = TRUE)

selected_features <- pca_result$x[, 1:8]

dim(selected_features)
selected_features

# Combine data frames along with column names
processed_data <- cbind(selected_features, filtered_data['Class'])

# Assign column names to the combined data frame
colnames(processed_data) <- c(colnames(selected_features), 'Class')

# Print the resulting data frame
print(processed_data)

# Calculate the correlation matrix
corr_matrix <- cor(numeric_columns)
#corr_matrix

# Create a function to format correlation values as text
format_cor <- function(cor_matrix) {
  format(round(cor_matrix, 2), nsmall = 2)
}

# Create a matrix of formatted correlation values
annot_values <- format_cor(corr_matrix)
#annot_values

pheatmap(corr_matrix, main = 'Correlation Matrix', color = colorRampPalette(c("blue", "white", "red"))(256), 
         scale = "none", display_numbers = TRUE)

principal_df <- processed_data
#head(principal_df)

# Display the class of filtered_data
print(class(principal_df))

# Display the structure of filtered_data
print(str(principal_df))

# Assuming 'filtered_data' is your dataframe
class_counts <- table(principal_df$Class)
print(class_counts)

# Create a count plot using ggplot2
ggplot(principal_df, aes(x = Class)) +
  geom_bar(fill = c('#c7dfeb', '#9ebbd7', '#7d92bf', '#636a9f', '#494373', '#2b1f3e', '#2b1f2e')) +
  labs(title = "Count Plot", x = "Class", y = "Count") +
  theme_minimal()

principal_df$Class <- as.factor(principal_df$Class)

set.seed(123)
ind <- sample(2, nrow(principal_df), replace = TRUE, prob = c(0.7, 0.3))
train <- principal_df[ind==1,]
test <- principal_df[ind==2,]

ind_wdr <- sample(2, nrow(filtered_data), replace = TRUE, prob = c(0.7, 0.3))
train_wdr <- principal_df[ind_wdr==1,]
test_wdr <- principal_df[ind_wdr==2,]
train_wdr$Class <- as.factor(train_wdr$Class)
test_wdr$Class <- as.factor(test_wdr$Class)

table(train$Class)
prop.table(table(train$Class))

# Load necessary libraries
library(randomForest)
library(class)
library(e1071)
library(xgboost)
library(kknn)


# Function to train and evaluate multiple models
train_and_evaluate_models <- function(train, test) {
  
  # List to store results
  results <- list()
  
  # Models to evaluate
  models <- c("Random Forest", "k-Nearest Neighbors", "Naive Bayes", "XGBoost")
  
  for (model_name in models) {
    cat(paste("Training and evaluating", model_name, "...\n"))
    
    # Train the model
    if (model_name == "Random Forest") {
      model <- randomForest(Class ~ ., data = train)
    } else if (model_name == "k-Nearest Neighbors") {
      model <- kknn(Class ~ ., train, test, k = 5)
    } else if (model_name == "Naive Bayes") {
      model <- naiveBayes(Class ~ ., data = train)
    } 
    
    # Predictions on the test set
    predictions <- predict(model, newdata = test)
    
    # Confusion matrix for evaluation
    confusion_matrix <- table(predictions, test$Class)
    
    # Calculate accuracy
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    # Print the evaluation result with asterisks
    cat(paste("**********", model_name, "**********\n"))
    cat("Accuracy:", accuracy, "\n")
    print(confusion_matrix)
    cat("\n\n")
    
    # Store the model and predictions in the results list
    results[[model_name]] <- list(model = model, predictions = predictions)
  }
  return(results)
}

# Example usage
results <- train_and_evaluate_models(train, test)
model_rf <- results[["Random Forest"]][["model"]]
predictions_rf <- results[["Random Forest"]][["predictions"]]
model_Kknn <- results[["k-Nearest Neighbors"]][["model"]]
predictions_Kknn <- results[["k-Nearest Neighbors"]][["predictions"]]
model_nb <- results[["Naive Bayes"]][["model"]]
predictions_nb <- results[["Naive Bayes"]][["predictions"]]
model_xg <- results[["XGBoost"]][["model"]]
predictions_xg <- results[["XGBoost"]][["predictions"]]

results_wdr <- train_and_evaluate_models(train_wdr, test_wdr)
model_rf_wdr <- results_wdr[["Random Forest"]][["model"]]
predictions_rf_wdr <- results_wdr[["Random Forest"]][["predictions"]]
model_Kknn_wdr <- results_wdr[["k-Nearest Neighbors"]][["model"]]
predictions_Kknn_wdr <- results_wdr[["k-Nearest Neighbors"]][["predictions"]]
model_nb_wdr <- results_wdr[["Naive Bayes"]][["model"]]
predictions_nb_wdr <- results_wdr[["Naive Bayes"]][["predictions"]]
model_xg_wdr <- results_wdr[["XGBoost"]][["model"]]
predictions_xg_wdr <- results_wdr[["XGBoost"]][["predictions"]]

# Combine predictions into a data frame
ensemble_preds <- data.frame(
  rf = predictions_rf,
  Kknn = predictions_Kknn,
  nb = predictions_nb,
  xg = predictions_xg
)


# Create a new column in the data frame to store the majority voted predictions
ensemble_preds$majority_vote <- apply(ensemble_preds, 1, function(row) {
  # Count occurrences of each class in the row
  class_counts <- table(row)
  
  # Find the class with the maximum count
  majority_class <- as.character(names(class_counts)[which.max(class_counts)])
  
  return(majority_class)
})


# Convert majority_vote to a factor with correct levels
ensemble_preds$majority_vote <- factor(ensemble_preds$majority_vote, levels = levels(test$Class))

# Check the lengths of vectors
length(ensemble_preds$majority_vote)
length(test$Class)

# Compare the levels of the factors
levels(ensemble_preds$majority_vote)
levels(test$Class)

library(caret) 
# Print the majority voted predictions
conf_matrix <- confusionMatrix(ensemble_preds$majority_vote, test$Class)
accuracy <- conf_matrix$overall["Accuracy"]
cat(paste("********** Ensemble model **********\n"))
cat("Accuracy:", accuracy, "\n")
#print(conf_matrix)
cat("\n\n")

# Example for Random Forest
conf_matrix_rf <- confusionMatrix(predictions_rf, test$Class)
#print(conf_matrix_rf)
# Access the confusion matrix
matrix_rf <- conf_matrix_rf$table


# Plot the confusion matrix using heatmap.2
heatmap.2(matrix_rf, 
          col = c("lightblue", "lightcoral"), 
          main = "Confusion Matrix Ran_for",
          xlab = "Predicted",
          ylab = "Actual",
          trace = "none",    # Remove diagonal line
          density.info = "none",  # Remove density plot
          key = TRUE,        # Show color key
          keysize = 1.5,     # Adjust key size
          margins = c(10,10), # Adjust margins
          cellnote = matrix_rf, # Add annotation values
          notecol = "black",  # Annotation text color
          cexRow = 1,        # Row label size
          cexCol = 1         # Column label size
)

# Example for XGBoost
conf_matrix_xg <- confusionMatrix(predictions_xg, test$Class)
#print(conf_matrix_xg)
# Access the confusion matrix
matrix_xg <- conf_matrix_xg$table

# Plot the confusion matrix using heatmap.2
heatmap.2(matrix_xg, 
          col = c("lightblue", "lightcoral"), 
          main = "Confusion Matrix XGB",
          xlab = "Predicted",
          ylab = "Actual",
          trace = "none",    # Remove diagonal line
          density.info = "none",  # Remove density plot
          key = TRUE,        # Show color key
          keysize = 1.5,     # Adjust key size
          margins = c(10,10), # Adjust margins
          cellnote = matrix_xg, # Add annotation values
          notecol = "black",  # Annotation text color
          cexRow = 1,        # Row label size
          cexCol = 1         # Column label size
)
#class(matrix_xg)


# Example for k-Nearest Neighbors
par(mfrow=c(1, 1))
conf_matrix_Kknn <- confusionMatrix(predictions_Kknn, test$Class)
#print(conf_matrix_Kknn)

# Access the confusion matrix
matrix_kkn <- conf_matrix_Kknn$table

# Plot the confusion matrix using heatmap.2
heatmap.2(matrix_kkn, 
          col = c("lightblue", "lightcoral"), 
          main = "Confusion Matrix KNN",
          xlab = "Predicted",
          ylab = "Actual",
          trace = "none",    # Remove diagonal line
          density.info = "none",  # Remove density plot
          key = TRUE,        # Show color key
          keysize = 1.5,     # Adjust key size
          margins = c(10,10), # Adjust margins
          cellnote = matrix_kkn, # Add annotation values
          notecol = "black",  # Annotation text color
          cexRow = 1,        # Row label size
          cexCol = 1         # Column label size
)


# Example for Ensemble
par(mfrow=c(1, 1))
conf_matrix_nb <- confusionMatrix(predictions_nb, test$Class)
#print(conf_matrix_nb)
# Access the confusion matrix
matrix_nb <- conf_matrix_nb$table

# Plot the confusion matrix using heatmap.2
heatmap.2(matrix_nb, 
          col = c("lightblue", "lightcoral"), 
          main = "Confusion Matrix Naive Bayes",
          xlab = "Predicted",
          ylab = "Actual",
          trace = "none",    # Remove diagonal line
          density.info = "none",  # Remove density plot
          key = TRUE,        # Show color key
          keysize = 1.5,     # Adjust key size
          margins = c(10,10), # Adjust margins
          cellnote = matrix_nb, # Add annotation values
          notecol = "black",  # Annotation text color
          cexRow = 1,        # Row label size
          cexCol = 1         # Column label size
)

# Example for Naive Bayes
par(mfrow=c(1, 1))

# Access the confusion matrix
matrix_en <- conf_matrix$table

# Plot the confusion matrix using heatmap.2
heatmap.2(matrix_en, 
          col = c("lightblue", "lightcoral"), 
          main = "Confusion Matrix Ensamble",
          xlab = "Predicted",
          ylab = "Actual",
          trace = "none",    # Remove diagonal line
          density.info = "none",  # Remove density plot
          key = TRUE,        # Show color key
          keysize = 1.5,     # Adjust key size
          margins = c(10,10), # Adjust margins
          cellnote = matrix_en, # Add annotation values
          notecol = "black",  # Annotation text color
          cexRow = 1,        # Row label size
          cexCol = 1         # Column label size
)


# Extract unique classes from the test$Class variable
class_labels <- unique(test$Class)

# Function to calculate class-wise accuracy
class_wise_accuracy <- function(predictions, actual) {
  confusion_matrix <- table(predictions, actual)
  accuracy <- diag(confusion_matrix) / rowSums(confusion_matrix)
  return(accuracy)
}

# Function to plot side-by-side barplot for class-wise accuracy across models
plot_class_wise_accuracy <- function(models, predictions_list, actual, class_labels) {
  # Create an empty data frame to store accuracy values
  accuracy_df <- data.frame(Class = class_labels)
  
  # Calculate accuracy for each model and store in the data frame
  for (i in seq_along(models)) {
    model_name <- models[i]
    predictions <- predictions_list[[i]]
    
    accuracy <- class_wise_accuracy(predictions, actual)
    
    # Add accuracy values to the data frame
    accuracy_df[[model_name]] <- accuracy
  }
  
  # Reshape data frame for plotting
  accuracy_melted <- reshape2::melt(accuracy_df, id.vars = "Class")
  
  # Plot side-by-side barplot
  ggplot(accuracy_melted, aes(x = Class, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(title = "Class-wise Accuracy Across Models",
         x = "Class", y = "Accuracy") +
    scale_fill_manual(values = rainbow(length(models))) +
    theme_minimal()
}

# Example models and predictions (replace with your actual models and predictions)
models <- c("Random Forest", "k-Nearest Neighbors", "Naive Bayes", "Ensemble","XGBoost")
predictions_list <- list(predictions_rf, predictions_Kknn, predictions_nb, ensemble_preds$majority_vote,predictions_xg)

models_wdr <- c("Random Forest", "k-Nearest Neighbors", "Naive Bayes","XGBoost")
predictions_list_wdr <- list(predictions_rf_wdr, predictions_Kknn_wdr, predictions_nb_wdr, predictions_xg_wdr)


# Plot the side-by-side barplot for class-wise accuracy
plot_class_wise_accuracy(models, predictions_list, test$Class, class_labels)
par(mfrow=c(1, 1))
# Plot the side-by-side barplot for class-wise accuracy
plot_class_wise_accuracy(models_wdr, predictions_list_wdr, test_wdr$Class, class_labels)


# Function to calculate overall accuracy for each model
overall_accuracy <- function(predictions, actual) {
  confusion_matrix <- table(predictions, actual)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(accuracy)
}

# Calculate overall accuracy for each model
accuracy_values <- sapply(predictions_list, overall_accuracy, actual = test$Class)
accuracy_values_wdr <- sapply(predictions_list_wdr, overall_accuracy, actual = test_wdr$Class)

# Plot bar plot for overall accuracy with decreased width and accuracy values
barplot(accuracy_values, names.arg = models, col = rainbow(length(models)),
        main = "Overall Accuracy for Each Model",
        xlab = "Models", ylab = "Accuracy",
        width = 0.5,  # Decrease the width of bars
        ylim = c(0, 1),  # Set y-axis limits
        beside = TRUE)  # Plot bars side by side

# Install and load necessary packages

library(knitr)

# Plot bar plot for overall accuracy with decreased width and accuracy values
barplot(accuracy_values_wdr, names.arg = models_wdr, col = rainbow(length(models_wdr)),
        main = "Overall Accuracy for Each Model",
        xlab = "Models", ylab = "Accuracy",
        width = 0.5,  # Decrease the width of bars
        ylim = c(0, 1),  # Set y-axis limits
        beside = TRUE)  # Plot bars side by side

# Install and load necessary packages

library(knitr)



# Create a data frame to store the accuracy values
performance_table <- data.frame(Class = class_labels)

# Calculate accuracy for each class and each model
for (i in seq_along(models)) {
  model_name <- models[i]
  predictions <- predictions_list[[i]]
  
  accuracy <- class_wise_accuracy(predictions, test$Class)
  
  # Add accuracy values to the data frame
  performance_table[[model_name]] <- accuracy
}

# Create a data frame to store the accuracy values
performance_table_wdr <- data.frame(Class = class_labels)

# Calculate accuracy for each class and each model
for (i in seq_along(models_wdr)) {
  model_name <- models_wdr[i]
  predictions <- predictions_list_wdr[[i]]
  
  accuracy <- class_wise_accuracy(predictions, test_wdr$Class)
  
  # Add accuracy values to the data frame
  performance_table_wdr[[model_name]] <- accuracy
}

# Print the performance comparison table
kable(performance_table, format = "markdown", digits = 3)

# Print the performance comparison table
kable(performance_table_wdr, format = "markdown", digits = 3)

library(caret)

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




cat("ensemble_preds")
mae_mse_ep <- calculate_mae_mse(test$Class,ensemble_preds$majority_vote)
print(mae_mse_ep)
f1_score <- calculate_f1_score(test$Class, ensemble_preds$majority_vote)
cat("F1-score:", f1_score, "\n")

cat("predictions_xg")
mae_mse_xg <- calculate_mae_mse(test$Class,predictions_xg)
print(mae_mse_xg)
f1_score <- calculate_f1_score(test$Class, predictions_xg)
cat("F1-score:", f1_score, "\n")

cat("predictions_nb")
mae_mse_nb <- calculate_mae_mse(test$Class,predictions_nb)
print(mae_mse_nb)
f1_score <- calculate_f1_score(test$Class, predictions_nb)
cat("F1-score:", f1_score, "\n")

cat("predictions_Kknn")
mae_mse_knn <- calculate_mae_mse(test$Class,predictions_Kknn)
print(mae_mse_knn)
f1_score <- calculate_f1_score(test$Class, predictions_Kknn)
cat("F1-score:", f1_score, "\n")

cat("predictions_rf")
mae_mse_rf <- calculate_mae_mse(test$Class,predictions_rf)
print(mae_mse_rf)
f1_score <- calculate_f1_score(test$Class, predictions_rf)
cat("F1-score:", f1_score, "\n")



# Calculate F1-score

cat("predictions_xg_wdr")
mae_mse_xg <- calculate_mae_mse(test_wdr$Class,predictions_xg_wdr)
print(mae_mse_xg)
f1_score <- calculate_f1_score(test_wdr$Class, predictions_xg_wdr)
cat("F1-score:", f1_score, "\n")

cat("predictions_nb")
mae_mse_nb <- calculate_mae_mse(test_wdr$Class,predictions_nb_wdr)
print(mae_mse_nb)
f1_score <- calculate_f1_score(test_wdr$Class, predictions_nb_wdr)
cat("F1-score:", f1_score, "\n")

cat("predictions_Kknn")
mae_mse_knn <- calculate_mae_mse(test_wdr$Class,predictions_Kknn_wdr)
print(mae_mse_knn)
f1_score <- calculate_f1_score(test_wdr$Class, predictions_Kknn_wdr)
cat("F1-score:", f1_score, "\n")

cat("predictions_rf")
mae_mse_rf <- calculate_mae_mse(test_wdr$Class,predictions_rf_wdr)
print(mae_mse_rf)
f1_score <- calculate_f1_score(test_wdr$Class, predictions_rf_wdr)
cat("F1-score:", f1_score, "\n")



# Create a random forest model
rf_model <- train(
  Class ~ .,
  data = train,  # Assuming you have balanced data
  method = "rf",  # Random Forest
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross-validation
  preProcess = c("center", "scale")  # Standardize the data
)

# Print the cross-validated results
print(rf_model)

# Access the performance metrics
summary(rf_model)

