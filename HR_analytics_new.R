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
install.packages("pROC")
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

hr_analytics_file_path <- "E:\\NCI\\Data Mining & Machine Learning 1\\Projects\\aug_train.csv"
HR_analytics <- read.csv(hr_analytics_file_path, na.strings = c("", "NA", "N/A", "9999"))



# Get null counts for data
HR_analytics_null_count <- colSums(is.na(HR_analytics))


# Print the null counts
print(HR_analytics_null_count)
#viewing the data
str(HR_analytics)
summary(HR_analytics)


#############################HR_Analytics###############################
str(HR_analytics)
dim(HR_analytics)
head(HR_analytics)
print(HR_analytics_null_count)

########EDA###########################
# Reset the plotting layout
# Set up a 3x3 grid
par(mfrow=c(3, 3))

# Plot 1
barplot(prop.table(table(HR_analytics$target)),
        col = rainbow(2),
        main = "Target Distribution")

# Plot 2
barplot(prop.table(table(HR_analytics$gender)),
        col = rainbow(3),
        main = "Gender Distribution")

# Plot 3
barplot(prop.table(table(HR_analytics$relevent_experience)),
        col = rainbow(4),
        main = "Relevant_experience Distribution")

# Plot 4
barplot(prop.table(table(HR_analytics$enrolled_university)),
        col = rainbow(7),
        main = "Enrolled_university Distribution")

# Plot 5
education_level_counts <- table(HR_analytics$education_level)
plot(education_level_counts, type = "o", col = rainbow(length(education_level_counts)),
     main = "Education_level Distribution", xlab = "Education_level", ylab = "Count")

# Plot 6
major_discipline_counts <- table(HR_analytics$major_discipline)
pie(major_discipline_counts, labels = names(major_discipline_counts),
    col = rainbow(length(major_discipline_counts)),
    main = "Major_discipline Distribution")

# Plot 7
barplot(prop.table(table(HR_analytics$experience)),
        col = rainbow(1),
        main = "Experience Distribution")

# Plot 8
barplot(prop.table(table(HR_analytics$company_size)),
        col = rainbow(1),
        main = "Company size Distribution")

# Plot 9
barplot(prop.table(table(HR_analytics$company_type)),
        col = rainbow(1),
        main = "Company type Distribution")

# Reset the layout to the default
par(mfrow=c(1, 1))



library(dplyr)

# Assuming your data frame is named category_notNull

HR_analytics <- HR_analytics %>%
  mutate(company_size = ifelse(company_size == '10/49', '10-49', company_size))
#head(HR_analytics)

# Define the Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply Mode function to fill missing values for all columns
HR_analytics_notnull <- HR_analytics %>%
  mutate_all(~ ifelse(is.na(.), Mode(.), .))

# Assuming HR_analytics_notnull is your dataframe
company_size_mode <- names(sort(table(HR_analytics$company_size), decreasing = TRUE))[1]

# Print the mode
print(company_size_mode)

HR_analytics_notnull$company_size[is.na(HR_analytics_notnull$company_size) | HR_analytics_notnull$company_size == '' | HR_analytics_notnull$company_size == 'NULL'] <- company_size_mode
head(HR_analytics_notnull)

colSums(is.na(HR_analytics))
colSums(is.na(HR_analytics_notnull))

head(HR_analytics_notnull)
str(HR_analytics_notnull)

numeric <- HR_analytics_notnull %>% select_if(is.numeric)
category <- HR_analytics_notnull %>% select_if(is.character)

#numeric
#colSums(is.na(category))
#print(names(numeric))
#print(names(category))
#print(class(category))
#print(class(numeric))


# Function to perform chi-square test for given categorical columns
chi_square_test <- function(data, categorical_columns) {
  chi2_array <- c()
  p_array <- c()
  
  for (column in categorical_columns) {
    # Create a contingency table
    crosstab <- table(data[[column]], data$target)
    
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
categorical_columns <- c( "city","gender","relevent_experience", "enrolled_university", "education_level","major_discipline","experience","company_size","company_type","last_new_job")
result <- chi_square_test(HR_analytics_notnull, categorical_columns)
print(result)

drop_cat <- c("company_type", "major_discipline", "gender")
category <- category[,!(names(category) %in% drop_cat)]
names(category)

category_n <- category
# Load necessary packages

# Define the order levels
order_levels1 <- c("No", "Primary School", "High School", "Graduate", "Masters", "Phd")
order_levels2 <- c("No", "<1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", ">20")
order_levels3 <- c("No", "<10", "10-49", "50-99", "100-500", "500-999", "1000-4999", "5000-9999", "10000+")
order_levels4 <- c("No", "1", "2", "3", "4", ">4", "never")

library(dplyr)

library(dplyr)

# Assuming your data frame is named category
category_encoded <- category %>%
  mutate(
    education_level = as.numeric(match(education_level, order_levels1)),
    experience = as.numeric(match(experience, order_levels2)),
    company_size = as.numeric(match(company_size, order_levels3)),
    last_new_job = as.numeric(match(last_new_job, order_levels4))
  )

# Display the result
#print(head(category_encoded))

columns_to_encode <- setdiff(names(category), c('education_level', 'experience', 'company_size', 'last_new_job'))
#columns_to_encode


# Define a function for label encoding
label_encode <- function(column) {
  unique_values <- unique(column)
  encoding <- seq_along(unique_values)
  encoded_column <- encoding[match(column, unique_values)]
  return(encoded_column)
}

for (column in columns_to_encode){
  category_encoded[[column]]<- label_encode(column)
}

#str(category_encoded)
#view(category_encoded)
#head(category_encoded)

# Remove 'target'  column from numeric
numeric_subset <- subset(numeric, select = -c(enrollee_id))
print(names(numeric_subset))
processed_HR_data <- cbind(numeric_subset, category_encoded)
names(category_encoded)

# Assuming df is your data frame
names(processed_HR_data) <- gsub(" ", "_", names(processed_HR_data))



library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(caTools)
library(ROCR)

str(processed_HR_data)

# Specify columns to scale
processed_HR_data[["training_hours"]] <- scale(processed_HR_data[["training_hours"]])

# Split the data into training and testing sets
set.seed(123)
ind <- sample(2, nrow(processed_HR_data), replace = TRUE, prob = c(0.7, 0.3))
train_data <- processed_HR_data[ind == 1, ]
test_data <- processed_HR_data[ind == 2, ]

# View the dimensions of the training and testing sets
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

#names(train_data)
#names(test_data)

lfjob_counts <- table(train_data$target)
print(lfjob_counts)

# Find the minimum count (to balance the classes)
min_count <- min(lfjob_counts)
max_count <- max(lfjob_counts)

# Perform oversampling to balance the classes
balanced_train_data <- ovun.sample(target ~ ., data = train_data, method = "over", N=13476+(max_count-min_count))

# Extract the balanced data frame
balanced_train_data <- balanced_train_data$data

# Print the new counts to verify balancing
new_lfjob_counts <- table(balanced_train_data$target)
print(new_lfjob_counts)

logistic_model <- glm(target ~.,family=binomial(link='logit'),data=balanced_train_data)
summary(logistic_model)
predict_reg <- predict(logistic_model,test_data, type = "response")
#predict_reg

predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test_data$target, predict_reg)


missing_classerr <- mean(predict_reg != test_data$target)
missing_classerr
print(paste('Accuracy =', 1 - missing_classerr))

# install the necessary packages
library(MASS)
library(parsnip)
library(yardstick)
library(pROC)
library(rpart)
library(dplyr)

# Create a decision tree model specification
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

balanced_train_data$target<- as.numeric(balanced_train_data$target)

# Fit the model to the training data
tree_fit <- tree_spec %>%
  fit(target ~ ., data = balanced_train_data)

# Make predictions on the testing data
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred)

# Convert regression to binary classification using a threshold
threshold <- 0.5
binary_predictions <- ifelse(predictions >= threshold, 1, 0)


table(test_data$target,binary_predictions)

missing_classerr_dt <- mean(binary_predictions != test_data$target)
missing_classerr_dt
print(paste('Accuracy =', 1 - missing_classerr))


library(randomForest)
library(datasets)
library(caret)

balanced_train_data$target <- as.factor(balanced_train_data$target)
rf <- randomForest(target ~ ., data = balanced_train_data, proximity = TRUE)
print(rf)

str(train_data)
p1 <- predict(rf, balanced_train_data)
confusionMatrix(p1, balanced_train_data$target)

str(test_data)
p2 <- predict(rf, test_data)
#print(p2)

# Assuming p2 is your predicted values
p2 <- as.factor(p2)

# Convert test_data$target to factor with the same levels
test_data$target <- as.factor(test_data$target)
levels(p2) <- levels(test_data$target)

# Now, you can create the confusion matrix
conf_matrix <- confusionMatrix(p2, test_data$target)
print(conf_matrix)

library(pROC)

table(test_data$target,p2)

missing_classerr_rf <- mean(p2 != test_data$target)
missing_classerr_rf
print(paste('Accuracy =', 1 - missing_classerr))

#ROC curve for all three datasets

roc_lr <- roc(test_data$target, as.numeric(predict_reg), levels = c(0, 1))
roc_dt <- roc(test_data$target, as.numeric(binary_predictions) - 1, levels = c(0, 1))
roc_rf <- roc(test_data$target, as.numeric(p2) - 1, levels = c(0, 1))

# Plot ROC curves
plot(roc_lr, col = "blue", main = "ROC Curves for Different Models", lwd = 2, cex.main = 1.5, cex.lab = 1.3)
lines(roc_dt, col = "red", lwd = 2)
lines(roc_rf, col = "green", lwd = 2)

# Add a legend
legend("bottomright", 
       legend = c("Logistic Regression", "Decion Tree", "Random Forest"), 
       col = c("blue", "red", "green"), 
       lwd = 2, 
       cex = 0.8) 

# Print AUC values
auc_lr <- auc(roc_lr)
auc_dt <- auc(roc_dt)
auc_rf <- auc(roc_rf)

# Display AUC values
cat("AUC for Logistic Regression:", auc_lr, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")
cat("AUC for Descision Tree:", auc_dt, "\n")

# AUC values
auc_values <- c( auc_lr, auc_rf, auc_dt)
# Custom colors for each bar
custom_colors <- c( "lightgreen", "salmon","orange")

# Model names
model_names <- c( "Logistic Regression", "Random Forest","Decision Tree")

# Create a data frame
auc_data <- data.frame(Model = model_names, AUC = auc_values)

# Plot the bar plot
barplot(auc_data$AUC, names.arg = auc_data$Model, col = custom_colors, main = "AUC Comparison",
        ylab = "AUC", ylim = c(0, 1), beside = TRUE)

# Assuming 'test_data' contains your test set and 'predict_reg', 'binary_predictions', 'p2' are the predictions

# Function to calculate F1-score
calculate_f1_score <- function(actual, predicted) {
  # Convert vectors to factors with the same levels
  actual <- factor(actual)
  predicted <- factor(predicted, levels = levels(actual))
  
  # Create a confusion matrix
  conf_matrix <- confusionMatrix(actual, predicted)
  
  # Calculate precision, recall, and F1-score
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(f1)
}


# Function to calculate MAE and MSE
calculate_mae_mse <- function(actual, predicted) {
  # Convert factors to numeric vectors
  actual <- as.numeric((actual))
  predicted <- as.numeric((predicted))
  
  mae <- mean(abs(actual - predicted))
  mse <- mean((actual - predicted)^2)
  
  return(c(MAE = mae, MSE = mse))
}

# Logistic Regression
mae_mse_lr <- calculate_mae_mse(test_data$target, predict_reg)
f1_lr <- calculate_f1_score(test_data$target, ifelse(predict_reg > 0.5, 1, 0))

# Decision Tree
mae_mse_dt <- calculate_mae_mse(test_data$target, binary_predictions)
f1_dt <- calculate_f1_score(test_data$target, binary_predictions)

# Random Forest
mae_mse_rf <- calculate_mae_mse(test_data$target, as.numeric(p2) - 1)
f1_rf <- calculate_f1_score(test_data$target, as.numeric(p2) - 1)

# Print or use the calculated metrics
print("Logistic Regression Metrics:")
print(mae_mse_lr)
print(f1_lr)

print("Decision Tree Metrics:")
print(mae_mse_dt)
print(f1_dt)

print("Random Forest Metrics:")
print(mae_mse_rf)
print(f1_rf)


#############################Unbalanced data ##################################

logistic_model <- glm(target ~.,family=binomial(link='logit'),data=train_data)
summary(logistic_model)
predict_reg <- predict(logistic_model,test_data, type = "response")
predict_reg

predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test_data$target, predict_reg)


missing_classerr <- mean(predict_reg != test_data$target)
missing_classerr
print(paste('Accuracy =', 1 - missing_classerr))

# install the necessary packages
library(MASS)
library(parsnip)
library(yardstick)
library(pROC)
library(rpart)
library(dplyr)

# Create a decision tree model specification
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

train_data$target<- as.numeric(train_data$target)

# Fit the model to the training data
tree_fit <- tree_spec %>%
  fit(target ~ ., data = train_data)

# Make predictions on the testing data
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred)

# Convert regression to binary classification using a threshold
threshold <- 0.5
binary_predictions <- ifelse(predictions >= threshold, 1, 0)


table(test_data$target,binary_predictions)

missing_classerr_dt <- mean(binary_predictions != test_data$target)
missing_classerr_dt
print(paste('Accuracy =', 1 - missing_classerr))


library(randomForest)
library(datasets)
library(caret)

train_data$target <- as.factor(train_data$target)
rf <- randomForest(target ~ ., data = train_data, proximity = TRUE)

print(rf)

str(train_data)
p1 <- predict(rf, train_data)
confusionMatrix(p1, train_data$target)

str(test_data)
p2 <- predict(rf, test_data)
print(p2)

# Assuming p2 is your predicted values
p2 <- as.factor(p2)

# Convert test_data$target to factor with the same levels
test_data$target <- as.factor(test_data$target)
levels(p2) <- levels(test_data$target)

# Now, you can create the confusion matrix
conf_matrix <- confusionMatrix(p2, test_data$target)
print(conf_matrix)

library(pROC)

table(test_data$target,p2)

missing_classerr_rf <- mean(p2 != test_data$target)
#missing_classerr_rf
print(paste('Accuracy =', 1 - missing_classerr))

#ROC curve for all three datasets

roc_lr <- roc(test_data$target, as.numeric(predict_reg), levels = c(0, 1))
roc_dt <- roc(test_data$target, as.numeric(binary_predictions) - 1, levels = c(0, 1))
roc_rf <- roc(test_data$target, as.numeric(p2) - 1, levels = c(0, 1))

# Plot ROC curves
plot(roc_lr, col = "blue", main = "ROC Curves for Different Models", lwd = 2, cex.main = 1.5, cex.lab = 1.3)
lines(roc_dt, col = "red", lwd = 2)
lines(roc_rf, col = "green", lwd = 2)

# Add a legend
legend("bottomright", 
       legend = c("Logistic Regression", "Decion Tree", "Random Forest"), 
       col = c("blue", "red", "green"), 
       lwd = 2, 
       cex = 0.8) 

# Print AUC values
auc_lr <- auc(roc_lr)
auc_dt <- auc(roc_dt)
auc_rf <- auc(roc_rf)

# Display AUC values
cat("AUC for Logistic Regression:", auc_lr, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")
cat("AUC for Descision Tree:", auc_dt, "\n")

# AUC values
auc_values <- c( auc_lr, auc_rf, auc_dt)
# Custom colors for each bar
custom_colors <- c( "lightgreen", "salmon","orange")

# Model names
model_names <- c( "Logistic Regression", "Random Forest","Decision Tree")

# Create a data frame
auc_data <- data.frame(Model = model_names, AUC = auc_values)

# Plot the bar plot
barplot(auc_data$AUC, names.arg = auc_data$Model, col = custom_colors, main = "AUC Comparison",
        ylab = "AUC", ylim = c(0, 1), beside = TRUE)

# Install necessary packages if not already installed
# install.packages("caret")

library(caret)

# Create a random forest model
rf_model <- train(
  target ~ .,
  data = balanced_train_data,  # Assuming you have balanced data
  method = "rf",  # Random Forest
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross-validation
  preProcess = c("center", "scale")  # Standardize the data
)

# Print the cross-validated results
print(rf_model)

# Access the performance metrics
summary(rf_model)

# Access the confusion matrix for each fold
confusion_matrices_rf <- rf_model$results$confusionMatrix

# Print confusion matrices for each fold
print(confusion_matrices_rf)



