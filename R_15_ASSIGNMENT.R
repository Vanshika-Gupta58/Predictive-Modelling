# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# Load the dataset
data <- read.csv("C:/Users/Vanshika Gupta/Desktop/ecommerce_sales_dataset_with_return_indicator.csv")
View(data)

# View the structure and summary of the dataset
str(data)
summary(data)

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Handle missing values
# Example: Fill numeric missing values with median and categorical with mode
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
  } else {
    data[[col]][is.na(data[[col]])] <- as.character(names(sort(table(data[[col]]), decreasing = TRUE))[1])
  }
}

# Summary after handling missing values
summary(data)

# Detect and handle outliers
# Using the IQR method
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    data[[col]][data[[col]] < lower_bound] <- lower_bound
    data[[col]][data[[col]] > upper_bound] <- upper_bound
  }
}

# Convert necessary columns to appropriate data types
data$Return <- as.factor(data$Return)
data$Return.Indicator <- as.factor(data$Return.Indicator)
data$Date.of.Purchase <- as.Date(data$Date.of.Purchase, format="%d-%m-%Y")

# Exploratory Data Analysis (EDA)

# 1. Summary of the dataset
summary(data)

# 2. Distribution of Returns
ggplot(data, aes(x = Return)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Returns", x = "Return Status", y = "Count")

# 3. Average Price by Category for Returned vs Non-Returned Products
data %>%
  group_by(Category, Return) %>%
  summarize(Average_Price = mean(Price)) %>%
  ggplot(aes(x = Category, y = Average_Price, fill = Return)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price by Category and Return Status", x = "Category", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Quantity vs Return Status
ggplot(data, aes(x = Quantity, fill = Return)) +
  geom_histogram(bins = 10, position = "dodge", alpha = 0.7) +
  labs(title = "Quantity Distribution by Return Status", x = "Quantity", y = "Count")



# Visualize distributions of numeric features
numeric_cols <- names(data)[sapply(data, is.numeric)]
for (col in numeric_cols) {
  print(ggplot(data, aes_string(x = col)) + 
          geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
          ggtitle(paste("Distribution of", col)) + 
          theme_minimal())
}

# Visualize relationships between features
# Example: Scatter plot for two numeric variables
if (length(numeric_cols) > 1) {
  print(ggplot(data, aes_string(x = numeric_cols[1], y = numeric_cols[2])) + 
          geom_point(alpha = 0.5) + 
          ggtitle(paste("Scatter plot of", numeric_cols[1], "and", numeric_cols[2])) + 
          theme_minimal())
}

# Correlation matrix for numeric features
cor_matrix <- cor(data[numeric_cols], use = "complete.obs")
print(cor_matrix)

# Plot correlation heatmap
corrplot(cor_matrix, method = "color", tl.col = "black", addCoef.col = "black", number.cex = 0.7)

# Save the cleaned dataset
write.csv(data, "cleaned_data.csv", row.names = FALSE)


# Prediction Model


# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$Return.Indicator, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a Random Forest model
rf_model <- randomForest(Return.Indicator ~ Category + Price + Discount + Quantity + Region + Customer.Segment + Payment.Method,
                         data = train_data, importance = TRUE, ntree = 100)

# Print the confusion matrix and model importance
print(confusion_matrix)
varImpPlot(rf_model)

# Save the Random Forest model
saveRDS(rf_model, "random_forest_model.rds")


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)


# Convert necessary columns to appropriate data types
data$Return <- as.factor(data$Return)
data$Return.Indicator <- as.factor(data$Return.Indicator)
data$Date.of.Purchase <- as.Date(data$Date.of.Purchase, format="%d-%m-%Y")


# Feature selection for linear regression (consider using correlation analysis or other techniques)
# Assuming Price and Discount are selected for linear regression
model_formula <- Return.Indicator ~ Price + Discount

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$Return.Indicator, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Print the confusion matrix
print("Linear Regression Confusion Matrix")
print(confusion_matrix)

# Train a logistic regression model (suitable for binary classification)
library(e1071)
glm_model <- glm(Return.Indicator ~ Price + Discount, family = binomial, data = train_data)

# Evaluate the model
glm_predictions <- predict(glm_model, newdata = test_data, type = "response")  # Get probabilities
glm_class_predictions <- factor(glm_predictions > 0.5)  # Threshold for predicting return

confusion_matrix <- confusionMatrix(glm_class_predictions, test_data$Return.Indicator)

# Print the confusion matrix
print("Logistic Regression Confusion Matrix")
print(confusion_matrix)

