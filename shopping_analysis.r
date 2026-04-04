# =============================================================================
# CUSTOMER NEXT PURCHASE PREDICTION ANALYSIS
# 
# BUSINESS OBJECTIVE:
# Understanding and predicting customer purchase behaviour enables the business to:
# - Proactively engage customers
# - Better allocate marketing resources
# - Tailor promotions
# - Segment customers by value/likelihood
# - Reduce churn and increase lifetime value
#
# ANALYTICS VALUE:
# This dataset provides rich visualization of demographics, purchase history,
# and behaviour patterns, along with predictive modelling capabilities for
# classification and multiview data visualisation.
# =============================================================================

# 1. LOAD NECESSARY LIBRARIES
# =============================================================================
library(tidyverse)    # Data manipulation and visualization
library(ggplot2)      # Advanced plotting
library(dplyr)        # Data wrangling
library(lubridate)    # Date handling
library(caret)        # Machine learning tools
library(rpart)        # Decision trees
library(rpart.plot)   # Decision tree visualization
library(pROC)         # ROC curves
library(corrplot)     # Correlation plots

# 2. LOAD AND EXAMINE THE DATASET
# =============================================================================
# Read the dataset
df <- read.csv("shopping_behavior.csv", stringsAsFactors = TRUE)

# Initial exploration
cat("Dataset Dimensions:\n")
print(dim(df))
cat("\nColumn Names:\n")
print(names(df))
cat("\nFirst Few Rows:\n")
print(head(df))
cat("\nDataset Structure:\n")
str(df)

# 3. DATA CLEANING AND TARGET VARIABLE CREATION
# =============================================================================
cat("\n=== DATA CLEANING ===\n")

# Check for missing values
cat("\nMissing Values per Column:\n")
missing_counts <- colSums(is.na(df))
print(missing_counts)

# Remove rows with missing values
df_clean <- na.omit(df)
cat("\nRows after removing missing values:", nrow(df_clean), "\n")

# Check for duplicates
cat("Duplicate rows:", sum(duplicated(df_clean)), "\n")
df_clean <- df_clean[!duplicated(df_clean), ]

# Convert categorical variables to factors
categorical_vars <- c("Gender", "Category", "Location", "Size", 
                      "Color", "Season", "Subscription.Status", 
                      "Shipping.Type", "Discount.Applied", 
                      "Promo.Code.Used", "Payment.Method")

for (var in categorical_vars) {
  if (var %in% names(df_clean)) {
    df_clean[[var]] <- as.factor(df_clean[[var]])
  }
}

# 3.1 CREATE TARGET VARIABLE: Next_Purchase_Flag
# =============================================================================
cat("\n=== CREATING TARGET VARIABLE: Next_Purchase_Flag ===\n")

# Strategy: Use Previous.Purchases as a proxy for purchase behavior
# Logic: Customers with 2+ previous purchases are likely to return (will purchase again)
#        Customers with 0-1 previous purchases are less likely to return

# Alternative approach if you have Review.Rating:
# High satisfaction (rating 4-5) + previous purchases = likely to return

if ("Previous.Purchases" %in% names(df_clean)) {
  # Approach 1: Based on purchase history
  # Threshold: Customers with 2+ purchases are more likely to make next purchase
  purchase_threshold <- 2
  
  df_clean$Next_Purchase_Flag <- ifelse(
    df_clean$Previous.Purchases >= purchase_threshold, 
    1,  # Yes - will make next purchase
    0   # No - less likely to make next purchase
  )
  
  cat("Using Previous.Purchases for target variable\n")
  cat("Threshold:", purchase_threshold, "purchases\n")
}

# Enhanced logic: Combine multiple factors
if ("Review.Rating" %in% names(df_clean) && "Subscription.Status" %in% names(df_clean)) {
  # More sophisticated approach
  df_clean$Next_Purchase_Flag <- ifelse(
    (df_clean$Previous.Purchases >= 2) |
    (df_clean$Review.Rating >= 4 & df_clean$Subscription.Status == "Yes") |
    (df_clean$Previous.Purchases >= 1 & df_clean$Review.Rating >= 4),
    1,  # Likely to purchase again
    0   # Less likely to purchase again
  )
  cat("Using combined logic: Previous.Purchases + Review.Rating + Subscription.Status\n")
}

# Convert to factor for classification
df_clean$Next_Purchase_Flag <- as.factor(df_clean$Next_Purchase_Flag)

# Check distribution of target variable
cat("\nTarget Variable Distribution:\n")
target_dist <- table(df_clean$Next_Purchase_Flag)
print(target_dist)
cat("Proportion:\n")
print(prop.table(target_dist))

# Ensure balanced or note imbalance
if (min(target_dist) / max(target_dist) < 0.3) {
  cat("\nWARNING: Imbalanced classes detected!\n")
  cat("Consider using balanced sampling or adjusting thresholds.\n")
}

cat("\nCleaned dataset summary:\n")
print(summary(df_clean))

# 4. DATA EXPLORATION & VISUALIZATION
# =============================================================================
cat("\n=== DATA EXPLORATION ===\n")

# Summary statistics for numeric variables
numeric_vars <- names(df_clean)[sapply(df_clean, is.numeric)]
cat("\nNumeric Variables Summary:\n")
print(summary(df_clean[, numeric_vars]))

# 4.1 ANALYZE TARGET VARIABLE BY FEATURES
# =============================================================================

# Previous Purchases by Next Purchase Flag
if ("Previous.Purchases" %in% names(df_clean)) {
  cat("\nPrevious Purchases by Next Purchase Flag:\n")
  print(aggregate(Previous.Purchases ~ Next_Purchase_Flag, 
                  data = df_clean, 
                  FUN = function(x) c(Mean = mean(x), SD = sd(x))))
}

# Review Rating by Next Purchase Flag
if ("Review.Rating" %in% names(df_clean)) {
  cat("\nReview Rating by Next Purchase Flag:\n")
  print(aggregate(Review.Rating ~ Next_Purchase_Flag, 
                  data = df_clean, 
                  FUN = function(x) c(Mean = mean(x), SD = sd(x))))
}

# 4.2 BOX PLOTS for Outlier Detection
# =============================================================================
cat("\nCreating boxplots for numeric variables...\n")

# Boxplot for Purchase Amount
if ("Purchase.Amount..USD." %in% names(df_clean)) {
  png("boxplot_purchase_amount.png", width = 800, height = 600)
  boxplot(df_clean$Purchase.Amount..USD.,
          main = "Boxplot of Purchase Amount (USD)",
          ylab = "Purchase Amount (USD)",
          col = "lightblue",
          border = "darkblue")
  dev.off()
}

# Boxplot for Previous Purchases by Target
if ("Previous.Purchases" %in% names(df_clean)) {
  png("boxplot_previous_purchases.png", width = 800, height = 600)
  boxplot(Previous.Purchases ~ Next_Purchase_Flag,
          data = df_clean,
          main = "Previous Purchases by Next Purchase Flag",
          xlab = "Next Purchase Flag (0 = No, 1 = Yes)",
          ylab = "Previous Purchases",
          col = c("lightcoral", "lightgreen"),
          border = "darkblue")
  dev.off()
}

# Boxplot for Age
if ("Age" %in% names(df_clean)) {
  png("boxplot_age.png", width = 800, height = 600)
  boxplot(df_clean$Age,
          main = "Boxplot of Customer Age",
          ylab = "Age",
          col = "lightgreen",
          border = "darkgreen")
  dev.off()
}

# 4.3 BAR PLOTS for Categorical Variables
# =============================================================================
cat("\nCreating bar plots for categorical variables...\n")

# Next Purchase Flag distribution
png("barplot_target_distribution.png", width = 800, height = 600)
ggplot(df_clean, aes(x = Next_Purchase_Flag, fill = Next_Purchase_Flag)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Next Purchase Flag",
       x = "Next Purchase Flag (0 = No, 1 = Yes)",
       y = "Count") +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#2ecc71")) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
dev.off()

# Subscription Status by Next Purchase Flag
if ("Subscription.Status" %in% names(df_clean)) {
  png("barplot_subscription_by_target.png", width = 800, height = 600)
  ggplot(df_clean, aes(x = Subscription.Status, fill = Next_Purchase_Flag)) +
    geom_bar(position = "fill") +
    theme_minimal() +
    labs(title = "Next Purchase Flag by Subscription Status",
         x = "Subscription Status",
         y = "Proportion",
         fill = "Next Purchase") +
    scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#2ecc71"))
  dev.off()
}

# Category distribution
if ("Category" %in% names(df_clean)) {
  png("barplot_category.png", width = 1000, height = 600)
  ggplot(df_clean, aes(x = reorder(Category, Category, function(x) -length(x)), 
                       fill = Category)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Product Categories Distribution",
         x = "Category",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}

# 4.4 HISTOGRAMS
# =============================================================================

# Previous Purchases distribution
if ("Previous.Purchases" %in% names(df_clean)) {
  png("histogram_previous_purchases.png", width = 800, height = 600)
  hist(df_clean$Previous.Purchases,
       main = "Distribution of Previous Purchases",
       xlab = "Number of Previous Purchases",
       col = "steelblue",
       breaks = 20)
  dev.off()
}

# Review Rating distribution
if ("Review.Rating" %in% names(df_clean)) {
  png("histogram_review_rating.png", width = 800, height = 600)
  hist(df_clean$Review.Rating,
       main = "Distribution of Review Ratings",
       xlab = "Review Rating (1-5)",
       col = "coral",
       breaks = 5)
  dev.off()
}

# 5. DECISION TREE ANALYSIS
# =============================================================================
cat("\n=== DECISION TREE ANALYSIS ===\n")

# Split data into training and testing sets (70/30)
set.seed(123)
trainIndex <- createDataPartition(df_clean$Next_Purchase_Flag, p = 0.7, list = FALSE)
train_data <- df_clean[trainIndex, ]
test_data <- df_clean[-trainIndex, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")
cat("Training set - Next Purchase Flag distribution:\n")
print(table(train_data$Next_Purchase_Flag))
cat("Testing set - Next Purchase Flag distribution:\n")
print(table(test_data$Next_Purchase_Flag))

# Build decision tree model
# Exclude target variable, Customer.ID, and Previous.Purchases (used to create target)
exclude_vars <- c("Next_Purchase_Flag", "Customer.ID", "Previous.Purchases")
predictors <- setdiff(names(df_clean), exclude_vars)

# Include only variables that exist in the data
predictors <- predictors[predictors %in% names(train_data)]

formula_dt <- as.formula(paste("Next_Purchase_Flag ~", 
                               paste(predictors, collapse = " + ")))

cat("\nBuilding Decision Tree Model...\n")
tree_model <- rpart(formula_dt, 
                    data = train_data, 
                    method = "class",
                    control = rpart.control(minsplit = 20, cp = 0.01, maxdepth = 10))

# Print tree summary
cat("\nDecision Tree Summary:\n")
print(tree_model)

# Variable importance
cat("\nVariable Importance:\n")
print(tree_model$variable.importance)

# Visualize the tree
png("decision_tree.png", width = 1600, height = 1200, res = 120)
rpart.plot(tree_model, 
           type = 4, 
           extra = 101,
           box.palette = c("pink", "lightgreen"),
           branch.lty = 3,
           shadow.col = "gray",
           main = "Decision Tree: Next Purchase Prediction")
dev.off()

cat("\nDecision tree visualization saved.\n")

# Make predictions on training data
train_predictions <- predict(tree_model, train_data, type = "class")
train_actual <- train_data$Next_Purchase_Flag

# Confusion matrix for training data
cat("\n=== TRAINING DATA PERFORMANCE ===\n")
train_cm <- confusionMatrix(train_predictions, train_actual, positive = "1")
print(train_cm)

# Make predictions on test data
test_predictions <- predict(tree_model, test_data, type = "class")
test_actual <- test_data$Next_Purchase_Flag

# Confusion matrix for test data
cat("\n=== TEST DATA PERFORMANCE ===\n")
test_cm <- confusionMatrix(test_predictions, test_actual, positive = "1")
print(test_cm)

# Extract metrics
dt_train_accuracy <- train_cm$overall['Accuracy']
dt_test_accuracy <- test_cm$overall['Accuracy']
dt_sensitivity <- test_cm$byClass['Sensitivity']
dt_specificity <- test_cm$byClass['Specificity']
dt_precision <- test_cm$byClass['Pos Pred Value']
dt_f1 <- test_cm$byClass['F1']

cat("\n=== DECISION TREE SUMMARY ===\n")
cat("Training Accuracy:", round(dt_train_accuracy * 100, 2), "%\n")
cat("Test Accuracy:", round(dt_test_accuracy * 100, 2), "%\n")
cat("Sensitivity (Recall):", round(dt_sensitivity * 100, 2), "%\n")
cat("Specificity:", round(dt_specificity * 100, 2), "%\n")
cat("Precision:", round(dt_precision * 100, 2), "%\n")
cat("F1 Score:", round(dt_f1, 3), "\n")

# 6. LOGISTIC REGRESSION ANALYSIS
# =============================================================================
cat("\n=== LOGISTIC REGRESSION ANALYSIS ===\n")

# Split data (80/20 for logistic regression)
set.seed(123)
trainIndex_lr <- createDataPartition(df_clean$Next_Purchase_Flag, p = 0.8, list = FALSE)
train_lr <- df_clean[trainIndex_lr, ]
test_lr <- df_clean[-trainIndex_lr, ]

cat("Logistic Regression Training set size:", nrow(train_lr), "\n")
cat("Logistic Regression Testing set size:", nrow(test_lr), "\n")

# Build logistic regression model
formula_lr <- as.formula(paste("Next_Purchase_Flag ~", 
                               paste(predictors, collapse = " + ")))

cat("\nBuilding Logistic Regression Model...\n")
log_model <- glm(formula_lr, 
                 data = train_lr, 
                 family = "binomial")

# Model summary
cat("\nLogistic Regression Model Summary:\n")
print(summary(log_model))

# Calculate odds ratios
cat("\n=== ODDS RATIOS ===\n")
odds_ratios <- exp(coef(log_model))
or_df <- data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = odds_ratios,
  Interpretation = ifelse(odds_ratios > 1, "Increases odds", "Decreases odds")
)
print(or_df)

# Significant predictors (p < 0.05)
model_summary <- summary(log_model)
significant_vars <- rownames(model_summary$coefficients)[model_summary$coefficients[, 4] < 0.05]
cat("\nStatistically Significant Predictors (p < 0.05):\n")
print(significant_vars)

# Make predictions (probabilities)
train_probs_lr <- predict(log_model, train_lr, type = "response")
test_probs_lr <- predict(log_model, test_lr, type = "response")

# Convert to class labels (threshold = 0.5)
train_preds_lr <- ifelse(train_probs_lr > 0.5, 1, 0)
test_preds_lr <- ifelse(test_probs_lr > 0.5, 1, 0)

train_preds_lr <- factor(train_preds_lr, levels = c(0, 1))
test_preds_lr <- factor(test_preds_lr, levels = c(0, 1))

# Confusion matrices
cat("\n=== LOGISTIC REGRESSION - TRAINING DATA ===\n")
train_cm_lr <- confusionMatrix(train_preds_lr, train_lr$Next_Purchase_Flag, positive = "1")
print(train_cm_lr)

cat("\n=== LOGISTIC REGRESSION - TEST DATA ===\n")
test_cm_lr <- confusionMatrix(test_preds_lr, test_lr$Next_Purchase_Flag, positive = "1")
print(test_cm_lr)

# Calculate additional metrics
lr_train_accuracy <- train_cm_lr$overall['Accuracy']
lr_test_accuracy <- test_cm_lr$overall['Accuracy']
lr_precision <- test_cm_lr$byClass['Pos Pred Value']
lr_recall <- test_cm_lr$byClass['Sensitivity']
lr_specificity <- test_cm_lr$byClass['Specificity']
lr_f1 <- test_cm_lr$byClass['F1']

cat("\n=== LOGISTIC REGRESSION SUMMARY ===\n")
cat("Training Accuracy:", round(lr_train_accuracy * 100, 2), "%\n")
cat("Test Accuracy:", round(lr_test_accuracy * 100, 2), "%\n")
cat("Precision:", round(lr_precision, 3), "\n")
cat("Recall (Sensitivity):", round(lr_recall, 3), "\n")
cat("Specificity:", round(lr_specificity, 3), "\n")
cat("F1 Score:", round(lr_f1, 3), "\n")

# 7. ROC CURVE & AUC
# =============================================================================
cat("\n=== ROC CURVE ANALYSIS ===\n")

# ROC for training data
roc_train <- roc(as.numeric(train_lr$Next_Purchase_Flag) - 1, train_probs_lr)
auc_train <- auc(roc_train)

# ROC for test data
roc_test <- roc(as.numeric(test_lr$Next_Purchase_Flag) - 1, test_probs_lr)
auc_test <- auc(roc_test)

cat("Training AUC:", round(auc_train, 4), "\n")
cat("Test AUC:", round(auc_test, 4), "\n")

# Interpretation
if (auc_test >= 0.8) {
  cat("Interpretation: Excellent discrimination\n")
} else if (auc_test >= 0.7) {
  cat("Interpretation: Acceptable discrimination\n")
} else if (auc_test >= 0.6) {
  cat("Interpretation: Poor discrimination\n")
} else {
  cat("Interpretation: Failed discrimination\n")
}

# Plot ROC curves
png("roc_curves.png", width = 1200, height = 600)
par(mfrow = c(1, 2))

# Training ROC
plot(roc_train, 
     main = "ROC Curve - Training Data",
     col = "blue",
     lwd = 2)
text(0.5, 0.3, paste("AUC =", round(auc_train, 4)), cex = 1.2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Test ROC
plot(roc_test, 
     main = "ROC Curve - Test Data",
     col = "red",
     lwd = 2)
text(0.5, 0.3, paste("AUC =", round(auc_test, 4)), cex = 1.2)
abline(a = 0, b = 1, lty = 2, col = "gray")

dev.off()
cat("\nROC curves saved.\n")

# 8. MODEL COMPARISON
# =============================================================================
cat("\n=== MODEL COMPARISON ===\n")

comparison <- data.frame(
  Model = c("Decision Tree", "Logistic Regression"),
  Test_Accuracy = c(round(dt_test_accuracy, 4), round(lr_test_accuracy, 4)),
  Sensitivity_Recall = c(round(dt_sensitivity, 4), round(lr_recall, 4)),
  Specificity = c(round(dt_specificity, 4), round(lr_specificity, 4)),
  Precision = c(round(dt_precision, 4), round(lr_precision, 4)),
  F1_Score = c(round(dt_f1, 4), round(lr_f1, 4)),
  AUC = c(NA, round(auc_test, 4))  # AUC only for logistic regression
)

print(comparison)

# Save comparison table
write.csv(comparison, "model_comparison.csv", row.names = FALSE)

# Determine best model
best_model <- ifelse(dt_test_accuracy > lr_test_accuracy, 
                     "Decision Tree", "Logistic Regression")

cat("\n=== FINAL RECOMMENDATION ===\n")
cat("Recommended Model:", best_model, "\n")
cat("\nReason:\n")
if (best_model == "Decision Tree") {
  cat("- Higher test accuracy\n")
  cat("- Better interpretability for business stakeholders\n")
  cat("- Captures non-linear relationships effectively\n")
} else {
  cat("- Better overall performance\n")
  cat("- Provides probability scores for ranking customers\n")
  cat("- Good discrimination (AUC)\n")
}

# 9. BUSINESS INSIGHTS
# =============================================================================
cat("\n=== BUSINESS INSIGHTS ===\n")

cat("\n1. KEY DRIVERS OF NEXT PURCHASE:\n")
if ("Review.Rating" %in% names(df_clean)) {
  cat("   - Customer satisfaction (Review Rating)\n")
}
if ("Subscription.Status" %in% names(df_clean)) {
  cat("   - Subscription status\n")
}
cat("   - Purchase history patterns\n")
cat("   - Product category preferences\n")

cat("\n2. CUSTOMER RETENTION STRATEGIES:\n")
cat("   - Target customers with high probability scores for retention campaigns\n")
cat("   - Focus on converting satisfied non-subscribers to subscribers\n")
cat("   - Personalize communications based on category preferences\n")

cat("\n3. RISK SEGMENTS:\n")
cat("   - Customers with low satisfaction scores need immediate attention\n")
cat("   - First-time buyers without subscription = high churn risk\n")
cat("   - Deploy win-back campaigns for at-risk segments\n")

cat("\n4. PREDICTIVE APPLICATIONS:\n")
cat("   - Real-time churn prediction at checkout\n")
cat("   - Personalized email campaigns (timing & content)\n")
cat("   - Dynamic discount allocation for at-risk customers\n")
cat("   - Inventory planning based on predicted repurchase rates\n")

# 10. SAVE SUMMARY REPORT
# =============================================================================
cat("\n=== SAVING SUMMARY REPORT ===\n")

summary_report <- list(
  Dataset_Info = list(
    Total_Records = nrow(df_clean),
    Target_Distribution = table(df_clean$Next_Purchase_Flag),
    Train_Size = nrow(train_data),
    Test_Size = nrow(test_data)
  ),
  Decision_Tree = list(
    Train_Accuracy = dt_train_accuracy,
    Test_Accuracy = dt_test_accuracy,
    Sensitivity = dt_sensitivity,
    Specificity = dt_specificity,
    Precision = dt_precision,
    F1_Score = dt_f1
  ),
  Logistic_Regression = list(
    Train_Accuracy = lr_train_accuracy,
    Test_Accuracy = lr_test_accuracy,
    Sensitivity = lr_recall,
    Specificity = lr_specificity,
    Precision = lr_precision,
    F1_Score = lr_f1,
    AUC = auc_test
  ),
  Recommended_Model = best_model
)

saveRDS(summary_report, "analysis_summary.rds")
cat("Summary report saved as 'analysis_summary.rds'\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All outputs and visualizations have been saved.\n")
cat("\nGenerated Files:\n")
cat("1. boxplot_purchase_amount.png\n")
cat("2. boxplot_previous_purchases.png\n")
cat("3. boxplot_age.png\n")
cat("4. barplot_target_distribution.png\n")
cat("5. barplot_subscription_by_target.png\n")
cat("6. barplot_category.png\n")
cat("7. histogram_previous_purchases.png\n")
cat("8. histogram_review_rating.png\n")
cat("9. decision_tree.png\n")
cat("10. roc_curves.png\n")
cat("11. model_comparison.csv\n")
cat("12. analysis_summary.rds\n")

cat("\n========================================\n")
cat("NEXT PURCHASE PREDICTION - KEY RESULTS\n")
cat("========================================\n")
cat("Target Variable: Next_Purchase_Flag\n")
cat("Decision Tree Accuracy:", round(dt_test_accuracy * 100, 2), "%\n")
cat("Logistic Regression Accuracy:", round(lr_test_accuracy * 100, 2), "%\n")
cat("Best Model:", best_model, "\n")
cat("========================================\n")
