getwd()
setwd("C:/Users/shant/Desktop/Utd/fall 2025/BA with R/project/")
# Install (if needed)
# install.packages(c("tidyverse", "janitor", "skimr"))

library(tidyverse)
library(janitor)
library(skimr)
# load the data-set 
data<- read.csv("C:/Users/shant/Desktop/Utd/fall 2025/BA with R/project/shopping_behavior_updated.csv")


# Quick checks
dim(data)
names(data)
head(data)
skim(data)

# Get a summary of each column
summary(data)
# structure of the data-set 
str(data)


# AUTO-CONVERT COMMON CATEGORICAL VARIABLES TO FACTORS
categorical_vars <- c(
  "Gender",
  "Category",
  "Location",
  "Size",
  "Color",
  "Season",
  "Subscription Status",
  "Shipping Type",
  "Discount Applied",
  "Promo Code Used",
  "Payment Method",
  "Frequency of Purchases"
)

# Convert only the columns that exist in your dataset
categorical_vars_present <- intersect(categorical_vars, names(data))



names(data)

# 1) Load janitor if not loaded
if(!"janitor" %in% installed.packages()[,1]) install.packages("janitor")
library(janitor)

# 2) Clean names (do this once)
data <- data %>% janitor::clean_names()

# 3) Confirm cleaned names (optional)
names(data)
# you should now see "previous_purchases", "subscription_status", "discount_applied"

# 4) Create Next_Purchase_Flag (combined METHOD A + METHOD B)
data <- data %>%
  mutate(
    # make comparisons robust to capitalization by converting to lowercase character
    subscription_status = tolower(as.character(subscription_status)),
    discount_applied    = tolower(as.character(discount_applied)),
    Next_Purchase_Flag = ifelse(
      previous_purchases > 10 |
        (subscription_status == "yes" & discount_applied == "yes"),
      1, 0
    ),
    Next_Purchase_Flag = factor(Next_Purchase_Flag, levels = c(0,1), labels = c("No","Yes"))
  )

# 5) Check results
table(data$Next_Purchase_Flag)
prop.table(table(data$Next_Purchase_Flag))

# Confirm names after Option A clean_names()
names(data)
# Quick structure check for the main columns we use
str(data[c("previous_purchases", "subscription_status", "discount_applied")])

# create Target variable 
data <- data %>%
  mutate(
    Next_Purchase_Flag = ifelse(
      previous_purchases > 10 |
        (subscription_status == "yes" & discount_applied == "yes"),
      1, 0
    ),
    Next_Purchase_Flag = factor(Next_Purchase_Flag, levels = c(0,1), labels = c("No","Yes"))
  )

# Verify
table(data$Next_Purchase_Flag)
prop.table(table(data$Next_Purchase_Flag))


library(ggplot2)
library(dplyr)

# Numerical summary
num_vars <- data %>% select(age, purchase_amount_usd, review_rating, previous_purchases)

summary(num_vars)

# boxplot for previous purchase
ggplot(data, aes(x = "", y = previous_purchases)) +
geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot: Previous Purchases", y = "Previous Purchases")

# boxplot for purchase amount 
ggplot(data, aes(x = "", y = purchase_amount_usd)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: Purchase Amount (USD)")

#Histograms for distribution

ggplot(data, aes(x = previous_purchases)) +
  geom_histogram(bins = 30, fill = "gold", color = "black") +
  labs(title = "Distribution of Previous Purchases")


ggplot(data, aes(x = purchase_amount_usd)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Distribution of Purchase Amount")

#Categorical variable visualization (Barplots)
# gender distribution 
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "coral") +
  labs(title = "Gender Distribution")

#category distribution 
ggplot(data, aes(x = category)) +
geom_bar(fill = "steelblue") +
  labs(title = "Product Category Distribution")

# Subscription Status 
ggplot(data, aes(x = subscription_status)) +
  geom_bar(fill = "darkorchid") +
  labs(title = "Subscription Status Distribution")

#Relationship visualization (Bivariate Analysis)

#Purchase amount by category

ggplot(data, aes(x = category, y = purchase_amount_usd)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Purchase Amount by Category")

# Previous purchases vs Next Purchase Flag

ggplot(data, aes(x = Next_Purchase_Flag, y = previous_purchases, fill = Next_Purchase_Flag)) +
  geom_boxplot() +
  labs(title = "Previous Purchases by Next Purchase Flag")

# Target Variable Distribution

library(ggplot2)
library(dplyr)

# Prepare data for percentage labels
target_df <- data %>%
  count(Next_Purchase_Flag) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

ggplot(target_df, aes(x = Next_Purchase_Flag, y = n, fill = Next_Purchase_Flag)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(percent, "%")), 
            vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#ff7f50", "#00bcd4")) +
  labs(
    title = "Next Purchase Flag Distribution",
    subtitle = "Highly Imbalanced Target: Majority Customers Expected to Buy Again",
    x = "Next Purchase Flag",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


# Train–Test Split (2/3 training, 1/3 testing)

set.seed(123)   # Reproducible

n <- nrow(data)
train_index <- sample(1:n, size = floor(2/3 * n))

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

# Verify class balance remains similar
prop.table(table(train_data$Next_Purchase_Flag))
prop.table(table(test_data$Next_Purchase_Flag))

# Decision Tree 
data$shipping_type <- as.factor(data$shipping_type)
str(data$shipping_type)


tree_medium <- rpart(
  Next_Purchase_Flag ~ subscription_status + discount_applied + shipping_type +
    review_rating + gender + season + category + payment_method +
    frequency_of_purchases + purchase_amount_usd,
  
  data = train_data,
  method = "class",
  
  control = rpart.control(
    cp = 0.002,
    minsplit = 25,
    minbucket = 10,
    maxdepth = 5
  )
)

rpart.plot(
  tree_medium,
  type = 4,
  extra = 104,
  nn = TRUE,
  fallen.leaves = TRUE,
  box.palette = c("Blues", "Greens"),
  main = "Decision Tree Including Shipping Type and Rating"
)
























































