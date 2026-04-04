# Customer Shopping Behavior Analysis
### Predicting Repeat Purchase Behavior using Machine Learning in R

---

## Project Overview

This project analyzes shopping behavior data from **3,900+ customers** to predict whether a customer will make a repeat purchase. Using supervised machine learning models built in R, the analysis identifies key behavioral drivers of customer retention and provides actionable recommendations for marketing strategy.

---

## Business Objective

Retailers lose significant revenue to customer churn. This project answers one key question:

> **Which customers are likely to buy again — and what drives that decision?**

Understanding this enables businesses to:
- Proactively engage high-risk customers before they churn
- Allocate marketing budgets more efficiently
- Personalize promotions based on predicted behavior
- Increase customer lifetime value

---

## Dataset

| Property | Details |
|---|---|
| File | `shopping_behavior_updated.csv` |
| Records | 3,900+ customers |
| Features | 18 columns including Age, Gender, Category, Purchase Amount, Review Rating, Subscription Status, Discount Applied, Previous Purchases, Payment Method, Frequency of Purchases |
| Target Variable | `Next_Purchase_Flag` — engineered from Previous Purchases, Subscription Status, and Discount usage |

---

## Tools & Technologies

- **Language:** R
- **Libraries:** tidyverse, ggplot2, dplyr, caret, rpart, rpart.plot, pROC, corrplot, janitor, skimr
- **Models:** Logistic Regression, Decision Tree
- **Evaluation:** Confusion Matrix, ROC Curve, AUC, Precision, Recall, F1 Score

---

## Project Structure

```
customer-shopping-behavior-analysis/
│
├── shopping_analysis.r              # Main analysis script (full pipeline)
├── BA_with_R_Project_code.R         # Exploratory analysis & visualization script
├── shopping_behavior_updated.csv    # Dataset
└── README.md
```

---

## Methodology

### 1. Data Cleaning
- Removed missing values and duplicate records
- Converted categorical variables to factors
- Cleaned column names using `janitor::clean_names()`

### 2. Feature Engineering
Created the target variable `Next_Purchase_Flag` using combined logic:
- Customers with **10+ previous purchases**, OR
- Customers with **active subscription + discount applied**
→ Flagged as likely to repurchase (1), otherwise 0

### 3. Exploratory Data Analysis
- Boxplots for Purchase Amount, Age, Previous Purchases
- Histograms for distribution analysis
- Bar plots for categorical variables (Gender, Category, Subscription Status)
- Bivariate analysis: Purchase Amount by Category, Previous Purchases by Target

### 4. Model Building
Two models built and compared using a 70/30 train-test split:

| Model | Train Split | Test Split |
|---|---|---|
| Decision Tree | 70% | 30% |
| Logistic Regression | 80% | 20% |

---

## Results

| Metric | Value |
|---|---|
| Test Accuracy | **91%** |
| Precision | **93%** |
| ROC Performance | Strong AUC |

### Key Predictors
- Subscription Status
- Discount Usage
- Review Rating
- Purchase Frequency

---

## Key Business Insights

1. **Subscription status** is the strongest predictor of repeat purchase
2. **Discount usage** significantly increases likelihood of return visits
3. **Satisfied customers** (high review ratings) are far more likely to repurchase
4. **First-time buyers without subscriptions** represent the highest churn risk

### Recommended Actions
- Target non-subscribers with a subscription offer after first purchase
- Deploy personalized discount campaigns for customers showing churn signals
- Prioritize win-back campaigns for customers with low review ratings
- Use model probability scores to rank customers for marketing outreach

---

## How to Run

**1. Clone this repository**
```bash
git clone https://github.com/annuchoudhary29/customer-shopping-behavior-analysis.git
```

**2. Install required R packages**
```r
install.packages(c("tidyverse", "caret", "rpart", "rpart.plot",
                   "pROC", "corrplot", "janitor", "skimr"))
```

**3. Run the main analysis script**
```r
source("shopping_analysis.r")
```

> Make sure `shopping_behavior_updated.csv` is in the same folder as the script.

---

## Author

**Annu Choudhary**
MS in Business Analytics & AI — University of Texas at Dallas (Dec 2026)
[LinkedIn](http://www.linkedin.com/in/annu-choudhary) · [GitHub](https://github.com/annuchoudhary29)
