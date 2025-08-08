# insurance_fraud_detection

## 1. Project Overview

This analysis focuses on detecting and understanding fraudulent insurance claims using a publicly available dataset from [Kaggle](https://www.kaggle.com/datasets/arpan129/insurance-fraud-detection/data), provided by **Arpan129**. The dataset includes 38 explonary variables along with a binary `fraud_reported` target variable, indicating whether a claim is reported as fraudulent. The primary goal is to explore the data, analyze patterns, and build predictive models for classifying fraud.

## 2. Problem Context

Insurance fraud represents a significant threat to the industry, often leading to billions in annual losses and increased premiums for honest policyholders. The detection of such fraud poses unique challenges, including class imbalance (fraud cases are rare) and complex categorical data, both of which demand careful modeling strategies.

## 3. Data Contents

The dataset contains diverse information about claimants and claims, including:

-   Policyholder demographic features (e.g., age, sex, education)

-   Incident-related variables(e.g., type, severity, location, property damage)

-   Financial components (e.g., claim amounts, deductible, premimums)

-   Vehicle details (e.g., year, model)

-   Claim response outcomes, such as whether authorities were contacted or a police report was filed

The target variable `fraud_reported` indicates if the claim was labeled as fraudulent.

### 4. Analysis Objectives

1.  Exploratory Data Analysis (EDA)

-   Understand variable distributions and relationships

-   Visualize categorical vs. fraud trends and numeric variable comparisons

---

2.  Data preprocessing

-   Handle missing values and outliers

-   Encode categorical variables appropriately, given the dataset's high cardinality

-   Address class imbalance techniques where needed

---

3.  Modem Development

-   Build and evaluate supervised classifiers such as logistic regression, decision trees, random forests, and XGBoost.

-   Exploire anomaly detection or ensemble learning methods as complementary strategies for rare event detection.

---

4.  Model Evaluation

-   Focus on metrics suitable for imbalanced datasets

-   Compare performances, including false positives vs detection rate trade-offs
