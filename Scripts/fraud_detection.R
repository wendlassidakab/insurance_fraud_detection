# Install if not already installed
install.packages(c("tidyverse", "skimr", "DataExplorer", "janitor", "readxl",
                   "rlang"))


# load librairies
library(tidyverse)
library(skimr)
library(DataExplorer)
library(janitor)
library(readxl)
library(ggplot2)
library(rlang)


# Load the dataset
df <- read_excel("Worksheet in Case Study question 2.xlsx")



# Quick data overview
summary(df)
colSums(is.na(df))


## Data cleaning
unique(df$insured_sex)

unique(df$insured_relationship)

unique(df$insured_education_level)

unique(df$collision_type)
df$collision_type[df$collision_type == "?"] <- "Unknow"

unique(df$incident_type)

unique(df$incident_severity)

unique(df$authorities_contacted)

unique(df$incident_state)

unique(df$incident_city)

unique(df$police_report_available)
table(df$police_report_available)
df$police_report_available[df$police_report_available == "?"] <- "NO"

unique(df$auto_make)

unique(df$auto_model)

unique(df$auto_year)

unique(df$fraud_reported)

unique(df$property_damage)
df$property_damage[df$property_damage == "?"] <- "NO"

df$number_of_vehicles_involved <- as.factor(df$number_of_vehicles_involved)
df$bodily_injuries <- as.factor(df$bodily_injuries)
df$witnesses <- as.factor(df$witnesses)
df$auto_year <- as.factor(df$auto_year)

# Distribution of frauds
ggplot(data = df, aes(x = fraud_reported)) +
    geom_bar() +
    labs(title = "Distribution of Frauds",
         x = "Fraud reported",
         y = "Count") +
    theme_minimal()


# Age
ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 5,
        fill = "skyblue",
        color = "white",
        alpha = 0.8) +
    labs(title = "Distribution of Age",
        x = "Age",
        y = "Count") +
    theme_minimal(base_size = 14)

ggplot(df, aes(x = age, fill = fraud_reported)) +
    geom_histogram(binwidth = 5,
        color = "white",
        alpha = 0.8,
        position = "identity") +
    labs(title = "Distribution of Age by Fraud Report Status",
        x = "Age",
        y = "Count",
        fill = "Fraud Reported") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# State
tbl <- table(df$policy_state, df$fraud_reported)
proportions <- tbl / rowSums(tbl)
pie_data <- as.data.frame(proportions)
colnames(pie_data) <- c("policy_state", "fraud_reported", "count")
pie_data$percent <- round(100 * pie_data$count, 1)

ggplot(pie_data, aes(x = "", y = count, fill = fraud_reported)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    facet_wrap(~ policy_state) +
    geom_text(aes(label = paste0(percent, "%")),
        position = position_stack(vjust = 0.5),
        color = "black",
        size = 3.5) +
    labs(title = "Fraud Proportion per Policy State",
        fill = "Fraud Reported") +
    theme_minimal(base_size = 13)

# Sex
tbl2 <- table(df$insured_sex, df$fraud_reported)
prop <- tbl2 / rowSums(tbl2)
pie_data2 <- as.data.frame(prop)
colnames(pie_data2) <- c("insured_sex", "fraud_reported", "count")
pie_data2$percent <- round(100 * pie_data2$count, 1)

ggplot(pie_data2, aes(x = "", y = count, fill = fraud_reported)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    facet_wrap(~ insured_sex) +
    geom_text(aes(label = paste0(percent, "%")),
              position = position_stack(vjust = 0.5),
              color = "black",
              size = 3.5) +
    labs(title = "Fraud proportion per Sex" ,
         fill = "Fraud Reported")+
    theme_minimal()


# fonction for categorial variables
plot_fraud_pie <- function(df, cat_var){
    # Convert the variable name to a symbol
    var_sym <- sym(cat_var)

    # create contingency table and row-wise proportions
    tbl <-  table(df[[cat_var]] ,  df$fraud_reported)
    prop <- tbl / rowSums(tbl)

    # Convert to data frame
    pie_data <- as.data.frame(prop)
    colnames(pie_data) <- c(cat_var, "fraud_reported", "count")
    pie_data$percent <- round(100 * pie_data$count, 1)

    # Create the pie chart
    ggplot(pie_data, aes(x="", y=count, fill = fraud_reported)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar(theta = "y") +
        facet_wrap(as.formula(paste("~", cat_var))) +
        geom_text(aes(label = paste0(percent, "%")),
                  position = position_stack(vjust = 0.5),
                  color = "black", size = 3.5) +
        labs(title = paste("Fraud Proportion by", cat_var),
             fill = "Fraud reported") +
        theme_minimal()
}
cat_vars <- c("policy_state", "insured_sex", "insured_education_level",
              "insured_occupation", "insured_hobbies", "insured_relationship",
              "incident_type", "collision_type", "incident_severity",
              "authorities_contacted", "incident_state", "incident_city",
              "property_damage", "police_report_available", "auto_make",
              "auto_model", "number_of_vehicles_involved", "auto_year",
              "bodily_injuries", "witnesses")


for (var in cat_vars) print(plot_fraud_pie(df, var))


# filter only fraud cases
df_fraud <- df[df$fraud_reported == "Y", ]

for (var in cat_vars) {
  p <- ggplot(df_fraud, aes(x = reorder(.data[[var]], -table(.data[[var]])[.data[[var]]]))) +
    geom_bar(fill = "steelblue", color = "white") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Distribution of", var, "for Fraud = Y"),
         x = var, y = "Count")
  print(p)
}


# Numeric variables
num_var <- c("age", "policy_deductable", "policy_annual_premium",
             "umbrella_limit", "capital-gains", "capital-loss", "total_claim_amount",
             "injury_claim", "property_claim", "vehicle_claim")

for (var in num_var){
    # Boxplot
    p1 <- ggplot(df, aes(x = fraud_reported, y = .data[[var]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Boxplot of", var, "by Fraud Reported"),
             x = "Fraud Reported", y = var) +
        theme_minimal()
    print(p1)

    # Bar plot of means
    mean_data <- summarise(group_by(df, fraud_reported),
                           mean_value = mean(.data[[var]], na.rm = TRUE))
    p2 <- ggplot(mean_data, aes(x = fraud_reported, y = mean_value)) +
        geom_col(fill = "steelblue") +
        labs(title = paste("Mean", var, "by Fraud Reported"),
             x = "Fraud Reported", y = paste("Mean", var)) +
        theme_minimal()
    print(p2)

    # Histogram
    p3 <- ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(fill = "gray", bins = 30, color = "black") +
        labs(title = paste("Histogral of", var),
             x = var, y = "Count") +
        theme_minimal()
    print(p3)
}






