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
              "auto_model")


for (var in cat_vars) print(plot_fraud_pie(df, var))









