install.packages("modeest")
install.packages("readr")
install.packages("psych")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tinytex")

# 1. Conduct basic descriptive statistics, including head, tail, dimensions, summary, structure, mean, mode, median, and check for any missing values.
library(dplyr)
library(readr)
library(psych)
library(modeest)
library(tidyverse)
library(ggplot2)
library(tinytex)

data <- read_csv("C:\\Users\\Tarun\\OneDrive\\Desktop\\R program\\Churn.csv")

dim(data) #to find the dimension of data
summary(data) #to find the summary of data
str(data) #to find the structure of data
describe(data) 

head(data) #to display the head of dataset
tail(data) #to display the tail of dataset

numeric_cols <- data %>% select_if(is.numeric) #to extract the numeric value columns 

mean_values <- sapply(numeric_cols, mean, na.rm = TRUE) #na.rm=TRUE to ignore the missing values 
median_values <- sapply(numeric_cols, median, na.rm = TRUE) 
mode_values <- sapply(numeric_cols, function(y) mfv(y, na_rm = TRUE)) #function(y)-Applies a function to each column of the data dataframe. mfv - most frequent value

which.max(table(data$ContractType)) #Finds the most frequent ContractType category in the dataset

missing_values <- sapply(data, function(y) sum(is.na(y))) #Counts missing (NA) values in each column of the dataset

list(
  Mean = mean_values,
  Median = median_values,
  Mode = mode_values,
  Missing = missing_values
)  # Creates a list storing mean, median, mode, and missing value counts for each column

#3.Select and compare two or more variables from the dataset, such as DataUsage and Churn. Create a table for the  selected variables using the df(dataframe) and select commands. 
# Provide summary statistics or visualizations to support your comparison. 

selected_data <- data %>% select(DataUsage, Churn) #selecting the variables from the csv file
view(selected_data) #to view the selected data in the table format
print(selected_data) # to print the table details.

cor_value <- cor(selected_data$DataUsage,selected_data$Churn) #Calculates the correlation between Data Usage and Churn to measure their relationship strength and direction.

print(paste("Co-relation value is", cor_value))

ggplot(selected_data, aes(x = as.factor(Churn), y = DataUsage, fill = as.factor(Churn))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(x = "Churn (0 = No, 1 = Yes)", y = "Data Usage",
       fill = "Churn", title = "Boxplot of Data Usage by Churn") +
  theme_minimal() # Creates a boxplot to visualize the distribution of Data Usage for each Churn category (0 = No, 1 = Yes), highlighting outliers in red.

#Conclusion
#Churn and DataUsage do not appear to be strongly correlated, according to the analysis.
#This is supported by correlation values and boxplots.
#Other elements such as pricing, customer service calls, CallFailures, SubscriptionLength, VoiceMinutes CustomerSupportCalls or contract type may be more accurate churn indicators.



#4. Transform at least one variable (any transformation such  as log(x) and generate a plot using 
#‘ggplot’ for the transformed variable.
data <- data %>% mutate(LogDataUsage = log(DataUsage + 1)) # Creates a new column 'LogDataUsage' by applying a log transformation to 'DataUsage' to normalize its distribution.


ggplot(data, aes(x = LogDataUsage)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(x = "Log(Data Usage)", y = "Density", 
       title = "Density Plot of Log-Transformed Data Usage") +
  theme_minimal() # Generates a density plot to visualize the distribution of log-transformed Data Usage, with a light blue fill and dark blue border.