library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(dplyr)
library(pROC)
library(naivebayes)

Heart_disease <- read.csv("C:\\Users\\Tarun\\OneDrive\\Desktop\\R program\\4th Assignment\\Heart_disease.csv")

duplicates <- Heart_disease[duplicated(Heart_disease), ]
print(nrow(duplicates))
print(duplicates)    

df_unique <- Heart_disease %>% distinct()

df_unique$Target <- ifelse(df_unique$MAX_HeartRate > 170, "Yes", "No")
df_unique$BP_New <- ifelse(df_unique$Blood_Pressure > 120, "Yes", "No")

target_table <- table(df_unique$Target)
print

ggplot(df_unique, aes(x = Target)) + geom_bar(fill = "steelblue") + ggtitle("Distribution of Target Variable")

Heart_disease30 <- df_unique[1:30, c("Target", "BP_New", "chest_pain_type")]
Object1 <- ftable(Heart_disease30)
Object1

Object2 <- ftable(Heart_disease30[,-1])
Object2

p1 <- Object1[3,1]/Object2[1,1] #Target=yes, BP_New=No & chest_pain_type=0
p2 <- Object1[3,2]/Object2[1,2] #Target= yes , BP_New=Yes & Chest_pain_type=1
p3 <- Object1[4,1]/Object2[2,1] #Target=yes , BP_New=Yes & Chect_pain_type=0
p4 <- Object1[4,2]/Object2[2,2] #Target=yes , BP_New=No & Chect_pain_type=1

Probability_Target <- rep(0, 30)

for (i in 1:30) {
  BP_New <- Heart_disease30$BP_New[i]  # Get BP_New value for row i
  chest_pain <- Heart_disease30$chest_pain_type[i]  # Get chest_pain_type for row i
  
  # Match the probability from precomputed values (p1, p2, p3, p4)
  if (BP_New == "No" & chest_pain == 0) {
    Probability_Target[i] <- p1
  } else if (BP_New == "Yes" & chest_pain == 1) {
    Probability_Target[i] <- p2
  } else if (BP_New == "Yes" & chest_pain == 0) {
    Probability_Target[i] <- p3
  } else if (BP_New == "No" & chest_pain == 1) {
    Probability_Target[i] <- p4
  }
}

Heart_disease30$Probability_Target <- Probability_Target
Heart_disease30$Pred_Probability <- ifelse(Heart_disease30$Probability_Target > 0.5, "Yes", "No")
Heart_disease30

# Compute total instances
total_count <- nrow(Heart_disease30)

# Compute P(Target = Yes)
p_target_yes <- sum(Heart_disease30$Target == "Yes") / total_count

# Compute P(BP_New = Yes, chest_pain_type = 1 | Target = Yes)
p_given_yes <- sum(Heart_disease30$BP_New == "Yes" & Heart_disease30$chest_pain_type == 1 & Heart_disease30$Target == "Yes") /
  sum(Heart_disease30$Target == "Yes")

# Compute P(BP_New = Yes, chest_pain_type = 1)
p_evidence <- sum(Heart_disease30$BP_New == "Yes" & Heart_disease30$chest_pain_type == 1) / total_count

# Compute the final probability using Bayes' Theorem
p_yes_given_bp_chest <- (p_given_yes * p_target_yes) / p_evidence

# Print result
print(p_yes_given_bp_chest) #cat

set.seed(123)

train.index <- createDataPartition(df_unique$Target, p = 0.6, list = FALSE)

train.df <- df_unique[train.index, ]
valid.df <- df_unique[-train.index, ]

nb_model <- naiveBayes(Target ~ chest_pain_type + BP_New, data = train.df)
train_pred <- predict(nb_model, train.df)
# Convert predictions and actual target variable to factors with the same levels
train.df$Target <- factor(train.df$Target)
train_pred <- factor(train_pred, levels = levels(train.df$Target))

# Compute confusion matrix
conf_matrix1 <- confusionMatrix(train_pred, train.df$Target)

conf_matrix1
