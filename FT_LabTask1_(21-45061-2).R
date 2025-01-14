
library(dplyr)


dataset <- read.csv("E:/Aiub 12th Semester/Data Science/Final-Term/online_gaming_behavior_dataset.csv", stringsAsFactors = FALSE)


if (is.factor(dataset$EngagementLevel) || is.character(dataset$EngagementLevel)) {
  dataset$EngagementLevel <- factor(dataset$EngagementLevel, levels = c("Low", "Medium", "High"))
}


dataset$EngagementLevel <- as.numeric(dataset$EngagementLevel)


View(dataset)



dataset <- na.omit(dataset)




categorical_columns <- c("Gender", "Location", "GameGenre", "GameDifficulty")


for (col in categorical_columns) {
  print(paste("Chi-Square Test for", col, "vs EngagementLevel"))
  chi_test <- chisq.test(table(dataset[[col]], dataset$EngagementLevel))
  print(chi_test)
}



for (i in 1:(length(categorical_columns) - 1)) {
  for (j in (i + 1):length(categorical_columns)) {
    col1 <- categorical_columns[i]
    col2 <- categorical_columns[j]
    print(paste("Chi-Square Test for", col1, "vs", col2))
    chi_test <- chisq.test(table(dataset[[col1]], dataset[[col2]]))
    print(chi_test)
  }
}



numerical_columns <- c("Age", "PlayTimeHours", "SessionsPerWeek", "AvgSessionDurationMinutes",
                       "PlayerLevel", "AchievementsUnlocked")


for (col in numerical_columns) {
  print(paste("Pearson Correlation for", col, "vs EngagementLevel"))
  cor_test <- cor.test(dataset[[col]], as.numeric(dataset$EngagementLevel), method = "pearson")
  print(cor_test)
}




for (i in 1:(length(numerical_columns) - 1)) {
  for (j in (i + 1):length(numerical_columns)) {
    col1 <- numerical_columns[i]
    col2 <- numerical_columns[j]
    print(paste("Pearson Correlation for", col1, "vs", col2))
    cor_test <- cor.test(dataset[[col1]], dataset[[col2]], method = "pearson")
    print(cor_test)
  }
}





numerical_columns <- names(dataset)[sapply(dataset, is.numeric)]


for (col in numerical_columns) {
  print(paste("ANOVA for", col, "vs EngagementLevel"))
  anova_model <- aov(dataset[[col]] ~ as.factor(dataset$EngagementLevel))
  print(summary(anova_model))
}


categorical_columns <- c("Gender", "Location", "GameGenre", "GameDifficulty")


for (num_col in numerical_columns) {
  for (cat_col in categorical_columns) {
    print(paste("ANOVA for", num_col, "vs", cat_col))
    anova_model <- aov(dataset[[num_col]] ~ dataset[[cat_col]])
    print(summary(anova_model))
  }
}





