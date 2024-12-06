# EDA.R: Exploratory Data Analysis for Student Exam Performance

# Libraries, data
library(tidyverse)
library(MASS)
data <- read.csv("Data/data.csv", header=TRUE, sep=',')

# Data Summaries
summary(data$Exam_Score)
summary(data$Hours_Studied)
summary(data$Attendance)
summary(data$Sleep_Hours)
summary(data$Previous_Scores)
summary(data$Tutoring_Sessions)
summary(data$Physical_Activity)

# Affect of Single Variables on Exam Score

# Affect of Attendance on Exam_Score
ggplot(data, aes(x = Attendance, y = Exam_Score)) +
  geom_point(color = "coral") +
  theme_minimal() +
  labs(title = "Attendance vs Exam Score", x = "Attendance", y = "Exam Score")

# Affect of Hours_Studied on Exam_Score
ggplot(data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Hours Studied vs Exam Score", x = "Hours Studied", y = "Exam Score")

# Affect of Previous_Scores on Exam_Score
ggplot(data, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_point(color = "darkgreen") +
  theme_minimal() +
  labs(title = "Previous Scores vs Exam Score", x = "Previous Scores", y = "Exam Score")

# Affect of Access_to_Resources on Exam_Score
ggplot(data, aes(x = Access_to_Resources, y = Exam_Score)) +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(title = "Access to Resources vs Exam Score", x = "Access to Resources", y = "Exam Score")

# Affect of Parental_Involvement on Exam_Score
ggplot(data, aes(x = Parental_Involvement, y = Exam_Score)) +
  geom_point(color = "darkviolet") +
  theme_minimal() +
  labs(title = "Parental Involvement vs Exam Score", x = "Parental Involvement", y = "Exam Score")

# Affect of Motivation_Level on Exam_Score
ggplot(data, aes(x = Motivation_Level, y = Exam_Score)) +
  geom_point(color = "darkorange") +
  theme_minimal() +
  labs(title = "Motivation Level vs Exam Score", x = "Motivation Level", y = "Exam Score")

# Checking correlation between variables

# Motivation Level and Parental Involvement
table(data$Motivation_Level, data$Parental_Involvement)
chisq.test(data$Motivation_Level, data$Parental_Involvement)

# Teacher Quality and School Type
table(data$Teacher_Quality, data$School_Type)
chisq.test(data$Teacher_Quality, data$School_Type)

# Access to Resources and Internet Access
table(data$Access_to_Resources, data$Internet_Access)
chisq.test(data$Access_to_Resources, data$Internet_Access)

# Family Income and Parental Education Level
table(data$Family_Income, data$Parental_Education_Level)
chisq.test(data$Family_Income, data$Parental_Education_Level)



