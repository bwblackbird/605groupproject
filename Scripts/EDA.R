# EDA.R: Exploratory Data Analysis for Student Exam Performance

# Libraries, data
library(tidyverse)
library(dyplr)
library(MASS)
data <- read.csv("Data/data.csv", header=TRUE, sep=',')
numeric_data <- data[sapply(data, is.numeric)] # dataset w/ only numeric variables

# Data Summaries
summary(data$Exam_Score)
summary(data$Hours_Studied)
summary(data$Attendance)
summary(data$Sleep_Hours)
summary(data$Previous_Scores)
summary(data$Tutoring_Sessions)
summary(data$Physical_Activity)

# Exam Score Analysis
top_20_percent_cutoff <- quantile(data$Exam_Score, 0.80)
num_top_20_percent <- sum(data$Exam_Score >= top_20_percent_cutoff)
percent_top_20 <- (num_top_20_percent / nrow(data)) * 100
top_20_percent_cutoff
percent_top_20
top_20_data <- data %>% filter(Exam_Score >= top_20_percent_cutoff) # All samples with exam scores in top 20%

top_1_percent_cutoff <- quantile(data$Exam_Score, 0.99)
num_top_1_percent <- sum(data$Exam_Score >= top_5_percent_cutoff)
percent_top_1 <- (num_top_10_percent / nrow(data)) * 100
top_1_percent_cutoff
percent_top_1
top_1_data <- data %>% filter(Exam_Score >= top_1_percent_cutoff) # All samples with exam scores in top 1%

ggplot(data, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Exam Scores", x = "Exam Score", y = "Count") +
  theme_minimal() # Visualizing Distribution of Exam_Score variable

ggplot(top_1_data, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 1, fill = "violet", color = "black") +
  labs(title = "Distribution of Top 1% Scorers", x = "Exam Score", y = "Count") +
  theme_minimal() # Visualizing Distribution of Top 1% Scorers

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

# Affect of Sleep_Hours on Exam_Score
ggplot(data, aes(x = Sleep_Hours, y = Exam_Score)) +
  geom_point(color = "purple") +
  theme_minimal() +
  labs(title = "Sleep Hours vs Exam Score", x = "Sleep Hours", y = "Exam Score")

# Affect of Access_to_Resources on Exam_Score
ggplot(data, aes(x = Access_to_Resources, y = Exam_Score)) +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(title = "Access to Resources vs Exam Score", x = "Access to Resources", y = "Exam Score")

# Affect of Parental_Involvement on Exam_Score
ggplot(data, aes(x = Parental_Involvement, y = Exam_Score)) +
  geom_point(color = "magenta") +
  theme_minimal() +
  labs(title = "Parental Involvement vs Exam Score", x = "Parental Involvement", y = "Exam Score")

# Affect of Tutoring_Sessions on Exam_Score
ggplot(data, aes(x = Tutoring_Sessions, y = Exam_Score)) +
  geom_point(color = "darkorange") +
  theme_minimal() +
  labs(title = "Tutoring Sessions vs Exam Score", x = "Tutoring Sessions", y = "Exam Score")

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

# Scatter plot Matrix
pairs(numeric_data[, -c(3,5,6)], main = "Scatter Plot Matrix") #Sleep_Hours(3), Tutoring_Sessions(5), and Physical_Activity(6) excluded due to limited range of values

