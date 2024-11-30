# EDA.R: Exploratory Data Analysis for Student Exam Performance

data <- read.csv("Data/data.csv", header=TRUE, sep=',')

summary(data)

summary(data$Hours_Studied)
summary(data$Attendance)
summary(data$Sleep_Hours)
summary(data$Previous_Scores)
summary(data$Tutoring_Sessions)
summary(data$Physical_Activity)



