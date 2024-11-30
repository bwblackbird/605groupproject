# regression_models.R: Regression Models for Student Exam Performance

data <- read.csv("Data/data.csv", header=TRUE, sep=",")

set.seed(3210) # set a seed for a random number generator

train_size <- floor(0.80 * nrow(data))  # use 80% for training
train_indices <- sample(seq_len(nrow(data)), size = train_size)
train_data <- data[train_indices, ]

# Model 1 (M1): Initial Model, all 19 predictor variables
m1_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Sleep_Hours+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Teacher_Quality+School_Type+Peer_Influence+Physical_Activity+Learning_Disabilities+Parental_Education_Level+Distance_from_Home+Gender
m1 <- lm(m1_formula, data=train_data)
summary (m1)

# M1 Diagnostic Plot
par(mfrow = c(2, 2)) 
plot(m1)

