# regression_models.R: Regression Models for Student Exam Performance

# Libraries, data
library("car")
library("MASS")
data <- read.csv("Data/data.csv", header=TRUE, sep=",")

# Random training sample using 80% of data
set.seed(3210) 
train_size <- floor(0.80 * nrow(data)) 
train_indices <- sample(seq_len(nrow(data)), size = train_size)
train_data <- data[train_indices, ]

# Model 1 (M1): Initial Model, all 19 predictor variables
m1_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Sleep_Hours+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Teacher_Quality+School_Type+Peer_Influence+Physical_Activity+Learning_Disabilities+Parental_Education_Level+Distance_from_Home+Gender
m1 <- lm(m1_formula, data=train_data)
summary (m1)
par(mfrow = c(2, 2)) 
plot(m1)

# Calculate VIF
vif(m1)

# Results from "summary(m1)" indicate Gender, Distance_from_Home, Parental_Education, School_Type,
#Teacher_Quality, and Sleep_Hours are not statistically significant at a 95% significance level.  

# Model 2 (M2): All variables that are statistically significant at a 95% significance level
m2_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Peer_Influence+Physical_Activity+Learning_Disabilities
m2 <- lm(m2_formula, data=train_data)
summary (m2)
plot(m2)
boxcox(m2)

# Results from "boxcox(m2)" indicate an inverse transformation could be insightful

# Model 3 (M3): Inverse Transformation of M2
m3_formula <- (1/Exam_Score) ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Peer_Influence+Physical_Activity+Learning_Disabilities
m3 <- lm(m3_formula, data=train_data)
summary (m3)
plot(m3)
boxcox(m3)

# Finding Outliers and Cooks Distances
n <- nrow(train_data)
residuals <- rstandard(m2)
outliers <- which(abs(residuals) > 2)
cooks_distances <- cooks.distance(m2)
cooks_threshold <- 4 / (n - 2)
high_cooks <- which(cooks_distances > cooks_threshold)
outliers
high_cooks




