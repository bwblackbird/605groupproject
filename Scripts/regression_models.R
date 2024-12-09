# regression_models.R: Regression Models for Student Exam Performance

# Libraries, data
library(car)
library(MASS)
library(lmtest)
library(glmnet)
data <- read.csv("Data/data.csv", header=TRUE, sep=",")

# Random training sample using 80% of data
set.seed(3210) 
train_size <- floor(0.80 * nrow(data)) 
train_indices <- sample(seq_len(nrow(data)), size = train_size)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Model 1 (M1): Initial Model, all 19 predictor variables
m1_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Sleep_Hours+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Teacher_Quality+School_Type+Peer_Influence+Physical_Activity+Learning_Disabilities+Parental_Education_Level+Distance_from_Home+Gender
m1 <- lm(m1_formula, data=train_data)
summary (m1)
par(mfrow=c(2,2))
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

# Finding, Removing Outliers
n <- nrow(train_data)
residuals <- rstandard(m2)
outliers <- which(abs(residuals) > 2)
cooks_distances <- cooks.distance(m2)
cooks_threshold <- 4 / (n - 2)
high_cooks <- which(cooks_distances > cooks_threshold)
train_data_clean <- train_data[-outliers, ]

# Model 1 with cleaned data
m1_clean <- lm(m1_formula, data=train_data_clean)
summary(m1_clean)
plot(m1_clean)

# Stepwise Selection, Forward Selection and Backward Elimination
null_model <- lm(train_data_clean$Exam_Score ~ 1, data=train_data_clean)
forward_model <- step(null_model, direction = "forward", scope = ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Sleep_Hours+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Teacher_Quality+School_Type+Peer_Influence+Physical_Activity+Learning_Disabilities+Parental_Education_Level+Distance_from_Home+Gender, trace = 1)
summary(forward_model)

backward_model <- step(m1_clean, direction = "backward", trace = 1)
summary(backward_model)

# Results from "summary(m1_clean)" indicate only School_Type and Gender 
#are not statistically significant at a 95% significance level.
# Results from stepwise selection show School_Type and Gender do not
#significantly reduce AIC.

# Model 4 (M4): All variables except School_Type and Gender
m4_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Extracurricular_Activities+Sleep_Hours+Previous_Scores+Motivation_Level+Internet_Access+Tutoring_Sessions+Family_Income+Teacher_Quality+Peer_Influence+Physical_Activity+Learning_Disabilities+Parental_Education_Level+Distance_from_Home
m4 <- lm(m4_formula, data=train_data_clean)
summary (m4)
plot(m4)
boxcox(m4)
anova(m4)
bptest(m4)

# Model 2 With cleaned data
m2_clean <- lm(m2_formula, data=train_data_clean)
summary (m2_clean)
plot(m2_clean)

# Model 5 (M5): Top 6 Most Influential Factors 
m5_formula <- Exam_Score ~ Hours_Studied+Attendance+Parental_Involvement+Access_to_Resources+Previous_Scores+Tutoring_Sessions
m5 <- lm(m5_formula, data=train_data_clean)
summary (m5)
plot(m5)

# Predictions
t <- 18
pred_point <- test_data[t, ]
prediction <- predict(m2_clean, newdata=pred_point, interval="prediction", level=0.95)
true_exam_score <- test_data[t, "Exam_Score"]

true_exam_score
prediction

