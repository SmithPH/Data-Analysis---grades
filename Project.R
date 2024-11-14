#Project R code file for the topic Data Engineering Semester 1 2024


####################################################################################################
#1)	Data Wrangling
####################################################################################################
#1a)	Loading the data
#Presenting your work on loading the data

# Import libraries
library(mongolite)  
library(tidyverse)
library(dplyr)
library(modelr) 
library(Metrics)  


#connect to the database
connection_string = '#### USE SAMEPLE DATA "grades" FROM MONGODB ####'

#load data "grades"
grades_collection = mongo(collection="grades", db="sample_training", url=connection_string)

# load the data 
load_grades = grades_collection$find()

#select class 20
grades = load_grades[load_grades$class_id %in% c('20'),]

# Figure 1
View(grades)

# unnest a list-column (scores) 
unnest_grades <- grades %>%
  unnest(scores)
# Figure 2
View(unnest_grades) 


##=========================================================================
#1b)	Handling missing data
#Presenting your work on finding missing data or incorrect data and your corresponding actions on the data in handling missing data

# check missing values  # Figure 3
colSums(is.na(unnest_grades)) # No missing data found

# check overall data
summary(unnest_grades)  # No anomalies data found


##=========================================================================
#1c) Tidying the data
#Presenting your work on tidying the data to ensure consistency in variable names, formats and data types

# It seems that each class_id has two homework with the same name.
# Rename the duplicate homework into "homework1" and "homework2"
# Figure 4
rename_grades <- unnest_grades
counter <- 0
for (i in 1:nrow(rename_grades)) {
  if (rename_grades$type[i] == "homework") {
    counter <- counter + 1
    rename_grades$type[i] <- paste0("homework", counter)
    if (counter == 2) {
      counter <- 0
    }
  }
}
# Figure 5
View(rename_grades)

# Count occurrences of each student ID  
student_counts <- table(rename_grades$student_id)
student_counts
# student 305 has enrolled to the class three times (12/4=3)

# Make the data frame wider
wider_grades <- rename_grades %>%
  pivot_wider(names_from = type, values_from = score, values_fn = mean)
# Figure 6
View(wider_grades)

####################################################################################################
#2)	Data Transformation
####################################################################################################
#2a)	Data Transformation techniques
#Presenting for your work on applying data transformation techniques to the data.

# Since our focus is on Class 20, it was filtered during the data loading process at step 4. 
# At the first try, attempting to load all the data to rename the homework variable took more than 5 minutes to complete. 
# Therefore, it is more efficient to filter the class from the database at the beginning before renaming and expanding them. 
# Handling the identical variable issue, particularly with "homework", is simpler and more effective at the beginning 
# before expanding the dataframe and performing data transformations. This approach is consistently applied throughout the data loading section
# and subsequent sections.

##=========================================================================
#2b)	Creating new variables
#Presenting your work on creating any new variables if applicable. 

# Add columns
# Calculate the total score and grade, assumed that the score weight is equal.
mutate_grades <- wider_grades %>%
  mutate(exam_result = case_when(
    quiz <= 49 ~ "F",
    quiz <= 64 ~ "P",
    quiz <= 74 ~ "C",
    quiz <= 84 ~ "D",
    TRUE ~ "HD"
  )) %>%
  mutate(quiz_result = case_when(
    quiz <= 49 ~ "F",
    quiz <= 64 ~ "P",
    quiz <= 74 ~ "C",
    quiz <= 84 ~ "D",
    TRUE ~ "HD"
  )) %>%
  mutate(homework1_result = case_when(
    homework1 <= 49 ~ "F",
    homework1 <= 64 ~ "P",
    homework1 <= 74 ~ "C",
    homework1 <= 84 ~ "D",
    TRUE ~ "HD"
  )) %>%
  mutate(homework2_result = case_when(
    homework2 <= 49 ~ "F",
    homework2 <= 64 ~ "P",
    homework2 <= 74 ~ "C",
    homework2 <= 84 ~ "D",
    TRUE ~ "HD"
  )) %>%
  mutate(final_score = (exam + quiz + homework1 + homework2) / 4) %>%
  mutate(grade = case_when(
    final_score <= 49 ~ "F",
    final_score <= 64 ~ "P",
    final_score <= 74 ~ "C",
    final_score <= 84 ~ "D",
    TRUE ~ "HD"
  )) %>%
  mutate(result = case_when(
    final_score <= 49 ~ "F",
    TRUE ~ "P"
  ))
# Figure 7
View(mutate_grades)


c20 <- mutate_grades
c20

####################################################################################################
#3)	Data Analysis
####################################################################################################
#a)	Statistical analysis or exploratory data analysis
#Presenting your work in statistical analysis or Exploratory data analysis to uncover patterns, trends or relationships within the data.

# Sort grade data, to make it look better (ordered by grade) when visualise the data.
sorted_c20 <- c20
grade_order <- c("F", "P", "C", "D", "HD")
# grade_order <- c("HD", "D", "C", "P", "F")
sorted_c20$grade <- factor(c20$grade, levels = grade_order)
sorted_c20$exam_result <- factor(c20$exam_result, levels = grade_order)
sorted_c20$quiz_result <- factor(c20$quiz_result, levels = grade_order)


# Grade Distribution
sorted_c20 %>%
  count(grade)

# Summary statistics  # Figure 8
summary(sorted_c20)

# frepoly
ggplot(data = sorted_c20, mapping = aes(x = exam, colour = grade)) +
  geom_freqpoly(binwidth = 1) 

ggplot(data = sorted_c20, mapping = aes(x = quiz, colour = grade)) +
  geom_freqpoly(binwidth = 1)

# Figure 9
ggplot(data = sorted_c20, mapping = aes(x = final_score, colour = grade)) +
  geom_freqpoly(binwidth = 1)


#Two continuous variables
# Figure 10
ggplot(data = sorted_c20) +
  geom_point(mapping = aes(x = exam, y = final_score, color=grade)) +
  labs(x = "Exam Score", y = "Final Score", title = "Scatter Plot: Exam Score vs Final Score")

#Two categorical variables
ggplot(data = sorted_c20) +
  geom_point(mapping = aes(x = final_score, y = exam_result, color=grade)) +
  labs(x = "Final Score", y = "Exam Result", title = "Scatter Plot: Final Score vs Exam Result")

sorted_c20 %>%
  count(grade, exam_result) %>%
  ggplot(mapping = aes(x = exam_result, y = grade)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low="white",high="skyblue") + 
  labs(x = "Exam Result", y = "Grade", title = "Heatmap: Exam Result vs Grade")

# Figure 11
sorted_c20 %>%
  count(grade, quiz_result) %>%
  ggplot(mapping = aes(x = quiz_result, y = grade)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low="white",high="skyblue") +
  labs(x = "Quiz Result", y = "Grade", title = "Heatmap: Quiz Result vs Grade")


##=========================================================================
#3b)	Data visualisation
#Presenting your work in applying data visualisation techniques

# Scatter plot 
# Figure 12
ggplot(data = sorted_c20, mapping = aes(x = exam, y = final_score)) +
  geom_point(mapping = aes(color = grade)) + 
  geom_smooth() +
  labs(x = "Exam Score", y = "Final Score", title = "Scatter Plot: Exam Score vs Final Score") 

ggplot(data = sorted_c20, mapping = aes(x = quiz, y = final_score)) +
  geom_point(mapping = aes(color = grade)) + 
  geom_smooth() +
  labs(x = "Quiz Score", y = "Final Score", title = "Scatter Plot: Quiz Score vs Final Score")


#Facets
# Figure 13
ggplot(data = sorted_c20) + 
  geom_point(mapping = aes(x = final_score, y = exam)) + 
  facet_wrap(~ grade, nrow = 2) +
  labs(x = "Final Score", y = "Exam", title = "Facets: Final Score vs Exam")

ggplot(data = sorted_c20) + 
  geom_point(mapping = aes(x = final_score, y = quiz)) + 
  facet_wrap(~ grade, nrow = 2) +
  labs(x = "Final Score", y = "Quiz", title = "Facets: Final Score vs Quiz")


# Bar Chart
# Figure 14
ggplot(data = sorted_c20) + 
  geom_bar(mapping = aes(x = result, fill = grade)) +
  labs(x = "Grade", y = "Frequency", title = "Frequency of Grade") 

# Figure 15
ggplot(data = sorted_c20) + 
  geom_bar(mapping = aes(x = grade, fill = grade)) +
  labs(x = "Grade", y = "Frequency", title = "Frequency of Grade") 

ggplot(data = sorted_c20) + 
  geom_bar(mapping = aes(x = exam_result, fill = grade),position = "fill") +
  labs(x = "Exam Result", y = "Frequency", title = "Frequency of Exam Grade")

ggplot(data = sorted_c20) + 
  geom_bar(mapping = aes(x = exam_result, fill = grade), position = "dodge") +
  labs(x = "Exam Result", y = "Frequency", title = "Frequency of Exam")

# Histogram 
# Figure 16
ggplot(sorted_c20, aes(x = final_score)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "blue") +
  labs(title = "Distribution of Final Scores", x = "Final Score", y = "Frequency")

ggplot(sorted_c20, aes(x = exam)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "blue") +
  labs(title = "Distribution of Exam Scores", x = "Exam Score", y = "Frequency")

ggplot(sorted_c20, aes(x = quiz)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "blue") +
  labs(title = "Distribution of Quiz Scores", x = "Quiz Score", y = "Frequency")


#Boxplots
# Figure 17
ggplot(sorted_c20, aes(x = grade, y = final_score, fill = grade)) +
  geom_boxplot() +
  labs(title = "Final Scores by Grade", x = "Grade", y = "Final Score") 

ggplot(sorted_c20, aes(x = grade, y = exam, fill = grade)) +
  geom_boxplot() +
  labs(title = "Exam Scores by Grade", x = "Grade", y = "Exam Score")

ggplot(sorted_c20, aes(x = grade, y = quiz, fill = grade)) +
  geom_boxplot() +
  labs(title = "Quiz Scores by Grade", x = "Grade", y = "Quiz Score")



####################################################################################################
#4)	Data Modelling
####################################################################################################
#4a)	Simple linear model
#Presenting your work in developing a simple linear model
# create a new dataframe to work with the model
c20_m1 <- c20

#Fit the model 1
c20_m1_mod1 <- lm(final_score ~ exam, data = c20_m1)
c20_m1_mod1 # Figure 18

#Create grid + Add prediction
c20_m1_grid<- c20_m1 %>% 
  data_grid(exam) %>%
  add_predictions(c20_m1_mod1) 
c20_m1_grid

#Plot the predictions
# Figure 19
ggplot(c20_m1, aes(exam)) +
  geom_point(aes(y = final_score)) +
  geom_line(aes(y = pred), data = c20_m1_grid, colour = "red", linewidth = 1) +
  labs(x = "Exam Score", y = "Final Score", title = "Simple linear model")


##=========================================================================
#4b)	General linear model
#4bi)	Predictors are categorical
#Presenting your work in developing a general linear model where the predictors are categorical.
# create a new dataframe to work with the model
c20_m2 <- sorted_c20

#Visualise the data
ggplot(c20_m2) + 
  geom_point(aes(quiz_result, final_score))

#Fit the model 2
c20_m2_mod1 <- lm(final_score ~ quiz_result, data = c20_m2)
c20_m2_mod1 # Figure 20

#Create grid + add prediction
c20_m2_grid <- c20_m2 %>% 
  data_grid(quiz_result) %>% 
  add_predictions(c20_m2_mod1)
c20_m2_grid

#Plot the predictions
# Figure 21
ggplot(c20_m2, aes(quiz_result)) + 
  geom_point(aes(y = final_score)) +
  geom_point(data = c20_m2_grid, aes(y = pred), colour = "red", size = 4) + 
  labs(x = "Quiz Result", y = "Final Score", title = "Predictors are categorical")


##=========================================================================
#4bii)	Predictors are categorical and continuous
#Presenting your work in developing a general linear model where the predictors are categorical and continuous.
#predictors: categorical and continuous
# create a new dataframe to work with the model
c20_m3 <- sorted_c20

#Visualise the data
ggplot(c20_m3, aes(exam, final_score)) + 
  geom_point(aes(colour = quiz_result))

#Fit the model 3_1
c20_m3_mod1 <- lm(final_score ~ exam + quiz_result, data = c20_m3)
c20_m3_mod1 # Figure 22

#Fit the model 3_2
c20_m3_mod2 <- lm(final_score ~ exam * quiz_result, data = c20_m3)
c20_m3_mod2 # Figure 23

#Create grid + add prediction
c20_m3_grid <- c20_m3 %>% 
  data_grid(exam, quiz_result) %>% 
  gather_predictions(c20_m3_mod1, c20_m3_mod2)
c20_m3_grid

#Plot the predictions
# Figure 24
ggplot(c20_m3, aes(exam, final_score, colour = quiz_result)) + 
  geom_point() + 
  geom_line(data = c20_m3_grid, aes(y = pred)) + 
  facet_wrap(~ model) +
  labs(x = "Exam", y = "Final Score", title = "Predictors are categorical and continuous")


#=======================================================
#4biii)	Predictors are continuous
#Presenting your work in developing a general linear model where the predictors are continuous.
#predictors: two continuous
# create a new dataframe to work with the model
c20_m4 <- c20

#Fit the model 4_1
c20_m4_mod1 <- lm(final_score ~ exam + quiz, data = c20_m4)
c20_m4_mod1 # Figure 25

#Fit the model 4_2
c20_m4_mod2 <- lm(final_score ~ exam * quiz, data = c20_m4)
c20_m4_mod2 # Figure 26

#Create grid + add prediction
c20_m4_grid <- c20_m4 %>% 
  data_grid(
    exam = seq_range(exam, 5), 
    quiz = seq_range(quiz, 5) 
  ) %>% 
  gather_predictions(c20_m4_mod1, c20_m4_mod2)
c20_m4_grid

#Plot the predictions
# Figure 27
ggplot(c20_m4_grid, aes(exam, quiz)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model) +
  labs(x = "Exam Score", y = "Quiz Score", title = "Predictors are two continuous")

#Plot the predictions
# Figure 28
ggplot(c20_m4_grid, aes(exam, pred, colour = quiz, group = quiz)) + 
  geom_line() +
  facet_wrap(~ model) +
  labs(title = "Predictors are two continuous")

#Plot the predictions
# Figure 29
ggplot(c20_m4_grid, aes(quiz, pred, colour = exam, group = exam)) + 
  geom_line() +
  facet_wrap(~ model) +
  labs(title = "Predictors are two continuous")

#########################################################################################
#4c)	Model evaluation
#########################################################################################
#Presenting your work in evaluating the models in part 4a and part 4b 

# RSME
# ===============================================================
#4a)	Simple linear model
# Model 1
c20_m1 <- c20_m1 %>%
  add_predictions(c20_m1_mod1)
rmse(c20_m1$final_score, c20_m1$pred)


# ===============================================================
#4bi)	Predictors are categorical
# Model 2
c20_m2 <- c20_m2 %>%
  add_predictions(c20_m2_mod1)
rmse(c20_m2$final_score, c20_m2$pred)


## ===============================================================
#4bii)	Predictors are categorical and continuous
# Model 3_1
c20_m3_1 <- c20_m3 %>%
  add_predictions(c20_m3_mod1)
rmse(c20_m3_1$final_score, c20_m3_1$pred)


#Model 3_2
c20_m3_2 <- c20_m3 %>%
  add_predictions(c20_m3_mod2)
rmse(c20_m3_2$final_score, c20_m3_2$pred)

## ==============================================================
#4biii) Predictors are continuous
#Model 4_1
c20_m4_1 <- c20_m4 %>%
  add_predictions(c20_m4_mod1)
rmse(c20_m4_1$final_score, c20_m4_1$pred)


#Model 4_2
c20_m4_2 <- c20_m4 %>%
  add_predictions(c20_m4_mod2)
rmse(c20_m4_2$final_score, c20_m4_2$pred)


#4d)	Model Interpretation
#Presenting your work in interpreting the models in part 4a and part 4b if applicable

# Model 1: This model predicts final scores based on a continuous variable (exam). The RMSE is 11.51905, which is the second highest among all models, suggesting potentially poorer prediction performance compared to other models.
# Model 2: This model predicts final scores based on a categorical variable (quiz_result). The RMSE is 12.94866, which is the highest among all models, indicating the poorest prediction performance.
# Model 3_1: This model combines both continuous and categorical (exam + quiz_result) as the predictors. The RMSE is 9.725987, which is the lower than Model 1 and Model 2. This indicates that using both exam and quiz_result can improve prediction accuracy.
# Model 3_2: This model combines both continuous and categorical (exam * quiz_result) as the predictors. The RMSE is 9.655133, which is slightly lower than Model 3_1. This shows that considering the interaction between exam and quiz_result further improve the prediction performance.
# Model 4_1: This model used two continuous variables (exam + quiz) as the predictors. The RMSE is9.737126, which is slightly higher than Model 3_1. This suggests that using both continuous variables (exam and quiz) may not improve prediction accuracy compared to Model 3_1.
# Model 4_2: This model includes interaction between two continuous variables (exam * quiz) to predict the final score. The RMSE is 9.721789, which is slightly lower than Model 4_1. This indicates that using exam and quiz may improve prediction performance.
# According to the RSME result, Model 3_2 appears to be the best model with the lowest RSME of 9.655133. However, it is important to consider the other factors such as model interpretability, especially the quality of training data. If the data is biased, the model is biased. Therefore, it is essential to access models comprehensively, considering various factors beyond numerical metrics.
