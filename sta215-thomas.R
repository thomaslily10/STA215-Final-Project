setwd("C:/Users/thoma/OneDrive/Desktop/STA215")

library(readr)
dataset <- read_csv("raw_data.csv")
#Table 2
table(dataset$grades_offered,dataset$absent_problem)
chisq.test(table(dataset$grades_offered,dataset$absent_problem))
#Boxplot
boxplot(state_aid ~ grades_offered, data = dataset)
# Calculate IQR and remove outliers, ignoring NA values
Q1 <- quantile(dataset$state_aid, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$state_aid, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Filter data to remove outliers, ignoring NA values
filtered_data <- dataset[dataset$state_aid >= (Q1 - 1.5 * IQR_value) & dataset$state_aid <= (Q3 + 1.5 * IQR_value), ]

# Create boxplot without outliers
boxplot(state_aid ~ grades_offered, data = filtered_data)
anova <- aov(state_aid ~ grades_offered, data = filtered_data)
summary(anova)

#figure 2 scatterplot
plot(dataset$students_per_district,dataset$state_aid)
abline(linear_relationship, col = "red")



#for missing variables
dataset_nomissing <- na.omit(dataset,c("state_aid", "students_per_district"))

#figure 3 residual plot
linear_relationship <- lm(dataset_nomissing$students_per_district ~ dataset_nomissing$state_aid, dataset_nomissing = dataset_nomissing)
summary(linear_relationship)
plot(dataset_nomissing$state_aid, residuals(linear_relationship))

#Table 1 descriptive statistics 
table(dataset$state_aid)
mean(dataset$state_aid)
sd(dataset$state_aid)

table(dataset$reading_score)
mean(dataset$reading_score)
sd(dataset$reading_score)

table(dataset$absent_problem)

table(dataset$grades_offered)

table(dataset$students_per_district)
mean(dataset$students_per_district)
sd(dataset$students_per_district)

table(dataset$student_teacher_ratio)
mean(dataset$student_teacher_ratio)
sd(dataset$student_teacher_ratio)

table(dataset$`total students`)
mean(dataset$`total students`)
sd(dataset$`total students`)

table(dataset$percentage_college_admissions_rate)
mean(dataset$percentage_college_admissions_rate)
sd(dataset$percentage_college_admissions_rate)

table(dataset$percentage_of_dropout_students)
mean(dataset$percentage_of_dropout_students)
sd(dataset$percentage_of_dropout_students)

table(dataset$number_of_4_years_experience_teachers)
mean(dataset$number_of_4_years_experience_teachers)
sd(dataset$number_of_4_years_experience_teachers)

table(dataset$teachers_average_annual_salary)
mean(dataset$teachers_average_annual_salary)
sd(dataset$teachers_average_annual_salary)

table(dataset$spending_per_student)
mean(dataset$spending_per_student)
sd(dataset$spending_per_student)

table(dataset$counseling_services)
mean(dataset$counseling_services)
sd(dataset$counseling_services)



