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
#for state aid
table(dataset$state_aid)
mean(dataset$state_aid)
sd(dataset$state_aid)

#for reading score
table(dataset$reading_score)
mean(dataset$reading_score)
sd(dataset$reading_score)

#for absent problem
table(dataset_nomissing$absent_problem)

#for students per district
table(dataset_nomissing$students_per_district)
mean(dataset_nomissing$students_per_district)
sd(dataset_nomissing$students_per_district)
summary(dataset_nomissing$students_per_district)

#for student teacher ratio
table(dataset_nomissing$student_teacher_ratio)
mean(dataset_nomissing$student_teacher_ratio)
sd(dataset_nomissing$student_teacher_ratio)

#for total students
table(dataset_nomissing$`total students`)
mean(dataset_nomissing$`total students`)
sd(dataset_nomissing$`total students`)

#for percentage college admissions rate
table(dataset_nomissing$percentage_college_admissions_rate)
mean(dataset_nomissing$percentage_college_admissions_rate)
sd(dataset_nomissing$percentage_college_admissions_rate)

#for percentage of dropout students
table(dataset_nomissing$percentage_of_dropout_students)
mean(dataset_nomissing$percentage_of_dropout_students)
sd(dataset_nomissing$percentage_of_dropout_students)

#for number of 4 years experience teachers
table(dataset_nomissing$number_of_4_years_experience_teachers)
mean(dataset_nomissing$number_of_4_years_experience_teachers)
sd(dataset_nomissing$number_of_4_years_experience_teachers)

#for teacher average annual salary
table(dataset_nomissing$teachers_average_annual_salary)
mean(dataset_nomissing$teachers_average_annual_salary)
sd(dataset_nomissing$teachers_average_annual_salary)

#for spending per student
table(dataset_nomissing$spending_per_student)
mean(dataset_nomissing$spending_per_student)
sd(dataset_nomissing$spending_per_student)

#for counseling services
table(dataset_nomissing$counseling_services)
mean(dataset_nomissing$counseling_services)
sd(dataset_nomissing$counseling_services)



