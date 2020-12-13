# Multivariate analysis against the target variable

# Will be using Density Graphs to do this analysis for cont, variable as it is  lucid and easily comprehendable
# will use histograms to do analysis for categorical var.


################### A) Employee Background #####################

#Age
master %>%
  ggplot(aes(x = Age, fill = Attrition)) + 
  geom_density(alpha = 0.5) +
  ggtitle("Attrition with Age")

# Younger employees upto 30 years have a higher attrition rate


#Gender

master %>% group_by(Gender) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = Gender, fill = Attrition)) + 
  geom_bar(col = "black", position = "fill") +
  ggtitle("Attrition with Gender") 

# Males have a slightly higher attrition rate than females 


#MaritalStatus
master %>% group_by(MaritalStatus) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = MaritalStatus, fill = Attrition)) + 
  geom_bar(col = "black", position = "fill") +
  ggtitle("Attrition with MaritalStatus") 

# Singles have a high attrition rate. 
# They are more likely to leave probably because they do not have spouse or kids to worry about, hence less risky to leave



#Education
master %>% group_by(Education) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = Education, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with Education") 

# almost 19% attrition rate for employees who have studied till +2
# Probably have left for higher studies

# Maximum attrition for employees who have completed Bachelor's
# Either for higher studies or for better job opportunities

#education vs income
master %>% group_by(Education) %>% 
  summarise(avg = median(MonthlyIncome))
#Surprisingly, the difference is not very significant



#EducationField

master %>% group_by(EducationField) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = EducationField, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with EducationField") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Attrition Rate is almost 50% for employees who have studied Human Resources
# Life Sciences and Medical also have high attrition


# Is this because HR employees have low salary
master %>% group_by(EducationField, Attrition) %>%
  summarise(avg_income = median(MonthlyIncome))
# No

#Employees who have studied HR, do they have the same job role? 
HR_edu <- master[which(master$EducationField == "human resources"),]
#No, Employees who have studied HR, belong to the same Department, but their job role is not that of HR.
#This maybe the reason for attrition as these employees did not work in fields they specialize in



#TotalWorkingYears
master %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with TotalWorkingYears")

# Higher Attrition for employees whose Total working years is less than 6-7
# This is because it gets riskier with age to quit a job due to reasons such as family


#NumCompaniesWorked
master %>%
  ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with NumCompaniesWorked")

# attrition tends to be higher for those employees who have worked in 5-7 companies


###################### B) Position and Experience  #####################
#Department
master %>% group_by(Department) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = Department, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with Department") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Here again, Atrrition Rate is highest for employees working in the HR department


#JobRole
master %>% group_by(JobRole) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = JobRole, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with JobRole") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Attrition is high for Research Scientist and Sales Executives
# Highest attrition rates are observed for Research Director

master %>% group_by(JobRole) %>%
  summarize(avg = median(MonthlyIncome))
# median Salary is almost same for all job roles, thus,does not seem to be a factor


#checking with job satisfaction
master %>%
  ggplot(aes(x = JobRole, fill = as.factor(JobSatisfaction))) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with JobRole") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
# yes, tese are the job roles with the highest number of "Low Satisfaction" employees


# JobLevel
master %>% group_by(JobLevel) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = JobLevel, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with JobLevel") 

# Attrition is high for entry or junior/associate level jobs 
# maybe due to low job security or in search better job opportunities/ higher studies



#YearsWithCurrManager
master %>%
  ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with YearsWithCurrManager")

#Attrition is high for employees if he/she is with the same manager for less than 1.5 years or 5-6 years.


# YearsAtCompany
master %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with YearsAtCompany")
# Attrition is very high for employees who have been in the company for less than 2-3 years



##############################   C) Payment/Salary   ####################

# MonthlyIncome 

master %>%
  ggplot(aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with Monthly income") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Monthly income has not made much of a difference in the attrition
# However employees with salary between 10,000-20,000 are more likely to resign



#PercentSalaryHike
master %>%
  ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with PercentSalaryHike")

# Almost the same



#StockOptionLevel
master %>% group_by(StockOptionLevel) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = StockOptionLevel, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with StockOptionLevel") 

# high attrition for employees having Stock option level of 1 and 2
# Less hassle for those Employees who do not own a stock in the company to resign



##################### D) Travel and Work Time #########################
#Distance From Home
master %>%
  ggplot(aes(x = DistanceFromHome, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with Distance From Home")

#Attrition rate sees a slight increase between 10-20 kms



#BusinessTravel
master %>% group_by(BusinessTravel) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = BusinessTravel, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with BusinessTravel") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# employees who travel frequently have a much higher attrition rate


#Overtime
master %>% group_by(Overtime) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = Overtime, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with Overtime") 

# Attrition rate is higher for employees working overtime


#NumOvertimeDays
master %>%
  ggplot(aes(x = NumOvertimeDays, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with Num of OvertimeDays")

# Employees who have worked overtime more than 150 days have very high attrition rate
# as compared to employees who have worked overtime for less than 150 days


#Average_HoursWorked
master %>%
  ggplot(aes(x = Average_HoursWorked, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with Average_HoursWorked")

# Attrition is very high for employees who work more than 8 hours a day



#LeaveTaken
master %>%
  ggplot(aes(x = LeaveTaken, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with LeaveTaken")

# attrition is high for those employees who have taken leave for 6-8 days and 14-16 days






################## E) Employee Satisfaction ##########################

# EnvironmentSatisfaction 
master %>% group_by(EnvironmentSatisfaction) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = EnvironmentSatisfaction, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with EnvironmentSatisfaction") 

# Attrition is higher for those employees who have low Environment Satisfaction



#JobSatisfaction
master %>% group_by(JobSatisfaction) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = JobSatisfaction, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with JobSatisfaction") 

# Attrition is higher for those employees who have low Job satisfaction too



#WorkLifeBalance
master %>% group_by(WorkLifeBalance) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = WorkLifeBalance, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with WorkLifeBalance") 

# Again, Attrition is higher for those employees who have low work life balance



################## F) Employee Performance ##########################

# JobInvolvement 
master %>% group_by(JobInvolvement) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = JobInvolvement, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with JobInvolvement") 

#Again, as obvious, employees with low involvement in the job are more likely to resign

  


# PerformanceRating
master %>% group_by(PerformanceRating) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = PerformanceRating, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with PerformanceRating") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# employees with higher performance rating have higher attrition rates



# EmployeeRating
master %>%
  ggplot(aes(x = EmployeeRating, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with EmployeeRating")

# attrition is very high for employees whose rating is below 13.
# after 13 it decreases gradually

################## G) Employee Develpoment ##########################

# YearsSinceLastPromotion 
master %>%
  ggplot(aes(x = YearsSinceLastPromotion , fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Attrition with YearsSinceLastPromotion ")

# employees with 0-1  and 6-7 years since last promotion have higher attrition rates


# TrainingTimesLastYear
master %>% group_by(TrainingTimesLastYear) %>% 
  summarize(attrition_rate = (sum(ifelse(Attrition == "yes", 1, 0)) / n() * 100))

master %>%
  ggplot(aes(x = TrainingTimesLastYear, fill = Attrition)) + 
  geom_bar(col = "black") +
  ggtitle("Attrition with TrainingTimesLastYear") 

# employees who have received more than 5 trainings in the last year have very low attrition rate


rm(list = ls()[! ls() %in% c("master","master1")])
dev.off()
#clear console

############### Please go over to "Model_Building.R"############### 