
########################  Univariate Analysis ##################################

summary(master)

# Dropping EmployeeCount, Over18 and StandardHours since it is the same for all employees
table(master$Over18)
master <- master[ , -c(9,16,18)]

# There are some attributes that are actually categorical, but in the data set are numeric
# Lets change them to categorical for now

master[ , c(1,7,10,16,22,23,24,25,26)] <-
  as.data.frame(sapply(master[ , c(1,7,10,16,22,23,24,25,26)], as.character))
str(master)


# Lets begin #

#Attrition first
round(prop.table(table(master$Attrition)) * 100, 2)
ggplot(master,aes(x = Attrition, fill = "Attrition")) + 
  geom_bar() + theme_minimal() +
  labs(x="Attrition", y="Count of Attrition") + 
  ggtitle("Attrition")
# Target class imbalance




# The variables have been grouped into 7 categories so that it is easier to understand and generate insights

################### A) Employee Background #####################


# Age
summary(master$Age)
plot(quantile((master$Age), seq(0, 1, by = 0.01))) # no outliers

ggplot(master, aes(x = Age)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 5) +
  ggtitle("Age")

# almost normally distributed
# most employees are between 30-36 yrs of age



# Gender
table(master$Gender)
ggplot(master, aes(x = Gender)) + geom_bar(fill = "maroon") + ggtitle("Gender")
round(prop.table(table(master$Gender)) * 100, 2)

# More than 50% of Male population



# MaritalStatus
table(master$MaritalStatus)
ggplot(master, aes(x = MaritalStatus)) + geom_bar(fill = "maroon") + ggtitle("MaritalStatus")
round(prop.table(table(master$MaritalStatus)) * 100, 2)

# 46% of the employees are married whereas 22% of them are divorced



# Education
table(master$Education)
ggplot(master, aes(x = Education)) + geom_bar(fill = "maroon") + ggtitle("Education")
round(prop.table(table(master$Education)) * 100, 2)

# More than 38% have a bachelors degree and 27% have a masters degree.



# EducationField
table(master$EducationField)
ggplot(master, aes(x = EducationField)) + geom_bar(fill = "maroon") + 
  ggtitle("EducationField") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
round(prop.table(table(master$EducationField)) * 100, 2)

# Almost 75% of the employees come from Life Sciences and Medical background



# TotalWorkingYears 
summary(master$TotalWorkingYears)
plot(quantile((master$TotalWorkingYears), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = TotalWorkingYears)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 3)+
  ggtitle("TotalWorkingYears")

# Majority of the employees have been working between 9-12 years



# NumCompaniesWorked 
summary(master$NumCompaniesWorked)
plot(quantile((master$NumCompaniesWorked), seq(0, 1, by = 0.01))) # no outliers

ggplot(master, aes(x = NumCompaniesWorked)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("NumCompaniesWorked")


# Most of the employees have worked for total 2 companies including the present one



##################### B) Position and Experience  #####################


# Department
table(master$Department)
ggplot(master, aes(x = Department)) + geom_bar(fill = "maroon") + 
  ggtitle("Department")

round(prop.table(table(master$Department)) * 100, 2)

# 65% of the employees work in Research & Development 



# JobRole
table(master$JobRole)
ggplot(master, aes(x = JobRole)) + geom_bar(fill = "maroon") + ggtitle("JobRole") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
round(prop.table(table(master$JobRole)) * 100, 2)

# Majority of the employees work as Sales Executives, Research Scientists and Laboratory Technicians




# JobLevel
table(master$JobLevel)
ggplot(master, aes(x = JobLevel)) + geom_bar(fill = "maroon") + 
  ggtitle("JobLevel") 
round(prop.table(table(master$JobLevel)) * 100, 2)

# Majority of the employees are Junior or Associate level jobs



# YearsWithCurrManager 
summary(master$YearsWithCurrManager)
plot(quantile((master$YearsWithCurrManager), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = YearsWithCurrManager)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("YearsWithCurrManager")




# YearsAtCompany 
summary(master$YearsAtCompany)
plot(quantile((master$YearsAtCompany), seq(0, 1, by = 0.01))) 

#lets treat the outliers
quantile((master$YearsAtCompany), seq(0, 1, by = 0.01))
master$YearsAtCompany[which(master$YearsAtCompany > 24)] <- 24

ggplot(master, aes(x = YearsAtCompany)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1)+
  ggtitle("YearsAtCompany")

#most employees have worked 1-5 years in this company


##############################   C) Payment/Salary   ################


# MonthlyIncome 
summary(master$MonthlyIncome)
plot(quantile((master$MonthlyIncome), seq(0, 1, by = 0.01))) # no outliers
# interesting to see no sudden increment in salary

ggplot(master, aes(x = MonthlyIncome)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 5000)+
  ggtitle("MonthlyIncome")

# mean salary is 65,020 and median salary is 49,190



# PercentSalaryHike 
summary(master$PercentSalaryHike)
plot(quantile((master$PercentSalaryHike), seq(0, 1, by = 0.01))) # no outliers

ggplot(master, aes(x = PercentSalaryHike)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1)+
  ggtitle("PercentSalaryHike")

# most employees have gotten about 11-14% increase in Salary



# StockOptionLevel
table(master$StockOptionLevel)
ggplot(master, aes(x = StockOptionLevel)) + geom_bar(fill = "maroon") + ggtitle("StockOptionLevel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
round(prop.table(table(master$StockOptionLevel)) * 100, 2)

# ~44 % of the employees have not undertaken ESOP option
# 41% are at level 1 while the remaining 15% are of high level
# the 15% are probably the top management and other equity holders




##################### D) Travel and Work Time #########################


#Distance From Home
summary(master$DistanceFromHome)
plot(quantile((master$DistanceFromHome), seq(0, 1, by = 0.01))) # no outliers

ggplot(master, aes(x = DistanceFromHome)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1)+
  ggtitle("DistanceFromHome")

# most employees live within 2 kms 




# BusinessTravel
table(master$BusinessTravel)
ggplot(master, aes(x = BusinessTravel)) + geom_bar(fill = "maroon") + ggtitle("BusinessTravel")
round(prop.table(table(master$BusinessTravel)) * 100, 2)

# More than 70% of the employees Travel rarely for work purpose




# Overtime
table(master$Overtime)
ggplot(master, aes(x = Overtime)) + geom_bar(fill = "maroon") + ggtitle("Overtime")
round(prop.table(table(master$Overtime)) * 100, 2)

# more than half the employees have worked overtime


#NumOvertimeDays
summary(master$NumOvertimeDays)

ggplot(master, aes(x = round(NumOvertimeDays, 0))) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 10)+
  ggtitle("NumOvertimeDays")

# majority of the employees either have no overtime or have worked overtime on more than 220 days



# Average_HoursWorked
summary(master$Average_HoursWorked)

ggplot(master, aes(x = round(Average_HoursWorked, 0))) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1)+
  ggtitle("Average_HoursWorked")

# most employees have worked within 7-8 hours, i.e., the standard working time

 

# LeaveTaken 
summary(master$LeaveTaken)
plot(quantile((master$LeaveTaken), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = LeaveTaken)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("LeaveTaken")

# avg leaves taken is 13 days



################## E) Employee Satisfaction ##########################



# EnvironmentSatisfaction 
table(master$EnvironmentSatisfaction)
ggplot(master, aes(x = EnvironmentSatisfaction)) + geom_bar(fill = "maroon") + ggtitle("EnvironmentSatisfaction")
round(prop.table(table(master$EnvironmentSatisfaction)) * 100, 2)

# 60% of the employees have a high or very high environment satisfaction.
# 20% of the employees have a very low satisfaction level



# JobSatisfaction 
table(master$JobSatisfaction)
ggplot(master, aes(x = JobSatisfaction)) + geom_bar(fill = "maroon") + ggtitle("JobSatisfaction")
round(prop.table(table(master$JobSatisfaction)) * 100, 2)

# Again, 60% of the employees have a high or very high job satisfaction.
# 20% of the employees have a very low satisfaction level



# WorkLifeBalance 
table(master$WorkLifeBalance)
ggplot(master, aes(x = WorkLifeBalance)) + geom_bar(fill = "maroon") + ggtitle("WorkLifeBalance")
round(prop.table(table(master$WorkLifeBalance)) * 100, 2)

# More than 60% of the employees feel they have a better work life balance


################## F) Employee Performance ##########################

# JobInvolvement 
table(master$JobInvolvement)
ggplot(master, aes(x = JobInvolvement)) + geom_bar(fill = "maroon") + ggtitle("JobInvolvement")
round(prop.table(table(master$JobInvolvement)) * 100, 2)

# 69% of the employees think that they have a High or a very High job involvement at work.




# PerformanceRating
table(master$PerformanceRating)
ggplot(master, aes(x = PerformanceRating)) + geom_bar(fill = "maroon") + ggtitle("PerformanceRating")
round(prop.table(table(master$PerformanceRating)) * 100, 2)

# 85% of the employees have Excellent Rerformance Rating




################## G) Employee Develpoment ##########################

# YearsSinceLastPromotion 
summary(master$YearsSinceLastPromotion)
plot(quantile((master$YearsSinceLastPromotion), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = YearsSinceLastPromotion)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("YearsSinceLastPromotion")

# Most employees have got promoted in the last 0-1 years
# means promotion does happen on a regular basis




# TrainingTimesLastYear 
summary(master$TrainingTimesLastYear)
plot(quantile((master$TrainingTimesLastYear), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = TrainingTimesLastYear)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("TrainingTimesLastYear")

# Most employees have got trained 2-3 times in the previous year



#################### Engineering another feature: EmployeeRating #####################

# summing up the values from employee's survey and manager's survey

# rating the employee out of 20

Rating <- bind_cols(ES = master$EnvironmentSatisfaction, 
                    JS = master$JobSatisfaction,
                    WLB = master$WorkLifeBalance,
                    JI = master$JobInvolvement,
                    PR = master$PerformanceRating)

Rating <- mutate_if(Rating, is.character, as.numeric)
Rating$EmployeeRating <- rowSums(Rating)


master$EmployeeRating <- Rating$EmployeeRating
# done

# EmployeeRating 
summary(master$EmployeeRating)
plot(quantile((master$EmployeeRating), seq(0, 1, by = 0.01))) 

ggplot(master, aes(x = EmployeeRating)) + 
  geom_histogram( fill = "maroon", col = "black", binwidth = 1) +
  ggtitle("EmployeeRating")

# almost normally distributed
# max ratings lie between 13-16

dev.off()
#clear console

# Next step is in "Bivariate Analysis.R". #############

