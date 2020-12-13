# use #### to navigate ####
#Clearing the screen
rm(list = ls())
dev.off()

require(readr)
require(dplyr)

# Handling intime and outime first to generate features which can then be binded to the master datset in "EDA.R"
# importing the datasets
in_time <- read_csv("in_time.csv")
out_time <- read_csv("out_time.csv")

# Renaming Primary Key
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# 52 x 5 = 260 columns (representative of working days Monday to Friday in 2015)


str(in_time)
# data-type of days with NA values are of logical type
# Columns with full NA values are public holidays such as Republic Day, May Day, Christmas and Gandhi Jayanti.
# or days when all employees did not come for work

sum(is.na(in_time)) == sum(is.na(out_time))
#seems like there is no logical error that an employee has checked in but has not checked out and vice-versa


################## Deriving the hours worked each day #####################
hours <- out_time - in_time

# eliminating the public holiday days
summary(hours)
final_hours <- hours[,which(colSums(is.na(hours)) != 4410)]

#remaining NA values are those when the employee did not come for work on a working day for whatever reason

str(final_hours) 

# converting difftime to as.numeric
for(i in 1:250)
{
  final_hours[,i] <- as.numeric(final_hours[,i])
}
str(final_hours)

################ Number of days worked overtime ################################

# An employee can be considered to have worked overtime only when he/she is entitled to receive payment for the same.
# if working time > 8(std. hours.), then Overtime is 1, else 0 
overtime <- as.data.frame(ifelse(final_hours > 8, "1", "0"))

# changing structure to numeric 
overtime <- mutate_if(overtime, is.character, as.numeric)

# no. of days worked overtime
overtime$days_overtime <- rowSums(overtime, na.rm = T)

summary(overtime$days_overtime)


############# Total Leave Taken by each employee ################

mean(rowSums(is.na(final_hours)))
# on an average, 13 days of leave has been taken by each employee
# because it is a 5 day working week, the total sick leaves allowed in a year is 15
# an adult worker in India is entitled to one day earned for every 25 days of service, thus 260/25 = 10 paid leaves


# engineering another feature with number of days taken off
# max permitted is 15 + 10 = 25
final_hours$Leave_Taken <- rowSums(is.na(final_hours))


################### Average Hours Worked by the employee per day #################
# avg hours worked daily = total hours worked/ no. of days worked

final_hours$Average_HoursWorked <- rowSums(final_hours, na.rm = T) / rowSums(!is.na(final_hours))


#employeeID
final_hours$EmployeeID <- in_time$EmployeeID

################# New Data frame with all features #################
HoursWorked <- bind_cols(EmployeeID = final_hours$EmployeeID, 
                         Average_HoursWorked = round(final_hours$Average_HoursWorked, 2),
                         LeaveTaken = final_hours$Leave_Taken,
                         NumOvertimeDays = overtime$days_overtime)



################ Has the employee worked overtime atleast for a day? ###############
# If employee worked more than the standard time per day(8 hours) on any day, then will be classified as overtime
HoursWorked$Overtime <- ifelse(overtime$days_overtime > 1, "yes", "no")
table(HoursWorked$Overtime)
round(prop.table(table(HoursWorked$Overtime)) * 100, 2)


#Clearing the unnecessary vars
rm(list = ls()[! ls() %in% c("HoursWorked")])
# clear the console


############ EDA for further analysis ######################

# Loading all the required packages
require(readr)
require(dplyr)
require(ggplot2)

# Loading all the datasets 
general_data <- read_csv("general_data.csv")
employee_survey <- read_csv("employee_survey_data.csv")
manager_survey <- read_csv("manager_survey_data.csv")

# merging all 3 datasets into a master dataset by EmployeeID
master <- merge(general_data, employee_survey, by = "EmployeeID", all = T)
master <- merge(master, manager_survey, by = "EmployeeID", all = T)
master <- merge(master, HoursWorked, by = "EmployeeID", all = T)
master1 <- master

######### Basic EDA ################
# checking general structure
glimpse(master)
#dealing with 2 types- numeric and character/factor

# Target class imbalance
table(master$Attrition)
round(prop.table(table(master$Attrition)) * 100, 2)
# crucial factor in determining model selection


# Converting to lowercase to prevent case - mistmatch
master <- mutate_if(master, is.character, tolower)

# No duplicate observations
sum(duplicated(master))

# Check for any missing/NA records
sum(is.na(master))
colSums(is.na(master))
master1 <- master

##################### NA Treatment ###################


# Lets first look at features with NAs from general_data.csv

# Employee's Current Job Satisfaction and manager's report will not make a difference to this variable

#################### Num Companies Worked  ######################
sum(is.na(master$NumCompaniesWorked)) / nrow(master) * 100
# just 0.4% obs from NumCompaniesWorked are missing

#grouping to get insights

g1 <- general_data %>% group_by(NumCompaniesWorked) %>%
  summarise(perc = round(n()/4410 * 100, 0),
            attrition_rate = round(sum(ifelse(Attrition == "Yes", 1, 0) / n() * 100),0),
            avg_workingyears = round(median(TotalWorkingYears, na.rm = T), 0),
            avg_age = round(median(Age),2),
            yearstcom = median(YearsAtCompany))

View(g1)
# What do we see?-
# Attrition rate is higher for those who have worked in more than 5 companies.
# median working years for employees who have worked in 1 or 2 companies is 7 years while median age is 32

# all three are characteristics which is seen in employees who have worked in 1-2 companies
# So, replacing with 1 

par(mfrow = c(1, 2))
hist(master$NumCompaniesWorked) # with NAs

master$NumCompaniesWorked[which(is.na(master$NumCompaniesWorked))] <- 1

hist(master$NumCompaniesWorked) # after removing

par(mfrow = c(1,1))

################  TotalWorkingYears   ##########################

sum(is.na(master$TotalWorkingYears)) / nrow(master) * 100
# just 0.2% obs from TotalWorkingYears are missing

#grouping to get insights

g2 <- general_data %>% group_by(TotalWorkingYears) %>%
  summarise(perc = round(n()/4410 * 100, 0),
            attrition_rate = round(sum(ifelse(Attrition == "Yes", 1, 0) / n() * 100),0),
            avg_companiesworked = round(median(NumCompaniesWorked, na.rm = T), 0),
            avg_age = round(median(Age),2),
            yearstcom = median(YearsAtCompany))

View(g2)

# Unable to identify any pattern

#Lets run a algorithm with target class as TotalWorkingYears(continuous)

# before that replace NA values in general dataset from NumCompaniesWorked
general_data$NumCompaniesWorked[which(is.na(general_data$NumCompaniesWorked))] <- 1

############### running a DT algorithm with TotalCompaniesWorked as target ###########

# Split avl. data into train and val. and test datasets
test_twy <- general_data[which(is.na(general_data$TotalWorkingYears)),]

train_twy <- general_data[which(!is.na(general_data$TotalWorkingYears)),] 

set.seed(100)
index = sample(1:4401, 0.75 * 4401, replace = F)

trn_twy <- general_data[index,]
val_twy <- general_data[-index,]

require(rpart)
require(rpart.plot)

twy_mod <- rpart(TotalWorkingYears ~., data = general_data,
                 control = rpart.control(cp = 0.01))
prp(twy_mod)

#predict
pred_twy <- predict(twy_mod, newdata = val_twy)

require(forecast)
accuracy(val_twy$TotalWorkingYears, pred_twy)


# replacing NA
test_twy$TotalWorkingYears <- predict(twy_mod, newdata = test_twy)

# binding into general
general_data <- bind_rows(train_twy, test_twy)

#all NA cleared in general-data

par(mfrow = c(1,2))
hist(master$TotalWorkingYears) # before
#replacing NA in master data-set from total working years column
master$TotalWorkingYears[which(is.na(master$TotalWorkingYears))] <-
  test_twy$TotalWorkingYears

hist(master$TotalWorkingYears)
# No change in its distribution


###################################################################3
### handling NAs from employee Survey table now ########3

colSums(is.na(master))

######################### Environment Satisfaction  ####################

g3 <- master %>% group_by(EnvironmentSatisfaction) %>%
  summarise(leave = median(LeaveTaken),
            ot_days = median(NumOvertimeDays),
            avg_hrs_worked = median(Average_HoursWorked),)
View(g3)


#We see that-
# Number of leaves that has been taken by such employees is very less
# median of number of days spent working overtime is maximum 
# daily average hours worked is also very high

# looking at the above 3 pointers, it is clear that the employees' Environment Satisfaction was very high
#thus we replace by 4


hist(master$EnvironmentSatisfaction) #before

master$EnvironmentSatisfaction[which(is.na(master$EnvironmentSatisfaction))] <- 4

hist(master$EnvironmentSatisfaction) #after
#Same distribution



################## JobSatisfaction #######################

g4 <- master %>% group_by(JobSatisfaction) %>%
  summarise(leave = median(LeaveTaken),
            ot_days = median(NumOvertimeDays),
            hrs_worked = median(Average_HoursWorked),
            last_prom = mean(YearsSinceLastPromotion),
            job_level = median(as.numeric(JobLevel)),
            salary = median(MonthlyIncome),
            attrition_rate = round(sum(ifelse(Attrition == "yes", 1, 0) / n() * 100),0),)
View(g4)

# We see that-
# employee has taken high number of leaves
# the no. of days they have worked overtime is very less
# daily avg hours worked in minimum
# employees got last promotion approx. 2.5 yrs ago which is the longest
# The level of job is 1, the lowest
# the monthly salary is the least of them all.

#From the above pointers we can say that the employees' Satisfaction with the job was very low
# thus we replace by 1

hist(master$JobSatisfaction) #with NAs

master$JobSatisfaction[which(is.na(master$JobSatisfaction))] <- 1

hist(master$JobSatisfaction) #without NAs



########################## WorkLifeBalance #######################

g5 <- master %>% group_by(WorkLifeBalance) %>%
  summarise(n(),leave = round(mean(LeaveTaken),1),
            ot_days = round(mean(NumOvertimeDays),0),
            last_prom = round(mean(YearsSinceLastPromotion),2),
            job_satisf = round(mean(as.numeric(JobSatisfaction)),2),
            env_satisf = round(mean(as.numeric(EnvironmentSatisfaction)),2),
            salary = median(MonthlyIncome),
            attrition_rate = round(sum(ifelse(Attrition == "yes", 1, 0) / n() * 100),0))
View(g5)

sum(is.na(master$WorkLifeBalance)) / nrow(master) * 100
# just 0.8%

# there is hardly any CLEAR pattern
####### lets run a quick DT algorithm with WorkLifeBalance as target class ############

train_wlb <- master[which(!is.na(master$WorkLifeBalance)),]
test_wlb <- master[which(is.na(master$WorkLifeBalance)),]

train_wlb$WorkLifeBalance <- as.character(train_wlb$WorkLifeBalance)

set.seed(100)
require(caTools)
index <- sample.split(train_wlb$WorkLifeBalance, SplitRatio = 0.75)

trn_wlb <- train_wlb[index,]
val_wlb <- train_wlb[!index,]

require(rpart)
require(rpart.plot)

dt_wlb <- rpart(WorkLifeBalance ~., data = trn_wlb[,-1],
                control = rpart.control(cp = 0.004))

pred_wlb <- predict(dt_wlb, newdata = val_wlb[,-1])

pred_wlb <- as.data.frame(pred_wlb)
pred_wlb <- mutate_if(pred_wlb, is.character, as.numeric)

str(pred_wlb)

colnames(pred_wlb) <- c("a","b","c","d")

for(j in 1:nrow(pred_wlb)) {
  pred_wlb$pred_worklife[j] <- colnames(pred_wlb)[which.max(pred_wlb[j, ])]
}

pred_wlb$pred_worklife[which(pred_wlb$pred_worklife == "a")] <- "1"
pred_wlb$pred_worklife[which(pred_wlb$pred_worklife == "b")] <- "2"
pred_wlb$pred_worklife[which(pred_wlb$pred_worklife == "c")] <- "3"
pred_wlb$pred_worklife[which(pred_wlb$pred_worklife == "d")] <- "4"

table(pred_wlb$pred_worklife)
table(val_wlb$WorkLifeBalance)

# WE see that the model is highly biased towards the majority class(mode)
# But it seems better than just replacing it with mode

predicted_wlb <- predict(dt_wlb, newdata = test_wlb[,-1])

predicted_wlb <- as.data.frame(predicted_wlb)
predicted_wlb <- mutate_if(predicted_wlb, is.character, as.numeric)

str(predicted_wlb)

colnames(predicted_wlb) <- c("a","b","c","d")

for(j in 1:nrow(predicted_wlb)) {
  predicted_wlb$predicted_worklife[j] <- colnames(predicted_wlb)[which.max(predicted_wlb[j, ])]
}

predicted_wlb$predicted_worklife[which(predicted_wlb$predicted_worklife == "a")] <- "1"
predicted_wlb$predicted_worklife[which(predicted_wlb$predicted_worklife == "b")] <- "2"
predicted_wlb$predicted_worklife[which(predicted_wlb$predicted_worklife == "c")] <- "3"
predicted_wlb$predicted_worklife[which(predicted_wlb$predicted_worklife == "d")] <- "4"


#replacing
test_wlb$WorkLifeBalance <- predicted_wlb$predicted_worklife

hist(master$WorkLifeBalance) #before

# binding into master
master <- bind_rows(train_wlb, test_wlb)

hist(as.numeric(master$WorkLifeBalance)) #After
# no change in distribution

#all NA cleared in Master
sum(is.na(master))


#removing unnecessary variables
rm(list = ls()[! ls() %in% c("HoursWorked","general_data", "employee_survey", "manager_survey","master", "master1")])
dev.off()
# clear console: Ctrl + l

################# "Univariate_Analysis" for in-depth indiviual analysis #############

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

# Next step is "Bivariate Analysis". #############

# Multivariate analysis against the target variable

# Will be using Density Graphs to do this analysis for cont, variable as it islucid and easily comprehendable
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

############### "Model_Building"############### 
#since we want to know the variable importance, we will try:
# Logistic regression
# Decision Tree
# Random Forest


# since Random Forest is efficient with data which has target class imbalance.
# we will build a model with that first
master1 <- master

str(master)

# Converting Categorical-Interval to continuous type
master[ , c(22:26)] <-
  as.data.frame(sapply(master[ , c(22:26)], as.numeric))
str(master)

#there are two Categorical-Ordinal variables: Education and Job Level
# will treat them as categorical

###################  Random Forest #################

# Random Forest requires all categorical variables as factor type
master <- mutate_if(master, is.character, as.factor)
str(master)

master_rf <- master

#separate into train and test whilst maintaining target class ratio in both
set.seed(200)
require(caTools)
i <- sample.split(master_rf$Attrition, SplitRatio = 0.75)
trn_rf <- master_rf[i,]
val_rf <- master_rf[!i,]

# target class proportion maintained
round(prop.table(table(trn_rf$Attrition)) * 100, 2)
round(prop.table(table(val_rf$Attrition)) * 100, 2)

# random forest
require(randomForest)

# training on trn_rf
set.seed(200)
rf_model <- randomForest(Attrition ~., data = trn_rf[,-1], 
                         ntree = 500, do.trace = T, importance = T)
summary(rf_model)

#predicting on val_rf
pred_attrition_rf <- predict(rf_model, newdata = val_rf[,-1], type = "prob")

# prediction returns in probability
# thus setting a optimal- cut off rate
prob_attrition_rf <- ifelse(pred_attrition_rf[,2] > 0.5, "yes", "no")

str(val_rf$Attrition)
prob_attrition_rf <- as.factor(prob_attrition_rf)

# checking accuracy
require(caret)
confusionMatrix(val_rf$Attrition, prob_attrition_rf, positive = "yes")

# Accuracy of 98.37% and sensitivity & specificity are almost perfectly balanced.
# sensitivity: 98.19
# specificity: 98.40

# Also, the target class ratio has been maintained, no bias is present
round(prop.table(table(val_rf$Attrition)) * 100, 2)
round(prop.table(table(prob_attrition_rf)) * 100, 2)


# Now lets see the variable importance
# based on mean decrease in GINI (type = 2)
rf_imp <- as.data.frame(importance(rf_model))
View(rf_imp)

varImpPlot(rf_model, sort = T, 
           n.var = 30,
           type = 2, main = deparse(substitute(rf_model)))



####################################Logistic Regression Models ###########################################

# Now let's try Binomial Logistic Regression model ##
# first without dealing with target class imbalance
# then after dealing with target class imbalance problem, so we can see the difference


############## Prepping data for Logistic Regression ###############
master_lgm <- master #will use this 

#scaling down the continuous variable
master_lgm <- mutate_if(master_lgm, is.numeric, scale)
summary(master_lgm)

# dummy variable
require(dummies)
master_lgm_dum <- dummy.data.frame(as.data.frame(master_lgm[,-1]))

master_lgm_dum$Attrition <- master_lgm_dum$Attritionyes

master_lgm_dum$Attritionno <- NULL
master_lgm_dum$Attritionyes <- NULL

master_lgm_dum <- bind_cols(EmployeeID = master_lgm$EmployeeID, master_lgm_dum)

# separating into trn and val datasets
require(caTools)
set.seed(200)
ind <- sample.split(master_lgm_dum$Attrition, SplitRatio = 0.75)
trn_lgm <- master_lgm_dum[ind,]
val_lgm <- master_lgm_dum[!ind,]

#ratio maintained
round(prop.table(table(master_lgm_dum$Attrition)) * 100, 2)
round(prop.table(table(trn_lgm$Attrition)) * 100, 2)
round(prop.table(table(val_lgm$Attrition)) * 100, 2)


############################## LGM w/o SMOTE #############################

#Building first default model with all variables
lgm1 <- glm(Attrition ~., data = trn_lgm[,-1], family = "binomial")
summary(lgm1)
# AIC: 2265.9

lgm2 <- step(lgm1)
summary(lgm2)
# 2235.1

require(car)
sort(vif(lgm2))
# no multi-colleanirity

#removing vars with high P

#`JobRoleresearch scientist`  
lgm3 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
              BusinessTraveltravel_frequently + `EducationFieldhuman resources` + 
              `EducationFieldlife sciences` + EducationFieldmedical + Genderfemale + 
              JobLevel2 + `JobRolemanufacturing director` + `JobRoleresearch director` + 
              `JobRolesales executive` + 
              MaritalStatusdivorced + MaritalStatusmarried + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
              YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
              WorkLifeBalance + Average_HoursWorked + NumOvertimeDays + 
              Overtimeno, family = "binomial", data = trn_lgm[, -1])
summary(lgm3)
# 2235.8


#`JobRolesales executive`  
lgm4 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
              BusinessTraveltravel_frequently + `EducationFieldhuman resources` + 
              `EducationFieldlife sciences` + EducationFieldmedical + Genderfemale + 
              JobLevel2 + `JobRolemanufacturing director` + `JobRoleresearch director` + 
              MaritalStatusdivorced + MaritalStatusmarried + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
              YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
              WorkLifeBalance + Average_HoursWorked + NumOvertimeDays + 
              Overtimeno, family = "binomial", data = trn_lgm[, -1])
summary(lgm4)
# 2237.1


#Overtimeno 
lgm5 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
              BusinessTraveltravel_frequently + `EducationFieldhuman resources` + 
              `EducationFieldlife sciences` + EducationFieldmedical + Genderfemale + 
              JobLevel2 + `JobRolemanufacturing director` + `JobRoleresearch director` + 
              MaritalStatusdivorced + MaritalStatusmarried + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
              YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
              WorkLifeBalance + Average_HoursWorked + NumOvertimeDays, 
            family = "binomial", data = trn_lgm[, -1])
summary(lgm5)
# 2238.2


#`EducationFieldlife sciences` 
lgm6 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
              BusinessTraveltravel_frequently + `EducationFieldhuman resources` + 
              EducationFieldmedical + Genderfemale + 
              JobLevel2 + `JobRolemanufacturing director` + `JobRoleresearch director` + 
              MaritalStatusdivorced + MaritalStatusmarried + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
              YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
              WorkLifeBalance + Average_HoursWorked + NumOvertimeDays, 
            family = "binomial", data = trn_lgm[, -1])
summary(lgm6)
# 2239.4

#All variables are important now. Predicting now on val_lgm
pred_attrition_lgm <- predict(lgm6, newdata = val_lgm[,-1], type = "response")

prob_attrition_lgm <- ifelse(pred_attrition_lgm > 0.79, 1, 0)

#checking accuracy
confusionMatrix(as.factor(val_lgm$Attrition), as.factor(prob_attrition_lgm), positive = "1")
# Accuracy of 0.8459
# Sensitivity is 0.78 
# specificity is 0.84, good balance

#But is this really a stable model?
round(prop.table(table(val_lgm$Attrition)) * 100, 2)
round(prop.table(table(prob_attrition_lgm)) * 100, 2)

# model is biased towards the majority class (Attrition = no class).



####################### LGM with SMOTE ###########################

# using smote to handle target class imbalance in the trn1 datset
require(DMwR)
trn_lgm1 <- trn_lgm

trn_lgm1$Attrition <- as.factor(trn_lgm1$Attrition)
trn_lgm1 <- mutate_if(trn_lgm1, is.character, as.factor)

set.seed(200)
trn_lgm1 <- SMOTE(Attrition ~., trn_lgm1, perc.over = 300, k = 5, perc.under = 200)

round(prop.table(table(trn_lgm1$Attrition))*100, 2)
# ratio is now balanced with 60% as no and 40% as yes

# building model

#Building first default model with all variables
slgm1 <- glm(Attrition ~., data = trn_lgm1[,-1], family = "binomial")
summary(slgm1)
# AIC: 5117.2

slgm2 <- step(slgm1)
summary(slgm2)
# 5106.1

require(car)
sort(vif(slgm2))
# Removing vars with high multi-collinearity

# Education 4
slgm3 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome + Education1 + Education2  + 
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + EducationFieldother + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 + StockOptionLevel2 + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + LeaveTaken + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm3)
# 5107.6

sort(vif(slgm3))
# all VIFs controlled

# now removing variables with high significance

#Education2
slgm4 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome + Education1   + 
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + EducationFieldother + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 + StockOptionLevel2 + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + LeaveTaken + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm4)
# 5106.1


#Education1    
slgm5 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome +  
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + EducationFieldother + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 + StockOptionLevel2 + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + LeaveTaken + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm5)
# 5105.5


#StockOptionLevel2  
slgm6 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome +  
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + EducationFieldother + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 +  TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + LeaveTaken + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm6)
# 5105.4


#LeaveTaken  
slgm7 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome +  
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + EducationFieldother + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 +  TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm7)
# 5105.8


#EducationFieldother    
slgm8 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome +  
               Education3 + `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 +  TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm8)
# 5106.1


#Education3 
slgm9 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmentresearch & development` + 
               DistanceFromHome +  
               `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
               EducationFieldmarketing + EducationFieldmedical + 
               Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
               `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked + PercentSalaryHike + StockOptionLevel0 + 
               StockOptionLevel1 +  TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               Average_HoursWorked + NumOvertimeDays + Overtimeno, 
             family = "binomial", data = trn_lgm1[, -1])
summary(slgm9)
# 5106.9


#PercentSalaryHike  
slgm10 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
                BusinessTraveltravel_frequently + `Departmentresearch & development` + 
                DistanceFromHome +  
                `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
                EducationFieldmarketing + EducationFieldmedical + 
                Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
                `JobRolelaboratory technician` + `JobRolemanufacturing director` + 
                `JobRoleresearch director` + `JobRoleresearch scientist` + 
                `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
                NumCompaniesWorked + StockOptionLevel0 + 
                StockOptionLevel1 +  TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                Average_HoursWorked + NumOvertimeDays + Overtimeno, 
              family = "binomial", data = trn_lgm1[, -1])
summary(slgm10)
# 5107.6


#`JobRolelaboratory technician`   
slgm11 <- glm(formula = Attrition ~ Age + `BusinessTravelnon-travel` + 
                BusinessTraveltravel_frequently + `Departmentresearch & development` + 
                DistanceFromHome +  
                `EducationFieldhuman resources` + `EducationFieldlife sciences` + 
                EducationFieldmarketing + EducationFieldmedical + 
                Genderfemale + JobLevel1 + JobLevel2 + JobLevel3 + JobLevel4 + 
                `JobRolemanufacturing director` + 
                `JobRoleresearch director` + `JobRoleresearch scientist` + 
                `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
                NumCompaniesWorked + StockOptionLevel0 + 
                StockOptionLevel1 +  TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                Average_HoursWorked + NumOvertimeDays + Overtimeno, 
              family = "binomial", data = trn_lgm1[, -1])
summary(slgm11)
# 5108.8


#All variables are important now. Predicting now on 'un-SMOTED' and original val

pred_attrition_lgm1 <- predict(slgm11, newdata = val_lgm[,-1], type = "response")

#setting cut-off
prob_attrition_lgm1 <- ifelse(pred_attrition_lgm1 > 0.9, 1, 0)

#checking accuracy
confusionMatrix(as.factor(val_lgm$Attrition), as.factor(prob_attrition_lgm1), positive = "1")
# Accuracy of 0.8531
# Sensitivity is 0.75
# specificity is 0.86, not a great balance


# Again, bias towards majority class 
round(prop.table(table(val_lgm$Attrition)) * 100, 2)
round(prop.table(table(prob_attrition_lgm1)) * 100, 2)


# the model w/o SMOTE has a better sensi-speci balance
# handling class imbalance using SMOTE did not make much of a difference


# Random Forest had given us a much better prediction with no bias
# LGM was not even close


###################### Decision Tree ##################

master_dt <- master

master_dt <- mutate_if(master_dt, is.factor, as.character)
#separate into train and test
set.seed(200)
require(caTools)
i <- sample.split(master_dt$Attrition, SplitRatio = 0.75)
trn_dt <- master[i,]
val_dt <- master[!i,]

#DT
require(rpart)
require(rpart.plot)

dt_model <- rpart(Attrition ~., data = trn_dt[,-1], 
                  parms = list(split = "information"),
                  control = rpart.control(cp = 0.01))
prp(dt_model)
summary(dt_model) #var imp.

pred_attrition_dt <- predict(dt_model, newdata = val_dt[,-1])
prob_attrition_dt <- ifelse(pred_attrition_dt[,2] > 0.9, "yes", "no")

confusionMatrix(as.factor(val_dt$Attrition), as.factor(prob_attrition_dt), positive = "yes")
# Accuracy of 0.8513
# Sensitivity is 0.888
# specificity is 0.85

# Good balance, a decent outcome


######### *Trying other models for comparison purposes* ###########


########################### KNN #######################
#Prepping data

master_knn <- master
str(master)

#scaling
master_knn <- mutate_if(master_knn, is.numeric, scale)

#dummy
master_knn <- dummy.data.frame(as.data.frame(master_knn[,-1]))
master_knn$Attrition <- master_knn$Attritionyes

master_knn$Attritionno <- NULL
master_knn$Attritionyes <- NULL

#binding
master_knn <- bind_cols(EmployeeID = master$EmployeeID, master_knn)

#correcting structure
master_knn <- mutate_if(master_knn, is.factor, as.character)
master_knn$Attrition <- as.factor(master_knn$Attrition)

# separating into trn and val datasets
require(caTools)
set.seed(200)
ind <- sample.split(master_knn$Attrition, SplitRatio = 0.75)
trn_knn <- master_knn[ind,]
val_knn <- master_knn[!ind,]


#KNN MODel
require(class)
sqrt(nrow(trn_knn))

knn_mod <- knn(train = trn_knn[, -c(1, 63),],
               test = val_knn[, -c(1, 63)],
               cl = trn_knn$Attrition,
               k = 57)
summary(knn_mod)

confusionMatrix(val_knn$Attrition, knn_mod, positive = "1")
# Accuracy of 84.77
# Sensitivity is 0.916
# specificity is 0.847

round(prop.table(table(val_knn$Attrition)) * 100, 2)
round(prop.table(table(knn_mod)) * 100, 2) #biased



################### Naive Bayes ###################

master_nb <- master

#NB requires factor inputs
master_nb <- mutate_if(master_nb, is.character, as.factor)


# separating into trn and val datasets
require(caTools)
set.seed(200)
ind <- sample.split(master_nb$Attrition, SplitRatio = 0.75)
trn_nb <- master_nb[ind,]
val_nb <- master_nb[!ind,]

#NB model
require(e1071)
nb_mod <- naiveBayes(Attrition ~., data = trn_nb[,-1])

#predict
pred_attrition_nb <- predict(nb_mod, newdata = val_nb[,-1], type = "raw")

prob_attrition_nb <- ifelse(pred_attrition_nb[,2] > 0.96, "yes", "no")

confusionMatrix(val_nb$Attrition, as.factor(prob_attrition_nb), positive = "yes")
# Accuracy of 0.8486
# Sensitivity is 0.823
# specificity is 0.848

# TPR or sensitivity is very low with just 55% correct predictions



################ SVM ###############

#Prepping data

master_svm <- master
str(master)

#scaling
master_svm <- mutate_if(master_svm, is.numeric, scale)

#dummy
master_svm <- dummy.data.frame(as.data.frame(master_svm[,-1]))
master_svm$Attrition <- master_svm$Attritionyes

master_svm$Attritionno <- NULL
master_svm$Attritionyes <- NULL

#binding
master_svm <- bind_cols(EmployeeID = master$EmployeeID, master_svm)

#correcting structure
master_svm <- mutate_if(master_svm, is.character, as.factor )


# separating into trn and val datasets
require(caTools)
set.seed(200)
ind <- sample.split(master_svm$Attrition, SplitRatio = 0.75)
trn_svm <- master_svm[ind,]
val_svm <- master_svm[!ind,]

#training time is 5 secs, approx.

#train
svm_model_rad <- svm(Attrition ~ ., data = trn_svm[ ,-1], kernel = "radial")
svm_model_lnr <- svm(Attrition ~ ., data = trn_svm[ ,-1], kernel = "linear")

#predict
pred_attrition_svm_rad <- predict(svm_model_rad, newdata = val_svm[ , -1])
pred_attrition_svm_lnr <- predict(svm_model_lnr, newdata = val_svm[ , -1])

prob_attrition_svm_rad <- ifelse(pred_attrition_svm_rad > 0.58, "1", "0")
prob_attrition_svm_lnr <- ifelse(pred_attrition_svm_lnr > 0.58, "1", "0")

#validate
confusionMatrix(as.factor(val_svm$Attrition), as.factor(prob_attrition_svm_rad),positive = "1")
# Accuracy of 0.9012
# Sensitivity is 0.897
# specificity is 0.902

round(prop.table(table(prob_attrition_svm_rad)) * 100,2) #not biased
# pretty good prediction with radial kernel


confusionMatrix(as.factor(val_svm$Attrition), as.factor(prob_attrition_svm_lnr), positive = "1")
# why the error, because
table(prob_attrition_svm_lnr)
# gives only 0 as prediction, thus super biased



############### END OF ANALYSIS ###############

############# Thank you ############3
# clear the environment and plot, and relieve R
rm(list = ls())
dev.off()
#clear console



