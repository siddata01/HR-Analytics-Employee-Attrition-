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

################# Go over to "Univariate_Analysis" for in-depth indiviual analysis #############