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

############# Thank you, R ############3
# clear the environment and plot, and relieve R
rm(list = ls())
dev.off()
#clear console


