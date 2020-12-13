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


############ Head over to "EDA.R" for further analysis ######################

