#this Script will Answer for the first Research question 
#By Reem Shai And Gady Schnieder

library(datasets)
library(data.table)
library(lubridate)                       # Install & load all package
library(plyr)
library(caret)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(padr)
library(class)
library(factoextra)
library(mclust)
library(kernlab)
library(scatterplot3d)
library(rcompanion)
#########################################################################
data = fread("healthcare-dataset-stroke-data.csv") ### read and save the data in data viarble.

##########################################################
str(data)  #### <- Check data type for each column

data_1 = data.table(select(data,-id)) ##########<- Removing the column id that not relevant for our
######### research.
data_1$bmi = as.numeric(data_1$bmi) ######### <- Convert the column from character to numeric
############################################################
#Check for each of all column if there is na (missing value)
###########################################################
which(is.na(data_1$gender)) 
which(is.na(data_1$age))  
which(is.na(data_1$hypertension))
which(is.na(data_1$heart_disease))
which(is.na(data_1$ever_married))
which(is.na(data_1$work_type))
which(is.na(data_1$Residence_type))
which(is.na(data_1$avg_glucose_level))
which(is.na(data_1$bmi))     ## we see a lot of na values       
which(is.na(data_1$smoking_status))
which(is.na(data_1$stroke)) 
#######################################################
data_1$bmi[is.na(data_1$bmi)] = mean(data_1$bmi, na.rm = TRUE)  #### Here we replace na values 
## with average of column.  
#############################################
#Because we have character (categorical data) variables, we will define them as factor.
################################################################
data_1$gender<-as.factor(data_1$gender)
data_1$ever_married<-as.factor(data_1$ever_married)
data_1$work_type<-as.factor(data_1$work_type)
data_1$Residence_type<-as.factor(data_1$Residence_type)
data_1$smoking_status<-as.factor(data_1$smoking_status)
###############################################################
### Here factors are stored as integers, and have labels associated with these unique integers. 
data_1$hypertension<- factor(data_1$hypertension, levels = c(0,1), labels = c("No", "Yes"))
data_1$heart_disease<- factor(data_1$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
data_1$stroke_category<- factor(data_1$stroke, levels = c(0,1), labels = c("No", "Yes"))
######################################################################################
summary(data_1) ### We will see data on each column separately such as: 
## Distribution of answers, minimum, maximum and average.
data_1 = data_1[!(data_1$gender=="Other"),] ### removing the all row of this observation 
## Because there is no way to divide this value and draw conclusions about gender 
#########################################################################################
##Checking for exceptions in the various columns using graphs.
###########################################
boxplot(data_1$age,  ### age parameter.
        main="Age Parameter",
        xlab="age",
        outline = T,
        horizontal= T,
        col="blue")
#################################
plot(data_1$bmi,  ### Bmi parameter.
     main="bmi",
     ylab="bmi",
     xlab="observations",
     type="p",
     col="blue")
### bmi>50 is outliers so we need to take them out.
##############################################
data_1 = data_1[data_1$bmi < 50]
##########################################
plot(data_1$bmi,  ### Bmi parameter.
     main="bmi after remove outliers",
     ylab="bmi",
     xlab="observations",
     type="p",
     col="blue")
#################################################
plot(data_1$avg_glucose_level,  ### avg_glucose_level parameter.
     main="avg_glucose_level",
     ylab="glucose",
     xlab="observations",
     type="p",
     col="blue")
####################################################
#gender
check_gender<-ggplot(data_1, aes(x = gender, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_gender
####################################################
#hypertension
check_hypertension<-ggplot(data_1, aes(x = hypertension, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_hypertension
####################################################
#heart_disease
check_heart_disease<-ggplot(data_1, aes(x = heart_disease, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_heart_disease
####################################################
#ever_married
check_ever_married<-ggplot(data_1, aes(x = ever_married, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_ever_married
####################################################
#work_type
check_work_type<-ggplot(data_1, aes(x = work_type, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_work_type
####################################################
#Residence_type
check_Residence_type<-ggplot(data_1, aes(x = Residence_type, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_Residence_type
####################################################
#smoking_status
check_smoking_status<-ggplot(data_1, aes(x = smoking_status, fill = stroke_category))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_smoking_status
####################################################
#age
check_age<-ggplot(data_1,aes(x=age,group=stroke,fill=stroke_category))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()
check_age
####################################################
#bmi
check_bmi<-ggplot(data_1,aes(x=bmi,group=stroke_category,fill=stroke_category))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()
check_bmi
####################################################
#avg_glucose_level
check_avg_glucose_level<-ggplot(data_1,aes(x=avg_glucose_level,group=stroke,fill=stroke_category))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()
check_avg_glucose_level
####################################################
summary(data_1) ### We will see data on each column separately such as: 
## Distribution of answers, minimum, maximum and average. 
#####################################################
# Cramer Correltion ## 
gender = cramerV(data_1$stroke,data_1$gender)
cat("stroke ~ Gender : " ,gender,"\n" )  ## result : 0.008769 
# check cor between stroke and hypertension
hypertension = cramerV(data_1$stroke,data_1$hypertension) 
cat("Storke~hypertension : " ,hypertension,"\n") # result : 0.132 
# check cor between stroke and heart_disease
heart_disease = cramerV(data_1$stroke,data_1$heart_disease)
cat("Storke~heart_disease : " ,heart_disease,"\n") # result : 0.1351
# check cor between stroke and ever_married
ever_married = cramerV(data_1$stroke,data_1$ever_married)
cat("Storke~ever_married : " ,ever_married,"\n") # result : 0.1097
# check cor between stroke and work_type
work_type=cramerV(data_1$stroke,data_1$work_type)
cat("Storke~work_type : " ,work_type,"\n" ) # result :   0.0994  
# check cor between stroke and Residence_type
Residence_type = cramerV(data_1$stroke,data_1$Residence_type)
cat("Storke~Residence_type : " ,Residence_type,"\n") # result :  0.01613 
# check cor between stroke and smoking_status
smoking_status = cramerV(data_1$stroke,data_1$smoking_status)
cat("Storke~smoking_status : " ,smoking_status,"\n") # result : 0.0765
##################################################################
# correlation Pearson (for numeric variables)
print("correlation Pearson (for numeric variables)")
# check cor between stroke and age
age=cor(data_1$stroke,data_1$age) 
cat("Stroke ~ age :",age,"\n")# result : 0.2467756
# check cor between stroke and avg_glucose_level
avg_glucose_level=cor(data_1$stroke,data_1$avg_glucose_level) 
cat("Storke~avg_glucose_level",avg_glucose_level,"\n")# result : 0.133079
# check cor between stroke and bmi
bmi=cor(data_1$stroke,data_1$bmi) 
cat("Storke~BMI",bmi,"\n")# result : 0.05461822
###################################################
# t.test (for numeric variables)
t.test(data_1$stroke,data_1$age) # result : 0.00000000000000022 / 2.2e-16
t.test(data_1$stroke,data_1$avg_glucose_level) # result : 0.00000000000000022 / 2.2e-16
t.test(data_1$stroke,data_1$bmi) # result : 0.00000000000000022 / 2.2e-16
#########################################
# chisq.test (for categorical variables) 
# check cor between stroke and gender
chisq.test(data_1$stroke,data_1$gender)  # result : 0.5783
# check cor between stroke and hypertension
chisq.test(data_1$stroke,data_1$hypertension)  # result : 0.00000000000000022 /2.2e-16
# check cor between stroke and heart_disease
chisq.test(data_1$stroke,data_1$heart_disease)  # result : 0.00000000000000022 /2.2e-16
# check cor between stroke and ever_married
chisq.test(data_1$stroke,data_1$ever_married)  # result : 1.234e-14
# check cor between stroke and work_type
chisq.test(data_1$stroke,data_1$work_type)  # result : 5.398e-10
# check cor between stroke and Residence_type
chisq.test(data_1$stroke,data_1$Residence_type)  # result : 4.182e-10
# check cor between stroke and smoking_status
chisq.test(data_1$stroke,data_1$smoking_status)  # result :  1.811e-06
##################################################################


