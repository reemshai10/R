#This Script will Answer for the Third Research question
#By Reem Shai And Gady Schnedier

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
library(gbm)
#########################################################################

data = fread("healthcare-dataset-stroke-data.csv") ### read and save the data in data viarble.

##########################################################
str(data)  #### <- Check data type for each column

###################################
data_1 = data.table(select(data,-id)) ##########<- Removing the column id that not relevant for our
data_1$bmi = as.numeric(data_1$bmi) 
data_1$bmi[is.na(data_1$bmi)] = mean(data_1$bmi, na.rm = TRUE)  #### Here we replace na values 
########################### #Because we have character (categorical data) variables, we will define them as factor.
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

### bmi>50 is outliers so we need to take them out.
##############################################
data_1 = data_1[data_1$bmi < 50]
#############################################
#remove other value in gender.
data_1 = data_1[!(data_1$gender=="Other"),]
#############################################
# Cramer Correltion ## 
cramerV(data_1$stroke,data_1$gender)  ## result : 0.008769 
# check cor between stroke and hypertension
cramerV(data_1$stroke,data_1$hypertension) # result : 0.132 
# check cor between stroke and heart_disease
cramerV(data_1$stroke,data_1$heart_disease) # result : 0.1351
# check cor between stroke and ever_married
cramerV(data_1$stroke,data_1$ever_married) # result : 0.1097
# check cor between stroke and work_type
cramerV(data_1$stroke,data_1$work_type) # result :   0.0994 
# check cor between stroke and Residence_type
cramerV(data_1$stroke,data_1$Residence_type) # result :  0.01613 
# check cor between stroke and smoking_status
cramerV(data_1$stroke,data_1$smoking_status) # result : 0.0765
##################################################################
# correlation Pearson (for numeric variables)
# check cor between stroke and age
cor(data_1$stroke,data_1$age) # result : 0.2467756
# check cor between stroke and avg_glucose_level
cor(data_1$stroke,data_1$avg_glucose_level) # result : 0.133079
# check cor between stroke and bmi
cor(data_1$stroke,data_1$bmi) # result : 0.05461822
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
#reg for stroke 
reg = lm(data_1$stroke~data_1$age+data_1$hypertension+data_1$heart_disease+data_1$ever_married+data_1$work_type+data_1$avg_glucose_level+data_1$bmi+data_1$smoking_status,data=data_1)
reg



#############################################
#Random Forest
inData1 = createDataPartition(y =data_1$stroke, p = 0.5, list = FALSE)
data1_train = data_1[inData1, ]
data1_test = data_1[-inData1, ]
modFit = train(stroke_category~age+hypertension+heart_disease+ever_married+work_type+avg_glucose_level+bmi+smoking_status, data=data1_train, method="rf",prox=TRUE)
modFit
###########
#predict by Random Forest
predRF = predict(modFit,data1_test)
table(predRF,data1_test$stroke)

##############################
#Boosing - gbm
inData1 = createDataPartition(y =data_1$stroke, p = 0.5, list = FALSE)
data1_train = data_1[inData1, ]
data1_test = data_1[-inData1, ]
gbm_modFit = train(stroke_category~age+hypertension+heart_disease+ever_married+work_type+avg_glucose_level+bmi+smoking_status, data=data1_train, method="gbm",verbose=FALSE)
gbm_modFit
################
#predict by gbm
predGBM = predict(gbm_modFit,data1_test)
table(predGBM,data1_test$stroke)