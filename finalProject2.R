#This Script will Answer for the second Research question
# By Reem Shai And Gady Schneider

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
library(gmodels)
#########################################################################
data = fread("healthcare-dataset-stroke-data.csv") ### read and save the data in data viarble.
##########################################################
str(data)  #### <- Check data type for each column
data_1 = data.table(select(data,-id,-gender,-age,-hypertension,-ever_married,-work_type,-Residence_type,-avg_glucose_level,-bmi,-stroke)) ##########<- Removing the column id that not relevant for our
############################################################
#Check for each of all column if there is na (missing value)
###########################################################
which(is.na(data_1$heart_disease))
which(is.na(data_1$smoking_status))
#######################################################
#############################################
#Because we have character (categorical data) variables, we will define them as factor.
################################################################
data_1$smoking_status<-as.factor(data_1$smoking_status)
###############################################################
### Here factors are stored as integers, and have labels associated with these unique integers. 
data_1$heart_disease1<- factor(data_1$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
######################################################################################
summary(data_1) ### We will see data on each column separately such as: 
## Distribution of answers, minimum, maximum and average.
###########################################
plot(data_1$heart_disease1,  ### heart_disease parameter.
     main="heart_disease",
     ylab="heart_disease",
     xlab="observations",
     type="p",
     col="blue")
##########################################
plot(data_1$smoking_status,  ### smoking_status parameter.
     main="smoking_status",
     ylab="smoking_status",
     xlab="observations",
     type="p",
     col="blue")
#################################################
# removing Unknown status in smoking_status
data_1 = data_1[data_1$smoking_status != "Unknown"]
##########################################
plot(data_1$smoking_status,  ### smoking_status parameter.
     main="smoking_status after removing Unknown",
     ylab="smoking_status",
     xlab="observations",
     type="p",
     col="blue")
check_Smoking<-ggplot(data_1, aes(x = heart_disease1, fill = smoking_status))+
  geom_bar(position = "fill",)+
  stat_count(geom = "text",
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), color = "black")
check_Smoking
###########################################
# Cramer Correltion ## 
# check cor between stroke and heart_disease
cramerV(data_1$smoking_status,data_1$heart_disease1) ## result : 0.07241
######################
# chisq.test (for categorical variables)
# check cor between stroke and heart_disease
chisq.test(data_1$smoking_status,data_1$heart_disease1) 
########
#frequency
sss<- data_1[data_1$heart_disease == 1, ]
round(prop.table(table(data_1$smoking_status))*100,2)

#regression between heart_disease and Smoking Status
reg = lm(data_1$heart_disease1~data_1$smoking_status,data=data_1)
reg

