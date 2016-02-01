#Importing test and train datasets 
test<-read.csv("D:/test_Y3wMUE5.csv")
train<-read.csv("D:/train_u6lujuX.csv")
str(test)
str(train)
head(test)
#Combining test and train datasets 
test$Loan_Status <- 0
data<-rbind(train,test)
head(data)
data$Loan_Status
#Loading Package Amelia for missing value imputation 
#Visualizing where the missing values are concentrated 
library(Amelia)
missmap(train)
#Loading plyr for Data Manipulation
#Recoding categorical variables 
library(plyr)
data[data$Gender =="",]$Gender="Male"
missmap(data$Gender)
data$Gender <- revalue(data$Gender, c("Male"="1", "Female"="2"))
head(data$Gender)
--------------------------------------------------------------------------------------
table(data$Married)
data[data$Married =="",]$Married="Yes"
data$Married
data$Married <- revalue(data$Married,c("Yes"="1", "No"="2"))
head(data$Married)
-----------------------------------------------------------------------------------------
table(data$Education)  
train$Education <- revalue(train$Education, c("Graduate"="1", "Not Graduate"="2"))
head(train$Education)
-------------------------------------------------------------------------------------------
table(data$Self_Employed)
data[data$Self_Employed =="",]$Self_Employed="No"
data$Self_Employed
data$Self_Employed<- revalue(data$Self_Employed, c("Yes"="1", "No"="2"))
head(data$Self_Employed)
--------------------------------------------------------------------------------------------
table(data$Property_Area)
data$Property_Area<- revalue(data$Property_Area, c("Rural"="1", "Semiurban"="2","Urban"="3"))
data$Property_Area
---------------------------------------------------------------------------------------------
table(data$Dependents)
data[data$Dependents =="",]$Dependents="0"
data$Dependents
-----------------------------------------------------------------------------------------------
summary(data$Loan_Amount_Term)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- "360"
data$Loan_Amount_Term
str(data)
----------------------------------------------------------------------------------------------
boxplot(data$LoanAmount)
summary(data$LoanAmount)
amount1=data$LoanAmount[data$LoanAmount < 230]
boxplot(amount1,horizontal=T)
data$LoanAmount[data$LoanAmount > 230 ] = 230
head(data$LoanAmount)
data$LoanAmount
data$LoanAmount[is.na(data$LoanAmount)] <- "133"
-----------------------------------------------------------------------------------------------
summary(data$Credit_History)
data$Credit_History[is.na(data$Credit_History)]="1"
data$Credit_History
------------------------------------------------------------------------------------------------
missmap(data)
nrow(train)
------------------------------------------------------------------------------------------------
View(data)
str(data)
train1 <- data[1:614,]
test1 <- data[615:981,]
head(test1)
-------------------------------------------------------------------------------------------------------
#Loading Random_Forest Package 
library(randomForest)
#Building a rfmodel 
forestmodel <- randomForest(as.factor(Loan_Status) ~ Gender + Dependents+Education+Self_Employed+LoanAmount+Credit_History+Property_Area, data = train1, nodesize = 1, ntree = 1000)
#Applying the model for prediciting in test dataset 
prediction <- predict(forestmodel, newdata = test1, type = 'class')
submit <- data.frame(ID = test$Loan_ID, Loan_Status = prediction)
submit
forestmodel
#Writing into csv for competetion submission 
write.csv(submit, file = 'sub21.csv', row.names = FALSE)
