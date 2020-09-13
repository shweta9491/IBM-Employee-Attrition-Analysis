library(ggplot2)
library(ggthemes) 
library(scales)
library(dplyr) 
library(gridExtra)
library(corrplot) 
library(GGally)
library(e1071)
library(DAAG)
library(tidyverse)
library(caret)


setwd("C:/Users/hp/Desktop/DSP/New folder (2)/assignment/R Project - Attrition")
data=read.csv("Attrition.csv", na.strings=c("","NA"))
View(data)
str(data)
summary(data)

#Removing same valueor highly biased variables
data<- data[!colnames(data) %in% c("Over18","EmployeeCount","StandardHours","EmployeeNumber")]

#Numeric to ordinal variable conversion
data$Education<-as.factor(data$Education)
data$Education<-factor(data$Education,ordered = TRUE,levels = c('1','2','3','4','5'))

data$EnvironmentSatisfaction<-as.factor(data$EnvironmentSatisfaction)
data$EnvironmentSatisfaction<-factor(data$EnvironmentSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$JobInvolvement<-as.factor(data$JobInvolvement)
data$JobInvolvement<-factor(data$JobInvolvement,ordered = TRUE,levels = c('1','2','3','4'))


data$JobSatisfaction<-as.factor(data$JobSatisfaction)
data$JobSatisfaction<-factor(data$JobSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$PerformanceRating<-as.factor(data$PerformanceRating)
data$PerformanceRating<-factor(data$PerformanceRating,ordered = TRUE,levels = c('3','4'))

data$RelationshipSatisfaction<-as.factor(data$RelationshipSatisfaction)
data$RelationshipSatisfaction<-factor(data$RelationshipSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$WorkLifeBalance<-as.factor(data$WorkLifeBalance)
data$WorkLifeBalance<-factor(data$WorkLifeBalance,ordered = TRUE,levels = c('1','2','3','4'))


data$StockOptionLevel<-as.factor(data$StockOptionLevel)
data$JobLevel<-as.factor(data$JobLevel)
data$TrainingTimesLastYear<-as.factor(data$TrainingTimesLastYear)


str(data)
summary(data)
dim(data)

#checking for duplicate rows
which(duplicated(data))

#segregate numeric and categorical variables
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]
colnames(data.factor)
colnames(data.numeric)

#na removal
(colSums(is.na(data))/nrow(data))*100


#outlier
#Analysing histogram of each numeric values
numplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
    geom_density(fill='blue', alpha=0.2)+
    xlab(column)
}

data.skewed <- apply(data.numeric, c(2), skewness)

np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)

#correlation
corrplot::corrplot(cor(data.numeric))
corrplot.mixed(cor(data.numeric), lower.col = "black", number.cex = .7)

data.numeric<- data.numeric[!colnames(data.numeric) %in% c("DistanceFromHome","HourlyRate","DailyRate","MonthlyRate",
                                   "PercentSalaryHike","TotalWorkingYears","YearsInCurrentRole",
                                   "YearsSinceLastPromotion")]

dim(data)
factplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_bar(fill = "blue", color = "black", alpha= 0.2)+ scale_y_continuous(labels=scales::percent_format())+
    xlab(column)
}
#calling all bar plot
fp <- lapply(colnames(data.factor[10:18]), factplot, df=data.factor)
do.call("grid.arrange", fp)


colnames(data.factor)
colnames(data.numeric)

#EDA
#Bivariate analysis
ggplot(data, aes(x=JobLevel,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by joblevel")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=Department,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by Department")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=Education,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by Education")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=EducationField,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by EducationField")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=StockOptionLevel,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by StockOptionLevel")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=EnvironmentSatisfaction,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by EnvironmentSatisfaction")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=JobSatisfaction,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by JobSatisfaction")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=BusinessTravel,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by BusinessTravel")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=OverTime,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by OverTime")+scale_y_continuous(labels=scales::percent_format())
#people doing overtime seems to resign

ggplot(data, aes(x=JobRole,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by JobRole")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=WorkLifeBalance,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by WorkLifeBalance")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=JobInvolvement,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by JobInvolvement")+scale_y_continuous(labels=scales::percent_format())
#Attrition decreases as job involvement increases

ggplot(data, aes(x=EnvironmentSatisfaction,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by EnvironmentSatisfaction")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=Gender,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by Gender")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=MaritalStatus,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by MaritalStatus")+scale_y_continuous(labels=scales::percent_format())
#no. ofmarried people resigning are less while single are more to resign

ggplot(data, aes(x=PerformanceRating,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by PerformanceRating")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=RelationshipSatisfaction,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by RelationshipSatisfaction")+scale_y_continuous(labels=scales::percent_format())

ggplot(data, aes(x=TrainingTimesLastYear,fill=Attrition))+theme_bw()+
  geom_bar(position = 'fill')+labs(y="no. of employees",title="attrition by TrainingTimesLastYear")+scale_y_continuous(labels=scales::percent_format())


chisq.test(data.factor$MaritalStatus, data.factor$Attrition)

x <- aov(data.numeric$YearsAtCompany~ data.factor$Attrition)
summary(x)

#cat-num analysis
colnames(data.numeric)
ggplot(data, aes(x = Age, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = DailyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = DistanceFromHome, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = HourlyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = MonthlyIncome, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = MonthlyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = NumCompaniesWorked, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = PercentSalaryHike, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = TotalWorkingYears, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsAtCompany, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsInCurrentRole, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsSinceLastPromotion, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsWithCurrManager, color = Attrition)) + geom_density() + theme_minimal()

data<-cbind(data.factor,data.numeric)
colnames(data)
dim(data)

#random sampling
set.seed(900)  
s=sample(1:nrow(data),0.70*nrow(data))

train=data[s,]
test=data[-s,]
View(train)
View(test)
summary(train)
summary(test)
ncol(test)
colnames(test)
test1 <- test[!colnames(test) %in% "Attrition"]
View(test1)

#Logistic regression 
model=glm(Attrition~.,data=(train),family = binomial("logit"))
summary(model)

#Stepwise modelling
nullmodel=glm(Attrition~1,data=train,family=binomial("logit"))
fullmodel=glm(Attrition~.,data=train,family=binomial("logit"))
fit=step(nullmodel,scope=list(lower=nullmodel,upper=fullmodel),direction="both")
fit

model<-glm(formula = Attrition ~ JobRole + OverTime + StockOptionLevel + 
             BusinessTravel + JobInvolvement + JobSatisfaction + WorkLifeBalance + 
             EnvironmentSatisfaction + JobLevel + YearsWithCurrManager + 
             NumCompaniesWorked + Age + Gender + RelationshipSatisfaction, 
           family = binomial("logit"), data = train)

summary(model)

fitted.results=predict(model,newdata = test1,type = 'response')
View(fitted.results)
fitted.results1<-ifelse(fitted.results>=0.5,1,0)
table(fitted.results1)
table(test$Attrition)
cf1=table(test$Attrition,fitted.results1)
cf1
TP=350
FN=33
FP=20
TN=38
accuracy= (TN+TP) / (TP+FP+FN+TN)
accuracy
Sensitivity=TP/(TP+FN)   
Sensitivity
specificity=TN/(TN+FP)   
specificity
precision=TP/(FP+TP)
error=(FP+FN)/(TP+TN)
error


attrtion2=ifelse(test$Attrition=="Yes",1,0)
library(pROC)
roccurve=roc(attrtion2, fitted.results1)
plot(roccurve)
auc(roccurve)


#decision tree
library(rpart)
library(rpart.plot)
regressor<-rpart(formula=Attrition~.,data=train,method="class")
regressor
?rpart.control
rpart.plot(regressor)
printcp(regressor)
regressor<-rpart(formula=Attrition~.,data=train,method="class",control=rpart.control(minsplit = 100,cp=0.01))
regressor

summary(regressor)
library(rattle)
#rattle()
fancyRpartPlot(regressor)
pred<-predict(regressor,test1,type='class')
View(pred)
#result & accuracy
conf_mat<-table(test$Attrition,pred)
TP=346
FP=41
FN=29
TN=25
acc<-(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)
Sensitivity=TP/(TP+FN)   
Sensitivity
specificity=TN/(TN+FP)   
specificity
precision=TP/(FP+TP)
error=(FP+FN)/(TP+TN)
error

library(pROC)
pred_test_roc=ifelse(pred=="Yes",1,0)
test_attrition_roc=ifelse(test$Attrition=="Yes",1,0)
roccurve=roc(test_attrition_roc,pred_test_roc)
plot(roccurve)
auc(roccurve)

#Random forest algo
library(randomForest)
m1 <- randomForest(Attrition~., data = train,ntree = 40, mtry = 6)

#ntree = 10, mtry=6
plot(m1)

m1$err.rate
m1$importance
m1$err.rate

importance(m1)
varImpPlot(m1)


p2 <- predict(m1, test1)

dim(test1)
length(p2)
table(p2,test$Attrition)

pred <- predict(m1, test1)
TP=364
FP=11
FN=51
TN=15
accuracy= (TN+TP) / (TP+FP+FN+TN)
accuracy
Sensitivity=TP/(TP+FN)   
Sensitivity
specificity=TN/(TN+FP)   
specificity
precision=TP/(FP+TP)


plot(m1)
pred=ifelse(p2=="Yes",1,0)
test_attrition_roc=ifelse(test$Attrition=="Yes",1,0)
roccurve=roc(test_attrition_roc,pred)
plot(roccurve)
auc(roccurve)

