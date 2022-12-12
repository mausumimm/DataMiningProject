rm(list=ls(all=TRUE))
install.packages('naivebayes')

library(dplyr)
library(ggplot2)
library(corrplot)
library(fastDummies)
library(class)
library(caret)
library(e1071)
library(ggthemes)
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(scales)
library(ggthemr)
library(gridExtra)
library(data.table)
library(rpart)
library(caret)
library(rpart.plot)
library(data.tree)
library(caTools)
library(FSelector)
library(pROC)
library(party)
library(readxl)
library(tidyverse)
library(pheatmap)
library(neuralnet)
library(tidyr)
library(ipred)
library(nnet)
library(naivebayes)



#Loading Data
IBM <- read_excel("WA_Fn-UseC_-HR-Employee-Attrition.xlsx", sheet="RawData")
summary(IBM)
View (IBM)

#Loading 2ndsheet for correlation matrix and vif
IBM1 <- read_excel("WA_Fn-UseC_-HR-Employee-Attrition.xlsx", sheet = 'Sheet1')
summary(IBM1)
View (IBM1)

#Testing for multicollinearlity in the data
model_all <- lm(Attrition~., data = IBM1)
summary(model_all)

#Using VIF for the regression model
vif(model_all)

vif_values <- vif(model_all)           #create vector of VIF values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value
abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation

#Creating correlation matrix for the data
correlation_matrix <- cor(na.omit(IBM1[,-c(13,17)]))   
correlation_matrix

#Creating a graphical display/plot of the correlation matrix
corrplot(correlation_matrix)

#Creating heatmap for the correlation matrix
pheatmap(correlation_matrix)


#Not running this time
IBM$Attrition <- as.factor(IBM$Attrition)
prop.table(table(IBM$Attrition))


summary(IBM)


#Dummy BusinessTravel
Travel_Type<-dummy_cols(IBM$BusinessTravel)
IBm <- cbind(IBM,Travel_Type)
View(IBm)
IBmd<-IBm[-c(3,36)]
colnames(IBmd)[35] = "Non_Traveller"
colnames(IBmd)[36] = "Travel_Frequently"
colnames(IBmd)[37] = "Travel_Rarely"
View(IBmd)

#Dummy Department
Dept<-dummy_cols(IBmd$Department)
IBmda <- cbind(IBmd,Dept)
View(IBmda)
IBmdat<-IBmda[-c(4,38)]
colnames(IBmdat)[37] = "Department_HumanResources"
colnames(IBmdat)[38] = "Department_ResearchandDevelopment"
colnames(IBmdat)[39] = "Department_Sales"
View(IBmdat)

#Dummy Education Field
Education_Field<-dummy_cols(IBmdat$EducationField)
IBmdata <- cbind(IBmdat,Education_Field)
View(IBmdata)
IBmD<-IBmdata[-c(6,40)]
colnames(IBmD)[39] = "Education_HumanResource"
colnames(IBmD)[40] = "Education_LifeSceinces"
colnames(IBmD)[41] = "Education_Marketing"
colnames(IBmD)[42] = "Education_Medical"
colnames(IBmD)[43] = "Education_Other"
colnames(IBmD)[44] = "Education_TechnicalDegree"
View(IBmD)

#Dummy Job Role
Job_Role<-dummy_cols(IBmD$JobRole)
IBmDA <- cbind(IBmD,Job_Role)
View(IBmDA)
IBm_d<-IBmDA[-c(13,45)]
colnames(IBm_d)[44] = "Job_Healthcare_Representative"
colnames(IBm_d)[45] = "Job_Human_Resources"
colnames(IBm_d)[46] = "Job_Labroratory_Technician"
colnames(IBm_d)[47] = "Job_Data_Manager"
colnames(IBm_d)[48] = "Job_Manufacturing_Director"
colnames(IBm_d)[49] = "Job_Research_Director"
colnames(IBm_d)[50] = "Job_Research_Scientist"
colnames(IBm_d)[51] = "Job_Sales_Executive"
colnames(IBm_d)[52] = "Job_SalesRepresentative"
View(IBm_d)

#Dummy Marital Status
Marital_Status<-dummy_cols(IBm_d$MaritalStatus)
IBm_da <- cbind(IBm_d,Marital_Status)
View(IBm_da)
IBm_dat<-IBm_da[-c(14,53)]
colnames(IBm_dat)[52] = "Status_Divorced"
colnames(IBm_dat)[53] = "Status_Maried"
colnames(IBm_dat)[54] = "Status_Single"
View(IBm_dat)

#Dummy Job Satisfaction
Job_Satisfaction<-dummy_cols(IBm_dat$JobSatisfaction)
IBm_data <- cbind(IBm_dat,Job_Satisfaction)
View(IBm_data)
IBm_DATA<-IBm_data[-c(13,55)]
colnames(IBm_DATA)[54] = "JobSatisfaction_1"
colnames(IBm_DATA)[55] = "JobSatisfaction_2"
colnames(IBm_DATA)[56] = "JobSatisfaction_3"
colnames(IBm_DATA)[57] = "JobSatisfaction_4"
View(IBm_DATA)

#Dummy Work Life Balance
WorKLife<-dummy_cols(IBm_DATA$WorkLifeBalance)
ibmda <- cbind(IBm_DATA,WorKLife)
View(ibmda)
ibmdata<-ibmda[-c(25,58)]
colnames(ibmdata)[57] = "WorKLifeBalance_1"
colnames(ibmdata)[58] = "WorKLifeBalance_2"
colnames(ibmdata)[59] = "WorKLifeBalance_3"
colnames(ibmdata)[60] = "WorKLifeBalance_4"
View(ibmdata)

#Dummy Relation Ship Satisfaction
Relationship<-dummy_cols(ibmdata$RelationshipSatisfaction)
dataibm <- cbind(ibmdata,Relationship)
View(dataibm)
datai<-dataibm[-c(20,61)]
colnames(datai)[60] = "RelationshipSatisfaction_1"
colnames(datai)[61] = "RelationshipSatisfaction_2"
colnames(datai)[62] = "RelationshipSatisfaction_3"
colnames(datai)[63] = "RelationshipSatisfaction_4"
View(datai)

#Dummy Performance Rating
Performance<-dummy_cols(datai$PerformanceRating)
datb <- cbind(datai,Performance)
View(datb)
IBMDATA<-datb[-c(19,64)]
colnames(IBMDATA)[63] = "PerformanceRating_3"
colnames(IBMDATA)[64] = "PerformanceRating_4"
View(IBMDATA)

#Dummy Gender Rating
Gender<-dummy_cols(IBMDATA$Gender)
datg <- cbind(IBMDATA,Gender)
View(datg)
DATAG<-datg[-c(9,65)]
colnames(DATAG)[64] = "Gender_Female"
colnames(DATAG)[65] = "Gender_Male"
View(DATAG)

#Dummy For Overtime
Overtime<-dummy_cols(DATAG$OverTime)
dato <- cbind(DATAG,Overtime)
View(dato)
DATAO<-dato[-c(16,66)]
colnames(DATAO)[65] = "OverTime_No"
colnames(DATAO)[66] = "Overtime_Yes"
View(DATAO)

###Dropping not required columns
IBMHRdata <- DATAO[-c(6,11,12,15,17,18,20)]
View(IBMHRdata)

###Converting AttritionintoDummies      
Attrition<-dummy_cols(IBMHRdata$Attrition)
dataa <- cbind(IBMHRdata,Attrition)
View(dataa)
DATAA<-dataa[-c(2,60)]
colnames(DATAA)[59] = "Attrition_No"
colnames(DATAA)[60] = "Attrition_Yes"
View(DATAA)

###Attrition
IBMHRdata_final <- IBMHRdata %>%
  mutate(Attrition = recode(Attrition, Yes = 1,No = 0 ))
View(IBMHRdata_final)

IBMHRdata_final$Attrition <- as.factor(IBMHRdata_final$Attrition)
table(IBMHRdata_final$Attrition)

##############Partitioning of data###

set.seed(1)
train.index <- sample(row.names(IBMHRdata_final), 0.7*dim(IBMHRdata_final)[1])
valid.index <- setdiff(row.names(IBMHRdata_final), c(train.index))
train.df <- IBMHRdata_final[train.index,]
valid.df <- IBMHRdata_final[valid.index,]

View(train.df)

View(valid.df)
table(train.df$Attrition)

##############################

###if Upsampling is required for below logistic regression, decision tree and KNN, then run this piece of code
library(ROSE)
prop.table(table(train.df$Attrition))
train.df <- ovun.sample(Attrition~., data = train.df, method = "over", N =1724)$data
table(train.df$Attrition)
View(train.df)
##now the data is balanced

#Normalizing the complete data set
train.norm.df <- train.df[,-2]
valid.norm.df <- valid.df[,-2]
norm.values <- preProcess(train.df[, -2], method=c("center", "scale"))
norm_scale <- predict(norm.values, as.data.frame(IBMHRdata_final))
train.norm.df <- predict(norm.values, train.df[, -2])
valid.norm.df <- predict(norm.values, valid.df[, -2])

View(train.norm.df)
View(valid.norm.df)
##################################################################
####KNN Model#####
# optimal k
accuracy.df <- data.frame(k = seq(1, 15, 1), overallaccuracy = rep(0, 15))
for(i in 1:15) {
  knn.pred <- class::knn(train = train.norm.df,
                         test = valid.norm.df,
                         cl = train.df$Attrition, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred,
                                       as.factor(valid.df$Attrition))$overall[1]}
accuracy.df

which(accuracy.df[,2] == max(accuracy.df[,2]))

knn.pred <- class::knn(train = train.norm.df,
                       test = valid.norm.df,
                       cl = train.df$Attrition, k = 1)
confusionMatrix(knn.pred, as.factor(valid.df$Attrition), positive = "1")

########k = 1 as it has good overall accuracy now
#################################################################
#Decision Tree Model
tree <- rpart(Attrition~., data = train.df,method = 'class')
prp(tree)
text(tree,pretty =0)
summary(tree)
# predictions
tree_pred <- as.data.frame(predict(tree, newdata = valid.df, type = "p"))
head(tree_pred)
##### confusion matrix with cutoff
table_DecsionTree <- table(valid.df$Attrition, tree_pred$`1` > .5)
table_DecsionTree

#ROC curve as it works in probability we are takinng the type as prob
tree_pred_prob <- predict(tree, valid.df, type = "prob")
auc_tree <- auc(valid.df$Attrition,tree_pred_prob[,2])
plot(roc(valid.df$Attrition,tree_pred_prob[,2]))

############################################################################
t(t(names(train.norm.df)))
str(train.df)
summary(train.df)# it has to be categorical but in summary it is changing into factors

train.df$Attrition = as.factor(train.df$Attrition)
summary(train.df)#Now the value is coming in factor, it shows the frequency

#Similarly done for validation Set
valid.df$Attrition = as.factor(valid.df$Attrition)
summary(train.df)
View(train.df)

#Logistic regression model 
logit<-glm(Attrition~.,data=train.df,family='binomial')
#Summary of Logit
summary(logit)
#prediction of training dataset
pred_train = predict(logit,train.df,type = "response") #these are the probabilities between 0 and 1
head(pred_train)
#if greater than 0.5 then 1 otherwise 0
#prediction of validation dataset
pred_valid = predict(logit,valid.df,type = "response") #these are the probabilities between 0 and 1
#confustion matrix
table(Actuals = valid.df$Attrition,Predictions =pred_valid > 0.5)

#Analyzing probability through ROC curve:
library(ROCR)
roc_prediction = prediction(pred_valid,valid.df$Attrition)
roc_performance = performance(roc_prediction,"tpr","fpr")#tpr: true positive rate, fpr:false positive  rate
roc_performance1 = performance(roc_prediction,"auc")#auc stands for are aunder the curve
plot(roc_performance)#It doesn't have meeting points to get that based on the company's requirement
roc_performance1@y.values
#0.8145553 very good value

###############################################################
###Neural Net Model
##Model 1: 1 layer, 2 nodes
#Normalization: Only for Neural Network as Attrition column is needed
set.seed(1)
train.index <- sample(row.names(IBMHRdata_final), 0.6*dim(IBMHRdata_final)[1])
valid.index <- setdiff(row.names(IBMHRdata_final), c(train.index))
train.df <- IBMHRdata_final[train.index,]
valid.df <- IBMHRdata_final[valid.index,]

##To do Upsampling for neural net

prop.table(table(train.df$Attrition))
train.df <- ovun.sample(Attrition~., data = train.df, method = "over", N =1724)$data
table(train.df$Attrition)
View(train.df) 

#Normalization: Only for Neural Network as Attrition column is needed
train.norm.df1 <- train.df
valid.norm.df1 <- valid.df
norm.values1 <- preProcess(train.df, method=c("center", "scale"))
train.norm.df1 <- predict(norm.values1, train.df)
valid.norm.df1 <- predict(norm.values1, valid.df)

View(train.norm.df1)
View(valid.norm.df1)

#Neural network model
##Model 1: 1 layer, 2 nodes
model_1layer_2nodes <- neuralnet(Attrition ~ Age + DailyRate + DistanceFromHome + Education + EmployeeNumber + EnvironmentSatisfaction + HourlyRate
                                 + JobInvolvement + MonthlyRate + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears
                                 + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + Non_Traveller
                                 + Travel_Frequently + Travel_Rarely + Department_HumanResources + Department_ResearchandDevelopment
                                 + Department_Sales + Education_HumanResource + Education_LifeSceinces + Education_Marketing + Education_Medical
                                 + Education_Other + Education_TechnicalDegree + Job_Healthcare_Representative + Job_Human_Resources
                                 + Job_Labroratory_Technician + Job_Data_Manager + Job_Manufacturing_Director + Job_Research_Director
                                 + Job_Research_Scientist + Job_Sales_Executive + Job_SalesRepresentative + Status_Divorced + Status_Maried
                                 + Status_Single + JobSatisfaction_1 + JobSatisfaction_2 + JobSatisfaction_3 + JobSatisfaction_4
                                 + WorKLifeBalance_1 + WorKLifeBalance_2 + WorKLifeBalance_3 + WorKLifeBalance_4 + RelationshipSatisfaction_1
                                 + RelationshipSatisfaction_2 + RelationshipSatisfaction_3 + RelationshipSatisfaction_4 + PerformanceRating_3
                                 + PerformanceRating_4 + Gender_Female + Gender_Male + OverTime_No + Overtime_Yes,
                                 data = train.norm.df1, linear.output = T, hidden = c(2))
#Plotting the neural net with a single hidden layer, 2 nodes
#plot(model_1layer_2nodes)

#Predictions for model1 with 1 layer and 2 nodes
training.prediction_model1 <- compute(model_1layer_2nodes, train.norm.df1)
validation.prediction_model1 <- compute(model_1layer_2nodes, valid.norm.df1)

neuralNet_actual_model1 <- ifelse(training.prediction_model1$net.result[,1]>0.5,1,0)
neuralNet_pred_model1 <- ifelse(train.norm.df1$Attrition > 0.5, 0, 1)

#Creating tables for confusionMatrix(actual, predicted)
table(as.factor(neuralNet_pred_model1))                      #Predicted
table(as.factor(neuralNet_actual_model1))                    #Actual

str(as.factor(neuralNet_pred_model1))
str(as.factor(neuralNet_actual_model1))

#Confusion Matrix for Neural Network model
confusionMatrix(as.factor(neuralNet_actual_model1), as.factor(neuralNet_pred_model1))

##################################################################################
####################Naive Bayes Model
IBMNB <- read_excel("WA_Fn-UseC_-HR-Employee-Attrition.xlsx", sheet="Sheet1")
str(IBMNB)

IBMNB$Attrition<-as.factor(IBMNB$Attrition)
IBMNB$BusinessTravel<-as.factor(IBMNB$BusinessTravel)
IBMNB$Department<-as.factor(IBMNB$Department)
IBMNB$Education<-as.factor(IBMNB$Education)
IBMNB$EducationField<-as.factor(IBMNB$EducationField)
IBMNB$Gender<-as.factor(IBMNB$Gender)
IBMNB$JobInvolvement<-as.factor(IBMNB$JobInvolvement)
IBMNB$JobRole<-as.factor(IBMNB$JobRole)
IBMNB$JobSatisfaction<-as.factor(IBMNB$JobSatisfaction)
IBMNB$MaritalStatus<-as.factor(IBMNB$MaritalStatus)
IBMNB$OverTime<-as.factor(IBMNB$OverTime)
IBMNB$PerformanceRating<-as.factor(IBMNB$PerformanceRating)
IBMNB$RelationshipSatisfaction<-as.factor(IBMNB$RelationshipSatisfaction)
IBMNB$WorkLifeBalance<-as.factor(IBMNB$WorkLifeBalance)
IBMNB$Attrition<-as.factor(IBMNB$Attrition)


IBMNB<- IBMNB[-c(24,11,13,17,26)]

View(IBMNB)
t(t(names(IBMNB)))
str(IBMNB)

set.seed(1)
train.index <- sample(row.names(IBMNB), 0.7*dim(IBMNB)[1])
valid.index <-  setdiff(row.names(IBMNB), c(train.index))
train.df <- IBMNB[train.index,]
valid.df <- IBMNB[valid.index,]

View(train.df)
View(valid.df)

##if Upsampling is required for naive bayes then run this piece of code

prop.table(table(train.df$Attrition))
train.df <- ovun.sample(Attrition~., data = train.df, method = "over", N =1724)$data
table(train.df$Attrition)
View(train.df) 

##########Normalization Of Data####
train.norm.df <- train.df[,-2]
valid.norm.df <- valid.df[,-2]
norm.values <- preProcess(train.df[, -2], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -2])
valid.norm.df <- predict(norm.values, valid.df[, -2])



model<-naive_bayes(Attrition~., data=train.df,usekernel = T)
model
#results: 16.22% attrition yes, 83.70% attrition no
#categorical variables have ranks and numeric variables have mean, meidan, sd
#ex: 0.39 probability attrtion yes from Department 0, 0.28 attrition no

plot(model)

#predicting
p<-predict(model,train.df,type = 'prob')
head(cbind(p,train))
#results: 1st employee has a 84% chance of leaving the company


#confusion matrix for training data
p1<-predict(model,train.df)
confusion_matrix1<-table(p1,train.df$Attrition)
confusion_matrix1
#results: 64 employees were correctly classified as attrition yes
#818 were correctly classified as attrition no
#missclassification
1-sum(diag(confusion_matrix1))/sum(confusion_matrix1)

#confusion matrix for validation data
p2<-predict(model,valid.df)
confusion_matrix2<-table(p2,valid.df$Attrition)
confusion_matrix2
#missclassification
1-sum(diag(confusion_matrix2))/sum(confusion_matrix2)








