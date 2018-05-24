#Load in libraries
library(rattle)
library(plotly)
library(rpart)
library(rpart.plot)
library(lattice)
library(Boruta)
library(Metrics)
library(caret)
library(RColorBrewer)
library(party)
library(gbm)
library(ROCR)
library(ggcorrplot)
library(data.table)
library(scales)
library(moments)
library(randomForest)
library(dplyr)

#Read in the data
setwd('/Users/shanehickey/Documents/Semester 2/Data Mining/Assessment/Hackathon Personal')
data = read.csv('data.csv', header = TRUE)


#Change target variable name
names(data)[2] = "Attrition"

#Check split of target var
table(data$Attrition)
prop.table(table(data$Attrition))


#Check for missing values in the dataset
table(is.na(data))
colSums(is.na(data))

#Summarise the data
summary(data)
str(data)

#Drop standard hours, employee number, over18 and employee count as they add nothing
drops = c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours")
data = data[, !(names(data) %in% drops)]

#Set level names as per supporting txt file
levels1 = c("Low", "Medium", "High", "Very High")
levels2 = c("Below College", "College", "Bachelor", "Master", "Doctor")
levels3 = c("Low", "Good", "Excellent", "Outstanding")
levels4 = c("Bad", "Good", "Better", "Best")

#Set appropriate variables to factors for inital visualition
data$Education = as.factor(data$Education)
levels(data$Education) = levels2
data$EnvironmentSatisfaction = as.factor(data$EnvironmentSatisfaction)
levels(data$EnvironmentSatisfaction) = levels1
data$JobInvolvement = as.factor(data$JobInvolvement)
levels(data$JobInvolvement) = levels1
data$JobLevel = as.factor(data$JobLevel)
data$JobSatisfaction = as.factor(data$JobSatisfaction)
levels(data$JobSatisfaction) = levels1
data$PerformanceRating = as.factor(data$PerformanceRating)
levels(data$PerformanceRating) = levels3
data$RelationshipSatisfaction = as.factor(data$RelationshipSatisfaction)
levels(data$RelationshipSatisfaction) = levels1
data$StockOptionLevel = as.factor(data$StockOptionLevel)
data$WorkLifeBalance = as.factor(data$WorkLifeBalance)
levels(data$WorkLifeBalance) = levels4
data$Attrition = as.factor(data$Attrition)
levels(data$Attrition)

#Basic data exploration - Variation of each variable
#Age
histogram(~Age, data = data)
#Department
histogram(~Department, data = data)
ggplot(data) + 
  geom_bar(aes(x = Department))
#Education
ggplot(data) + 
  geom_bar(aes(x = Education))
histogram(~Education, data)
#Education Field
ggplot(data)+
  geom_bar(aes(x = EducationField))
histogram(~EducationField, data)
#Job Involvement
ggplot(data)+
  geom_bar(aes(JobInvolvement))
histogram(~JobInvolvement, data)
#Environment Satisfaction
ggplot(data)+
  geom_bar(aes(EnvironmentSatisfaction))
histogram(~EnvironmentSatisfaction, data)
#Job Satisfaction
ggplot(data)+
  geom_bar(aes(JobSatisfaction))
histogram(~JobSatisfaction, data)
#Job Level
ggplot(data)+
  geom_bar(aes(JobLevel))
#Monthly Income
ggplot(data)+
  geom_histogram(aes(MonthlyIncome))
histogram(~MonthlyIncome, data)
bwplot(~MonthlyIncome, data)
#Gender
ggplot(data)+
  geom_bar(aes(Gender))
histogram(~Gender, data)
#Number of Companies Worked
ggplot(data)+
  geom_histogram(aes(NumCompaniesWorked))
histogram (~NumCompaniesWorked, data)
#Total Working Years
ggplot(data)+
  geom_histogram(aes(TotalWorkingYears))
#Years at Company
ggplot(data)+
  geom_histogram(aes(YearsAtCompany))
#Performance Rating
ggplot(data)+
  geom_bar(aes(PerformanceRating))
histogram(~PerformanceRating, data)
#Work Life Balance
ggplot(data)+
  geom_bar(aes(WorkLifeBalance))
histogram(~WorkLifeBalance, data)

#Make all variables numeric for heatmap plot
data$BusinessTravel = as.numeric(as.factor(data$BusinessTravel))
data$Department = as.numeric(as.factor(data$Department))
data$EducationField = as.numeric(as.factor(data$EducationField))
data$Gender = as.numeric(as.factor(data$Gender))
data$JobRole = as.numeric(as.factor(data$JobRole))
data$MaritalStatus = as.numeric(as.factor(data$MaritalStatus))
data$Over18 = as.numeric(as.factor(data$Over18))
data$OverTime = as.numeric(as.factor(data$OverTime))
data$Attrition = as.numeric(as.factor(data$Attrition))
data$WorkLifeBalance = as.numeric(as.factor(data$WorkLifeBalance))
data$EnvironmentSatisfaction = as.numeric(as.factor(data$EnvironmentSatisfaction))
data$JobInvolvement = as.numeric(as.factor(data$JobInvolvement))
data$Education = as.numeric(as.factor(data$Education))
data$PerformanceRating = as.numeric(as.factor(data$PerformanceRating))
data$RelationshipSatisfaction = as.numeric(as.factor(data$RelationshipSatisfaction))
data$JobLevel = as.numeric(as.factor(data$JobLevel))
data$StockOptionLevel = as.numeric(as.factor(data$StockOptionLevel))
data$JobSatisfaction = as.numeric(as.factor(data$StockOptionLevel))

#Correltion Heatmap to establish variables related to TargetVar
corr = round(cor(data),2)
p.mat = cor_pmat(data)
ggcorrplot(corr, type = 'lower',
           ggtheme = ggplot2::theme_gray,
           p.mat = p.mat,
           insig = 'blank',
           tl.cex = 7)+
  ggtitle('Variable Correlation Visualisation')+
  theme(plot.title = element_text(hjust = 0.5))

#Change them all the above back to categorical for graphing
data$Education = as.factor(data$Education)
levels(data$Education) = levels2
data$EnvironmentSatisfaction = as.factor(data$EnvironmentSatisfaction)
levels(data$EnvironmentSatisfaction) = levels1
data$JobInvolvement = as.factor(data$JobInvolvement)
levels(data$JobInvolvement) = levels1
data$JobLevel = as.factor(data$JobLevel)
data$JobSatisfaction = as.factor(data$JobSatisfaction)
levels(data$JobSatisfaction) = levels1
data$PerformanceRating = as.factor(data$PerformanceRating)
levels(data$PerformanceRating) = levels3
data$RelationshipSatisfaction = as.factor(data$RelationshipSatisfaction)
levels(data$RelationshipSatisfaction) = levels1
data$StockOptionLevel = as.factor(data$StockOptionLevel)
data$WorkLifeBalance = as.factor(data$WorkLifeBalance)
levels(data$WorkLifeBalance) = levels4
data$Attrition = as.factor(data$Attrition)
levels(data$Attrition) = c("No", "Yes")

#Explore relationships based on heatmap results
#Performance Rating and Percent Salary Hike
bwplot(~PercentSalaryHike|PerformanceRating, data)
#Monthly Income vs Job Level
bwplot(~MonthlyIncome|JobLevel, data, layout = c(5, 1))
ggplot(data, aes(x = JobLevel, y = MonthlyIncome)) +
  geom_boxplot()
#Total Working Years and MonthlyIncome
ggplot(data)+
  geom_hex(aes(TotalWorkingYears, MonthlyIncome))
ggplot(data, aes(TotalWorkingYears, MonthlyIncome)) + 
  geom_boxplot(aes(group = cut_width(TotalWorkingYears, 0.1)))
#Stock Option Level vs Marriage Status
ggplot(data)+
  geom_count(aes(MaritalStatus, StockOptionLevel))
#Total Working Years vs Job Level
ggplot(data, aes(JobLevel, y = TotalWorkingYears)) +
  geom_boxplot()

#Explore the target variables relationship with some other variables (as per the heatmap)
#Job Level
histogram(~CurrentEmployee, data)
ggplot(data)+
  geom_count(aes(JobLevel, CurrentEmployee))
histogram(~CurrentEmployee|JobLevel, data, layout = c(5, 1))
#Marital Status
histogram(~CurrentEmployee|MaritalStatus, data)
#Age
ggplot(data, aes(CurrentEmployee, y = Age)) +
  geom_boxplot()
#Monthly Income
ggplot(data, aes(CurrentEmployee, MonthlyIncome)) +
  geom_boxplot()
#Overtime
histogram(~CurrentEmployee|OverTime, data)
#OT vs EnvironmentSat
histogram(~OverTime|JobSatisfaction, data, layout = c(4, 1))
#Stock Option Level
histogram(~CurrentEmployee|StockOptionLevel, data, layout = c(4, 1))
#Total Working Years
ggplot(data, aes(CurrentEmployee, TotalWorkingYears)) +
  geom_boxplot()
#Years at Company
ggplot(data, aes(CurrentEmployee, YearsAtCompany)) +
  geom_boxplot()
#Years in Current Role
ggplot(data, aes(CurrentEmployee, YearsInCurrentRole))+
  geom_boxplot()
#Years with Current Manager
ggplot(data, aes(CurrentEmployee, YearsWithCurrManager))+
  geom_boxplot()
#Environment Satisfaction
histogram(~CurrentEmployee|EnvironmentSatisfaction, data, layout = c(4, 1))

#Explore some hypotheses
#Attrition may differ depending on department
ggplot(data, aes(Department, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Department') +
  ylab('Count') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = Department)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
#Attrition may differ depending on job role
ggplot(data, aes(JobRole, fill = Attrition)) +
  stat_count(width = 0.75) +
  xlab('Job Role') +
  ylab('Count') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = JobRole))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  theme(axis.text.x=element_text(angle=45, hjust=1))
count(data$Attrition, data$JobRole)
#Attrition may differ on Job Levels
ggplot(data, aes(JobLevel, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Job Level') +
  ylab('Count') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = JobLevel))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
#Overtime may have an effect on attrition
ggplot(data, aes(OverTime, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('OverTime') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = OverTime))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
#Job Satisfaction
ggplot(data, aes(JobSatisfaction, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Job Satisfaction') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = JobSatisfaction))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
#Environment Satisfaction
ggplot(data, aes(EnvironmentSatisfaction, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Environment Satisfaction') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = EnvironmentSatisfaction))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
#Job Involvement
ggplot(data, aes(JobInvolvement, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Job Involvement') +
  ylab('Count') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = JobInvolvement))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  ylab("Percentage")
#Business Travel
ggplot(data, aes(BusinessTravel, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Business Travel') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = BusinessTravel))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
#Attrition based on Business Travel per Job Role
ggplot(data, aes(BusinessTravel, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Business Travel') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = BusinessTravel))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~JobRole)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('Business Travel')+
  ylab('Percentage')+
  ggtitle('Attrition Levels Per Business Travels Categories Grouped By Job Role')+
  theme(plot.title = element_text(hjust = 0.5))
#Attrition based on OverTime per Job Role
ggplot(data, aes(OverTime, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Business Travel') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
ggplot(data, aes(x = OverTime))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~JobRole)+
  theme(axis.text.x=element_text(angle=0, hjust=1))+
  xlab('Over Time')+
  ylab('Percentage')+
  ggtitle('Attrition Levels Per Over Time Categories Grouped By Job Role')+
  theme(plot.title = element_text(hjust = 0.5))
#Attrition based on OverTime per Department
ggplot(data, aes(OverTime, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab('Business Travel') +
  ylab('Percentage') +
  labs(fill = 'Attrition')
#Attrition Based on Job Satisfaction per Dept
ggplot(data, aes(x = JobSatisfaction))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~Department)+
  theme(axis.text.x=element_text(angle=45, hjust=1))
#Attrition Based on Job Satisfaction per Job Role
ggplot(data, aes(x = JobSatisfaction))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~JobRole)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('Job Satisfaction')+
  ylab('Percentage')+
  ggtitle('Attrition Levels Per Job Satisfaction Grouped By Job Role')+
  theme(plot.title = element_text(hjust = 0.5))
#Work Life Balance Per Job Role
ggplot(data, aes(x = WorkLifeBalance))+ geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~JobRole)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('Work Life Balance')+
  ylab('Percentage')+
  ggtitle('Attrition Levels Per Work Life Balance Categories Grouped By Job Role')+
  theme(plot.title = element_text(hjust = 0.5))
#Monthly Income
MnthlyIncome = cut(data$MonthlyIncome, 10, include.lowest = TRUE, labels=c(1,2,3,4,5,6,7,8,9,10))
ggplot(data, aes(MnthlyIncome))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Monthly Income Deciles')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Deciles of Monthly Income')+
  theme(plot.title = element_text(hjust = 0.5))
#Work Life Balance
ggplot(data, aes(WorkLifeBalance))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Job Involvement Level')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Job Involvement Categories')+
  theme(plot.title = element_text(hjust = 0.5))
#Age
ages = cut(data$Age, 10, include.lowest = TRUE, labels=c(1,2,3,4,5,6,7,8,9,10))
ggplot(data, aes(ages))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Age Deciles')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Age Deciles')+
  theme(plot.title = element_text(hjust = 0.5))
#Total Working Years
TWY = cut(data$TotalWorkingYears, length(unique(data$TotalWorkingYears)), include.lowest = TRUE, labels=unique(sort(data$TotalWorkingYears)))
ggplot(data, aes(TWY))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Total Working Years Deciles')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Total Working Years Deciles')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
#Years at Company
YAC = cut(data$YearsAtCompany, length(unique(data$YearsAtCompany)), include.lowest = TRUE, labels = unique(sort(data$YearsAtCompany)))
ggplot(data, aes(YAC))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Years At Company Deciles')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Years At Company Deciles')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle=0, hjust=1))
#Job Level
ggplot(data, aes(JobLevel))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Job Level')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Job Level Categories')+
  theme(plot.title = element_text(hjust = 0.5))
#Stock Option Level
ggplot(data, aes(StockOptionLevel))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Job Level')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Job Level Categories')+
  theme(plot.title = element_text(hjust = 0.5))
#Performance Rating
ggplot(data, aes(PerformanceRating))+ geom_bar(aes(fill = factor(Attrition)), position = 'fill')+
  scale_y_continuous(labels = percent_format())+
  xlab('Job Level')+
  ylab('Percentage')+
  ggtitle('Attriton Levels Per Job Level Categories')+
  theme(plot.title = element_text(hjust = 0.5))

#Calculate tables for Appendices
#Total Working Years
TWY = cut(data$TotalWorkingYears, 10, include.lowest = TRUE,labels=c(1,2,3,4,5,6,7,8,9,10))
twy = table(TWY, data$Attrition)
prop.table(ftable(twy), 1)
#Years at Company
YAC = cut(data$YearsAtCompany, 10, include.lowest = TRUE,labels=c(1,2,3,4,5,6,7,8,9,10))
yac = table(YAC, data$Attrition)
prop.table(ftable(yac), 1)
#Overtime
ot = table(data$Attrition, data$OverTime)
prop.table(ftable(ot), 2)
#WLB
wlb = table(data$Attrition, data$WorkLifeBalance)
prop.table(ftable(wlb), 2)
#Job Satisfcation
js = table(data$Attrition, data$JobSatisfaction)
prop.table(ftable(js), 2)
#Environment Satisfaction
es = table(data$Attrition, data$EnvironmentSatisfaction)
prop.table(ftable(es), 2)
#Job Level
jl = table(data$Attrition, data$JobLevel)
prop.table(ftable(jl), 2)

#Predictive Modelling
#Full dataset - 82% accuracy and 57% AUC
#Make train test split - 75/25 currently
dataPartition = sample(2,nrow(data),replace=TRUE,prob=c(0.75,0.25))
train = data[dataPartition ==1,]
test = data[dataPartition ==2,]

table(test$Attrition)
291/nrow(test)

modelCart = rpart(Attrition ~ ., data=train, method="class")
prp(modelCart)

#Make prediction
#Predict the test data
predictionCart = predict(modelCart, newdata=test, type="class")

#CART Accuracy
#Confusion matrix 
t1 = table(test$Attrition, predictionCart)

#CART model accuracy
(t1[1]+t1[4])/(nrow(test))

#CART AUC
roc_cart = roc(as.numeric(predictionCart), as.factor(as.numeric(test$Attrition)))
auc(roc_cart)
#Prune the tree - Accuracy: 83% AUC: 59%
ptree_r = prune(modelCart, cp = modelCart$cptable[which.min(modelCart$cptable[, "xerror"]), "CP"])
printcp(ptree_r)
plotcp(ptree_r)
rpart.plot(ptree_r, main = "Pruned Tree Full Dataset")

#Pruned predictions
predictions_pruned = predict(ptree_r, test, type = 'class')
#Accuracy
#Confusion matrix 
t1_p = table(test$Attrition, predictions_pruned)

#Pruned model accuracy
(t1_p[1]+t1_p[4])/(nrow(test))

#AUC of pruned
predictions_pruned_num = as.numeric(predictions_pruned)
roc_pruned = roc(predictions_pruned_num, as.factor(test_num))
auc(roc_pruned)
plot(roc_pruned)

#Random Forest - No improvement
modelRf = randomForest(Attrition ~ ., data=train, ntree = 100, mtry = 5, importance = TRUE, method="class")

#OOB vs No. Of Trees
plot(modelRf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")

#List the importance of the variables.
impVar <- round(randomForest::importance(modelRf), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

#Tuning Random Forest
tunedRf = tuneRF(x = train[,-2], 
                  y=as.factor(train$Attrition),
                  mtryStart = 50, 
                  ntreeTry=60, 
                  stepFactor = 2, 
                  improve = 0.001, 
                  trace=TRUE, 
                  plot = TRUE,
                  doBest = TRUE,
                  nodesize = 5, 
                  importance=TRUE
)

impvarTunedRf = tunedRf$importance
impvarTunedRf[order(impvarTunedRf[,3], decreasing=TRUE),]

predictionRf = predict(tunedRf, test, type="class")
num_predictionRf = as.numeric(predictionRf)
test_num = as.numeric(test$Attrition)
#RandomForest Accuracy
#Confusion matrix 
t2 = table(test$Attrition, predictionRf)

#RandomForest model accuracy
(t2[1]+t2[4])/(nrow(test))

roc_obj = roc(num_predictionRf, as.factor(test_num))
auc(roc_obj)
plot(roc_obj)

#Run Boruta analysis
boruta_output = Boruta(Attrition ~., data = data)
print(boruta_output)
plot(boruta_output)
par(las = 1)
boruta.df = attStats(boruta_output)
print(boruta.df)
newdf = boruta.df[order(-boruta.df$meanImp),]
print(newdf)


#Reduced dataset - Accuracy 83% and AUC  
refined = rpart(Attrition~OverTime + MonthlyIncome + TotalWorkingYears +
                  Age + JobLevel + StockOptionLevel + YearsAtCompany + 
                  MaritalStatus + YearsWithCurrManager + JobRole + 
                  YearsInCurrentRole, data = train, method = 'class')
prp(refined)

#Predict the test data
predictionrefined <- predict(refined, newdata=test, type="class")

#CART Accuracy
#Confusion matrix 
t1_refined = table(test$Attrition, predictionrefined)


#CART model accuracy
(t1_refined[1]+t1_refined[4])/(nrow(test))

roc_reduced = roc(as.numeric(predictionrefined), as.factor(test$Attrition))
auc(roc_reduced)


#Random Forest
refined_rf = randomForest(Attrition~OverTime + MonthlyIncome + TotalWorkingYears +
                            Age + JobLevel + StockOptionLevel + YearsAtCompany + 
                            MaritalStatus + YearsWithCurrManager + JobRole + 
                            YearsInCurrentRole, data = train, method = 'class', 
                          ntree = 100, mtry = 5, importance = TRUE)


#OOB vs No. Of Trees
plot(refined_rf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")

## List the importance of the variables.
impVar <- round(randomForest::importance(refined_rf), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
tunedRf_r = tuneRF(x = train[,-2], 
                  y=as.factor(train$Attrition),
                  mtryStart = 50, 
                  ntreeTry=60, 
                  stepFactor = 2, 
                  improve = 0.001, 
                  trace=TRUE, 
                  plot = TRUE,
                  doBest = TRUE,
                  nodesize = 5, 
                  importance=TRUE
)

impvarTunedRf_r = tunedRf_r$importance
impvarTunedRf_r[order(impvarTunedRf[,3], decreasing=TRUE),]

predictionRf_r = predict(tunedRf_r, test, type="class")
num_predictionRf_r = as.numeric(predictionRf_r)
test_num_r = as.numeric(test$Attrition)
#RandomForest Accuracy
#Confusion matrix 
t2_r = table(test$Attrition, predictionRf_r)

#RandomForest model accuracy
(t2_r[1]+t2_r[4])/(nrow(test))

roc_rf_refined = roc(as.numeric(predictionRf_r), as.factor(test$Attrition))
auc(roc_rf_refined)

#GBM
gbm_fit = train(Attrition ~.,
                data = train,
                method = 'gbm',
                verbose = FALSE)
predict_gbm = predict(gbm_fit, test, type = 'raw')
gbm_accuracy = confusionMatrix(test$Attrition, predict_gbm)
gbm_accuracy

gbm_fit_r = train(Attrition~OverTime + MonthlyIncome + TotalWorkingYears +
                    Age + JobLevel + StockOptionLevel + YearsAtCompany + 
                    MaritalStatus + YearsWithCurrManager + JobRole + 
                    YearsInCurrentRole,
                  data = train,
                  method = 'gbm',
                  verbose = FALSE)
predict_gbm_r = predict(gbm_fit_r, test, type = 'raw')
gbm_r_accuracy = confusionMatrix(predict_gbm_r, test$Attrition)
gbm_r_accuracy

#Balance the dataset
bal_train = ROSE(Attrition~., data = train)
bal_train
table(train$Attrition)
table(bal_train$data$Attrition)

bal_tree = rpart(Attrition~., data = bal_train$data, method = 'class')
pruned_bal = prune(bal_tree, cp = bal_tree$cptable[which.min(bal_tree$cptable[, "xerror"]), "CP"])
predict_bal_pruned = predict(pruned_bal, test, type = 'class')
confusionMatrix(predict_bal_pruned, test$Attrition)


random_bal = randomForest(Attrition~., data = bal_tree$data, method = 'class', 
                          ntree = 100, mtry = 5, importance = TRUE)


#C50 Method
#Code: Create Independent and Dependent data
C50data = data[sample(nrow(data)),]
summary(C50data)
dim(C50data)
C50x = C50data[,-c(2)] # independent
C50y = C50data[,c(2)] # dependent

# Code: Create train and test splits
C50xtrain = C50x[1:1176, ]
C50xtest = C50x[1177:1470, ]
C50ytrain = C50y[1:1176]
C50ytest = C50y[1177:1470]

# Code: Try Boosting with 10 trials
c50model = C50::C5.0(C50xtrain,C50ytrain,trials=10)

# Code: Evaluate the model using the Test Set (y)
preds = predict(c50model, C50xtest, type='class' )
sum(p == C50ytest) / length(p)
confusionMatrix(preds, C50ytest)
roc_c50 = roc(as.numeric(preds), as.factor(C50ytest))


#Read the dataset in again from fresh as all columns are needed
data_full = read.csv('data.csv', header = TRUE)
names(data_full)[2] = "Attrition"

#Set Variables to factors for inital visualition
data_full$Education = as.factor(data_full$Education)
levels(data_full$Education) = levels2
data_full$EnvironmentSatisfaction = as.factor(data_full$EnvironmentSatisfaction)
levels(data_full$EnvironmentSatisfaction) = levels1
data_full$JobInvolvement = as.factor(data_full$JobInvolvement)
levels(data_full$JobInvolvement) = levels1
data_full$JobLevel = as.factor(data_full$JobLevel)
data_full$JobSatisfaction = as.factor(data_full$JobSatisfaction)
levels(data_full$JobSatisfaction) = levels1
data_full$PerformanceRating = as.factor(data_full$PerformanceRating)
levels(data_full$PerformanceRating) = levels3
data_full$RelationshipSatisfaction = as.factor(data_full$RelationshipSatisfaction)
levels(data_full$RelationshipSatisfaction) = levels1
data_full$StockOptionLevel = as.factor(data_full$StockOptionLevel)
data_full$WorkLifeBalance = as.factor(data_full$WorkLifeBalance)
levels(data_full$WorkLifeBalance) = levels4
data_full$Attrition = as.factor(data_full$Attrition)
levels(data_full$Attrition) = c("No", "Yes")

C50data = data_full[sample(nrow(data_full)),]
#Split dataset for 5 fold validation
spec = c(d1 = .2, d2 = .2, d3 = .2, d4 = .2, d5 = .2)

g = sample(cut(
  seq(nrow(C50data)), 
  nrow(C50data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(C50data, g)

sapply(res, nrow)/nrow(C50data)

#Make them each their own data set
d1 = res$d1
d2 = res$d2
d3 = res$d3
d4 = res$d4
d5 = res$d5

#Save employee numbers before dropping as they are needed for report
employee_no = unlist(list(d1$EmployeeNumber,d2$EmployeeNumber,d3$EmployeeNumber,d4$EmployeeNumber,d5$EmployeeNumber))
#Drop these columns from the predictive set as they do not improve it
drops = c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours")
d1 = d1[ , !(names(d1) %in% drops)]
d2 = d2[ , !(names(d2) %in% drops)]
d3 = d3[ , !(names(d3) %in% drops)]
d4 = d4[ , !(names(d4) %in% drops)]
d5 = d5[ , !(names(d5) %in% drops)]

#Split each of the datasets into dependent and independent variables
d1X = d1[, -c(2)]
d1y = as.factor(d1[, c(2)])
d2X = d2[, -c(2)]
d2y = as.factor(d2[, c(2)])
d3X = d3[, -c(2)]
d3y = as.factor(d3[, c(2)])
d4X = d4[, -c(2)]
d4y = as.factor(d4[, c(2)])
d5X = d5[, -c(2)]
d5y = as.factor(d5[, c(2)])

#Define train and test sets for each of 5 models
train_set1X = rbind(d1X, d2X, d3X, d4X)
train_set1y = unlist(list(d1y, d2y, d3y, d4y))
test_set1X = d5X
test_set1y = d5y
train_set2X = rbind(d1X, d2X, d3X, d5X)
train_set2y = unlist(list(d1y, d2y, d3y, d5y))
test_set2X = d4X
test_set2y = d4y
train_set3X = rbind(d1X, d2X, d5X, d4X)
train_set3y = unlist(list(d1y, d2y, d5y, d4y))
test_set3X = d3X
test_set3y = d3y
train_set4X = rbind(d1X, d5X, d3X, d4X)
train_set4y = unlist(list(d1y, d5y, d3y, d4y))
test_set4X = d2X
test_set4y = d2y
train_set5X = rbind(d5X, d2X, d3X, d4X)
train_set5y = unlist(list(d5y, d2y, d3y, d4y))
test_set5X = d1X
test_set5y = d1y


#Run C50 model on trainset_1 - boosting 100 times
c50model1 = C50::C5.0(train_set1X,train_set1y,trials=100)

#Code: Evaluate the model using the Test Set (y)
preds1 = predict(c50model1, test_set1X, type='class')
res1 = confusionMatrix(preds1, test_set1y) #Accuracy = 86.73
roc_c501 = roc(as.numeric(preds1), as.factor(test_set1y))
auc1 = auc(roc_c501) #0.5
res1 = res1$overall['Accuracy']

#Run C50 model on trainset_2
c50model2 = C50::C5.0(train_set2X,train_set2y,trials=100)

#Code: Evaluate the model using the Test Set (y)
preds2 = predict(c50model2, test_set2X, type='class')
res2 = confusionMatrix(preds2, test_set2y) #Accuracy = 85.37
roc_c502 = roc(as.numeric(preds2), as.factor(test_set2y))
auc2 = auc(roc_c502) #0.5
res2 = res2$overall['Accuracy']


#Run C50 model on trainset_3
c50model3 = C50::C5.0(train_set3X,train_set3y,trials=100)

#Code: Evaluate the model using the Test Set (y)
preds3 = predict(c50model3, test_set3X, type='class')
res3 = confusionMatrix(preds3, test_set3y) #Accuracy = 95.58
roc_c503 = roc(as.numeric(preds3), as.factor(test_set3y))
auc3 = auc(roc_c503) #0.893
res3 = res3$overall['Accuracy']

#Run C50 model on trainset_4
c50model4 = C50::C5.0(train_set4X,train_set4y,trials=100)

#Code: Evaluate the model using the Test Set (y)
preds4 = predict(c50model4, test_set4X, type='class')
res4 = confusionMatrix(preds4, test_set4y) #Accuracy = 81.97
roc_c504 = roc(as.numeric(preds4), as.factor(test_set4y))
auc4 = auc(roc_c504) #0.5
res4 = res4$overall['Accuracy']


#Run C50 model on trainset_5
c50model5 = C50::C5.0(train_set5X,train_set5y,trials=100)

#Code: Evaluate the model using the Test Set (y)
preds5 = predict(c50model5, test_set5X, type='class')
res5 = confusionMatrix(preds5, test_set5y) #Accuracy = 82.31
roc_c505 = roc(as.numeric(preds5), as.factor(test_set5y))
auc5 = auc(roc_c505) #0.4967
res5 = res5$overall['Accuracy']

#Get importance of variables for report
C5imp(c50model5, metrics = 'splits')


#Calculate mean of the accuracies and AUCs
final_res = mean(res1, res2, res3, res4, res5)
final_auc = mean(auc1, auc2, auc3, auc4, auc5)
final_res
final_auc

#Construct complete dataframe with predictions to see who is current but predicted to leave. 
finalX = rbind(d1, d2, d3, d4, d5)
finalX
final_pred = unlist(list(preds1, preds2, preds3, preds4, preds5))
final_df = cbind(finalX, final_pred)
dim(final_df)

#Add employee numbers in who are leaving
final_df$EmployeeNum = employee_no
#Identify those who are current employees but model predicts they are leaving
leaving = final_df[final_df$Attrition == "No" & final_df$final_pred == "Yes",]
#Split it per depts
leaving_sales = final_df[final_df$Attrition == "No"
                         & final_df$final_pred == "Yes"
                         & final_df$Department == "Sales",]
leaving_hr = final_df[final_df$Attrition == "No"
                         & final_df$final_pred == "Yes"
                         & final_df$Department == "Human Resources",]
leaving_rd = final_df[final_df$Attrition == "No"
                         & final_df$final_pred == "Yes"
                         & final_df$Department == "Research & Development",]

#List of employee numbers who are likely to leave
leaving$EmployeeNum

#Absolute Counts
nrow(leaving_sales)
nrow(leaving_hr)
nrow(leaving_rd)

#Get a current headcount on each of the depts
sales_hc = nrow(final_df[final_df$Attrition == "No" & final_df$Department == "Sales",])
hr_hc = nrow(final_df[final_df$Attrition == "No" & final_df$Department == "Human Resources",])
rd_hc = nrow(final_df[final_df$Attrition == "No" & final_df$Department == "Research & Development",])

#Final Figures as percentages 
sales_leaving_pct = (nrow(leaving_sales)/sales_hc)*100 
hr_leaving_pct = (nrow(leaving_hr)/hr_hc)*100 
rd_leaving_pct = (nrow(leaving_rd)/rd_hc)*100 
sales_leaving_pct
hr_leaving_pct
rd_leaving_pct


#Compare to historic levels
historic = table(data$Attrition, data$Department)
prop.table(ftable(historic), 2)

#Clustering
#Need to normalise all the data for clustering
normal_data = data

#Make factors into numeric for normalisation
normal_data$BusinessTravel = as.numeric(as.factor(normal_data$BusinessTravel))
normal_data$Department = as.numeric(as.factor(normal_data$Department))
normal_data$EducationField = as.numeric(as.factor(normal_data$EducationField))
normal_data$Gender = as.numeric(as.factor(normal_data$Gender))
normal_data$JobRole = as.numeric(as.factor(normal_data$JobRole))
normal_data$MaritalStatus = as.numeric(as.factor(normal_data$MaritalStatus))
normal_data$OverTime = as.numeric(as.factor(normal_data$OverTime))
normal_data$Attrition = as.numeric(as.factor(normal_data$Attrition))
normal_data$WorkLifeBalance = as.numeric(as.factor(normal_data$WorkLifeBalance))
normal_data$EnvironmentSatisfaction = as.numeric(as.factor(normal_data$EnvironmentSatisfaction))
normal_data$JobInvolvement = as.numeric(as.factor(normal_data$JobInvolvement))
normal_data$Education = as.numeric(as.factor(normal_data$Education))
normal_data$PerformanceRating = as.numeric(as.factor(normal_data$PerformanceRating))
normal_data$RelationshipSatisfaction = as.numeric(as.factor(normal_data$RelationshipSatisfaction))
normal_data$JobLevel = as.numeric(as.factor(normal_data$JobLevel))
normal_data$StockOptionLevel = as.numeric(as.factor(normal_data$StockOptionLevel))
normal_data$JobSatisfaction = as.numeric(as.factor(normal_data$StockOptionLevel))

#Define normalisation parameters
normal_data_params = preProcess(normal_data, method = 'range')
#Transform data
normal_data = predict(normal_data_params, normal_data)

#Clustering
#MonthlyIncome, Total Working Years and Years at Company
hrCluster = kmeans(normal_data[, c("MonthlyIncome", "Age", "YearsAtCompany")], 4, nstart = 20)

hrCluster$cluster = as.factor(hrCluster$cluster)

#3d plotting using plotly
plot_ly(normal_data, x = ~TotalWorkingYears,
        y = ~YearsAtCompany,
        z = ~MonthlyIncome,
        color = hrCluster$cluster,
        text = ~hrCluster$cluster) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'Years At Company'),
                      zaxis = list(title = 'Monthly Income')))

#Age, Job Level and Monthly Income
hrclus1 = kmeans(normal_data[, c("Age", "MonthlyIncome", "JobLevel")], 3, nstart = 25)
hrclus1$cluster = as.factor(hrclus1$cluster)
ggplot(normal_data, aes(Age, MonthlyIncome, color = hrclus1$cluster))+
  geom_point()+
  geom_text(aes(label = data$Attrition))
plot_ly(normal_data, x = ~Age,
        y = ~JobLevel,
        z = ~MonthlyIncome,
        color = hrclus1$cluster,
        text = ~hrclus1$cluster) %>%
  add_markers() %>%
  layout(title = "Clustering on Monthly Income, Age and Job Level",
         scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'Job Level'),
                      zaxis = list(title = 'Monthly Income')))

#Calculation of percentages leaving or not in the clusters
normal_data$clus  = hrclus1$cluster
tot1 = nrow(normal_data[normal_data$clus == 1,])
tot2 = nrow(normal_data[normal_data$clus == 2,])
tot3 = nrow(normal_data[normal_data$clus == 3,])
tot4 = nrow(normal_data[normal_data$clus == 4,])
no1 = nrow(normal_data[normal_data$clus == 1 & normal_data$Attrition == 0,])
no2 =  nrow(normal_data[normal_data$clus == 2 & normal_data$Attrition == 0,])
no3 = nrow(normal_data[normal_data$clus == 3 & normal_data$Attrition == 0,])
no4 = nrow(normal_data[normal_data$clus == 4 & normal_data$Attrition == 0,])
yes1 = nrow(normal_data[normal_data$clus == 1 & normal_data$Attrition == 1,])
yes2 = nrow(normal_data[normal_data$clus == 2 & normal_data$Attrition == 1,])
yes3 = nrow(normal_data[normal_data$clus == 3 & normal_data$Attrition == 1,])
yes4 = nrow(normal_data[normal_data$clus == 4 & normal_data$Attrition == 1,])
yes1/tot1
yes2/tot2
yes3/tot3
yes4/tot4


#Get count of departments in the whole dataset
#Sales reps - 5.6%
83/1470

#Human Resources - 3.5%
52/1470

#Research Scientist - 19%
(245+47)/1470

#Lab Technician - 17%
(197+62)/1470

#Sales Exec - 22%
(269+57)/1470