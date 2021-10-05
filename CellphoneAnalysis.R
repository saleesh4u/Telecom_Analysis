#All necessory libraries have been pre-loaded.
#EDA
Cellphone=read_xlsx("Cellphone.xlsx")
#Univariate analysis

str(Cellphone)
head(Cellphone)
attach(Cellphone)

summary(Cellphone)
hist(AccountWeeks)
hist(DataUsage)
hist(DayMins)
hist(DayCalls)
hist(MonthlyCharge)
hist(OverageFee)
hist(RoamMins)

#Bivariate analysis
plot(AccountWeeks,DataUsage)
plot(AccountWeeks,DayMins)
plot(AccountWeeks,DayCalls)
plot(AccountWeeks,MonthlyCharge)
plot(AccountWeeks,OverageFee)
plot(AccountWeeks,RoamMins)
plot(DataUsage,DayMins)
plot(DataUsage,DayCalls)
plot(DataUsage,MonthlyCharge)
plot(DataUsage,OverageFee)
plot(DataUsage,RoamMins)
plot(DayMins,DayCalls)
plot(DayMins,MonthlyCharge)
plot(DayMins,OverageFee)
plot(DayMins,RoamMins)
plot(DayCalls,MonthlyCharge)
plot(DayCalls,OverageFee)
plot(DayCalls,RoamMins)
plot(MonthlyCharge,OverageFee)
plot(MonthlyCharge,RoamMins)
plot(OverageFee,RoamMins)

CP <-cor(Cellphone)
corrplot(CP, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#Checking Multicolinearity and treating it

lm.churn.full=lm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins)
vif(lm.churn.full)
summary(lm.churn.full)

lm.churn.partial=lm(Churn~AccountWeeks+ContractRenewal+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins)
vif(lm.churn.partial)
summary(lm.churn.partial)

CellphoneData=Cellphone[,-(4:5)]
summary(CellphoneData)

#Identify missing values
is.na(CellphoneData)
summary(is.na(CellphoneData))

#Identify outliers
boxplot(AccountWeeks)
boxplot(CustServCalls)
boxplot(DayMins)
boxplot(DayCalls)
boxplot(MonthlyCharge)
boxplot(OverageFee)
boxplot(RoamMins)


#Logistic Regression
Churn=as.factor(Churn)
summary(Churn)
483/(2850+483)#Churn rate=14.5%
glm(Churn~AccountWeeks+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+MonthlyCharge,data=CellphoneData,family="binomial")
summary(glm(Churn~AccountWeeks+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+MonthlyCharge,data=CellphoneData,family="binomial"))
vif(glm(Churn~AccountWeeks+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+MonthlyCharge,data=CellphoneData,family="binomial"))

Logistic.Churn=glm(Churn~CustServCalls+DayMins+MonthlyCharge+OverageFee+RoamMins+MonthlyCharge,data=CellphoneData,family="binomial")
summary(Logistic.Churn)

Logistic.Churn$fitted.values
plot(Churn,Logistic.Churn$fitted.values)
Churn.predicted=ifelse(Logistic.Churn$fitted.values>0.9,"Churn","No")
table(Churn,Churn.predicted)
roc(Churn,Logistic.Churn$fitted.values)
plot.roc(Churn,Logistic.Churn$fitted.values)

#KNN

dim(CellphoneData)
set.seed(1)
index=sample(3333,2222)
CPData.train=CellphoneData[index,]
CPData.test=CellphoneData[-index,]
knn(CPData.train[,c(4,8)],CPData.test[,c(4,8)],CPData.train$Churn,k=5)
KNN.Churn=knn(scale(CPData.train[,c(4,8)]),scale(CPData.test[,c(4,8)]),CPData.train$Churn,k=2)
summary(KNN.Churn)
table(CPData.test$Churn,KNN.Churn)

#Naive Bayes
naiveBayes(Churn~ContractRenewal+MonthlyCharge,data=CPData.train)
NB.Churn=naiveBayes(Churn~ContractRenewal+MonthlyCharge,data=CPData.train)
predNB=predict(NB.Churn,type="raw",newdata=CPData.test)
predNB
plot(Churn,predict(NB.Churn,type="raw",newdata=CellphoneData)[,2])




