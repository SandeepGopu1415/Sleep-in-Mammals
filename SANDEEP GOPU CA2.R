##########CA2####Q1#############
#Consider a relational dataset and specify your input and output variables , then:
#heart_failure_clinical data set
#In my dataset DEATH_EVENT is Output and remaining columns are Input.
#(a)	Train the model using 80% of this dataset and suggest an appropriate GLM to model ouput to input variables.  
data=read.csv(file.choose())
head(data)
n=nrow(data); n


####### a)split the dataset to 80% as a trainset and 20% as the testset.
indexes = sample(n,n*(80/100))
trainset = data[indexes,]
testset = data[-indexes,]
logr=glm(DEATH_EVENT~.,family='binomial', data=trainset) 
####### b)explore the related hypotheses test. Estimate the parameters of your model. #################################
summary(logr)
coef(logr)
#age,ejection_fraction,serum_creatinine,serum_sodium,time these coefficents are less than 0.05.
#######(c)	Predict the output of the test dataset using the trained model. 
#Provide the functional form of the optimal predictive model. #################################

actual=testset$DEATH_EVENT; actual
length(actual)


pred_logr=predict(logr, testset, type='response') # prediction
L = length(pred_logr)
pred=rep(0,L); pred
pred[pred_logr>0.5]=1 ; pred # probability of DEATH_EVENT being 1, if p<0.5 then DEATH_EVENT=0



########### (d)	Provide the confusion matrix and obtain the probability of correctness of predictions. ) 


confusion_matrix=table(actual,pred);confusion_matrix
acc=mean(actual==pred) ; acc # correctness of prediction
df=data.frame(actual,pred); head(df)
rmse_logr=sqrt(mean(actual-pred)^2)
rmse_logr



################################# CA2_Q3 ################################################
#Use the a particular stock market dataset and apply the following steps to accomplish the time series analysis: 
# time series analysis
# please load the following packages
library(TTR); library(quantmod)
library(forecast)
data=getSymbols('LLY', src='yahoo', from='2021-04-17',auto.assign = FALSE)
head(data); tail(data)
M=data$LLY.Volume
M
##############a)Check whether the time series is stationary in mean and variance. #############################
plot(M)
abline(reg=lm(M~time(M)), col="red")
S=diff(M)
plot(S)
N=diff(log(M))
plot(N)
############## b)Use acf() and pacf() functions to identify the order of AR and MA.                                                                                 #############################
acf(M)
pacf(M)
summary(M)

########c) Use auto.arima() to learn the best ARIMA model. #############################
auto.fit=auto.arima(M, seasonal=T)
auto.fit
auto.fit=auto.arima(M, seasonal=F)
auto.fit
######d)Forecast h=10 step ahead prediction of the time series variable and plot it with the original time series.
auto.fcast = forecast(auto.fit, h=10) 
plot(auto.fcast)


