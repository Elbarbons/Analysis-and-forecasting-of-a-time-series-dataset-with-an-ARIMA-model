rm(list=ls())
library("forecast")
library("bayesforecast")


########## LOADING AND PREPARING DATA ##########
data("BJsales")

# Divide training data from test data that we need to forecast
train <- BJsales[1:140]
test <- BJsales[141:150]


########## CHECKING STATIONARITY OF THE DATA ##########
plot(train, type="l", ylab="BJsales")
title(main="Trajectory")

#The variance seems more or less constant. The mean seems not stationary
#We check the autocorrelation plot to see if the data is stationary
ggacf(train)

#Data is non stationary, we apply the first order differencing and check
#ACF plot again
z<-diff(train)
ggacf(z)
ggPacf(z)
#After applying  first order differencing data is stationary
# So we will consider ARIMA(p,1,q) models 


########## MODELS AND SCORES ##########
#Initialize variables and matrices to store scores
#In matrix[i,j] will be stored the the model ARIMA(i-1, 1, j-1)
pmax<-3
qmax<-3
modelsAIC <- matrix(9999,pmax+1,qmax+1)
modelsAICc <- matrix(9999,pmax+1,qmax+1)
modelsBIC <- matrix(9999,pmax+1,qmax+1)
modelsLogLik <- matrix(9999,pmax+1,qmax+1)

for (i in 0:pmax){
  for (j in 0:qmax){
    fit<-Arima(train, order=c(i,1,j), method="ML",include.mean=FALSE)
    modelsAIC[i+1,j+1] <- fit$aic
    modelsAICc[i+1,j+1] <- fit$aicc
    modelsBIC[i+1,j+1] <- fit$bic
    modelsLogLik[i+1,j+1] <- fit$loglik
  }
}

#Gaining indexes of the best model both for AICc and BIC (the one with lower score)
BestPQAICCc <- which(modelsAICc == min(modelsAICc), arr.ind = TRUE)-1
BestPQBIC <- which(modelsBIC == min(modelsBIC), arr.ind = TRUE)-1
#Printing the best ARIMA(p,1,q) model
print(paste(
  "The best model according to AICc scores is ARIMA(",BestPQAICCc[1],",1,",BestPQAICCc[2],")"))
print(paste(
  "The best model according to AICc scores is ARIMA(",BestPQBIC[1],",1,",BestPQBIC[2],")"))


########## FORECASTING ##########
fit <- Arima(train, order=c(BestPQAICCc[1],1,BestPQAICCc[2]), method="ML",include.mean=FALSE)
checkresiduals(fit)
frcst <- forecast(fit)
plot(frcst,lwd=2)
lines(c(141:150),test,col="red",lwd=2)

#As baseline method we use the sample mean of the differenced data
BaselinePoints <- rep(c(train[140]+mean(z)), times=10)
lines(c(141:150),BaselinePoints ,col="green",lwd=2)

#Define function to calculate mean squared error from test data and predicted data
MSE <- function(n, trueData, forecasted){
  sum <- 0
  for(i in 1:n){
  sum <- sum + (trueData[i]-forecasted[i])^2
  }
  tot <- (1/n)*sum
}

#confront baseline model MSE and arima model MSE
MSEbaseline <- MSE(10,test,BaselinePoints)
MSEArima <- MSE(10,test,frcst$mean)

print(paste(
  "MSE for baseline model",MSEbaseline))
print(paste(
  "MSE for ARIMA model",MSEArima))

