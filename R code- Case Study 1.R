library("forecast") #load forecast library

# ###################Load data#####################
demandData <- read_csv("C:\Users\hreed\OneDrive\Desktop\Spring 2022 classes\IDS 552-suppply chain managment\Walmart case study\Case Study 1.csv")
  
# ###########Extract time series and plot#########
dataFreq= 4 #Data frequency of time series. Set to 12 and 4 for monthly and quaterly data, respectively
startEntry= c(2010,2) #Time stamp of first entry in time series e.g. c(2010,2) implies second quarter of 2010 
demandTS <- ts(demandData$Demand, frequency=dataFreq,start=startEntry) #create a time series
# 
df = plot(demandTS,main = "Sales Demand",xlab="Sales",ylab="Year")  #plot time series.

# ###########Decompose time series and plot#########
# #Decompose methods return an object containing
tsDecomp <- decompose(demandTS, type="multiplicative") #classical decomposition. Can be set to multiplicative or additive
plot(tsDecomp) #plot decomposed time series

# ###########Prepare time series for forecasting#########
# ###We partition the time series into a training set for 
# ###forecasting and a test set to evaluate accuracy
trainSetStart= c(2010,2) #training set start location in time series (typically the first entry)
trainSetEnd= c(2012,1) #training set end location in time series (typically covers 70% of time series)
testSetStart= c(2012,2) #test set start location in time series (typically location of entry after training set ends)
testSetEnd= c(2013,1) #test set end location in time series (typically end of time series)
#   
demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd)#extract training set
demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set

# ###########Forecast#########
numForcPeriods = 4 #number of periods to forecast in the future
#   
HWForcModel <- HoltWinters(demandTrain, seasonal = "multiplicative") #Train Holt-Winters forecasting model. Can be additive or multiplicative
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = HoltWinters)
HWForecast1 <- HWForecast

HWForcModel <- Arima(demandTrain) #Train Arima forecasting model
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = Arima)
HWForecast2 <- HWForecast

HWForcModel <- naive(demandTrain) #Train naive model
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = naive)
HWForecast3 <- HWForecast

HWForcModel <- naive(demandTrain, seasonal = "multiplicative") #Train Naive Seasonal model
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = naive)
HWForecast4 <- HWForecast

par(mfrow = c(2,2))

plot(HWForecast1, main="HoltWinter ",xlab="Year.Quarter",ylab="Demand") #plot the training demand, and forecast with prediction intervals
plot(HWForecast2, main="Arima",xlab="Year.Quarter",ylab="Demand")
plot(HWForecast3, main="Naive",xlab="Year.Quarter",ylab="Demand")
plot(HWForecast4, main="Naive Seasonal",xlab="Year.Quarter",ylab="Demand")

lines(demandTest,col=10) #add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2),legend=c("Training Demand","Forecast","Testing Demand")) #create plot legend

# ###########Analyze forecasting error#########
error = HWForecast$mean - demandTest #difference between forecast and actual demand
AD=abs(error) #absolute value of error

#Create empty vectors to store errors
MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
bias <- matrix(, nrow = numForcPeriods, ncol = 1)
TS <- matrix(, nrow = numForcPeriods, ncol = 1)
#Label columns of matrices using name of error
colnames(MSE) <- "MSE"
colnames(MAD) <- "MAD"
colnames(MAPE) <- "MAPE"
colnames(bias) <- "bias"
colnames(TS) <- "TS"

#compute errors
for(t in 1:numForcPeriods){
  MSE[t] <- mean(error[1:t]*error[1:t])
  MAD[t] <- mean(AD[1:t])
  MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
  bias[t] <- sum(error[1:t])
  TS[t]= bias[t]/MAD[t]
}

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
colnames(error_Meas)[1] <- "Year"
colnames(error_Meas)[2] <- "Qtr"
colnames(error_Meas)[3] <- "Actual demand"
colnames(error_Meas)[4] <- "Forecast"
