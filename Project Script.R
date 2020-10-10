#import tencent stock dataset
#setwd("C:\\Users\\Inno Mvula\\Desktop\\MSc Quantitative Finance\\S2.MM905 - Financial Econometrics\\Assignment")
tcehy = read.csv("C:\\Users\\Inno Mvula\\Desktop\\MSc Quantitative Finance\\S2.MM905 - Financial Econometrics\\Assignment\\TCEHY.csv")

#import libraries
library(TSA)
library(tseries)
library(PerformanceAnalytics)
library(FinTS)
library(robustbase)
library(forecast)
library(xtable)
library(ggplot2)
# Time Series Plotting

#observe dataset, 
head(tcehy, 5)

#In order to access variables in the dataframe we must use the attach function to attach the datafram to the R search Path
attach(tcehy)

#The main variable of interest is the closing Price. Call the closing price column and store it as the a new variable
cprices = tcehy$Close

#adjust date format for R
dates <- as.Date(tcehy$Date)

#The first step of any time series is to observe the stationarity
#we will take 2 approaches towards this

#1. Visualization of plots of our time series. Looking for any evidence of trends and or seasonality
#plots1 = function(n=1,m=1)par(mfrow=c(n,m)) 
#plots1(3,1)
plot(cprices ~ dates, type="l", xlab="Date",ylab="Closing Price", main = "Closing Stock Prices: TCEHY")
acf(cprices)

#2. Review Summary Statistics. Observe any changes in basic statistics like the mean and Variance
hist(cprices)
split = round(length(cprices)/2)
split
cprice1 = cprices[1:split]
m1 = mean(cprice1)
v1 = var(cprice1)
cprice2 = cprices[split:1259]
m2 = mean(cprice2)
v2 = var(cprice2)
group1 = c(m1, v1)
group2 = c(m2, v2)
sumstat = c("Mean", "Variance")
sumstats = data.frame(sumstat, group1, group2)
sumstats
print(xtable(sumstats, caption = "Summary Statistics"))
#3. Run a statistical test for stationarity using the Augmented Dickey-Fuller Test
adf.test(cprices)
print(xtable(adf.test(cprices), caption = "Augmented Dickey-Fuller Test: Closing Prices"), table.placement = 'H')


#After discovering non stationarity we will take the log returns which often resolves non-stationarity
lcprices = diff(log(cprices))
#confirm stationarity
plot(lcprices, type = "l")
acf(lcprices)

hist(lcprices)
lcprice1 = lcprices[1:split]
lm1 = mean(lcprice1)
lv1 = var(lcprice1)
lcprice2 = lcprices[split:1258]
lm2 = mean(lcprice2)
lv2 = var(lcprice2)
lgroup1 = c(lm1, lv1)
lgroup2 = c(lm2, lv2)
lsumstat = c("Mean", "Variance")
lsumstats = data.frame(lsumstat, lgroup1, lgroup2)
lsumstats

adf.test(lcprices)

#Part 2:Study the distribution of the returns. Calculate Value at Risk at di???erent con???dence levels.

#Distribution
mean(lcprices)
var(lcprices)
skewness(lcprices)
kurtosis(lcprices)
#test for normality
ntest = rnorm(630, mean(lcprices), sqrt(var(lcprices)))
ks.test(lcprices, ntest)

#Find the 95%, 97.5% and 99% VAR
vatr = quantile(lcprices, c(0.05, 0.025, 0.01))
vatr

#historical approach
VaR(lcprices, 0.95, "historical")
VaR(lcprices, 0.975, "historical")
VaR(lcprices, 0.99, "historical")

#Gaussian approach
VaR(lcprices, 0.95, "gaussian")
VaR(lcprices, 0.975, "gaussian")
VaR(lcprices, 0.99, "guassian")

#Modified Cornish-Fisher approach
VaR(lcprices, 0.95, "modified")
VaR(lcprices, 0.975, "modified")
VaR(lcprices, 0.99, method = "modified")

#kernel approach
VaR(lcprices, 0.95, "kernel")
VaR(lcprices, 0.975, "kernel")
VaR(lcprices, 0.99, "kernel")

#3.Fit a reasonable time series model to your data and check the ???tted model.
#before we attempt to fit a model to our data we will check for any white noise, a test for predictability
#run Ljung-Box test on daily returns. Ho = White noise, Ha = No white noise
Box.test(lcprices)

#Identify Suitable ARMA model
acf(lcprices, 50)
pacf(lcprices, 50)
ord = ar(lcprices)
ord$aic
ord$order

model = auto.arima(lcprices, approximation = FALSE, stepwise = FALSE)
model
#assess model
summary(model)
checkresiduals(model, 50)
plot(forecast(model, 100))

#4.4. Build a model to describe the volatility of the log-returns.
g = garch(lcprices, order = c(1,1))
summary(g)

#plots1 = function(n=1,m=1)par(mfrow=c(n,m)) 
#plots1(3,1)
plot(g$residuals, type = "l")
qqnorm(g$residuals); qqline(g$residuals, col = 2)
plot((fitted(g)[,1])^2, ylab = "Conditional Variance", xlab = "t", type = "l")
