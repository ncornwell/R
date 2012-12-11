#Uncomment this to install the quantmod package for the first time
#install.packages("quantmod")
#install.packages("ggplot2")

#Load required libraries
require('quantmod')
require('ggplot2')

periods <- 'monthly'
pStart <- '2003-12-01'
pRange <- '2003-12::2011-12'

#Load AAPL, MMM, and S&P500
#getSymbols(c("AAPL", "MMM", "^GSPC"), src="yahoo", from = pStart)

#Convert data to monthly returns
appleMonthly <- periodReturn(AAPL$AAPL.Adjusted,period=periods,subset=pRange)
mmmMonthly <- periodReturn(MMM$MMM.Adjusted,period=periods,subset=pRange)
marketMonthly <- periodReturn(GSPC$GSPC.Adjusted,period=periods,subset=pRange)

#average return (AVERAGE), 
avgApple = mean(appleMonthly)
avgMMM = mean(mmmMonthly)
avgMarket = mean(marketMonthly)
#standard deviation of returns (STDEV), 
stdApple = sqrt(var(appleMonthly))
stdMMM = sqrt(var(mmmMonthly))
stdMarket = sqrt(var(marketMonthly))
#and variance of returns (VAR). 
varApple = var(appleMonthly)
varMMM = var(mmmMonthly)
varMarket = var(marketMonthly)
#What is the covariance (COVAR) and 
#correlation (CORREL) between the returns of stock 1 and stock 2?
covar = cov(appleMonthly, mmmMonthly)
correl = cor(appleMonthly, mmmMonthly)

p <- ggplot()

