#Uncomment this to install the quantmod package for the first time
#install.packages("quantmod")

#Load required libraries
require('quantmod')

capm <- function(symbol, 
                 length = 61, 
                 end = Sys.Date(), 
                 periodicity = 'monthly', 
                 marketIndex = '^GSPC', #S&P 500
                 riskFree = '^FVX') #5 year treasury
{
  #Convert the end date and subtract
  start <- as.POSIXlt(end)
  switch(periodicity, 
         monthly = start$mon <- start$mon - length,
         weekly = start$mday <- start$mday - (length*7),
         daily = start$mday <- start$mday - length)
  
  periodRange <- paste(start, "::", end, sep = "")
  print(paste("Getting CAPM for the", periodicity, "period", periodRange, sep=" "))
  
  #Load Data
  getSymbols(c(symbol, marketIndex, riskFree), src="yahoo", from = start, to = end)
  
  #Get our loaded variables
  equity <- normString(symbol)
  market <- normString(marketIndex)
  rf <- normString(riskFree)
  
  #Convert data to monthly returns
  equityMonthly <- periodReturn(equity,period=periodicity,subset=periodRange)
  marketMonthly <- periodReturn(market,period=periodicity,subset=periodRange)
  riskFreeMonthly <- periodReturn(rf,period=periodicity,subset=periodRange)
  
  #Calculate excess returns
  equityExcessReturn = equityMonthly$monthly.returns - riskFreeMonthly$monthly.returns
  marketExcessReturn = marketMonthly$monthly.returns - riskFreeMonthly$monthly.returns
  
  #Create Model
  lm(equityExcessReturn ~ marketExcessReturn,na.action = NULL)
}

normString <- function(s) get(sub("^", "", s, fixed=TRUE))

capmModel = capm("DIS")# capm("DIS", 61, end = "2009-09-30", periodicity='monthly')

#Display Model
print(summary(capmModel))