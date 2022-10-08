#Uncomment this to install the quantmod package for the first time
#did you uncomment??
#install.packages("quantmod")

#Load required libraries
require('quantmod')

periods <- 'monthly'
pStart <- '2004-09-01'
pRange <- '2004-10::2009-09'

pRange <- '2010-11::2011-11'

#Load Disney, S&P500, and Risk Free Rate data set
#^IRX = 13 week treasury
#^FVX = 5 year treasury
getSymbols(c("DIS", "^GSPC", "^IRX"), src="yahoo", from = pStart)

#Convert data to monthly returns
disneyMonthly <- periodReturn(DIS$DIS.Adjusted,period=periods,subset=pRange)
marketMonthly <- periodReturn(GSPC$GSPC.Adjusted,period=periods,subset=pRange)

#^FVX is an index representing the current interest rate for a given day, so divide 
#the rate into a monthly return and then divide by 100 to get the monthly return
riskFreeMonthly <- apply.monthly(IRX[pRange], last)$IRX.Adjusted / 1200
#riskFreeMonthly <- apply.weekly(IRX[pRange], last)$IRX.Adjusted / 5200


#Calculate excess returns
disneyExcessReturn = disneyMonthly - riskFreeMonthly
marketExcessReturn = marketMonthly - riskFreeMonthly

#Create Model
capmModel <- lm(disneyExcessReturn ~ marketExcessReturn)

#Display Model
print(summary(capmModel))
