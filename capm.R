#Uncomment this to install the quantmod package for the first time
#install.packages("quantmod")

#Load required libraries
require('quantmod')

periodToUse <- 'monthly'
periodStart <- '2004-09-01'
periodRange <- '2004-10::2009-09'

#Load Disney, S&P500, and Risk Free Rate data set
#^IRX = 13 week treasury
#^FVX = 5 year treasury
getSymbols(c("DIS", "^GSPC", "^IRX"), src="yahoo", from = periodStart)

#Convert data to monthly returns
disneyMonthly <- periodReturn(DIS,period=periodToUse,subset=periodRange)
marketMonthly <- periodReturn(GSPC,period=periodToUse,subset=periodRange)
riskFreeMonthly <- apply.monthly(IRX[periodRange], last)$IRX.Adjusted / 1200


#Calculate excess returns
disneyExcessReturn = disneyMonthly - riskFreeMonthly
marketExcessReturn = marketMonthly - riskFreeMonthly

#Create Model
capmModel <- lm(disneyExcessReturn ~ marketExcessReturn)

#Display Model
print(summary(capmModel))
