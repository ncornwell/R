#Uncomment this to install the quantmod package for the first time
#install.packages("quantmod")

#Load required libraries
require('quantmod')

periodToUse <- 'monthly'
periodStart <- '2005-01-01'
periodRange <- '2005-01::2011-09'
famaPeriodStart <- 200501

#Url for fama french monthly data and the file name in the zip
famaFrenchZip <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Factors_Monthly.zip"
famaFrenchFile <- "F-F_Benchmark_Factors_Monthly.txt"

temp <- tempfile()
download.file(famaFrenchZip,temp)
famaFrenchData <- read.table(unz(temp, famaFrenchFile),  header = TRUE)
unlink(temp)

famaSubset = famaFrenchData[famaFrenchData$Rm. >= famaPeriodStart, c("Rm.", "Rf", "SMB", "HML")]

getSymbols(c("ADM", "^GSPC"), src="yahoo", from = periodStart)

#Convert data to monthly returns
admMonthly <- periodReturn(ADM,period=periodToUse,subset=periodRange)
marketMonthly <- periodReturn(GSPC,period=periodToUse,subset=periodRange)


admExcessReturn = admMonthly - famaSubset$Rf
marketExcessReturn = marketMonthly - famaSubset$Rf

threeFactorModel = lm(admExcessReturn ~ famaSubset$SMB + famaSubset$HML + marketExcessReturn)

print(summary(threeFactorModel))


