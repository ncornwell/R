#Uncomment this to install the quantmod package for the first time
#install.packages("quantmod")

#Load required libraries
require('quantmod')

periodToUse <- 'monthly'
periodStart <- '1970-02-01'
periodRange <- '1970-02::2009-12'
famaPeriodStart <- 197002

#Url for fama french monthly data and the file name in the zip
famaFrenchZip <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Factors_Monthly.zip"
famaFrenchFile <- "F-F_Benchmark_Factors_Monthly.txt"

temp <- tempfile()
download.file(famaFrenchZip,temp)
famaFrenchData <- read.table(unz(temp, famaFrenchFile),  header = TRUE)
unlink(temp)

famaSubset = famaFrenchData[famaFrenchData$Rm. >= famaPeriodStart, c("Rm.", "Rf", "SMB", "HML")]
famaSubset = famaSubset[famaSubset$Rm. <= 200912, c("Rm.", "Rf", "SMB", "HML")]

getSymbols(c("JNJ", "^IRX"), src="yahoo", from = periodStart)

#Convert data to monthly returns
equityMonthly <- periodReturn(JNJ$JNJ.Adjusted,period=periodToUse,subset=periodRange) * 100
riskFreeMonthly <- apply.monthly(IRX[periodRange], last)$IRX.Adjusted / 12

equityExcessReturn = equityMonthly - riskFreeMonthly

capmModel = lm(equityExcessReturn ~ famaSubset$Rf)
threeFactorModel = lm(equityExcessReturn ~ famaSubset$Rf + famaSubset$SMB + famaSubset$HML)

print(summary(capmModel))
print(summary(threeFactorModel))


