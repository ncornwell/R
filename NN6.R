

library(doMC)
registerDoMC(6)

optionData = read.csv("allData.csv", header=F, 
                          col.names=c("symbol", "quotedate", "meaniv", "calliv", "putiv", "callvol", 
                                     "putvol", "calloi", "putoi", "close", "volume"))
optionData = optionData[order(optionData$symbol, optionData$quotedate),]

colNames = c("meaniv", "calliv", "putiv", "callvol", "putvol", "calloi", "putoi", "close", "volume")
meanDuration = 20
rowCount = nrow(optionData)
subData = as.matrix(optionData[,colNames])
newData = matrix(NA, nrow(subData), ncol(subData))
sysTime = Sys.time()
for (rowNum in meanDuration:rowCount){
  firstRow = rowNum - meanDuration
  lastRow = rowNum - 1
  curRow = subData[rowNum,]
  meanCols = colMeans(subData[firstRow:lastRow,])
  meanCols[meanCols == 0] <- 1
  newData[rowNum,] = curRow / meanCols
}
newData[is.na(newData)] <- 0
print(Sys.time() - sysTime)

optionDataNormalized = transform(newData, symbol = optionData$symbol, quotedate = optionData$quotedate)

write.csv(optionDataNormalized, file=paste("allData", meanDuration, "dayMA.csv", sep=""), row.names=F, quote=F)

