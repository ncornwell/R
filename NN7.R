

library("quantmod")
library("RSNNS")

optionData = read.csv("allData.csv", header=F, 
                      col.names=c("symbol", "quotedate", "meaniv", "calliv", "putiv", "callvol", 
                                  "putvol", "incalloi", "inputoi", "close", "volume"))
optionData = optionData[order(optionData$symbol, optionData$quotedate),]

columNames = c("meaniv", "calliv", "putiv", "callvol", "putvol", "incalloi", "inputoi", "close", "volume")
meanDuration = 20
rowCount = nrow(optionData)
subData = as.matrix(optionData[,columNames])
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

tData = as.data.frame(newData)
colnames(tData) <-columNames
todayRet = c(log(Lag(optionData$close, k=1) / optionData$close))
optionDataNormalized = transform(tData, 
                                 symbol = optionData$symbol, 
                                 quotedate = optionData$quotedate,
                                 todayReturn = todayRet,
                                 #inyesterdayReturn = c(Lag(todayRet, k=1)),
                                 #inyesterDayMeanReturn = c(Lag(tData$close, k=1)),
                                 instockVolumeL1 = c(Lag(tData$volume, k=1)),
                                 inmeanivL1 = c(Lag(tData$meaniv, k=1)),
                                 incallvolL1 = c(Lag(tData$callvol, k=1)),
                                 inmputvolL1 = c(Lag(tData$putvol, k=1)),
                                 outPositiveRet = todayRet > 0.05
                                 )

learnData = optionDataNormalized[optionDataNormalized$quotedate >= optionDataNormalized[meanDuration+1,]$quotedate & 
                                   optionDataNormalized$quotedate <= 20110721 &
                                   optionDataNormalized$meaniv < 10 &
                                   optionDataNormalized$callvol < 10 &
                                   optionDataNormalized$volume < 10 &
                                   optionDataNormalized$incalloi < 10 &
                                   optionDataNormalized$todayReturn < 10
                                   ,]

inputs <- learnData[,inputColumns(learnData)]
targets <- learnData[,outputColumns(learnData)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

print(Sys.time())
sysTime = Sys.time()
model <- mlp(patterns$inputsTrain, patterns$targetsTrain,
               size = c(20, 20), learnFuncParams = c(0.1, 0.0), maxit = 200,
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest)
print(Sys.time() - sysTime)

testData = optionDataNormalized[optionDataNormalized$quotedate > 20110721,]
ceInputs <- testData[,inputColumns(testData)]
predictions = predict(model, ceInputs)


preds = predictions > 0.5

actualReturn = preds * testData$todayReturn
trueTrues = preds & testData$outPositiveRet

print(sum(trueTrues)/sum(preds))

print(mean(actualReturn))
print(summary(actualReturn))
