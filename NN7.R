
meanDuration = 30

library("quantmod")
library("RSNNS")

optionData = read.csv("allData.csv", header=F, 
                      col.names=c("symbol", "quotedate", "meaniv", "calliv", "putiv", "callvol", 
                                  "putvol", "incalloi", "inputoi", "close", "volume"))
optionData = optionData[order(optionData$symbol, optionData$quotedate),]

columNames = c("meaniv", "calliv", "putiv", "callvol", "putvol", "incalloi", "inputoi", "close", "volume")

rowCount = nrow(optionData)
subData = as.matrix(optionData[,columNames])
newData = matrix(NA, nrow(subData), ncol(subData))
sysTime = Sys.time()
for (rowNum in meanDuration:rowCount){
  firstRow = rowNum - meanDuration
  lastRow = rowNum - 1
  curRow = subData[rowNum,]
  meanCols = colMeans(subData[firstRow:lastRow,])
  meanCols[meanCols == 0] <- 0.01
  newData[rowNum,] = curRow / meanCols
}
print(Sys.time() - sysTime)
newData[is.na(newData)] <- 0

tData = as.data.frame(newData)
colnames(tData) <-columNames
todayRet = c(log(optionData$close / Lag(optionData$close, k=1)))
optionDataNormalized = transform(tData, 
                                 symbol = optionData$symbol, 
                                 quotedate = optionData$quotedate,
                                 todayReturn = todayRet,
                                 inyesterdayReturn = c(Lag(todayRet, k=1)),
                                 inyesterDayMeanReturn = c(Lag(tData$close, k=1)),
                                 instockVolumeL1 = c(Lag(tData$volume, k=1)),
                                 inmeanivL1 = c(Lag(tData$meaniv, k=1)),
                                 incallL1 = c(Lag(tData$callvol, k=1)),
                                 inmputL1 = c(Lag(tData$putvol, k=1)),
                                 outPositiveRet = (todayRet > 0.02) * 1
)

learnData = optionDataNormalized[optionDataNormalized$quotedate >= optionDataNormalized[meanDuration+1,]$quotedate,]

inputs <- learnData[,inputColumns(learnData[learnData$quotedate <= 20111000,])]
targets <- learnData[,outputColumns(learnData[learnData$quotedate <= 20111000,])]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.35)

rm(list=setdiff(ls(), c("patterns", "learnData")))

print(Sys.time())
sysTime = Sys.time()
model <- mlp(patterns$inputsTrain, patterns$targetsTrain,
               size = c(20, 10), learnFuncParams = c(0.1), maxit = 50,
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest)
print(Sys.time() - sysTime)

testData = learnData[learnData$quotedate > 20111000,]

sysTime = Sys.time()
predictions = predict(model,  testData[,inputColumns(testData)])
print(Sys.time() - sysTime)

preds = predictions > 0.8

trueTrues = preds & testData$outPositiveRet

print(sum(trueTrues)/sum(preds))

actualReturn = testData$todayReturn * preds

print(summary(actualReturn))
print(mean(actualReturn))
