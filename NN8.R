

library("quantmod")
library("RSNNS")
library("caret")
#library(doMC)
#registerDoMC(2)

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
                                 inyesterDayMeanReturn = c(Lag(tData$close, k=1)),
                                 instockVolumeL1 = c(Lag(tData$volume, k=1)),
                                 inmeanivL1 = c(Lag(tData$meaniv, k=1)),
                                 incallL1 = c(Lag(tData$callvol, k=1)),
                                 inmputL1 = c(Lag(tData$putvol, k=1)),
                                 outPositiveRet = (todayRet > 0.03) * 1
)

learnData = optionDataNormalized[optionDataNormalized$quotedate >= optionDataNormalized[meanDuration+1,]$quotedate & optionDataNormalized$quotedate <= 20110721,]

inputs <- learnData[,inputColumns(learnData)]
targets <- learnData[,outputColumns(learnData)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

sysTime = Sys.time()
model.grid <- expand.grid(.layer1=c(20), .layer2=c(20), .layer3=c(20))
model <- train(patterns$inputsTrain, patterns$targetsTrain,
                     method = "neuralnet")  

print(Sys.time() - sysTime)

testData = optionDataNormalized[optionDataNormalized$quotedate > 20111121,]
ceInputs <- testData[,inputColumns(testData)]
predictions = predict(model, ceInputs)


preds = predictions > 0.5

actualReturn = preds * testData$todayReturn
trueTrues = preds & testData$outPositiveRet

print(sum(trueTrues)/sum(preds))

print(mean(actualReturn))
print(summary(actualReturn))
