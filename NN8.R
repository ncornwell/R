

meanDuration = 20

library("quantmod")
library("RSNNS")
library("caret")
library(doMC)
registerDoMC(4)


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

#learnData = learnData[learnData$symbol == "AAPL",]

inputs <- learnData[,inputColumns(learnData)]
targets <- learnData[,outputColumns(learnData)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.5)

rm(list=setdiff(ls(), "patterns"))

sysTime = Sys.time()
model <- train(patterns$inputsTrain, patterns$targetsTrain,
                     method = "neuralnet")
print(Sys.time() - sysTime)

sysTime = Sys.time()
predictions = predict(model, patterns$inputsTest)
print(Sys.time() - sysTime)

preds = predictions > 0.5

trueTrues = preds & patterns$targetsTest

print(sum(trueTrues)/sum(preds))
