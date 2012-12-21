

library("quantmod")
library("RSNNS")
library("caret")
library(doMC)
registerDoMC(6)

aapl = read.csv("allDataNormalized.csv")

aapl = aapl[order(aapl$symbol, aapl$quotedate),]

aapl = transform(aapl, todayReturn = c(log(Lag(aapl$close, k=1) / aapl$close)))
aapl = transform(aapl, inYesterdayReturn = c(Lag(aapl$todayReturn, k=1)))
aapl = transform(aapl, inmeanivL1 = c(Lag(aapl$meaniv, k=1)))
aapl = transform(aapl, involumeL1 = c(Lag(aapl$volume, k=1)))
aapl = transform(aapl, outPositiveRet = c(aapl$todayReturn > 0.02))

aaplLearn = aapl[aapl$quotedate >= 20110105 & aapl$quotedate  < 20111121,]


trainIndex <- createDataPartition(aapl$outPositiveRet, p=.7, list=F)
nnmodel.train <- aaplLearn[trainIndex, ]
nnmodel.test <- aaplLearn[-trainIndex, ]

aaplLearn = nnmodel.test

inputs <- aaplLearn[,inputColumns(aaplLearn)]
targets <- aaplLearn[,outputColumns(aaplLearn)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

ptm <- proc.time()
model <- elman(patterns$inputsTrain, patterns$targetsTrain,
               size = c(20, 20, 20), learnFuncParams = c(0.1), maxit = 500,
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest, 
               linOut = F)
print(proc.time() - ptm)

#Test with new data
aaplTest = aapl[aapl$quotedate >= 20111121,]
ceInputs <- aaplTest[,inputColumns(aaplTest)]
predictions = predict(model, ceInputs)


preds = predictions > 0.9

t1 = preds & aaplTest$outPositiveRet

t2 = t1 * aaplTest$todayReturn

print(sum(t1)/sum(preds))
