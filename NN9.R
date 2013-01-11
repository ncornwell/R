
library("RSNNS")

optionDataNormalized = read.csv("allData20mean.csv")

learnData = optionDataNormalized[optionDataNormalized$quotedate <= 20111212,]


inputs <- learnData[,inputColumns(learnData)]
targets <- learnData[,outputColumns(learnData)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

print(Sys.time())
sysTime = Sys.time()
model <- mlp(patterns$inputsTrain, patterns$targetsTrain,
             size = c(20, 20), learnFuncParams = c(0.1), maxit = 200,
             inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest)
print(Sys.time() - sysTime)

testData = optionDataNormalized[optionDataNormalized$quotedate > 20111212,]
ceInputs <- testData[,inputColumns(testData)]
predictions = predict(model, ceInputs)


preds = predictions > 0.6

actualReturn = preds * testData$todayReturn
trueTrues = preds & testData$outPositiveRet

print(sum(trueTrues)/sum(preds))

print(mean(actualReturn))
print(summary(actualReturn))
