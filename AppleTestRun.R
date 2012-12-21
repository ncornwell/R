

library("quantmod")
library("RSNNS")
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

model <- elman(patterns$inputsTrain, patterns$targetsTrain,
               size = c(20, 20, 20), learnFuncParams = c(0.1), maxit = 500,
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest, 
               linOut = F)


#net = neuralnet(outPositiveRet ~ incalloi + 
#                  inputoi + 
#                  inmeanivL1 +
#                  involumeL1 +
#                  inYesterdayReturn,
#                aaplLearn, hidden=10, rep=2)


plotIterativeError(model)
plotRegressionError(patterns$targetsTrain, model$fitted.values, main="Regression Plot Fit")
plotRegressionError(patterns$targetsTest, model$fittedTestValues, main="Regression Plot Test")
hist(model$fitted.values - patterns$targetsTrain, col="lightblue", main="Error Histogram Fit")

plot(targets, type = "l") 
lines(model$fitted.values, col = "green")



#Test with new data
aaplTest = aapl[aapl$quotedate >= 20111121,]
ceInputs <- aaplTest[,inputColumns(aaplTest)]
predictions = predict(model, ceInputs)

hist(predictions - aaplTest$outPositiveRet)
plot(aaplTest$outPositiveRet[1:200], type = "l") 
lines(predictions[1:200], col = "green")

preds = predictions > 0.5

t1 = preds & aaplTest$outPositiveRet

t2 = t1 * aaplTest$todayReturn

print(sum(t1)/sum(preds))
summary(preds)

aaplTest = transform(aaplTest, actualReturn = t1 * aaplTest$todayReturn)

aaplTest = aaplTest[aaplTest$actualReturn > 0  ,]

