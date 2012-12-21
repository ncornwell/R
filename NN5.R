

library("quantmod")
library("RSNNS")
library("caret")
library(doMC)
registerDoMC(2)

aapl = read.csv("allDataNormalized.csv")

aapl = aapl[order(aapl$symbol, aapl$quotedate),]

aapl = transform(aapl, todayReturn = c(log(Lag(aapl$close, k=1) / aapl$close)))
aapl = transform(aapl, inYesterdayReturn = c(Lag(aapl$todayReturn, k=1)))
aapl = transform(aapl, inmeanivL1 = c(Lag(aapl$meaniv, k=1)))
aapl = transform(aapl, involumeL1 = c(Lag(aapl$volume, k=1)))
aapl = transform(aapl, outPositiveRet = c(aapl$todayReturn > 0.02))


aaplLearn = aapl[aapl$quotedate >= 20110105 & aapl$quotedate  < 20111121,]
trainIndex <- createDataPartition(aaplLearn$outPositiveRet, p=.7, list=F)
nnmodel.train <- aaplLearn[trainIndex, ]
nnmodel.test <- aaplLearn[-trainIndex, ]


nnmodel.grid <- expand.grid(.layer1=c(20), .layer2=c(20), .layer3=c(20))

ptm <- proc.time()
nnmodel.fit <- train(todayReturn ~ incalloi + inputoi + inmeanivL1 + involumeL1 + inYesterdayReturn, 
                     data = nnmodel.train, method = "neuralnet", tuneGrid = nnmodel.grid,
                     rep=5)  

print(proc.time() - ptm)

#Test with new data
nnmodel.predict <- predict(nnmodel.fit, newdata = nnmodel.test)
nnmodel.rmse <- sqrt(mean((nnmodel.predict - nnmodel.test$PositiveReturn)^2)) 

print(nnmodel.rmse)

t1 = nnmodel.test$PositiveReturn > 0
t2 = nnmodel.predict > 0.5
t3 = summary(t1 == t2)
print(t3)


preds = nnmodel.predict > 0.75

t1 = preds & (nnmodel.test$PositiveReturn > 0)

t2 = t1 * aaplTest$todayReturn

print(sum(t1)/sum(preds))

print(mean(t2))



