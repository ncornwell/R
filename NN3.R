

library("RSNNS")


regData = read.csv("~/Downloads/regdata.csv")

regData = transform(regData, inNCallOILag1 = c(NCallOI))
regData = transform(regData, inNCallOILag2 = c(inNCallOILag1[-1], NA))
regData = transform(regData, inNCallOILag3 = c(inNCallOILag2[-1], NA))
regData = transform(regData, inNPutOILag1 = c(NPutOI[-1], NA))
regData = transform(regData, inNVolLag1 = c(NVol[-1], NA))
regData = transform(regData, inPutCallOIRatioLag1 = c(PutCallOIRatio[-1], NA))
regData = transform(regData, inNMeanIVLag1 = c(NMeanIV[-1], NA))
regData = transform(regData, inNMeanIVLag2 = c(inNMeanIVLag1[-1], NA))
regData = transform(regData, outPositiveReturn = c((Return > 0.02)*1))


nnBase = regData[regData$DateDiff >= -139 & regData$DateDiff <= -31 ,]
nnCE = regData[regData$DateDiff >= -30 & regData$DateDiff <= -10 ,]

inputs <- nnBase[,inputColumns(nnBase)]
targets <- nnBase[,outputColumns(nnBase)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

model <- elman(patterns$inputsTrain, patterns$targetsTrain, 
               size = c(20, 20, 20), learnFuncParams = c(0.1), maxit = 500, 
               inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest, 
               linOut = F)


plot(targets[1:100], type = "l") 
lines(model$fitted.values[1:100], col = "green")


#Test with new data
ceInputs <- nnCE[,inputColumns(nnCE)]
predictions = predict(model, ceInputs)

hist(predictions - nnCE$outPositiveReturn)
plot(nnCE$outPositiveReturn[1:100], type = "l") 
lines(predictions[1:100], col = "green")

preds = predictions > 0.75

t1 = preds & nnCE$outPositiveReturn

print(sum(t1)/sum(preds))

