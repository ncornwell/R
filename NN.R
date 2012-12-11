
require("neuralnet")


regData = read.csv("~/Downloads/regdata.csv")

regData = transform(regData, NCallOILag1 = c(NCallOI[-1], NA))
regData = transform(regData, NCallOILag2 = c(NCallOILag1[-1], NA))
regData = transform(regData, NCallOILag3 = c(NCallOILag2[-1], NA))
#regData = transform(regData, NCallOIChangedLag1 = c(NCallOIChanged[-1], NA))
regData = transform(regData, NPutOILag1 = c(NPutOI[-1], NA))
#regData = transform(regData, NPutOIChangeLag1 = c(NPutOIChange[-1], NA))
regData = transform(regData, NVolLag1 = c(NVol[-1], NA))
#regData = transform(regData, NVolChangeLag1 = c(NVolChange[-1], NA))
regData = transform(regData, PutCallOIRatioLag1 = c(PutCallOIRatio[-1], NA))
regData = transform(regData, NMeanIVLag1 = c(NMeanIV[-1], NA))
regData = transform(regData, NMeanIVLag2 = c(NMeanIVLag1[-1], NA))
regData = transform(regData, MeanIVLag1 = c(MeanIV[-1], NA))
regData = transform(regData, CallIVLag1 = c(CallIV[-1], NA))
regData = transform(regData, PutIVLag1 = c(PutIV[-1], NA))
regData = transform(regData, Excess.ReturnLag1 = c(Excess.Return[-1], NA))
regData = transform(regData, PositiveReturn = c(Excess.Return > 0.01))

regDataBase = regData[regData$DateDiff >= -141 & regData$DateDiff <= -41 ,]
regData30 = regData[regData$DateDiff >= -30 & regData$DateDiff <= -1,]
regData20 = regData[regData$DateDiff >= -20 & regData$DateDiff <= -1,]
regData10 = regData[regData$DateDiff >= -10 & regData$DateDiff <= -1,]

nnBase = regData[regData$DateDiff >= -139 & regData$DateDiff <= -61 ,]
nnTest = regData[regData$DateDiff >= -60 & regData$DateDiff <= -41 ,]

computeData = data.frame(nnTest$NMeanIVLag1, 
                         nnTest$NCallOILag1, 
                         nnTest$NCallOILag2,
                         nnTest$NCallOILag3
                         )

net = neuralnet(Return ~ NMeanIVLag1 + 
                  NCallOILag1 + 
                  NCallOILag2 +
                  NCallOILag3,
                nnBase, hidden=10, rep=1)

res = compute(net, computeData)

print(summary(res$net.result))

tRes = res$net.result > 0
tActuall = nnTest$Return > 0
print(sum(tRes == tActuall) / 820)




