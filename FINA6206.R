
require("texreg")

regData = read.csv("~/Downloads/regdata.csv")

regData = transform(regData, NCallOILag1 = c(NCallOI))
#regData = transform(regData, NCallOIChangedLag1 = c(NCallOIChanged[-1], NA))
regData = transform(regData, NPutOILag1 = c(NPutOI[-1], NA))
#regData = transform(regData, NPutOIChangeLag1 = c(NPutOIChange[-1], NA))
regData = transform(regData, NVolLag1 = c(NVol[-1], NA))
#regData = transform(regData, NVolChangeLag1 = c(NVolChange[-1], NA))
regData = transform(regData, PutCallOIRatioLag1 = c(PutCallOIRatio[-1], NA))
regData = transform(regData, NMeanIVLag1 = c(NMeanIV[-1], NA))
regData = transform(regData, MeanIVLag1 = c(MeanIV[-1], NA))
regData = transform(regData, CallIVLag1 = c(CallIV[-1], NA))
regData = transform(regData, PutIVLag1 = c(PutIV[-1], NA))
regData = transform(regData, Excess.ReturnLag1 = c(Excess.Return[-1], NA))

regDataBase = regData[regData$DateDiff >= -141 & regData$DateDiff <= -41 ,]
regData30 = regData[regData$DateDiff >= -30 & regData$DateDiff <= -1,]
regData20 = regData[regData$DateDiff >= -20 & regData$DateDiff <= -1,]
regData10 = regData[regData$DateDiff >= -10 & regData$DateDiff <= -1,]

baseModel <- function(dataToUse) {
  lm(Excess.Return ~ 
       NVolLag1 + PutCallOIRatioLag1 + NCallOILag1 + NPutOILag1
     + NVol + PutCallOIRatio + NCallOI + NPutOI
     , data=dataToUse)
}
baseModelLagOnly <- function(dataToUse) {
  lm(Excess.Return ~ 
       NVolLag1 + PutCallOIRatioLag1 + NCallOILag1 + NPutOILag1
     , data=dataToUse)
}

baseModelIV <- function(dataToUse) {
  lm(Excess.Return ~ 
       NVolLag1 + PutCallOIRatioLag1 + NCallOILag1 + NPutOILag1 + NMeanIVLag1
     + NVol + PutCallOIRatio + NCallOI + NPutOI + NMeanIV
     , data=dataToUse)
}
baseModelIVLagOnly <- function(dataToUse) {
  lm(Excess.Return ~ 
       NVolLag1 + PutCallOIRatioLag1 + NCallOILag1 + NPutOILag1 + NMeanIVLag1
     , data=dataToUse)
}

latexToPng <- function(latexS, fileName) {
  latexFileName = paste(fileName, ".tex", sep='')
  dviFileName = paste(fileName, ".dvi", sep='')
  
  fileConn<-file(latexFileName)
  #\\usepackage[paperwidth=5.5in,paperheight=7in,noheadfoot,margin=0in]{geometry}
  beginLines = '\\documentclass{report}
  \\usepackage{booktabs}
  \\usepackage{dcolumn}
  \\begin{document}\\pagestyle{empty}
  \\begin{table}
  \\begin{center}
  '
  endLines = '
  \\end{center}
  \\label{table:coefficients}
  \\end{table}
  \\end{document}
  '
  writeLines(c(beginLines, latexS, endLines), fileConn)
  close(fileConn)
  
  invisible(system(paste("latex ", latexFileName)))
  invisible(system(paste("dvipng -T tight", "-D", 600, dviFileName)))
  
}

modelBase = baseModel(regDataBase)
modelBaseLag = baseModelLagOnly(regDataBase)
model30 = baseModel(regData30)
model30Lag = baseModelLagOnly(regData30)
model20 = baseModel(regData20)
model20Lag = baseModelLagOnly(regData20)
model10 = baseModel(regData10)
model10Lag = baseModelLagOnly(regData10)


modelBaseIV = baseModelIV(regDataBase)
modelBaseIVLag = baseModelIVLagOnly(regDataBase)
model30IV = baseModelIV(regData30)
model30IVLag = baseModelIVLagOnly(regData30)
model20IV = baseModelIV(regData20)
model20IVLag = baseModelIVLagOnly(regData20)
model10IV = baseModelIV(regData10)
model10IVLag = baseModelIVLagOnly(regData10)

latexBaseModel = texreg(list(modelBase, modelBaseLag, model30,model30Lag, 
                             model20,model20Lag,model10,model10Lag), 
                    model.names=c("[-141,-41]","[-141,-41]",  "[-30,-1]","[-30,-1]", 
                                  "[-20,-1]","[-20,-1]","[-10,-1]","[-10,-1]"), 
                    digits = 5, table=FALSE, booktabs=FALSE, dcolumn=FALSE)

latexToPng(latexBaseModel, "baseModel")

latexBaseModelIV = texreg(list(modelBaseIV, modelBaseIVLag, model30IV,model30IVLag, 
                               model20IV,model20IVLag,model10IV,model10IVLag), 
                    model.names=c("[-141,-41]","[-141,-41]",  "[-30,-1]","[-30,-1]", 
                                  "[-20,-1]","[-20,-1]", "[-10,-1]","[-10,-1]"), 
                    digits = 5, table=FALSE, booktabs=FALSE, dcolumn=FALSE)

latexToPng(latexBaseModelIV, "baseModelIV")
