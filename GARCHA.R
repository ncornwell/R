library(quantmod)
library(rugarch)

getSymbols("SPY", from = "2000-01-01")
spyRets = na.trim( ROC( Cl( SPY ) ) )

# Train over 2000-2004, forecast 2005
ss = spyRets["2000/2005"]
outOfSample = NROW(ss["2005"])

spec = ugarchspec(
            variance.model=list(garchOrder=c(1,1)),
            mean.model=list(armaOrder=c(4,5), include.mean=T),
            distribution.model="sged")
fit = ugarchfit(spec=spec, data=ss, out.sample=outOfSample)
fore = ugarchforecast(fit, n.ahead=1, n.roll=outOfSample)

# Build some sort of indicator base on the forecasts
ind = xts(head(as.array(fore)[,2,],-1), order.by=index(ss["2005"]))
ind = ifelse(ind < 0, -1, 1)

# Compute the performance
mm = merge( ss["2005"], ind, all=F )
tail(cumprod(mm[,1]*mm[,2]+1))

# Output (last line): 2005-12-30  1.118082