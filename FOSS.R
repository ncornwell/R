require(quantstrat)
require(PerformanceAnalytics)

# Set initial values
initDate <- "2002-07-31"
endDate <- "2009-10-31"
initEq <- 100000

# Pull Yahoo Finance data
symbols <- c("IEF", "SPY")
getSymbols(symbols, from=initDate, to=endDate, index.class=c("POSIXt","POSIXct"))

# adjust for splits/dividends (comment to replicate blotter example)
#IEF <- adjustOHLC(IEF, use.Adjusted=TRUE)
#SPY <- adjustOHLC(SPY, use.Adjusted=TRUE)

# convert to monthly
IEF <- to.monthly(IEF, indexAt="endof")
SPY <- to.monthly(SPY, indexAt="endof")

# Set up instruments with FinancialInstruments package
currency("USD")
for(symbol in symbols) {
  stock(symbol, currency="USD", multiplier=1)
}

# Delete portfolio, account, and order book if they already exist
suppressWarnings(rm("account.faber","portfolio.faber",pos=.blotter))
suppressWarnings(rm("order_book.faber",pos=.strategy))

# Initialize portfolio and account
initPortf("faber", symbols=symbols, initDate=initDate)
initAcct("faber", portfolios="faber", initDate=initDate, initEq=initEq)
initOrders(portfolio="faber", initDate=initDate)

# Initialize a strategy object
stratFaber <- strategy("faber")

# Add the 10-month SMA indicator
stratFaber <- add.indicator(strategy=stratFaber, name="SMA",
  arguments=list(x=quote(Cl(mktdata)), n=10), label="SMA10")

# There are two signals:
# The first is when monthly price crosses over the 10-month SMA
stratFaber <- add.signal(stratFaber, name="sigComparison",
  arguments=list(columns=c("Close","SMA10"),relationship="gte"), label="Cl.gt.SMA")
# The second is when the monthly price crosses under the 10-month SMA
stratFaber <- add.signal(stratFaber, name="sigComparison",
  arguments=list(columns=c("Close","SMA10"),relationship="lt"), label="Cl.lt.SMA")

# There are two rules:
# The first is to buy when the price crosses above the SMA
stratFaber <- add.rule(stratFaber, name="ruleSignal",
  arguments=list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=1000, ordertype="market",
  orderside="long", pricemethod="market", TxnFees=-5, osFUN=osMaxPos), type="enter", path.dep=TRUE)
# The second is to sell when the price crosses below the SMA
stratFaber <- add.rule(stratFaber, name="ruleSignal",
  arguments=list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty="all", ordertype="market",
  orderside="long", pricemethod="market", TxnFees=-5), type="exit", path.dep=TRUE)

# Set position limits so we don't add to the position every month Close > SMA10
addPosLimit("faber", "SPY", timestamp=initDate, maxpos=1000, minpos=0)
addPosLimit("faber", "IEF", timestamp=initDate, maxpos=1000, minpos=0)

# Process the indicators and generate trades
out <- try(applyStrategy(strategy=stratFaber, portfolios="faber"))
updatePortf("faber")

# Evaluate results
portRet <- PortfReturns("faber")
portRet$Total <- rowSums(portRet, na.rm=TRUE)
charts.PerformanceSummary(portRet$Total)
tradeStats("faber")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
