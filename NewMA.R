#get MAImp code from GIST
#thanks to http://systematicinvestor.wordpress.com
#for showing me how to do this
#con=url("https://raw.github.com/gist/1405187/92e7c24ff2459a0830c45b828f1dba41143e9436/MAImp.r")
#source(con)

require(quantmod)
require(PerformanceAnalytics)

getSymbols("^GSPC",from = "1896-01-01", to = Sys.Date())

#test 20 day simple crossover 50 day simple
signals <- MAImp(GSPC, k1=20, k2=50)
#another example would be a simple 100 day moving average price cross
#signals <- MAImp(GSPC, k1=100)

#use lag 1 to eliminate foresight
#signal is 1 (in) and 0 (out)
ret <- merge(rep(ROC(GSPC[,4],type="discrete",n=1),3) * lag(signals,k=1),ROC(GSPC[,4],type="discrete",n=1))
colnames(ret) <- c(colnames(signals),"BuyHold")

jpeg(filename="performance summary.jpg",
  quality=100,width=6.25, height = 8,  units="in",res=96)
charts.PerformanceSummary(ret,ylog=TRUE,
	main = "Test of Improved Moving Average",
	colorset=c("cadetblue","darkolivegreen3","gray70","bisque3"))
#dev.off()
#table.AnnualizedReturns(ret)

require(ggplot2)
#ggplot downside statistics
downside <- as.data.frame(table.DownsideRisk(ret))
#get rid of drawdown since it messes up the chart
#and is already shown on Performance Summary
downside <- downside[c(1:6,8:11),]
downsideMelt<-melt(cbind(rownames(downside),downside),id.vars=1)
colnames(downsideMelt)<-c("Statistic","System","Value")
jpeg(filename="ggplot downside.jpg",
	quality=100,width=6.25, height = 8,  units="in",res=96)
ggplot(downsideMelt, stat="identity", aes(x=Statistic,y=Value,fill=System)) + 
	geom_bar(position="dodge") + 
	coord_flip() +
	scale_fill_manual(values=c("cadetblue","darkolivegreen3","gray70","bisque3","purple")) +
	theme_bw()
#dev.off()

#ggplot CAPM statistics
capm <- as.data.frame(table.CAPM(ret[,1:3],ret[,4]))
capmTable<-melt(cbind(rownames(capm),capm),id.vars=1)
colnames(capmTable)<-c("Statistic","System","Return")
jpeg(filename="ggplot capm.jpg",
	quality=100,width=6.25, height = 8,  units="in",res=96)
ggplot(capmTable, stat="identity", aes(x=Statistic,y=Return,fill=System)) + 
	geom_bar(position="dodge") + 
	coord_flip() +
	scale_fill_manual(values=c("cadetblue","darkolivegreen3","gray70","bisque3","purple")) +
	theme_bw()
#dev.off()


#test 10 month Mebane Faber style moving average for another example
GSPC.monthly <- to.monthly(GSPC)[,4]
#convert date index to yyyy-mm-dd
index(GSPC.monthly) <- as.Date(index(GSPC.monthly))
signals.monthly <- MAImp(GSPC.monthly, k1=10)

#use lag 1 to eliminate foresight
#signal is 1 (in) and 0 (out)
ret.monthly <- merge(rep(ROC(GSPC.monthly,type="discrete",n=1),3) * lag(signals.monthly,k=1),ROC(GSPC.monthly,type="discrete",n=1))
colnames(ret.monthly) <- c(colnames(signals.monthly),"BuyHold")
jpeg(filename="performance monthly.jpg",
	quality=100,width=6.25, height = 8,  units="in",res=96)
charts.PerformanceSummary(ret.monthly,ylog=TRUE,
	main = "Test of Improved Moving Average on Monthly",
	colorset=c("cadetblue","darkolivegreen3","gray70","bisque3"))
#dev.off()