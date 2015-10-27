xvalues<-seq(0,2,length=1000)
plot(xvalues, dchisq(xvalues, 1), type="l", lwd=2)
dfValues = c(1, 2, 4, 6, 8, 10, 20, 40, 100, 1000)
require("RColorBrewer")
# display.brewer.all()

dfColors = brewer.pal(length(dfValues), "RdYlGn")
names(dfColors) = dfValues
for(df in dfValues)
	lines(xvalues, dchisq(xvalues*df, df)*df, 
		col=dfColors[as.character(df)])
legend(1.5, 8,
	as.character(dfValues),
		text.col=dfColors,
		col=dfColors,
		lty=1,
		lwd=c(2, rep(1,length(dfColors)-1)))
title("dchisq(xvalues*df, df)*df")


####
xvalues<-seq(0,5,length=1000)

plot(xvalues, 1-pchisq(xvalues, 1), type="l", lwd=2)
dfValues = c(1, 2, 4, 6, 8, 10, 20, 40, 100, 1000)
require("RColorBrewer")
# display.brewer.all()

dfColors = brewer.pal(length(dfValues), "RdYlGn")
names(dfColors) = dfValues
for(df in dfValues)
  lines(xvalues, 1-pchisq(xvalues*df, df)*df, 
        col=dfColors[as.character(df)])
points(3.84, 0.05, cex=3)
legend(1.5, 8,
       as.character(dfValues),
       text.col=dfColors,
       col=dfColors,
       lty=1,
       lwd=c(2, rep(1,length(dfColors)-1)))
title("dchisq(xvalues*df, df)*df")