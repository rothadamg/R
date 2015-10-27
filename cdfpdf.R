plot(x<-seq(-4,4,length=100), dnorm(x))
SAMPLE = rnorm(1e6)
plot.ecdf(rnorm(100))
lines(x, pnorm(x),col="red", lwd=3)
qqplot(pnorm(SAMPLE), runif(1e6))
plot(sort(pnorm(SAMPLE)), sort(runif(1e6)))

diagonal(col="black")
