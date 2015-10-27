x = rnbinom(10000, 1, .5)
summary(x)
range(x)
hist(x)

xtable = table(x)

plot(dnbinom(as.numeric(names(xtable)), 1, .5),
	xtable/10000)
abline(a=0, b=1)


plot(pnbinom(as.numeric(names(xtable)), 1, .5),
	cumsum(xtable/10000))

