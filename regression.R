####    regression.R     ####################

require(mvtnorm)

K = 10 #  number of covariates
N = 10 #  Sample size
X = data.frame(rmvnorm(n=N, mean=rep(0,K)))   #  design matrix 
###  For now, the predictors are independent.
saveOpts = options(digits=3); X; options(saveOpts)

###  First generative model:  Y depends on X1 alone, linearly.  
beta0 = 5;  beta1 = 2
Y = beta0 + beta1 * X$X1 + rnorm(N)
simData = data.frame(Y, X)
plot(simData)
plot(X$X1, Y)
abline(beta0, beta1)  ### The "truth"

lm.out = lm(Y ~ X1, data=simData)
lm.out;   str(lm.out)
cbind(lm.out$coef, c(beta0, beta1)); coef(lm.out);  coefficients(lm.out)
abline(coef(lm.out), lty=2, col="green",  lwd=4)  ### The "fit"

with(lm.out, {
	for(i in 1:N)
		lines(rep(X$X1[i], 2), 
			c(fitted.values[i],   fitted.values[i] + residuals[i]),  col="blue")
	}
)
par(mfrow=c(2,2)); plot(lm.out, ask=FALSE)
par(mfrow=c(1,1));  plot(X$X1, resid(lm.out));  abline(h=0, lwd=2, lty=2, col="orange")
influence.measures(lm.out)

Q = sum(lm.out$residuals^2)   ## sum of squared residuals 

sigmaSqHat.biased =  Q/N
sigmaSqHat.unbiased =  Q/(N-2)
cat(
	"sigmaSqHat.biased = ",  sigmaSqHat.biased,
	"\nsigmaSqHat.unbiased = ", sigmaSqHat.unbiased,
	"\n"
)
abline(h=c(-1,1)*sqrt(sigmaSqHat.unbiased), col="blue")

######   Calculating lm.out$coef ourselves.
X = as.matrix(cbind(X0=rep(1,N), X))
X01 = X[ , 1:2]
myCoef = print(solve(t(X01) %*% X01) %*% t(X01) %*% Y)
lm.out$coef

##########    Now, "overspecified" model:  too many parameters!

lm.out.all = lm(data=simData, 
	Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
plot(X$X1, resid(lm.out.all));
  abline(h=0, lwd=2, lty=2, col="orange")
resid(lm.out.all)
lm.out.all
sum(lm.out.all$residuals^2)   ## sum of squared residuals 

######  Investigating intercepts
X = as.matrix(X) 
lm(Y ~ X);	
lm(Y ~ X[ , -1]); 
lm(Y ~ -1 + X)

plot(lm(Y ~ -1 + X[ , -c(1, 3:11)]))

######   Multiple regression

beta = c(beta0, beta1, 
		beta1, beta1, rep(0, ncol(X)-3))

Y = beta0 + as.matrix(X) %*% beta[-1] + rnorm(N)
YX = data.frame(Y,X)
lm.out = lm(data=YX, Y ~ .)
resid(lm.out)

step(lm(data=YX, Y ~ 1), scope=Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9)
lm(data=YX, Y ~ . - X10)


for(k in (1:10)) {
	cat( "=======", k, "=======\n")
	theFormula = as.formula("Y ~  " %&%
			paste("X", 1:k, sep="", collapse=" + "))
	print(theFormula)
	lm.out = lm(data=YX, theFormula)
#	print(lm.out$coef)
	print(summary(lm.out))
}

for(k in 1:10) {
	lm.out = lm(Y ~ X[ , 1:k])
	Q = sum(lm.out$residuals^2)   ## sum of squares 
	sigmaHat.biased =  Q/N
	sigmaHat.unbiased =  Q/(N - (k+1))
	cat("k = ", k,		"   sigmaHat.biased = ",  sigmaHat.biased,
							"   sigmaHat.unbiased = ", sigmaHat.unbiased,   "\n"	)
}

