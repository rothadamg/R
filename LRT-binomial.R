n1 = 30
n2 = 40
p1 = 0.4
p2 = 0.7

N=1000

pvalue = sapply(1:N, function(ignoreMe) {
  X1 = rbinom(1, n1, p1)
  X2 = rbinom(1, n2, p2)
  
  p1hat = X1/n1
  p2hat = X2/n2
  phat = (X1+X2)/(n1+n2)
  
  numerator = dbinom(X1, n1, p1hat) * dbinom(X2, n2, p2hat)
  denominator = dbinom(X1, n1, phat) * dbinom(X2, n2, phat)
  
  W = numerator/denominator
  
  LLR = 2*log(W)
  
  1 - pchisq(W, df=1)
})
