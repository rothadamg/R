## from "11-estimation Part 4.doc"
cheeseX=c(0.86, 1.53, 1.57, 1.81, 0.99, 1.09, 1.29, 1.78, 1.29, 1.58)
mean(X); sd(X)
mu = 1.379;   sig = 0.3277;   n=10

simulateConfidenceInterval = function(X=cheeseX, CImethod=c("normal", "t"), 
                                      simMethod=c("normal","Bootstrap","cauchy"), 
                                      nsims=10000, nreps=2) {
  CImethod = CImethod[1]
  simMethod = simMethod[1]
  mu = mean(X); sig = sd(X)
  n = length(X)
  
  unix.time( {
    simFunction = function(repNumber) {
      x = switch(simMethod,
                 normal=rnorm(n, mu, sig),
                 Bootstrap=sample(X, 10, replace=TRUE),
                 cauchy=rcauchy(n, mu, sig)
      )
      interval = 
        switch(CImethod,
               normal=mean(x)+
                 c(-1,1)*qnorm(0.95)*sd(x)/sqrt(n),
               t     =mean(x)+
                 c(-1,1)*qt(0.95,n-1)*sd(x)/sqrt(n)
        )
      covers = (mu > interval[1]) & (mu < interval[2])
      return(covers)   ## Boolean
    }
    for(rep in 1:nreps) {
      coverResult = sapply(1:nsims, simFunction)
      cat(mean(coverResult), " CImethod=", CImethod, "   simMethod=", simMethod, "\n") 
    }
  })
}

simulateConfidenceInterval()
simulateConfidenceInterval(CImethod="t")
simulateConfidenceInterval(simMethod="Bootstrap", nsims=1e5)
simulateConfidenceInterval(CImethod="t", simMethod="Bootstrap")

simulateConfidenceInterval(CImethod="normal", simMethod="cauchy")
simulateConfidenceInterval(CImethod="t", simMethod="cauchy")


