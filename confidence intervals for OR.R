N = 1000
P = rep(1/4, 4)
P = c(0.1, 0.3, 0.4, 0.2)
Nreps = 1000
tables = rmultinom(n=Nreps, size=N, prob=P)
str(tables)
OR = apply(tables, 2, function(x) x[1]*x[4]/x[2]/x[3])
var(log(OR))   # 0.01569
sum(1/(P*N))   # 0.016

var(OR)   ## 0.01574
(P[1]*P[4]/P[2]/P[3])^2 * sum(1/(P*N))   # 0.016  Delta method again.

log(3.809524) + c(-1,1)* 1.96 * sqrt(0.2703869)  # 0.3183289 2.3566796
exp(..())   ### 1.374828 10.555843

fisher.test(matrix(c(10,12,14,64), nrow=2))  ## 1.197927 11.801156 