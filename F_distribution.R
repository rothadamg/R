### The F distribution.  See notes on testing variances.

pf(1/3,5,20) + 1 - pf(3,5,20)

df(1/3,5,20) 
df(3,5,20)

### Hunting for the other (lower) point with the same F density:
temp = function(lower) {df(lower,5,20) - df(3,5,20)}
temp(3)  ##  Verifying the upper root.
uniroot(f=temp, lower=0, upper=4)  ###  0.0301423
temp(.Last.value$root) ### Yes, close to zero.

## Upper and lower 5% quantiles.
lowerbound = qf(0.05, 5, 20) / 3
upperbound = qf(0.95, 5, 20) / 3
print(c(lowerbound, upperbound))

#### Why do you need to give uniroot a range?
plot(xvalues, sin(1/xvalues*10), type="l")
abline(h=0)
