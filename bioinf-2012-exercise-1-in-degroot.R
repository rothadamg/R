##  bioinf-2012-exercise-1-in-degroot

### problem  1.7 #7
exp(lgamma(21)-lgamma(9)-12*log(20))
[1] 0.0147314



### problem 1.9 #4
factorial(3)^2 * factorial(2)/factorial(10)
[1] 1.984127e-05


###  1.10 #4
prAnyMatches(3)    #  [1] 0.6666667
## 1.10 #5
1 - prAnyMatches(4)  # 0.375   or 9/24


# 8:   answer is zero.


# 10
n * Pr(1st is correct) * pr(all others incorrect | first is correct)  [DISJOINT]

n *  1/n * (1-prAnyMatches(n-1))
=
1-pAnyMatches(2)=  1/2

