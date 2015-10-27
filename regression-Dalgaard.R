# regression-Dalgaard

install.packages("ISwR"); library(ISwR)
data("thuesen")
help("thuesen")
dim(thuesen)
thuesen
attach(thuesen)
search()
ls(pos=2)
plot(short.velocity, blood.glucose)

summary(lm(blood.glucose ~ short.velocity))

abline(lm(blood.glucose ~ short.velocity))

lm.out=lm(data=thuesen[-is.na(thuesen$short.velocity), ],
          blood.glucose ~ short.velocity)

points(short.velocity, predict(lm.out), col="red", pch=2)


