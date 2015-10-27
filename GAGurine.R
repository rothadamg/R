require(MASS)
data(GAGurine)
?GAGurine
head(GAGurine)
dim(GAGurine)
with(GAGurine, plot(Age, GAG))

GAG.lm = lm(GAG ~ Age, data=GAGurine)
summary(GAG.lm)
anova(GAG.lm)

lines(GAGurine$Age, predict(GAG.lm), pch="x", type="b", col="blue")

with(GAGurine, lines.loess(Age, GAG, col="red", lwd=2))

par(mfrow=c(2,2))
plot(GAG.lm)

##########  Let's try logging GAG

GAGurine$logGAG = log10(GAGurine$GAG)
par(mfrow=c(1,1))
with(GAGurine, plot(Age, logGAG))

logGAG.lm = lm(logGAG ~ Age, data=GAGurine)
summary(logGAG.lm)
anova(logGAG.lm)

lines(GAGurine$Age, predict(logGAG.lm), pch="x", type="b", col="blue", lwd=3)

with(GAGurine, lines.loess(Age, logGAG, col="red", lwd=2))

par(mfrow=c(2,2))
plot(logGAG.lm)
## observation 314 is labeled most extreme.  May have too much effect.