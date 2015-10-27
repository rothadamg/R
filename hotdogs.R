hotdogs = scan(what=list(character(0),numeric(0),numeric(0)))#,	sep="\t")
Beef	186	495
Beef	181	477
Beef	176	425
Beef	149	322
Beef	184	482
Beef	190	587
Beef	158	370
Beef	139	322
Beef	175	479
Beef	148	375
Beef	152	330
Beef	111	300
Beef	141	386
Beef	153	401
Beef	190	645
Beef	157	440
Beef	131	317
Beef	149	319
Beef	135	298
Beef	132	253
Meat	173	458
Meat	191	506
Meat	182	473
Meat	190	545
Meat	172	496
Meat	147	360
Meat	146	387
Meat	139	386
Meat	175	507
Meat	136	393
Meat	179	405
Meat	153	372
Meat	107	144
Meat	195	511
Meat	135	405
Meat	140	428
Meat	138	339
Poultry	129	430
Poultry	132	375
Poultry	102	396
Poultry	106	383
Poultry	94	387
Poultry	102	542
Poultry	87	359
Poultry	99	357
Poultry	107	528
Poultry	113	513
Poultry	135	426
Poultry	142	513
Poultry	86	358
Poultry	143	581
Poultry	152	588
Poultry	146	522
Poultry	144	545

##Blank line ends the input for scan().
hotdogs = as.data.frame(hotdogs)
names(hotdogs) = strsplit("Type	Calories	Sodium", split="\t")[[1]]

hotdogs[1:6,]

sapply(split(hotdogs$Calories, hotdogs$Type), summary)
smartBarPlot(split(hotdogs$Calories, hotdogs$Type))
plot(hotdogs$Calories, hotdogs$Sodium) 
plot(hotdogs$Calories, hotdogs$Sodium, 
	col=c("blue","red","green")[match(hotdogs$Type, c("Beef","Meat","Poultry"))],
	pch=c("B","M","P")[match(hotdogs$Type, c("Beef","Meat","Poultry"))]
)

lm.hotdogs = lm(Calories ~ Type, data=hotdogs, x=TRUE)

lm.hotdogs$x   #### This is a DESIGN MATRIX.

hotdogs.factored = cbind(hotdogs, lm.hotdogs$x)
names(hotdogs.factored)

hotdogs.factored$TypeBeef = as.numeric((hotdogs.factored$TypeMeat == 0)  & 
					 (hotdogs.factored$TypePoultry == 0))

lm(Calories ~ TypeMeat + TypePoultry + TypeBeef, data= hotdogs.factored)


anova(lm.hotdogs)

####   Verify.

with(hotdogs, 
{
	n.i <<- table(Type)
	Ybarplusplus <<- mean(Calories)
	Ybar.i.plus <<- sapply(split(Calories, Type), mean)
	S2within <<- sum( (Calories - rep(Ybar.i.plus, n.i))^2 )
	S2between <<- sum( n.i * (Ybar.i.plus -  Ybarplusplus)^2)
	S2total <<- sum( (Calories - Ybarplusplus)^2 )		
})

###Verify.
print(c(S2within + S2between, S2total))

###  Do the F test.

p = 3
df.within = sum(n.i) - p
df.between = p - 1
U2 = (S2between/df.between) / (S2within/df.within) 

P.value = print(1 - pf(U2, df.between, df.within))
