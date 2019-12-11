install.packages("moments")
install.packages("goftest")
install.packages("nortest")
library(nortest)
library(moments)
library(goftest)
A=read.csv(file.choose())
A
Ft=A$FOOT
skewness(Ft)
kurtosis(Ft)
shapiro.test(Ft)
ad.test(Ft)
cvm.test(Ft)
lillie.test(Ft)
qqnorm(Ft)
qqline(Ft, col="Red", lwd=3)
qqnorm(Ft);qqline(Ft, col="Red", lwd=3)

t=rnorm(1000, mean = 0, sd=1)
qqnorm(t);qqline(t, col="Red", lwd=3)

s=runif(1000)
qqnorm(s);qqline(s, col="Red", lwd=3)
ks.test(t,s)
var.test(t,s)
fligner.test(t,s)

r=PlantGrowth 
bartlett.test(weight~group, r)
