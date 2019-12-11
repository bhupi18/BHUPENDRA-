x=read.csv(file.choose())
x
head(x)
Job=x$JOB
Cons=x$CONSERVATIVE
Soc=x$SOCIAL
Out=x$OUTDOOR
Jcode=ifelse(Job=='Mechanic',1,0)
plot(Out,jitter(Jcode, 0.15),pch=19, 
     xlab = "Outdoor", ylab = "Job(0 - Customer Service, 1 - Mechanic)")
g=glm(JOB~.,family=binomial, data=x)
g
summary(g)

