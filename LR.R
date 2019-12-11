Y=cars
Y
scatter.smooth(x=cars$speed,y=cars$dist, main="Dist ~ Speed")

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed,main="Speed")  # box plot for 'speed'
boxplot(cars$dist, main="Distance")  # box plot for 'distance'

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")

plot(density(cars$dist), main="Density Plot: Distance", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

linearMod=lm(dist ~ speed, data=cars)  # build linear regression model on full data
linearMod
summary(linearMod)


set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data


lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)


actuals_preds=data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy=cor(actuals_preds)  
head(actuals_preds)

min_max_accuracy=mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape=mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy
mape




mlr=read.csv(file.choose())
mlr
head(mlr)
attach(mlr)
plot(mlr)
corr=round(cor(mlr), 2)
corr

mlrfit=lm(SH~., data=mlr)
mlrfit
summary(mlrfit)
anova(mlrfit)


