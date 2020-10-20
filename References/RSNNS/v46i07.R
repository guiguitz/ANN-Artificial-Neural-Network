### R code from vignette source 'jss703.Stex'
### Encoding: UTF-8

###################################################
### code chunk number 1: jss703.Stex:78-86
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
require("RSNNS")
data("snnsData")

seed <- 3
set.seed(seed)
setSnnsRSeedValue(seed)



###################################################
### code chunk number 2: jss703.Stex:226-232 (eval = FALSE)
###################################################
## mlp(x, y, size = c(5), maxit = 100, 
##     initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3), 
##     learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2, 0.0), 
##     updateFunc = "Topological_Order", updateFuncParams = c(0.0),
##     hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, 
##     linOut = FALSE, inputsTest = NULL, targetsTest = NULL)


###################################################
### code chunk number 3: jss703.Stex:253-256
###################################################
encData <- snnsData$encoder.pat
inputs <- encData[,inputColumns(encData)]
targets <- encData[,outputColumns(encData)]


###################################################
### code chunk number 4: jss703.Stex:266-267
###################################################
getSnnsRFunctionTable()[196:202,]


###################################################
### code chunk number 5: jss703.Stex:272-274
###################################################
encoder <- mlp(inputs, targets)
encoder


###################################################
### code chunk number 6: jss703.Stex:279-280 (eval = FALSE)
###################################################
## summary(encoder)


###################################################
### code chunk number 7: jss703.Stex:297-299
###################################################
data("snnsData")
names(snnsData)


###################################################
### code chunk number 8: jss703.Stex:304-308
###################################################
laser <- snnsData$laser_1000.pat
inputs <- laser[,inputColumns(laser)]
targets <- laser[,outputColumns(laser)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)


###################################################
### code chunk number 9: jss703.Stex:324-328
###################################################
model <- elman(patterns$inputsTrain, patterns$targetsTrain, 
  size = c(8, 8), learnFuncParams = c(0.1), maxit = 500, 
  inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest, 
  linOut = FALSE)


###################################################
### code chunk number 10: jss703.Stex:333-336 (eval = FALSE)
###################################################
## plot(inputs, type = "l")
## plot(targets[1:100], type = "l")
## lines(model$fitted.values[1:100], col = "green")


###################################################
### code chunk number 11: jss703.Stex:341-345
###################################################
par(mfrow=c(1,2))
plot(inputs, type="l", ylab="Values", main="(a) Laser time series")
plot(targets[1:100], type="l", ylab="Values", main="(b) Target and fitted values")
lines(model$fitted.values[1:100], col="green")


###################################################
### code chunk number 12: jss703.Stex:355-359 (eval = FALSE)
###################################################
## plotIterativeError(model)
## plotRegressionError(patterns$targetsTrain, model$fitted.values)
## plotRegressionError(patterns$targetsTest, model$fittedTestValues)
## hist(model$fitted.values - patterns$targetsTrain)


###################################################
### code chunk number 13: jss703.Stex:364-372
###################################################
par(mfrow=c(2,2))
plotIterativeError(model, main="(a) Iterative errors")
plotRegressionError(patterns$targetsTrain, model$fitted.values, 
                                 main="(b) Regression plot fit", xlab="Targets", ylab="Fits", pch=3)
plotRegressionError(patterns$targetsTest, model$fittedTestValues, 
                                 main="(c) Regression plot test", xlab="Targets", ylab="Fits", pch=3)
hist(model$fitted.values - patterns$targetsTrain, col="lightblue", 
                                 main="(d) Error histogram fit", xlab="Error")


###################################################
### code chunk number 14: jss703.Stex:386-393
###################################################
data("iris")
iris <- iris[sample(1:nrow(iris) ,length(1:nrow(iris))), 1:ncol(iris)]
irisValues <- iris[,1:4]
irisTargets <- iris[,5]
irisDecTargets <- decodeClassLabels(irisTargets)
iris <- splitForTrainingAndTest(irisValues, irisDecTargets, ratio = 0.15)
iris <- normTrainingAndTestSet(iris)


###################################################
### code chunk number 15: jss703.Stex:399-403
###################################################
model <- mlp(iris$inputsTrain, iris$targetsTrain, size = 5, 
    learnFuncParams = c(0.1), maxit = 60, inputsTest = iris$inputsTest, 
    targetsTest = iris$targetsTest)
predictions <- predict(model, iris$inputsTest)


###################################################
### code chunk number 16: jss703.Stex:408-412 (eval = FALSE)
###################################################
## plotIterativeError(model)
## plotRegressionError(predictions[,2], iris$targetsTest[,2], pch = 3)
## plotROC(fitted.values(model)[,2], iris$targetsTrain[,2])
## plotROC(predictions[,2], iris$targetsTest[,2])


###################################################
### code chunk number 17: jss703.Stex:417-422
###################################################
par(mfrow=c(2,2))
plotIterativeError(model, main="(a) Iterative errors")
plotRegressionError(predictions[,2], iris$targetsTest[,2], ylab="Fits", xlab="Targets", main="(b) Regression plot test", pch=3)
plotROC(fitted.values(model)[,2], iris$targetsTrain[,2], main="(c) ROC second class fit", ylab="Sensitivity", xlab="1 - Specificity")
plotROC(predictions[,2], iris$targetsTest[,2], main="(d) ROC second class test", ylab="Sensitivity", xlab="1 - Specificity")


###################################################
### code chunk number 18: jss703.Stex:432-437
###################################################
confusionMatrix(iris$targetsTrain, fitted.values(model))
confusionMatrix(iris$targetsTest, predictions)
confusionMatrix(iris$targetsTrain, 
                encodeClassLabels(fitted.values(model),
                method = "402040", l = 0.4, h = 0.6))


###################################################
### code chunk number 19: jss703.Stex:442-443 (eval = FALSE)
###################################################
## weightMatrix(model)


###################################################
### code chunk number 20: jss703.Stex:452-454
###################################################
model <- som(irisValues, mapX = 16, mapY = 16, maxit = 500, 
                targets = irisTargets)


###################################################
### code chunk number 21: jss703.Stex:472-477 (eval = FALSE)
###################################################
## plotActMap(model$map, col = rev(heat.colors(12)))
## plotActMap(log(model$map + 1), col = rev(heat.colors(12)))
## persp(1:model$archParams$mapX, 1:model$archParams$mapY, log(model$map + 1), 
##       theta = 30, phi = 30, expand = 0.5, col = "lightblue")
## plotActMap(model$labeledMap)


###################################################
### code chunk number 22: jss703.Stex:482-488
###################################################
par(mfrow=c(2,2))
plotActMap(model$map, col=rev(heat.colors(12)), main="(a) SOM of the iris example")
plotActMap(log(model$map+1), col=rev(heat.colors(12)), main="(b) SOM, logarithmic scale")
persp(1:model$archParams$mapX, 1:model$archParams$mapY, log(model$map+1), 
      theta = 30, phi = 30, expand = 0.5, col = "lightblue", main="(c) SOM, logarithmic scale", xlab="x", ylab="y", zlab="Frequency")
plotActMap(model$labeledMap, main="(d) SOM, with target class labels")


###################################################
### code chunk number 23: jss703.Stex:496-498 (eval = FALSE)
###################################################
## for(i in 1:ncol(irisValues)) plotActMap(model$componentMaps[[i]], 
##                          col = rev(topo.colors(12)))


###################################################
### code chunk number 24: jss703.Stex:503-507
###################################################
par(mfrow=c(2,2))
labels <- c("(a)","(b)","(c)","(d)")
for(i in 1:ncol(irisValues)) plotActMap(model$componentMaps[[i]], 
                         col=rev(topo.colors(12)), main=labels[i])


###################################################
### code chunk number 25: jss703.Stex:523-527
###################################################
patterns <- snnsData$art2_tetra_med.pat
model <- art2(patterns, f2Units = 5, 
                 learnFuncParams = c(0.99, 20, 20, 0.1, 0), 
                 updateFuncParams = c(0.99, 20, 20, 0.1, 0))


###################################################
### code chunk number 26: jss703.Stex:532-534 (eval = FALSE)
###################################################
## library("scatterplot3d")
## scatterplot3d(patterns, pch=encodeClassLabels(model$fitted.values))


###################################################
### code chunk number 27: jss703.Stex:541-548
###################################################
library("scatterplot3d")
par(mfrow=c(1,2))
scatterplot3d(patterns, pch=encodeClassLabels(model$fitted.values)+1, xlab="", ylab="", zlab="", main="(a) Medium noise level")

patHigh <- snnsData$art2_tetra_high.pat
modHigh <- art2(patHigh, f2Units=5, learnFuncParams=c(0.99, 20, 20, 0.1, 0), updateFuncParams=c(0.99, 20, 20, 0.1, 0))
scatterplot3d(patHigh, pch=encodeClassLabels(modHigh$fitted.values)+1, xlab="", ylab="", zlab="", main="(b) High noise level")


###################################################
### code chunk number 28: jss703.Stex:560-561
###################################################
filename <- NULL


###################################################
### code chunk number 29: jss703.Stex:564-568 (eval = FALSE)
###################################################
## exportToSnnsNetFile(model, filename)
## readPatFile(filename)
## savePatFile(inputs, targets, filename)
## readResFile(filename)


