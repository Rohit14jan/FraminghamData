rm(list = ls())

# Read in the dataset
framingham = read.csv("framingham.csv")
summary(framingham)
View(framingham)

library(caTools)
set.seed(5)
#split1 = sample.split(framingham, SplitRatio = 0.8)
split2 = sample.split(framingham$TenYearCHD, SplitRatio = 0.8)

train = subset(framingham, split2 == TRUE)
test = subset(framingham, split2 == FALSE)

model1 = glm(TenYearCHD ~ .,data = train, family = binomial)
summary(model1)

model2 = glm(TenYearCHD ~ male+age+ cigsPerDay+prevalentHyp+ sysBP+glucose,data = train, family = binomial)
summary(model2)

predictions = predict(model2,type = "response", newdata = test)
predDF = data.frame(test, predictions)
predDF$HeartPrediction = predDF$predictions>0.5

cf = as.data.frame.matrix(table(predDF$TenYearCHD, predDF$HeartPrediction))

accuracy = (643+9)/(643+9+11+109)
truePositive = (643)/(643+9+11+109)
trueNegative = (9)/(643+9+11+109)

falsePositive = (11)/(643+9+11+109) #also called  type 1 error
falseNegative = (109)/(643+9+11+109) #also called  type 2 error





