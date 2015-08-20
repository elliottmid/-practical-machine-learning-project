library(tidyr)
data = read.csv(file="/Users/elliottmiddleton/Documents/machineLearning/pml-training.csv",header=TRUE)
pml.training <- read.csv("~/Documents/machineLearning/pml-training.csv", header=FALSE)
View(pml.training)
str(pml.training)

training = pml.training[2:nrow(pml.training),]
head(training)
training = data.matrix(training)
training = data.frame(training)
training$V160 = as.factor(training$V160)
str(pml.training)
head(training)
completes = training[complete.cases(training),]
dim(completes)

# # remove constant columns
# for (i in 9:159) {
#   print(i)  
#   print(all(is.na(training[,i])))
# }
# for (i in 9:159) {
#   training[,sapply(training, function(v) var(v, na.rm=TRUE)!=0)]
# }

## these are the data munging steps
na_count <-sapply(training, function(y) sum(length(which(is.na(y)))))
na_count
drops = (na_count > 19200)
drops
keeps = na_count <= 19200
keeps
new = training[,keeps==TRUE]
keepers = names(new)
str(keepers)
class(keepers)
typeof(keepers)

completes = new[complete.cases(new),]
dim(completes)

library(randomForest);library(e1071)
library(caret)
training = completes[,9:60]

set.seed(825)
dim(training)
training.bak = training
training = training.bak[sample(1:nrow(training.bak), 5000,replace=FALSE),]
str(training)
training = data.matrix(training)
training = as.data.frame(training)
training$V160 = as.factor(training$V160)
dim(training)
class(training$V160)
head(training)

fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# gbmFit1 <- train(training[,52] ~ ., data = training,
#                  method = "gbm",
#                  trControl = fitControl,
#                  ## This last option is actually one
#                  ## for rf() that passes through
#                  verbose = FALSE)
# gbmFit1

rfFit1 <- train(V160 ~ ., data = training,
                 method = "rf",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for rf() that passes through
                 verbose = FALSE)
rfFit1

# prediction on own test data
testdata = training.bak[!(row.names(training.bak) %in% row.names(training)), ]
testdata = data.matrix(testdata)
testdata = as.data.frame(testdata)
testdata$V160 = as.factor(testdata$V160)
dim(testdata)
names(testdata)
class(testdata)
str(testdata)
head(testdata)

pred = predict(rfFit1, newdata = testdata)
testdata$predRight = pred == testdata$V160
table(pred,testdata$V160)
confusionMatrix(testdata$V160,pred)

#prediction on course test data
pml.testing <- read.csv("~/Documents/machineLearning/pml-testing.csv", header=FALSE)
View(pml.testing)
dim(pml.testing)
testing = pml.testing[2:21,]
View(testing)
testing = data.matrix(testing)
testing = as.data.frame(testing)
str(testing)
testing$V160 = as.factor(testing$V160)

new = testing[,keeps==TRUE]
dim(new)
testing = new[,9:60]

pred2 <- predict(rfFit1, newdata=testing)
pred2


