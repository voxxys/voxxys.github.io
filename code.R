library(ISLR)
library(caret)
library(kernlab)
library(randomForest)

set.seed(123)

data <- read.csv("pml-training.csv")
full <- data[,2:160]

inTrain <- createDataPartition(y=full$classe, p=0.75, list=FALSE)
training <- full[inTrain,]
testing <- full[-inTrain,]
dim(training)

numcol = dim(training)[2]
subset = 1:numcol

for (i in 1:(numcol-1)) {
    
  if (class(training[,i])=="factor") {
    training[,i] <- as.numeric(training[,i])
    testing[,i] <- as.numeric(testing[,i])
  }
  
  if ((sum(is.na(training[,i]))>0.95*dim(training)[1])|((sum(training[,i]==1)>0.95*dim(training)[1])))  {
    subset[i] <- 0
  }

}
  
training <- training[,subset]

rf <- randomForest(classe ~ ., data = training,ntree = 100)

pr <- predict(rf,testing)
pr
confusionMatrix(testing$classe,pr)

###########################################


test_data <- read.csv("pml-testing.csv")
test_full <- test_data[,2:160]

for (i in 1:(numcol-1)) {
  
  if (class(test_full[,i])=="factor") {
    test_full[,i] <- as.numeric(test_full[,i])
  }
}

test_full <- test_full[,subset]
pr_1 <- predict(rf,test_full)

###########################################

training <- full


for (i in 1:(numcol-1)) {
  
  if (class(training[,i])=="factor") {
    training[,i] <- as.numeric(training[,i])
  }
  
  if ((sum(is.na(training[,i]))>0.95*dim(training)[1])|((sum(training[,i]==1)>0.95*dim(training)[1])))  {
    subset[i] <- 0
  }
  
}

training <- training[,subset]

rf <- randomForest(classe ~ ., data = training,ntree = 100)

pr_2 <- predict(rf,test_full)

