# Predicting Titanic`s Death
library("caret")

# Load data from Titanic data set
titanicData <- read.csv("data/train.csv")
titanicData$Name <- as.character(titanicData$Name)
titanicData$Ticket <- as.character(titanicData$Ticket)
titanicData$Cabin <- as.character(titanicData$Cabin)
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Pclass <- as.factor(titanicData$Pclass)

# Create trainning datasets
inTrain <- createDataPartition(y = titanicData$Survived, p = 0.75, list = F)
trainning <- titanicData[inTrain,]
testing <- titanicData[-inTrain,]

# Fit first model
modelFit <- train(Survived ~ ., data = titanicData, method="glm")