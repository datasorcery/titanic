# Predicting Titanic`s Death
library("caret")

# Load data from Titanic data set
titanicData <- read.csv("data/train.csv")

# Filter relevant columns
# Columns left out that may help: Ticket, Fare, Cabin, Embarked
titanicData <- titanicData[c("PassengerId","Survived","Pclass","Sex",
                             "Age","SibSp","Parch")]

# Convert variables to factor 
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Pclass <- as.factor(titanicData$Pclass)

# Preprocess training data, imputing age data
maleAge   <- median(titanicData$Age[titanicData$Sex=="male"], na.rm=TRUE)
femaleAge <- median(titanicData$Age[titanicData$Sex=="female"], na.rm=TRUE)
titanicData$Age[titanicData$Sex=="female" & is.na(titanicData$Age) ] <- femaleAge
titanicData$Age[titanicData$Sex=="male" & is.na(titanicData$Age) ] <- maleAge

# Save a copy of the original dataset after preprocessing but before tests
originalData <- titanicData

# Create trainning datasets
inTrain <- createDataPartition(y = titanicData$Survived, p = 0.75, list = F)
trainning <- titanicData[inTrain,]
testing <- titanicData[-inTrain,]

# Fit first model
modelFit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                  data = titanicData,  
                  method=c("rpart"))

# Test the model
predictions <- predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$Survived)


# Now, gonna run the real thing.
# Use the full dataset
finalFit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                  data = originalData,  
                  method=c("rpart"))

# Load and preprocess data for the target set
predictData <- read.csv("data/test.csv")
predictData <- predictData[c("PassengerId","Pclass","Sex",
                             "Age","SibSp","Parch")]
predictData$Pclass <- as.factor(predictData$Pclass)
predictData$Age[predictData$Sex=="female" & is.na(predictData$Age) ] <- femaleAge
predictData$Age[predictData$Sex=="male" & is.na(predictData$Age) ] <- maleAge

# Make predictions
finalPred <- predict(finalFit, newdata = predictData)

# Prepare data for exporting
exportData <- data.frame(cbind(PassengerId = predictData$PassengerId,
                               Survived = finalPred))
exportData$Survived[exportData$Survived == 1] <- 0
exportData$Survived[exportData$Survived == 2] <- 1

# Save file
write.csv(exportData, file="predictions.csv", row.names=F, quote=F)













# Devo preprocessar o dataset? preprocess=c("center","scale")
# Imput data? preProcess=c("knnImpute"),
#preObj <- preProcess(testing, method="knnImpute")

# Exploratory analysis
featurePlot(x=trainning[,c("Pclass","Sex","Age","SibSp","Parch")],
            y=trainning$Survived,plot="pairs")
qplot(Age, Survived, color = Pclass, data = titanicData)
