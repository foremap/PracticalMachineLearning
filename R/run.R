library(caret)

# data preProcess for predictors
myPreProcess <- function(data) {
    # Get char type variable 
    data.x <- data[ , -160]
    a <- sapply(data.x, is.numeric)
    charVar <- names(data.x[ ,!a])

    # Covert the char to numeric
    for (i in charVar[-c(1:3)]) {
        data[ , i] <- as.numeric(data[ , i])
    }

    # Covert the 'new_window' to numeric
    data[ , "new_window"] <- as.numeric(as.factor(data[ , "new_window"]))

    #remove 'X', 'user_name', 'cvtd_timestamp'. These columns are not reasonable for this case
    data <- subset(data, select=-c(X, user_name, cvtd_timestamp, raw_timestamp_part_1, raw_timestamp_part_2, num_window))

    #remove N/A in predictors, because the carvet train would error = =....
    data <- data[,colSums(is.na(data))==0]
    return(data)
}

trainingData <- myPreProcess(read.csv("../data/pml-training.csv", header = TRUE, stringsAsFactors = FALSE))
#trainingData <- myPreProcess(read.csv("/Users/foremap/Sean/Coursera/ML/PracticalMachineLearning/data/pml-training.csv", header = TRUE, stringsAsFactors = FALSE))

# Factor 'classe' -> y 
trainingData$classe <- as.factor(trainingData$classe)

# Prepare a testing Data for latest validation
set.seed(552)
inTraining <- createDataPartition(trainingData$classe, p = 0.6, list = FALSE)
training <- trainingData[inTraining, ]
testing <- trainingData[-inTraining, ]

# Do K-fold
set.seed(100)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
rfFit <- train(classe ~ ., data = training, method = "rf", trControl = fitControl)

rfFit
plot(varImp(rfFit, scale=FALSE), top=20)

result <- predict(rfFit, newdata = testing)
accuracy <- sum(result == testing$classe)/length(testing$classe)
confusionMatrix(predictions,testing$classe)


testData <- myPreProcess(read.csv("../data/pml-testing.csv", header = TRUE, stringsAsFactors=FALSE))
#testData <- myPreProcess(read.csv("/Users/foremap/Sean/Coursera/ML/PracticalMachineLearning/data/pml-testing.csv", header = TRUE, stringsAsFactors=FALSE))
dim(testData)
result <- predict(rfFit, newdata = testData)
result
