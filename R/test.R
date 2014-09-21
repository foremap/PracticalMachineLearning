#trainingData <- read.csv("/Users/foremap/Sean/Coursera/ML/PracticalMachineLearning/data/pml-training.csv", header = TRUE, stringsAsFactors=FALSE)
#testData <- read.csv("/Users/foremap/Sean/Coursera/ML/PracticalMachineLearning/data/pml-testing.csv", header = TRUE, stringsAsFactors=FALSE)


trainingData <- read.csv("../data/pml-training.csv", header = TRUE, stringsAsFactors=FALSE)
testData <- read.csv("../data/pml-testing.csv", header = TRUE, stringsAsFactors=FALSE)

# Factor 'classe' 
trainingData$classe <- as.factor(trainingData$classe)

# Get char type variable 
trainingData.x <- trainingData[ , -160]
a <- sapply(trainingData.x, is.numeric)
charVar <- names(trainingData.x[ ,!a])
# Covert the some of char to numeric
for (i in charVar[-c(1:3)]) {
    trainingData[ , i] <- as.numeric(trainingData[ , i])
}

trainingData[ , "new_window"] <- as.numeric(as.factor(trainingData[ , "new_window"]))

#remove X, user_name, cvtd_timestamp. These columns are not reasonable for me
trainingData <- subset(trainingData, select=-c(X, user_name, cvtd_timestamp))

inTrain <- createDataPartition(trainingData$classe, p = 0.7, list = FALSE)
length(inTrain)



myPreProcess <- function(data) {
    # Factor 'classe'
    data$classe <- as.factor(data$classe)

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
    data <- subset(data, select=-c(X, user_name, cvtd_timestamp))

    return(data)
}