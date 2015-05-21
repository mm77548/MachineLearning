

#Load the training data and specify the NAs in our data
trainset <- read.csv("C:/Users/Meagan/Desktop/Coursera/MachineLearningProject/pml_training.csv", na.strings=c("NA","#DIV/0!", ""))
testset <- read.csv('C:/Users/Meagan/Desktop/Coursera/MachineLearningProject/pml_testing.csv', na.strings=c("NA","#DIV/0!", ""))

#Load the necessary packages
library(caret)
library(randomForest)

#Basic exploratory data analysis of our train set
dim(trainset)
table(trainset$classe)
plot(trainset$classe, col="black", main="Levels of the variable classe", 
     xlab="classe levels", ylab="Frequency")

#Basic exploratory data analysis of our test set
dim(testset)
#We will predict the classe for this set

#Set the seed so our numbers don't change
set.seed(1892)

#Delete unecessary columns 
trainset <-trainset[,colSums(is.na(trainset)) == 0]
testset <-testset[,colSums(is.na(testset)) == 0]

trainset   <-trainset[,-c(1:7)]
testset <-testset[,-c(1:7)]

#See new dimensions
dim(trainset)
dim(testset)

#In order to perform cross-validation, the training data set is partionned 
#into 2 sets: training (75%) and testing (25%).
#This will be performed using random subsampling without replacement.

subsamp <- createDataPartition(trainset$classe, p=0.75, list=FALSE)
subtrain <- trainset[subsamp, ]
subtest <-   trainset[-subsamp, ]

model <- randomForest(classe ~., data=subtrain, method="class")
prediction <- predict(model, subtest, type="class")
confusionMatrix(prediction, subtest$classe)

Accuracy <- 0.9953
SampleError <- 1-Accuracy
SampleError

#My out of sample error is expected to be 0.0047 or 0.4%.  

###############################  Save the test answers into individual text files for submission. 
setwd("C:/Users/Meagan/Desktop/Coursera/MachineLearningProject/20_tests")

predictfinal <- predict(model, testset, type="class")
predictfinal

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictfinal)









