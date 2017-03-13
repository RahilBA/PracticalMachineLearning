Summary

In this report, 2 different Machine Learning algorithms (Random Forest and Decision Tree) are applied to predict the manner in which 6 individuals do their exercises. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways that were assigned to the "classe" variable. Random Forest is chosen as the best classifier,because it gives the highest accuracy 

Load libraries:
```{r}
library(caret)
library(rpart)
library(randomForest)
library(RCurl)
library(e1071)
```
set the seed
```{r}
set.seed(1234)
```
Let's download the data:
```{r}
trainingURL <- getURL("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
training_CSV  <- read.csv(text = trainingURL, header=TRUE, sep=",", na.strings=c("NA",""))
testingURL <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
testing_CSV  <- read.csv(text = testingURL, header=TRUE, sep=",", na.strings=c("NA",""))
```
Let's clean the data:
I am going to remove the first column that sounds like an index variable since it doesn’t add any value to my prediction model.
```{r}
training_CSV <- training_CSV[,-1]
Data <- training_CSV
table(Data$classe)
```
with in these 160 variables, there are some variables that have NA values and are not related to accelerometers on the belt, forearm, arm, and dumbbell.So, I am going to removed theose columns that have about 70% of NAs. Also,I will remove those variables that are not necessary to fit the model like "time" variables.They have to be removed from test data set as well.
```{r}
Data <- Data[,-c(1:6)] # remove time variables
sum((colSums(!is.na(Data[,-ncol(Data)])) < 0.7*nrow(Data))) # number of columns with less than 70% of data
Keep <- c((colSums(!is.na(Data[,-ncol(Data)])) >= 0.7*nrow(Data)))
Data  <-  Data[,Keep] 
dim(Data)
```
there are about 52 predictors and "classe" variable.
Multicollinearity:
I calculated  the correlations among pairs of predictors. Those from a pair that are highly correlated can be predicted with the other one. Multicollinearity doesnt cause a problem while fitting nonlinear models like Random Forest and Decisin Trees.But, I prefered to have less
predictors in my model.
Let's find the correlation matrix:
```{r}
corrMatrix <- cor(na.omit(Data[sapply(Data, is.numeric)]))
```
Lets's plot the correlations. There are some correlation values that are values that considered as high (>.9). So, I am going to drop those predictors.
```{r}
hist(corrMatrix,main="Histogram of correlations",xlab="Correlation",ylim = c(0,800),breaks=15,right=TRUE,col="blue",labels=TRUE)
highCorr <- findCorrelation(corrMatrix, cutoff = .90, verbose = T)
Final_Data <- Data[,-highCorr]
```

Data Partition: 
Partition Data set to "training"" and "validation"" data sets. About 60 percent of data set is considered as training set.
```{r}
inTrain <- createDataPartition(Final_Data$classe, p=0.60, list=FALSE)
training <- Final_Data[inTrain,]
validating <- Final_Data[-inTrain,]
dim(training)
dim(validating)
```
Modeling:
In the section of this report, I am going to fit 2 classifiers to predict the "classe"" in the test data set : Random Forest and Decision Tree.Then, I will evaluate which have a better performance based on the accuracy.
1) Random Forest 
let's fit RF model and getting the k folds cross validation. In this case I going to set k=4.
```{r}
rfFit <- train(classe ~ ., method = "rf", data = training, importance = T, trControl = trainControl(method = "cv", number = 4))
Pred_RF <- predict(rfFit, newdata=validating)
# Check model performance by using confusion matrix
confusionMatrix(Pred_RF,validating$classe)
```
2) Decision tree
```{r}
dtFit <- train(classe ~ .,trControl=trainControl(method = "cv", number = 4),method="rpart",data=training)
Pred_DT <- predict(dtFit, newdata=validating)
# Check model performance by using confusion matrix
confusionMatrix(Pred_DT,validating$classe)
```
Conclusion:
Random Forest has higher accuracy.So,I will apply RF on my test data set to predict "Classe".
Model Test data set:
Finally, I am going to predict the new values in test data set.First, I apply the same data preparation operations on test data set.
```{r}
dim(testing_CSV)
testing_CSV <- testing_CSV [,-1] # Remove the first column that represents a ID Row
testing <- testing_CSV[,-c(1:6)]
testing <- testing[ , Keep] 
testing <- testing[,-highCorr]
dim(testing)
```
Prediction:
I measure a performance of my predictive model, the final accuracy on the testing set is very high.
```{r}
predict <- predict(rfFit, newdata=testing)
print(predict)
```
The following function to create the files to answers the Prediction Assignment Submission.
```{r}
write_files <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    filename <- paste0("problem_id", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE,col.names=FALSE)
  }
}
write_files(predict)
```
