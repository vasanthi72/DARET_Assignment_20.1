bicepsdata <- read.csv("D:/kamagyana/Computing/DARET/Assignments/bicepsdata.csv", stringsAsFactors=FALSE)
View(bicepsdata)
biceps <- read.csv("D:/kamagyana/Computing/DARET/Assignments/biceps.csv", header=FALSE, stringsAsFactors=FALSE)
View(biceps)
bitest <- read.csv("D:/kamagyana/Computing/DARET/Assignments/bitest.csv", stringsAsFactors=FALSE)
View(bitest)
bitrain <- read.csv("D:/kamagyana/Computing/DARET/Assignments/bitrain.csv", stringsAsFactors=FALSE)
View(bitrain)
compbicepsdata <- read.csv("D:/kamagyana/Computing/DARET/Assignments/compbicepsdata.csv", stringsAsFactors=FALSE)
View(compbicepsdata)
library(C50)
library(caret)
library(rpart)
train_control <- trainControl(method="cv", number=10)
library(randomForest)
model1 <- train(classe~., data = bitrain[,-52], trContol = train_control,method ="rf")
pred1 <- predict(model1,bitest[,-52]); pred1 <- cbind(bitest,pred1); confusionMatrix(pred1$classe, pred1$pred1)
pred1$classe <- as.factor(perd1$classe)
pred1$classe <- as.factor(pred1$classe)
pred1$pred1 <- as.factor(pred1$pred1)
confusionMatrix(pred1$classe, pred1$pred1)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metric <- 'Accuracy'
set.seed(seed)
mtry <- sqrt(ncol(bitrain[,-52]))
tunegrid <- expand.grid(.mtry = mtry)
rf_default <- train(classe~.,data = bitrain[,-52], method ="rf", metric = metric, tuneGrid = tunegrid, trControl = control)
rf_default
pred2 <- predict(rf_default,bitest[,-52]); pred2 <- cbind(bitest,pred2);
pred2$classe <- as.factor(pred2$classe);pred2$pred2 <- as.factor(pred2$pred2);confusionMatrix(pred2$classe, pred2$pred2)
varImp(rf_default)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'random')
set.seed(seed)
mtry <- sqrt(ncol(bitrain[,-52]))
rf_random <- control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = 'grid')
gbm_mod <- train(classe~.,data = bitrain[,-52], method ='gbm', metric = metric, trControl = control)

rf_random
pred3 <- predict(rf_random,bitest[,-52]); pred3 <- cbind(bitest,pred3);
pred3$classe <- as.factor(pred3$classe);pred3$pred3 <- as.factor(pred3$pred3);confusionMatrix(pred3$classe, pred3$pred3)
varImp(rf_random)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'grid'); set.seed(seed); tunegrid <- expand.grid(.mtry = c(1:15))
rf_gridsearch <- train(classe~.,data = bitrain[,-52], method ="rf", metric = metric, tuneGrid = tunegrid, trControl = control); rf_gridsearch
rf_gridsearch;pred4 <- predict(rf_gridsearch,bitest[,-52]); pred4 <- cbind(bitest,pred4);
pred4$classe <- as.factor(pred4$classe);pred4$pred4 <- as.factor(pred4$pred4);confusionMatrix(pred4$classe, pred4$pred4)
varImp(rf_gridsearch)
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = 'grid')
gbm_mod <- train(classe~.,data = bitrain[,-52], method ='gbm', metric = metric, trControl = control)
print(gbm_mod)
plot(gbm_mod)
summary(gbm_mod)
pred5 <- predict(gbm_mod,bitest[,-52]); pred5 <- cbind(bitest,pred5);pred5$classe <- as.factor(pred5$classe);pred5$pred5 <- as.factor(pred5$pred5);confusionMatrix(pred5$classe, pred5$pred5)



