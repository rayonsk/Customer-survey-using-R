#importing the pre-processing r script fie
source("Survey_Preprocessing.R")

#EDA
#converting the factor variables from double to factor 
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
summary(CompleteResponses)
levels(CompleteResponses$brand) <- c("Acer", "Sony")


CompleteResponses <-as.data.frame(CompleteResponses)
set.seed(123)
#creating training test data from the complete response 
inTraining <- createDataPartition(CompleteResponses$brand,
                                  p=.75,list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#10 fold validation with 1 repeat
fitControl_ROC_random <- trainControl(method = "repeatedcv",
                           number = 10, repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")
fitControl_random <- trainControl(method = "repeatedcv",
                                  number = 10, repeats = 1,
                                  search = "random")


fitControl_auto <- trainControl(method = "repeatedcv",
                                  number = 10, repeats = 1)

#training the model
rdaFit1 <- train(brand ~ ., data = training,
                 tuneLength = 5, method = 'rda',
                 metric = "ROC",
                 trControl = fitControl_random)
rdaFit1
ggplot(rdaFit1) + theme(legend.position = "top")
# result : 
#ROC was used to select the optimal
# model using the largest value.
# The final values used for the model
# were gamma = 0.3315616 and lambda
# = 0.1555332.

featureImportance <- varImp(rdaFit1, scale = FALSE)
`featureImportance`


boostedTreeFit1<- train(brand ~ ., data = training,
                        tuneLength = 5, method = 'C5.0',
                        trControl = fitControl_auto)
boostedTreeFit1


# Accuracy was used to select the
# optimal model using the largest value.
# The final values used for the model
# were trials = 10, model = tree and winnow
# = FALSE.
featureImportance <- varImp(boostedTreeFit1, scale = FALSE)
featureImportance
plot(featureImportance, top = 20)

boostedTreeFit2<- train(brand ~ ., data = training,
                        tuneLength = 5, method = 'C5.0',
                        trControl = fitControl_random)
boostedTreeFit2

rfGrid <- expand.grid(mtry = c(1,10,20))
rfFit1<- train(brand ~ ., data = training,
                        tuneLength = 5, method = 'rf',
                        trControl = fitControl_auto)
rfFit1
rfFit2<- train(brand ~ ., data = training,
               tuneLength = 5, method = 'rf',
               trControl = fitControl_auto,
               tuneGrid =rfGrid)
rfFit2
featureImportance <- varImp(rfFit2, scale = FALSE)
featureImportance
plot(featureImportance, top = 20)

#prediction
predicts_boostedTree1 <- predict(boostedTreeFit1,testing)
predicts_boostedTree1
postResample(predicts_boostedTree1,testing$brand)

predicts_boostedTree2 <- predict(boostedTreeFit2,testing)
predicts_boostedTree2
postResample(predicts_boostedTree2,testing$brand)

actuals_preds <-data.frame(cbind( actuals = training$brand,
                                  predictions = predicts_boostedTree1))
head(actuals_preds)

predicts_rf1 <- predict(rfFit1,testing)
predicts_rf1
postResample(predicts_rf1,testing$brand)

predicts_rf2 <- predict(rfFit2,testing)
predicts_rf2
postResample(predicts_rf2,testing$brand)

actuals_preds <-data.frame(cbind( actuals = training$brand,
                                  predictions = predicts_rf))
head(actuals_preds)

#feature selection
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(training[,1:6], training[,7], sizes=c(1:5), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

model_rf <- ranger(brand ~ .,data = CompleteResponses,importance = "impurity")
v<-as.vector(model_rf$variable.importance)
w<-(as.vector((names(model_rf$variable))))
DF<-cbind(w,v)
DF<-as.data.frame(DF)
DF
DF$v <- as.numeric(DF$v)

ggplot(DF, aes(x=reorder(w,v), y=v,fill=v))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  scale_fill_gradient(low="red", high="blue")


#*summary
#*from the above random forest of manual tuning & mtry = 20, is the high 
#*performance algorithm
#*

#completing the incomplete excel - SurveyIncomplete
#algorithm - rfFit2
#converting the factor variables from double to factor 
SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
summary(SurveyIncomplete)

fitControl_auto <- trainControl(method = "repeatedcv",
                                number = 10, repeats = 1)

rfGrid <- expand.grid(mtry = 20)
rfFit<- train(brand ~ ., data = SurveyIncomplete,
               tuneLength = 5, method = 'rf',
               trControl = fitControl_auto,
              tuneGrid = rfGrid)
rfFit
predicts <- predict(rfFit,SurveyIncomplete)
predicts

CompletedDataSet<-data.frame(cbind( 
  SurveyIncomplete[,-7],
  Predicted_Brand = predicts))
head(CompletedDataSet)
