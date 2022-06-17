library(caret)
library(tidyverse)
fasteats_data <- read.csv('FastEats_data.csv', header=T)

fasteats_data_predictors<-select(fasteats_data,-Response)
dummies_model <- dummyVars(~ ., data = fasteats_data_predictors)
fasteats_predictors_dummy<- data.frame(predict(dummies_model, newdata = fasteats_data)) 
fasteats_data <- cbind(Response=fasteats_data$Response, fasteats_predictors_dummy) 

fasteats_data$Response<-as.factor(fasteats_data$Response)
fasteats_data$Response<-fct_recode(fasteats_data$Response, No = "0", Yes = "1") 


set.seed(99)
index <-createDataPartition(fasteats_data$Response, p = .8,list = FALSE)
fasteats_train <- fasteats_data[index,]
fasteats_test <- fasteats_data[-index,]

library(xgboost)
library(e1071) 

xgb_grid <- expand.grid(
  nrounds = c(50,200),
  eta = c(0.025, 0.05),
  max_depth = c(2, 3),
  gamma = 0,
  colsample_bytree = 1,
  subsample = 1)

set.seed(8)
model_gbm <- train(Response ~ .,
                   data = fasteats_train,
                   method = "xgbTree",
                   trControl =trainControl(method = "cv",number = 5,
                                           ## Estimate class probabilities
                                           classProbs = TRUE,
                                           #needed to get ROC
                                           summaryFunction = twoClassSummary),
                   metric="ROC") 
plot(model_gbm)
model_gbm$bestTune
plot(varImp(model_gbm))


predprob_fasteats<-predict(model_gbm, fasteats_test, type="prob")

library(ROCR)
pred_lasso <- prediction(predprob_fasteats[,2], fasteats_test$Response,label.ordering = c("No","Yes"))
perf_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(perf_lasso, colorize=TRUE)


auc_lasso<-unlist(slot(performance(pred_lasso, "auc"), "y.values"))
auc_lasso



library(SHAPforxgboost)

Xdata<-as.matrix(select(fasteats_train,-Response)) # change data to matrix for plots
shap <- shap.prep(model_gbm$finalModel, X_train = Xdata)

shap.plot.summary(shap)
top4<-shap.importance(shap, names_only = TRUE)[1:4]

for (x in top4) {
  p <- shap.plot.dependence(
    shap, 
    x = x, 
    color_feature = "auto", 
    smooth = FALSE, 
    jitter_width = 0.01, 
    alpha = 0.4
  ) +
    ggtitle(x)
  print(p)
}
