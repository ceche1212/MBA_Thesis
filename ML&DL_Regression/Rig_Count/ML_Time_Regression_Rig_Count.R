library(tidyverse)
library(dplyr)
library(ggplot2)
library(BAS)
library(corrplot)
library(purrr)
library(RColorBrewer)
library(gridExtra)
library(broom)
library(cluster)
library(lubridate)
library(Metrics)
library(caret)
library(xgboost)
library(keras)

setwd("~/Data Science/Tesis MBA/Machine learning time slice regression Rig Count")

load("Oil_Modified.RData")

OIL_MODIFIED$Gov_ctrl_party<-as.factor(OIL_MODIFIED$Gov_ctrl_party)
OIL_MODIFIED$Gov_ctrl_type<-as.factor(OIL_MODIFIED$Gov_ctrl_type)
OIL_MODIFIED$Eco_Rec<-as.factor(OIL_MODIFIED$Eco_Rec)
OIL_MODIFIED$Season<-as.factor(OIL_MODIFIED$Season)

OIL_MODIFIED<-OIL_MODIFIED %>% select(-Drilled_Wells)

#input active conflicst missing values

OIL_MODIFIED[is.na(OIL_MODIFIED$Active_Conflicts),"Active_Conflicts"]<-200

OIL_MODIFIED<-OIL_MODIFIED[complete.cases(OIL_MODIFIED),]

# filter year 1973 for no price variation

OIL_MODIFIED <- OIL_MODIFIED %>% filter(Year > 1973)


oil_train <- OIL_MODIFIED[1:504,]

oil_test_6M <- OIL_MODIFIED[505:510,]

oil_test_1Y <- OIL_MODIFIED[505:516,]

oil_test_1.5Y <- OIL_MODIFIED[505:522,]

oil_test_2Y<- OIL_MODIFIED[505:528,]

MDirAcc <- function(actual, predicted, lag=1) {
        return( mean(sign(diff(actual, lag=lag))==sign(diff(predicted, lag=lag))) )
}

RMSE_REL <- function(actual, predicted) {
        
        return(sqrt(mean(((predicted-actual)/actual)^2)))
}

#references

mean(diff(oil_train$Active_Rigs)>0)

sqrt(mean((mean(oil_train$Active_Rigs)-oil_test_2Y$Active_Rigs)^2))

mean(abs(mean(oil_train$Active_Rigs)-oil_test_2Y$Active_Rigs))

#forecasting test to determine if white noise or autocorrelation

OIL_MODIFIED_TS <- OIL_MODIFIED  %>%
        mutate(Oil_Price=ts(Oil_Price,start = 1974,frequency = 12),
               Active_Rigs=ts(Active_Rigs,start = 1974,frequency = 12))

#arima model comparison

arimaauto<-forecast::auto.arima(y=OIL_MODIFIED_TS[1:504,"Active_Rigs"])

forecast::checkresiduals(arimaauto, lag=24)

#metrics in train

(RMSE_ARIMA_tr<-Metrics::rmse(actual = oil_train$Active_Rigs,predicted = arimaauto$fitted))

(MAE_ARIMA_tr<-Metrics::mae(actual = oil_train$Active_Rig,predicted = arimaauto$fitted))

(RRSE_ARIMA_tr<-Metrics::rrse(actual = oil_train$Active_Rig,predicted = arimaauto$fitted))

(DAR_ARIMA_tr<-MDirAcc(actual = oil_train$Active_Rig,predicted = arimaauto$fitted))

RMSE_REL(actual = oil_train$Active_Rig,predicted = arimaauto$fitted)

# 6M ARMA

(RMSE_ARIMA_ts6M<-Metrics::rmse(actual = oil_test_6M$Active_Rigs,predicted = forecast::forecast(arimaauto,h=6)$mean))

(MAE_ARIMA_ts6M<-Metrics::mae(actual = oil_test_6M$Active_Rigs,predicted = forecast::forecast(arimaauto,h=6)$mean))

(RRSE_ARIMA_ts6M<-Metrics::rrse(actual = oil_test_6M$Active_Rigs,predicted = forecast::forecast(arimaauto,h=6)$mean))

(DAR_ARIMA_ts6M<-MDirAcc(actual = oil_test_6M$Active_Rigs,predicted = forecast::forecast(arimaauto,h=6)$mean))

RMSE_REL(actual = oil_test_6M$Active_Rigs,predicted = forecast::forecast(arimaauto,h=6)$mean)


# 1Y ARMA 

(RMSE_ARIMA_ts1Y<-Metrics::rmse(actual = oil_test_1Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=12)$mean))

(MAE_ARIMA_ts1Y<-Metrics::mae(actual = oil_test_1Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=12)$mean))

(RRSE_ARIMA_ts1Y<-Metrics::rrse(actual = oil_test_1Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=12)$mean))

(DAR_ARIMA_ts1Y<-MDirAcc(actual = oil_test_1Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=12)$mean))

RMSE_REL(actual = oil_test_1Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=12)$mean)

#1.5Y ARMA

(RMSE_ARIMA_ts1.5Y<-Metrics::rmse(actual = oil_test_1.5Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=18)$mean))

(MAE_ARIMA_ts1.5Y<-Metrics::mae(actual = oil_test_1.5Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=18)$mean))

(RRSE_ARIMA_ts1.5Y<-Metrics::rrse(actual = oil_test_1.5Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=18)$mean))

(DAR_ARIMA_ts1.5Y<-MDirAcc(actual = oil_test_1.5Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=18)$mean))

RMSE_REL(actual = oil_test_1.5Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=18)$mean)

#2Y metrics

(RMSE_ARIMA_ts2Y<-Metrics::rmse(actual = oil_test_2Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=24)$mean))

(MAE_ARIMA_ts2Y<-Metrics::mae(actual = oil_test_2Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=24)$mean))

(RRSE_ARIMA_ts2Y<-Metrics::rrse(actual = oil_test_2Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=24)$mean))

(DAR_ARIMA_ts2Y<-MDirAcc(actual = oil_test_2Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=24)$mean))

RMSE_REL(actual = oil_test_2Y$Active_Rigs,predicted = forecast::forecast(arimaauto,h=24)$mean)

# Prediction formula for Active Rigs

total_formula_Rigs <-as.formula(Active_Rigs ~ Month +
                                        Oil_Price +
                                        Year +
                                        US_Population +
                                        President_Party +
                                        Senate +
                                        House +
                                        life_exp +
                                        GDP +
                                        Oil_Production +
                                        Oil_Stock +
                                        HDD +
                                        CDD +
                                        Energy_Consumption +
                                        Net_imports_Energy +
                                        Oil_Reserves +
                                        Active_Conflicts +
                                        Eco_Rec +
                                        Season +
                                        Gov_ctrl_type +
                                        Gov_ctrl_party)

# linear model bench mark using caret

set.seed(123)


time_control_Rigs <- trainControl(method = "timeslice",
                             initialWindow = 200,
                             horizon = 24,
                             fixedWindow = TRUE,
                             verboseIter = TRUE,
                             allowParallel = TRUE)

lm_tune_grid <- expand.grid(intercept=FALSE)

tictoc::tic()

lm_model_Rigs <- train(total_formula_Rigs,
                        data = oil_train,
                        tuneGrid = lm_tune_grid,
                        trControl = time_control_Rigs,
                        method  = "lm")

tictoc::toc()

lm_model_Rigs

# in training performace metric metrics

(RMSE_lm_tr<-Metrics::rmse(actual = oil_train$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_train)))

(MAE_lm_tr<-Metrics::mae(actual = oil_train$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_train)))

(RRSE_lm_tr<-Metrics::rrse(actual = oil_train$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_train)))

(DAR_lm_tr<-MDirAcc(actual = oil_train$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_train)))

RMSE_REL(actual = oil_train$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_train))

# 6M test performance metrics

(RMSE_lm_ts6M<-Metrics::rmse(actual = oil_test_6M$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_6M)))

(MAE_lm_ts6M<-Metrics::mae(actual = oil_test_6M$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_6M)))

(RRSE_lm_ts6M<-Metrics::rrse(actual = oil_test_6M$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_6M)))

(DAR_lm_ts6M<-MDirAcc(actual = oil_test_6M$Active_Rigs, predicted = predict(lm_model_Rigs, newdata=oil_test_6M)))

RMSE_REL(actual = oil_test_6M$Active_Rigs, predicted = predict(lm_model_Rigs, newdata=oil_test_6M))

# 1Y test performance metrics

(RMSE_lm_ts1Y<-Metrics::rmse(actual = oil_test_1Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1Y)))

(MAE_lm_ts1Y<-Metrics::mae(actual = oil_test_1Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1Y)))

(RRSE_lm_ts1Y<-Metrics::rrse(actual = oil_test_1Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1Y)))

(DAR_lm_ts1Y<-MDirAcc(actual = oil_test_1Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1Y)))

RMSE_REL(actual = oil_test_1Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1Y))

# 1.5Y test performance metrics

(RMSE_lm_ts1.5Y<-Metrics::rmse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1.5Y)))

(MAE_lm_ts1.5Y<-Metrics::mae(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1.5Y)))

(RRSE_lm_ts1.5Y<-Metrics::rrse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1.5Y)))

(DAR_lm_ts1.5Y<-MDirAcc(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1.5Y)))

RMSE_REL(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_1.5Y))

# 2Y test performance metrics

(RMSE_lm_ts2Y<-Metrics::rmse(actual = oil_test_2Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_2Y)))

(MAE_lm_ts2Y<-Metrics::mae(actual = oil_test_2Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_2Y)))

(RRSE_lm_ts2Y<-Metrics::rrse(actual = oil_test_2Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_2Y)))

(DAR_lm_ts2Y<-MDirAcc(actual = oil_test_2Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_2Y)))

RMSE_REL(actual = oil_test_2Y$Active_Rigs,predicted = predict(lm_model_Rigs, newdata=oil_test_2Y))

# random forest modeling with 10 possible values of mtry-------------------------------------------------------

set.seed(123)

tictoc::tic()

randomF_model_Rigs <- train(total_formula_Rigs,
                              data=oil_train,
                              method="ranger",
                              tuneLength = 5,
                              trControl = time_control_Rigs)

tictoc::toc()

randomF_model_Rigs


# in training performace metric metrics

(RMSE_RF_tr<-Metrics::rmse(actual = oil_train$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_train)))

(MAE_RF_tr<-Metrics::mae(actual = oil_train$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_train)))

(RRSE_RF_tr<-Metrics::rrse(actual = oil_train$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_train)))

(DAR_RF_tr<-MDirAcc(actual = oil_train$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_train)))

RMSE_REL(actual = oil_train$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_train))

# 6M test performance metrics

(RMSE_RF_ts6M<-Metrics::rmse(actual = oil_test_6M$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_6M)))

(MAE_RF_ts6M<-Metrics::mae(actual = oil_test_6M$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_6M)))

(RRSE_RF_ts6M<-Metrics::rrse(actual = oil_test_6M$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_6M)))

(DAR_RF_ts6M<-MDirAcc(actual = oil_test_6M$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_6M)))

RMSE_REL(actual = oil_test_6M$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_6M))

(predict(randomF_model_Rigs, newdata=oil_test_6M)-oil_test_6M$Active_Rigs)/oil_test_6M$Active_Rigs

# 1Y test performance metrics

(RMSE_RF_ts1Y<-Metrics::rmse(actual = oil_test_1Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1Y)))

(MAE_RF_ts1Y<-Metrics::mae(actual = oil_test_1Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1Y)))

(RRSE_RF_ts1Y<-Metrics::rrse(actual = oil_test_1Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1Y)))

(DAR_RF_ts1Y<-MDirAcc(actual = oil_test_1Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1Y)))

RMSE_REL(actual = oil_test_1Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1Y))

# 1.5Y test performance metrics

(RMSE_RF_ts1.5Y<-Metrics::rmse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1.5Y)))

(MAE_RF_ts1.5Y<-Metrics::mae(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1.5Y)))

(RRSE_RF_ts1.5Y<-Metrics::rrse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1.5Y)))

(DAR_RF_ts1.5Y<-MDirAcc(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1.5Y)))

RMSE_REL(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_1.5Y))

# 2Y test performance metrics

(RMSE_RF_ts2Y<-Metrics::rmse(actual = oil_test_2Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_2Y)))

(MAE_RF_ts2Y<-Metrics::mae(actual = oil_test_2Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_2Y)))

(RRSE_RF_ts2Y<-Metrics::rrse(actual = oil_test_2Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_2Y)))

(DAR_RF_ts2Y<-MDirAcc(actual = oil_test_2Y$Active_Rigs,predicted = predict(randomF_model_Rigs, newdata=oil_test_2Y)))



# Xgboost benchmark----------------------------------------------------------------------------------------

trainX_Rigs<- data.matrix(select(oil_train,-Active_Rigs,-Date))

set.seed(123)

tunegrid_xg <- expand.grid(
        nrounds = seq(from = 100, to = 2000, by = 100),
        max_depth = 7,
        eta = 0.1,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1)

tictoc::tic()

XGboost_model_Rigs <- train(x=trainX_Rigs,
                            y=oil_train$Active_Rigs,
                            trControl = time_control_Rigs,
                            method = "xgbTree",
                            tuneGrid = tunegrid_xg,
                            verbose = TRUE)


tictoc::toc()

XGboost_model_Rigs

# in training performace metric metrics

(RMSE_XG_tr<-Metrics::rmse(actual = oil_train$Active_Rigs,
                           predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_train))))

(MAE_XG_tr<-Metrics::mae(actual = oil_train$Active_Rigs,
                         predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_train))))

(RRSE_XG_tr<-Metrics::rrse(actual = oil_train$Active_Rigs,
                           predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_train))))

(DAR_XG_tr<-MDirAcc(actual = oil_train$Active_Rigs,
                    predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_train))))

# 6M test performance metrics

(RMSE_XG_ts6M<-Metrics::rmse(actual = oil_test_6M$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_6M))))

(MAE_XG_ts6M<-Metrics::mae(actual = oil_test_6M$Active_Rigs,
                           predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_6M))))

(RRSE_XG_ts6M<-Metrics::rrse(actual = oil_test_6M$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_6M))))

(DAR_XG_ts6M<-MDirAcc(actual = oil_test_6M$Active_Rigs,
                      predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_6M))))

# 1Y test performance metrics

(RMSE_XG_ts1Y<-Metrics::rmse(actual = oil_test_1Y$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1Y))))

(MAE_XG_ts1Y<-Metrics::mae(actual = oil_test_1Y$Active_Rigs,
                           predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1Y))))

(RRSE_XG_ts1Y<-Metrics::rrse(actual = oil_test_1Y$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1Y))))

(DAR_XG_ts1Y<-MDirAcc(actual = oil_test_1Y$Active_Rigs,
                      predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1Y))))

# 1.5Y test performance metrics

(RMSE_XG_ts1.5Y<-Metrics::rmse(actual = oil_test_1.5Y$Active_Rigs,
                               predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1.5Y))))

(MAE_XG_ts1.5Y<-Metrics::mae(actual = oil_test_1.5Y$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1.5Y))))

(RRSE_XG_ts1.5Y<-Metrics::rrse(actual = oil_test_1.5Y$Active_Rigs,
                               predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1.5Y))))

(DAR_XG_ts1.5Y<-MDirAcc(actual = oil_test_1.5Y$Active_Rigs,
                        predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_1.5Y))))

# 2Y test performance metrics

(RMSE_XG_ts2Y<-Metrics::rmse(actual = oil_test_2Y$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_2Y))))

(MAE_XG_ts2Y<-Metrics::mae(actual = oil_test_2Y$Active_Rigs,
                           predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_2Y))))

(RRSE_XG_ts2Y<-Metrics::rrse(actual = oil_test_2Y$Active_Rigs,
                             predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_2Y))))

(DAR_XG_ts2Y<-MDirAcc(actual = oil_test_2Y$Active_Rigs,
                      predicted = predict(XGboost_model_Rigs, newdata=data.matrix(oil_test_2Y))))


# Support Vector Machine model--------------------------------------------------------------------------------

set.seed(123)

tictoc::tic()

SVMgrid <- expand.grid(sigma = seq(0.005,0.02,by=0.005), C = c(100,200,300,400))

SVM_model_Rigs <- train(total_formula_Rigs,
                         data=oil_train,
                         method="svmRadial",
                         tuneGrid = SVMgrid ,
                         trControl = time_control)

tictoc::toc()

SVM_model_Rigs

SVMgrid_LM <- expand.grid(C = c(0.01,0.05,0.1))

tictoc::tic()

SVM_model_Rigs_Lin<-train(total_formula_Rigs,
                          data=oil_train,
                          method="svmLinear",
                          tuneGrid = SVMgrid_LM ,
                          trControl = time_control)

tictoc::toc()

SVM_model_Rigs_Lin

SVMgrid_poly <- expand.grid(degree=c(1),
                            C = c(0.01,0.025,0.05),
                            scale=c(0.001))

tictoc::tic()

SVM_model_Rigs_Poly<-train(total_formula_Rigs,
                          data=oil_train,
                          method="svmPoly",
                          tuneGrid = SVMgrid_poly ,
                          trControl = time_control)

tictoc::toc()

SVM_model_Rigs_Poly

# in training performace metric metrics for SVM-RBF

(RMSE_SVM_tr<-Metrics::rmse(actual = oil_train$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_train)))

(MAE_SVM_tr<-Metrics::mae(actual = oil_train$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_train)))

(RRSE_SVM_tr<-Metrics::rrse(actual = oil_train$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_train)))

(DAR_SVM_tr<-MDirAcc(actual = oil_train$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_train)))

# 6M test performance metrics

(RMSE_SVM_ts6M<-Metrics::rmse(actual = oil_test_6M$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_6M)))

(MAE_SVM_ts6M<-Metrics::mae(actual = oil_test_6M$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_6M)))

(RRSE_SVM_ts6M<-Metrics::rrse(actual = oil_test_6M$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_6M)))

(DAR_SVM_ts6M<-MDirAcc(actual = oil_test_6M$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_6M)))

# 1Y test performance metrics

(RMSE_SVM_ts1Y<-Metrics::rmse(actual = oil_test_1Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1Y)))

(MAE_SVM_ts1Y<-Metrics::mae(actual = oil_test_1Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1Y)))

(RRSE_SVM_ts1Y<-Metrics::rrse(actual = oil_test_1Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1Y)))

(DAR_SVM_ts1Y<-MDirAcc(actual = oil_test_1Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1Y)))

# 1.5Y test performance metrics

(RMSE_SVM_ts1.5Y<-Metrics::rmse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1.5Y)))

(MAE_SVM_ts1.5Y<-Metrics::mae(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1.5Y)))

(RRSE_SVM_ts1.5Y<-Metrics::rrse(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1.5Y)))

(DAR_SVM_ts1.5Y<-MDirAcc(actual = oil_test_1.5Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_1.5Y)))

# 2Y test performance metrics

(RMSE_SVM_ts2Y<-Metrics::rmse(actual = oil_test_2Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_2Y)))

(MAE_SVM_ts2Y<-Metrics::mae(actual = oil_test_2Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_2Y)))

(RRSE_SVM_ts2Y<-Metrics::rrse(actual = oil_test_2Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_2Y)))

(DAR_SVM_ts2Y<-MDirAcc(actual = oil_test_2Y$Active_Rigs,predicted = predict(SVM_model_Rigs, newdata=oil_test_2Y)))

# Model performance metrics at different time horizons for evaluation


TR<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA"),
               RMSE=c(RMSE_lm_tr,RMSE_RF_tr,RMSE_XG_tr,RMSE_SVM_tr,RMSE_ARIMA_tr),
               MAE=c(MAE_lm_tr,MAE_RF_tr,MAE_XG_tr,MAE_SVM_tr,MAE_ARIMA_tr),
               RRSE=c(RRSE_lm_tr,RRSE_RF_tr,RRSE_XG_tr,RRSE_SVM_tr,RRSE_ARIMA_tr),
               DAR=c(DAR_lm_tr,DAR_RF_tr,DAR_XG_tr,DAR_SVM_tr,DAR_ARIMA_tr)) 



half_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA"),
                      RMSE=c(RMSE_lm_ts6M,RMSE_RF_ts6M,RMSE_XG_ts6M,RMSE_SVM_ts6M,RMSE_ARIMA_ts6M),
                      MAE=c(MAE_lm_ts6M,MAE_RF_ts6M,MAE_XG_ts6M,MAE_SVM_ts6M,MAE_ARIMA_ts6M),
                      RRSE=c(RRSE_lm_ts6M,RRSE_RF_ts6M,RRSE_XG_ts6M,RRSE_SVM_ts6M,RRSE_ARIMA_ts6M),
                      DAR=c(DAR_lm_ts6M,DAR_RF_ts6M,DAR_XG_ts6M,DAR_SVM_ts6M,DAR_ARIMA_ts6M)) 


one_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA"),
                     RMSE=c(RMSE_lm_ts1Y,RMSE_RF_ts1Y,RMSE_XG_ts1Y,RMSE_SVM_ts1Y,RMSE_ARIMA_ts1Y),
                     MAE=c(MAE_lm_ts1Y,MAE_RF_ts1Y,MAE_XG_ts1Y,MAE_SVM_ts1Y,MAE_ARIMA_ts1Y),
                     RRSE=c(RRSE_lm_ts1Y,RRSE_RF_ts1Y,RRSE_XG_ts1Y,RRSE_SVM_ts1Y,RRSE_ARIMA_ts1Y),
                     DAR=c(DAR_lm_ts1Y,DAR_RF_ts1Y,DAR_XG_ts1Y,DAR_SVM_ts1Y,DAR_ARIMA_ts1Y)) 


one_half_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA"),
                          RMSE=c(RMSE_lm_ts1.5Y,RMSE_RF_ts1.5Y,RMSE_XG_ts1.5Y,RMSE_SVM_ts1.5Y,RMSE_ARIMA_ts1.5Y),
                          MAE=c(MAE_lm_ts1.5Y,MAE_RF_ts1.5Y,MAE_XG_ts1.5Y,MAE_SVM_ts1.5Y,MAE_ARIMA_ts1.5Y),
                          RRSE=c(RRSE_lm_ts1.5Y,RRSE_RF_ts1.5Y,RRSE_XG_ts1.5Y,RRSE_SVM_ts1.5Y,RRSE_ARIMA_ts1.5Y),
                          DAR=c(DAR_lm_ts1.5Y,DAR_RF_ts1.5Y,DAR_XG_ts1.5Y,DAR_SVM_ts1.5Y,DAR_ARIMA_ts1.5Y)) 


two_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA"),
                     RMSE=c(RMSE_lm_ts2Y,RMSE_RF_ts2Y,RMSE_XG_ts2Y,RMSE_SVM_ts2Y,RMSE_ARIMA_ts2Y),
                     MAE=c(MAE_lm_ts2Y,MAE_RF_ts2Y,MAE_XG_ts2Y,MAE_SVM_ts2Y,MAE_ARIMA_ts2Y),
                     RRSE=c(RRSE_lm_ts2Y,RRSE_RF_ts2Y,RRSE_XG_ts2Y,RRSE_SVM_ts2Y,RRSE_ARIMA_ts2Y),
                     DAR=c(DAR_lm_ts2Y,DAR_RF_ts2Y,DAR_XG_ts2Y,DAR_SVM_ts2Y,DAR_ARIMA_ts2Y)) 

results<-list(TR,half_year,one_year,one_half_year,two_year)

bind_rows(results,.id="Horizon") %>% mutate(Horizon = case_when(Horizon == 1 ~ "0 Months",
                                                                Horizon == 2 ~ "6 Months",
                                                                Horizon == 3 ~ "12 Months",
                                                                Horizon == 4 ~ "18 Months",
                                                                Horizon == 5 ~ "24 Months")) %>% 
        select(-MAE,-RRSE) %>%
        knitr::kable()



bind_rows(results,.id="Horizon") %>% mutate(Horizon = case_when(Horizon == 1 ~ 0,
                                                                Horizon == 2 ~ 6,
                                                                Horizon == 3 ~ 12,
                                                                Horizon == 4 ~ 18,
                                                                Horizon == 5 ~ 24)) %>%
        ggplot(.,aes(x=factor(Horizon),y=DAR,color=models,shape=models))+geom_point(size=5)+
        geom_hline(yintercept = mean(diff(oil_train$Active_Rigs)>0),color="red",linetype="dashed")+
        labs(title = "ML Oil Rig count Models DAR",
             caption = "Red line corresponds to the DAR of a model that assumes only Growing Rig Activity",
             x="Time Horizon",
             y="Directional Accuracy Ration")+ggthemes::theme_tufte()


bind_rows(results,.id="Horizon") %>% mutate(Horizon = case_when(Horizon == 1 ~ 0,
                                                                Horizon == 2 ~ 6,
                                                                Horizon == 3 ~ 12,
                                                                Horizon == 4 ~ 18,
                                                                Horizon == 5 ~ 24)) %>%
        ggplot(.,aes(x=factor(Horizon),y=RMSE,color=models,shape=models))+geom_point(size=4)+
        geom_hline(yintercept = sqrt(mean((mean(oil_train$Active_Rigs)-oil_test_2Y$Active_Rigs)^2)),color="red",linetype="dashed")+
        geom_hline(yintercept = sqrt(mean((oil_train$Active_Rigs[504]-oil_test_2Y$Oil_Price)^2)),color="black",linetype="dashed")+
        labs(title = "ML Rig Count Models RMSE",
             caption = "Red line corresponds to the RMSE of predicting future
             Rig count using the mean Rig count in the training as Model,the black line corresponds to last registered value on training set",
             x="Time Horizon",
             y="Root Mean Squared Error")+ggthemes::theme_tufte()




sqrt(mean((oil_train$Active_Rigs[504]-oil_test_2Y$Active_Rigs)^2))
mean(abs(oil_train$Active_Rigs[504]-oil_test_2Y$Active_Rigs))

