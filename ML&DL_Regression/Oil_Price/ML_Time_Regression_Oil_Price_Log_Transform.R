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

setwd("~/Data Science/Tesis MBA/Machine learning time slice regression Oil Price")

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

# Evaluating response variable

OIL_MODIFIED %>% ggplot(.,aes(x=Oil_Price))+
        geom_histogram(binwidth = 10,fill="darkgreen")+
        ggthemes::theme_tufte()+
        labs(title = "Oil Price Histogram",
             x="Oil Price ($ USD")

OIL_MODIFIED %>% ggplot(.,aes(x=log(Oil_Price)))+geom_density()

#determine the best lambda for a box plot

forecast::BoxCox.lambda(OIL_MODIFIED$Oil_Price)

# transforming Oil price to Log oil price

OIL_MODIFIED<-OIL_MODIFIED %>% mutate(Oil_Price = log(Oil_Price))

oil_train <- OIL_MODIFIED[1:504,]

oil_test_6M <- OIL_MODIFIED[505:510,]

oil_test_1Y <- OIL_MODIFIED[505:516,]

oil_test_1.5Y <- OIL_MODIFIED[505:522,]

oil_test_2Y<- OIL_MODIFIED[505:528,]

MDirAcc <- function(actual, predicted, lag=1) {
        return( mean(sign(diff(actual, lag=lag))==sign(diff(predicted, lag=lag))) )
}


#forecasting test to determine if white noise or autocorrelation

OIL_MODIFIED_TS <- OIL_MODIFIED  %>%
        mutate(Oil_Price=ts(Oil_Price,start = 1974,frequency = 12),
               Active_Rigs=ts(Active_Rigs,start = 1974,frequency = 12))

# Ljung-Box test for price and diff ACF analysis


forecast::Acf(diff(OIL_MODIFIED_TS$Oil_Price),main="Oil Diff Price ACF Plot")

Box.test(diff(OIL_MODIFIED_TS$Oil_Price),lag = 1,type = "Ljung")


#arima model comparison

arimaauto<-forecast::auto.arima(y=OIL_MODIFIED_TS[1:504,"Oil_Price"])

summary(arimaauto)

forecast::checkresiduals(arimaauto)

#metrics in train

(RMSE_ARIMA_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price),predicted = exp(arimaauto$fitted)))

(MAE_ARIMA_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price),predicted = exp(arimaauto$fitted)))

(RRSE_ARIMA_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price),predicted = exp(arimaauto$fitted)))

(DAR_ARIMA_tr<-MDirAcc(actual = exp(oil_train$Oil_Price),predicted = exp(arimaauto$fitted)))

# 6Months

(RMSE_ARIMA_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=6)$mean)))

(MAE_ARIMA_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=6)$mean)))

(RRSE_ARIMA_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=6)$mean)))

(DAR_ARIMA_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=6)$mean)))

# 1Y ARMA 

(RMSE_ARIMA_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=12)$mean)))

(MAE_ARIMA_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=12)$mean)))

(RRSE_ARIMA_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=12)$mean)))

(DAR_ARIMA_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=12)$mean)))

#1.5Y ARMA

(RMSE_ARIMA_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=18)$mean)))

(MAE_ARIMA_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=18)$mean)))

(RRSE_ARIMA_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=18)$mean)))

(DAR_ARIMA_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=18)$mean)))

#2Y metrics

(RMSE_ARIMA_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=24)$mean)))

(MAE_ARIMA_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=24)$mean)))

(RRSE_ARIMA_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=24)$mean)))

(DAR_ARIMA_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(arimaauto,h=24)$mean)))


(RMSE_BASELINE<-sqrt(mean((mean(exp(oil_train$Oil_Price))-exp(oil_test_2Y$Oil_Price))^2)))

(DAR_BASELINE<-mean(diff(oil_train$Oil_Price)>0)*100)

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_ARIMA_tr,RMSE_ARIMA_ts6M,RMSE_ARIMA_ts1Y,RMSE_ARIMA_ts1.5Y,RMSE_ARIMA_ts2Y),
           DAR= c(DAR_ARIMA_tr,DAR_ARIMA_ts6M,DAR_ARIMA_ts1Y,DAR_ARIMA_ts1.5Y,DAR_ARIMA_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_ARIMA_tr,RMSE_ARIMA_ts6M,RMSE_ARIMA_ts1Y,RMSE_ARIMA_ts1.5Y,RMSE_ARIMA_ts2Y),
           DAR= c(DAR_ARIMA_tr,DAR_ARIMA_ts6M,DAR_ARIMA_ts1Y,DAR_ARIMA_ts1.5Y,DAR_ARIMA_ts2Y)) %>% 
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        labs(title = "Oil Price ARIMA(2,1,1) Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_ARIMA_tr,RMSE_ARIMA_ts6M,RMSE_ARIMA_ts1Y,RMSE_ARIMA_ts1.5Y,RMSE_ARIMA_ts2Y),
           DAR= c(DAR_ARIMA_tr,DAR_ARIMA_ts6M,DAR_ARIMA_ts1Y,DAR_ARIMA_ts1.5Y,DAR_ARIMA_ts2Y)) %>% 
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        labs(title = "Oil Price ARIMA(2,1,1) Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()



# Prediction formula for oil price

total_formula_price<-as.formula(Oil_Price ~ Month +
                                        Year +
                                        Active_Rigs +
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


# function to predict directonal response accuracy 




# GLMNET model bench mark using caret

set.seed(123)


time_control <- trainControl(method = "timeslice",
                             initialWindow = 48,
                             horizon = 24,
                             fixedWindow = TRUE,
                             verboseIter = TRUE,
                             allowParallel = TRUE)

lm_tune_grid <- expand.grid(intercept=FALSE)


tictoc::tic()

lm_model_price <- train(total_formula_price,
                        data = oil_train,
                        tuneGrid = lm_tune_grid,
                        trControl = time_control,
                        method  = "lm")

tictoc::toc()

lm_model_price

summary(lm_model_price) 

# in training performace metric metrics

(RMSE_lm_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_train))))

(MAE_lm_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_train))))

(RRSE_lm_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_train))))

(DAR_lm_tr<-MDirAcc(actual = exp(oil_train$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_train))))

# 6M test performance metrics

(RMSE_lm_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_6M))))

(MAE_lm_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_6M))))

(RRSE_lm_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_6M))))

(DAR_lm_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_6M))))

# 1Y test performance metrics

(RMSE_lm_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1Y))))

(MAE_lm_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1Y))))

(RRSE_lm_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1Y))))

(DAR_lm_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1Y))))

# 1.5Y test performance metrics

(RMSE_lm_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1.5Y))))

(MAE_lm_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1.5Y))))

(RRSE_lm_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1.5Y))))

(DAR_lm_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_1.5Y))))

# 2Y test performance metrics

(RMSE_lm_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_2Y))))

(MAE_lm_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_2Y))))

(RRSE_lm_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_2Y))))

(DAR_lm_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(lm_model_price, newdata=oil_test_2Y))))

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_lm_tr,RMSE_lm_ts6M,RMSE_lm_ts1Y,RMSE_lm_ts1.5Y,RMSE_lm_ts2Y),
           DAR= c(DAR_lm_tr,DAR_lm_ts6M,DAR_lm_ts1Y,DAR_lm_ts1.5Y,DAR_lm_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_lm_tr,RMSE_lm_ts6M,RMSE_lm_ts1Y,RMSE_lm_ts1.5Y,RMSE_lm_ts2Y),
           DAR= c(DAR_lm_tr,DAR_lm_ts6M,DAR_lm_ts1Y,DAR_lm_ts1.5Y,DAR_lm_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Linear Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_lm_tr,RMSE_lm_ts6M,RMSE_lm_ts1Y,RMSE_lm_ts1.5Y,RMSE_lm_ts2Y),
           DAR= c(DAR_lm_tr,DAR_lm_ts6M,DAR_lm_ts1Y,DAR_lm_ts1.5Y,DAR_lm_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Linear Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()


# random forest modeling with 10 possible values of mtry-------------------------------------------------------

set.seed(123)

tictoc::tic()

randomF_model_price <- train(total_formula_price,
                             data=oil_train,
                             method="ranger",
                             tuneLength = 10,
                             trControl = time_control)

tictoc::toc()
                                  
randomF_model_price

set.seed(123)

time_control2 <- trainControl(method = "timeslice",
                             initialWindow = 48,
                             horizon = 24,
                             fixedWindow = TRUE,
                             verboseIter = TRUE,
                             allowParallel = TRUE)


tictoc::tic()

randomF_model_price2 <- train(total_formula_price,
                             data=oil_train,
                             method="ranger",
                             tuneLength = 5,
                             trControl = time_control2)

tictoc::toc()

randomF_model_price2

plot(randomF_model_price2)

set.seed(123)

time_control3 <- trainControl(method = "timeslice",
                              initialWindow = 24,
                              horizon = 24,
                              fixedWindow = TRUE,
                              verboseIter = TRUE,
                              allowParallel = TRUE)


tictoc::tic()

randomF_model_price3 <- train(total_formula_price,
                              data=oil_train,
                              method="ranger",
                              tuneLength = 5,
                              trControl = time_control3)

tictoc::toc()

randomF_model_price3

# in training performace metric metrics

(RMSE_RF_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_train))))

(MAE_RF_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_train))))

(RRSE_RF_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_train))))

(DAR_RF_tr<-MDirAcc(actual = exp(oil_train$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_train))))

# 6M test performance metrics

(RMSE_RF_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_6M))))

(MAE_RF_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_6M))))

(RRSE_RF_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_6M))))

(DAR_RF_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_6M))))

# 1Y test performance metrics

(RMSE_RF_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1Y))))

(MAE_RF_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1Y))))

(RRSE_RF_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1Y))))

(DAR_RF_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1Y))))

# 1.5Y test performance metrics

(RMSE_RF_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1.5Y))))

(MAE_RF_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1.5Y))))

(RRSE_RF_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1.5Y))))

(DAR_RF_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_1.5Y))))


# 2Y test performance metrics

(RMSE_RF_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_2Y))))

(MAE_RF_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_2Y))))

(RRSE_RF_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_2Y))))

(DAR_RF_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(randomF_model_price2, newdata=oil_test_2Y))))


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_RF_tr,RMSE_RF_ts6M,RMSE_RF_ts1Y,RMSE_RF_ts1.5Y,RMSE_RF_ts2Y),
           DAR= c(DAR_RF_tr,DAR_RF_ts6M,DAR_RF_ts1Y,DAR_RF_ts1.5Y,DAR_RF_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_RF_tr,RMSE_RF_ts6M,RMSE_RF_ts1Y,RMSE_RF_ts1.5Y,RMSE_RF_ts2Y),
           DAR= c(DAR_RF_tr,DAR_RF_ts6M,DAR_RF_ts1Y,DAR_RF_ts1.5Y,DAR_RF_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Random Forest Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_RF_tr,RMSE_RF_ts6M,RMSE_RF_ts1Y,RMSE_RF_ts1.5Y,RMSE_RF_ts2Y),
           DAR= c(DAR_RF_tr,DAR_RF_ts6M,DAR_RF_ts1Y,DAR_RF_ts1.5Y,DAR_RF_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Random Forest Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()



# Xgboost benchmark----------------------------------------------------------------------------------------

trainX<- data.matrix(select(oil_train,-Oil_Price,-Date))

set.seed(123)

tunegrid_xg_default <- expand.grid(
        nrounds = 100,
        max_depth = 6,
        eta = 0.3,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1)

tictoc::tic()

XGboost_model_price_base <- train(x=trainX,
                                  y=oil_train$Oil_Price,
                                  trControl = time_control,
                                  method = "xgbTree",
                                  tuneGrid=tunegrid_xg_default,
                                  verbose = TRUE)

tictoc::toc()

XGboost_model_price_base

tictoc::tic()

XGboost_model_price_2 <- train(x=trainX,
                                  y=oil_train$Oil_Price,
                                  trControl = time_control2,
                                  method = "xgbTree",
                                  tuneGrid=tunegrid_xg_default,
                                  verbose = TRUE)

tictoc::toc()

XGboost_model_price_2

tictoc::tic()

XGboost_model_price_3 <- train(x=trainX,
                               y=oil_train$Oil_Price,
                               trControl = time_control3,
                               method = "xgbTree",
                               tuneGrid=tunegrid_xg_default,
                               verbose = TRUE)

tictoc::toc()

XGboost_model_price_3

# in training performace metric metrics

(RMSE_XG_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price),
              predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_train)))))

(MAE_XG_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price),
                         predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_train)))))

(RRSE_XG_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price),
                           predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_train)))))

(DAR_XG_tr<-MDirAcc(actual = exp(oil_train$Oil_Price),
                    predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_train)))))

# 6M test performance metrics

(RMSE_XG_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),
              predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_6M)))))

(MAE_XG_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),
                           predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_6M)))))

(RRSE_XG_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),
                             predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_6M)))))

(DAR_XG_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),
                      predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_6M)))))


# 1Y test performance metrics

(RMSE_XG_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),
              predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1Y)))))

(MAE_XG_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),
                           predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1Y)))))

(RRSE_XG_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),
        predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1Y)))))

(DAR_XG_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),
                      predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1Y)))))

# 1.5Y test performance metrics

(RMSE_XG_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),
                            predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1.5Y)))))

(MAE_XG_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),
                             predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1.5Y)))))

(RRSE_XG_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),
                               predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1.5Y)))))

(DAR_XG_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),
                        predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_1.5Y)))))

# 2Y test performance metrics

(RMSE_XG_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),
                              predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_2Y)))))

(MAE_XG_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),
                           predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_2Y)))))

(RRSE_XG_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),
                             predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_2Y)))))

(DAR_XG_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),
                      predicted = exp(predict(XGboost_model_price_2, newdata=data.matrix(oil_test_2Y)))))


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_XG_tr,RMSE_XG_ts6M,RMSE_XG_ts1Y,RMSE_XG_ts1.5Y,RMSE_XG_ts2Y),
           DAR= c(DAR_XG_tr,DAR_XG_ts6M,DAR_XG_ts1Y,DAR_XG_ts1.5Y,DAR_XG_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_XG_tr,RMSE_XG_ts6M,RMSE_XG_ts1Y,RMSE_XG_ts1.5Y,RMSE_XG_ts2Y),
           DAR= c(DAR_XG_tr,DAR_XG_ts6M,DAR_XG_ts1Y,DAR_XG_ts1.5Y,DAR_XG_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price XGboost Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_XG_tr,RMSE_XG_ts6M,RMSE_XG_ts1Y,RMSE_XG_ts1.5Y,RMSE_XG_ts2Y),
           DAR= c(DAR_XG_tr,DAR_XG_ts6M,DAR_XG_ts1Y,DAR_XG_ts1.5Y,DAR_XG_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price XGboost Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()


# Support Vector Machine model--------------------------------------------------------------------------------

set.seed(123)

tictoc::tic()

SVMgrid <- expand.grid(sigma = seq(0.005,0.02,by=0.005), C = c(100,200,300,400))

SVM_model_price <- train(total_formula_price,
                             data=oil_train,
                             method="svmRadial",
                             tuneGrid = SVMgrid ,
                             trControl = time_control)

tictoc::toc()

tictoc::tic()

SVM_model_price2 <- train(total_formula_price,
                         data=oil_train,
                         method="svmRadial",
                         tuneGrid = SVMgrid ,
                         trControl = time_control2)

tictoc::toc()

tuneplot <- function(x, probs = .90) {
        ggplot(x) +
                coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
                theme_bw()
}

tuneplot(SVM_model_price2)


# in training performace metric metrics

(RMSE_SVM_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_train))))

(MAE_SVM_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_train))))

(RRSE_SVM_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_train))))

(DAR_SVM_tr<-MDirAcc(actual = exp(oil_train$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_train))))

# 6M test performance metrics

(RMSE_SVM_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_6M))))

(MAE_SVM_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_6M))))

(RRSE_SVM_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_6M))))

(DAR_SVM_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_6M))))

# 1Y test performance metrics

(RMSE_SVM_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1Y))))

(MAE_SVM_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1Y))))

(RRSE_SVM_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1Y))))

(DAR_SVM_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1Y))))

# 1.5Y test performance metrics

(RMSE_SVM_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1.5Y))))

(MAE_SVM_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1.5Y))))

(RRSE_SVM_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1.5Y))))

(DAR_SVM_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_1.5Y))))

# 2Y test performance metrics

(RMSE_SVM_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_2Y))))

(MAE_SVM_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_2Y))))

(RRSE_SVM_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_2Y))))

(DAR_SVM_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(predict(SVM_model_price2, newdata=oil_test_2Y))))

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_SVM_tr,RMSE_SVM_ts6M,RMSE_SVM_ts1Y,RMSE_SVM_ts1.5Y,RMSE_SVM_ts2Y),
           DAR= c(DAR_SVM_tr,DAR_SVM_ts6M,DAR_SVM_ts1Y,DAR_SVM_ts1.5Y,DAR_SVM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_SVM_tr,RMSE_SVM_ts6M,RMSE_SVM_ts1Y,RMSE_SVM_ts1.5Y,RMSE_SVM_ts2Y),
           DAR= c(DAR_SVM_tr,DAR_SVM_ts6M,DAR_SVM_ts1Y,DAR_SVM_ts1.5Y,DAR_SVM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price SVM-RBF Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_SVM_tr,RMSE_SVM_ts6M,RMSE_SVM_ts1Y,RMSE_SVM_ts1.5Y,RMSE_SVM_ts2Y),
           DAR= c(DAR_SVM_tr,DAR_SVM_ts6M,DAR_SVM_ts1Y,DAR_SVM_ts1.5Y,DAR_SVM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price SVM-RBF Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()

#Neutal net work test---------------------------------------------------------------------------------------------------------



trainX_DL<-trainX

dimnames(trainX_DL) <- NULL

m<-apply(trainX_DL, 2, mean)

std<-apply(trainX_DL, 2, sd)

trainX_DL<-scale(trainX_DL,center = m,scale = std)

target<-data.matrix(oil_train$Oil_Price)

set.seed(123)

model_nnet_price <- keras_model_sequential() 

model_nnet_price %>% 
        layer_flatten(input_shape = dim(trainX_DL)[[2]]) %>%
        layer_dense(units = 128,activation = "relu",kernel_regularizer = regularizer_l2(0.01)) %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 1)

model_nnet_price

model_nnet_price %>% compile(
        optimizer = "adam",
        loss = "mse",
        metrics = c("mae")
)

net <- model_nnet_price %>%
        fit(trainX_DL,
            target,
            epochs = 20,
            batch_size=1
        )

plot(net)+ggthemes::theme_tufte()+labs(title = "Neural Network Train process")

# in train predictions

pred_train<-model_nnet_price %>% predict(trainX_DL) %>% exp()

(RMSE_NNET_tr<-Metrics::rmse(actual = exp(target),predicted = pred_train))

(MAE_NNET_tr<-Metrics::mae(actual = exp(target),predicted = pred_train))

(RRSE_NNET_tr<-Metrics::rrse(actual = exp(target),predicted = pred_train))

(DAR_NNET_tr<-MDirAcc(actual = exp(target),predicted = pred_train))

#6M metrics

oil_test_6M_DL<- select(oil_test_6M,-Oil_Price,-Date) %>% data.matrix()

dimnames(oil_test_6M_DL) <- NULL

oil_test_6M_DL<-scale(oil_test_6M_DL,center = m,scale = std)

pred6M<-model_nnet_price %>% predict(oil_test_6M_DL) %>% exp()

(RMSE_NNET_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = pred6M))

(MAE_NNET_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = pred6M))

(RRSE_NNET_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = pred6M))

(DAR_NNET_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = pred6M))

#1Y metrics

oil_test_1Y_DL<- select(oil_test_1Y,-Oil_Price,-Date) %>% data.matrix()

dimnames(oil_test_1Y_DL) <- NULL

oil_test_1Y_DL<-scale(oil_test_1Y_DL,center = m,scale = std)

pred1Y<-model_nnet_price %>% predict(oil_test_1Y_DL) %>% exp()

(RMSE_NNET_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = pred1Y))

(MAE_NNET_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = pred1Y))

(RRSE_NNET_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = pred1Y))

(DAR_NNET_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = pred1Y))

#1.5Y metrics

oil_test_1.5Y_DL<- select(oil_test_1.5Y,-Oil_Price,-Date) %>% data.matrix()

dimnames(oil_test_1.5Y_DL) <- NULL

oil_test_1.5Y_DL<-scale(oil_test_1.5Y_DL,center = m,scale = std)

pred1.5Y<-model_nnet_price %>% predict(oil_test_1.5Y_DL) %>% exp()

(RMSE_NNET_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = pred1.5Y))

(MAE_NNET_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = pred1.5Y))

(RRSE_NNET_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = pred1.5Y))

(DAR_NNET_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = pred1.5Y))

#2Y metrics

oil_test_2Y_DL<- select(oil_test_2Y,-Oil_Price,-Date) %>% data.matrix()

dimnames(oil_test_2Y_DL) <- NULL

oil_test_2Y_DL<-scale(oil_test_2Y_DL,center = m,scale = std)

pred2Y<-model_nnet_price %>% predict(oil_test_2Y_DL) %>% exp()

(RMSE_NNET_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = pred2Y))

(MAE_NNET_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = pred2Y))

(RRSE_NNET_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = pred2Y))

(DAR_NNET_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = pred2Y))

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNET_tr,RMSE_NNET_ts6M,RMSE_NNET_ts1Y,RMSE_NNET_ts1.5Y,RMSE_NNET_ts2Y),
           DAR= c(DAR_NNET_tr,DAR_NNET_ts6M,DAR_NNET_ts1Y,DAR_NNET_ts1.5Y,DAR_NNET_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNET_tr,RMSE_NNET_ts6M,RMSE_NNET_ts1Y,RMSE_NNET_ts1.5Y,RMSE_NNET_ts2Y),
           DAR= c(DAR_NNET_tr,DAR_NNET_ts6M,DAR_NNET_ts1Y,DAR_NNET_ts1.5Y,DAR_NNET_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% 
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Neural Net MLP Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNET_tr,RMSE_NNET_ts6M,RMSE_NNET_ts1Y,RMSE_NNET_ts1.5Y,RMSE_NNET_ts2Y),
           DAR= c(DAR_NNET_tr,DAR_NNET_ts6M,DAR_NNET_ts1Y,DAR_NNET_ts1.5Y,DAR_NNET_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% 
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Neural Net MLP Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()

#---------------------------------- Recurrent NNET LSTM --------------------------------------------------------------------

Oil_LSTM <- select(OIL_MODIFIED,-Date,-Year,-Month)

Oil_LSTM <- Oil_LSTM %>% mutate_if(is.factor,as.numeric)

m2<-apply(Oil_LSTM, 2, mean)

m2[10]

std2<-apply(Oil_LSTM, 2, sd)

std2[10]

Oil_LSTM<-scale(Oil_LSTM,center = m2,scale = std2)

datalags = 12
train = Oil_LSTM[seq(480 + datalags), ]
test = Oil_LSTM[480 + datalags + seq(24+ datalags), ]
batch.size = 4

x.train = array(data = lag(cbind(train[,1], 
                                 train[,2],
                                 train[,3],
                                 train[,4],
                                 train[,5],
                                 train[,6],
                                 train[,7],
                                 train[,8],
                                 train[,9],
                                 train[,10],
                                 train[,11],
                                 train[,12],
                                 train[,13],
                                 train[,14],
                                 train[,15],
                                 train[,16],
                                 train[,17],
                                 train[,18],
                                 train[,19],
                                 train[,20]), datalags)[-(1:datalags), ],
                dim = c(nrow(train) - datalags, datalags, 20))
                        

y.train = array(data = train[,10][-(1:datalags)], dim = c(nrow(train)-datalags, 1))


x.test = array(data = lag(cbind(test[,1], 
                                test[,2],
                                test[,3],
                                test[,4],
                                test[,5],
                                test[,6],
                                test[,7],
                                test[,8],
                                test[,9],
                                test[,10],
                                test[,11],
                                test[,12],
                                test[,13],
                                test[,14],
                                test[,15],
                                test[,16],
                                test[,17],
                                test[,18],
                                test[,19],
                                test[,20]), datalags)[-(1:datalags), ], 
               dim = c(nrow(test) - datalags, datalags, 20))

y.test = array(data = test[,10][-(1:datalags)], dim = c(nrow(test) - datalags, 1))


set.seed(123)

model <- keras_model_sequential()

model %>%
        layer_lstm(units = 100,
                   input_shape = c(datalags, 20),
                   batch_size = batch.size,
                   return_sequences = TRUE,
                   stateful = TRUE) %>%
        layer_dropout(rate = 0.5) %>%
        layer_lstm(units = 50,
                   return_sequences = FALSE,
                   stateful = TRUE) %>%
        layer_dropout(rate = 0.5) %>%
        layer_dense(units = 1)

model %>%
        compile(loss = 'mae', optimizer = 'adam')

model

for(i in 1:480){
        model %>% fit(x = x.train,
                      y = y.train,
                      batch_size = batch.size,
                      epochs = 1,
                      verbose = 0,
                      shuffle = FALSE)
        model %>% reset_states()
        cat("Epoch: ", i)
}



pred_out <- model %>% predict(x.test, batch_size = batch.size) %>% .[,1] 

pred_out_rescaled <- pred_out*std2[10]+m2[10] %>% as.vector()

pred_out_rescaled <- pred_out_rescaled[1:24] %>% exp()

test_rescaled <- y.test*std2[10]+m2[10] %>% as.vector()

test_rescaled <- test_rescaled[1:24] %>% exp()

#in train data predictions

pred_train <- model %>% predict(x.train, batch_size = batch.size) %>% .[,1]

pred_train_rescaled <- pred_train*std2[10]+m2[10] %>% as.vector()

pred_train_rescaled <- pred_train_rescaled[1:480] %>% exp()

y.train_rescaled <- y.train*std2[10]+m2[10] %>% as.vector()

y.train_rescaled <- y.train_rescaled[1:480]

# in train

(RMSE_LSTM_tr<-Metrics::rmse(actual = y.train_rescaled,predicted = pred_train_rescaled))

(MAE_LSTM_tr<-Metrics::mae(actual = y.train_rescaled,predicted = pred_train_rescaled))

(RRSE_LSTM_tr<-Metrics::rrse(actual = y.train_rescaled,predicted = pred_train_rescaled))

(DAR_LSTM_tr<-MDirAcc(actual = y.train_rescaled,predicted = pred_train_rescaled))

#6 Months

(RMSE_LSTM_ts6M<-Metrics::rmse(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6]))

(MAE_LSTM_ts6M<-Metrics::mae(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6]))

(RRSE_LSTM_ts6M<-Metrics::rrse(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6]))

(DAR_LSTM_ts6M<- MDirAcc(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6]))


Metrics::rmse(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6])

MDirAcc(actual = test_rescaled[1:6],predicted = pred_out_rescaled[1:6])

#12 months

(RMSE_LSTM_ts1Y<-Metrics::rmse(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12]))

(MAE_LSTM_ts1Y<-Metrics::mae(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12]))

(RRSE_LSTM_ts1Y<-Metrics::rrse(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12]))

(DAR_LSTM_ts1Y<-MDirAcc(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12]))

Metrics::rmse(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12])

MDirAcc(actual = test_rescaled[1:12],predicted = pred_out_rescaled[1:12])

#18 months

(RMSE_LSTM_ts1.5Y<-Metrics::rmse(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18]))

(MAE_LSTM_ts1.5Y<-Metrics::mae(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18]))

(RRSE_LSTM_ts1.5Y<-Metrics::rrse(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18]))

(DAR_LSTM_ts1.5Y<-MDirAcc(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18]))

Metrics::rmse(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18])

MDirAcc(actual = test_rescaled[1:18],predicted = pred_out_rescaled[1:18])

#24 months

(RMSE_LSTM_ts2Y<-Metrics::rmse(actual = test_rescaled,predicted = pred_out_rescaled))

(MAE_LSTM_ts2Y<-Metrics::mae(actual = test_rescaled,predicted = pred_out_rescaled))

(RRSE_LSTM_ts2Y<-Metrics::rrse(actual = test_rescaled,predicted = pred_out_rescaled))

(DAR_LSTM_ts2Y<-MDirAcc(actual = test_rescaled,predicted = pred_out_rescaled))

Metrics::rmse(actual = test_rescaled,predicted = pred_out_rescaled)

MDirAcc(actual = test_rescaled,predicted = pred_out_rescaled)

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_LSTM_tr,RMSE_LSTM_ts6M,RMSE_LSTM_ts1Y,RMSE_LSTM_ts1.5Y,RMSE_LSTM_ts2Y),
           DAR= c(DAR_LSTM_tr,DAR_LSTM_ts6M,DAR_LSTM_ts1Y,DAR_LSTM_ts1.5Y,DAR_LSTM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_LSTM_tr,RMSE_LSTM_ts6M,RMSE_LSTM_ts1Y,RMSE_LSTM_ts1.5Y,RMSE_LSTM_ts2Y),
           DAR= c(DAR_LSTM_tr,DAR_LSTM_ts6M,DAR_LSTM_ts1Y,DAR_LSTM_ts1.5Y,DAR_LSTM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% 
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price LSTM Neural Net Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_LSTM_tr,RMSE_LSTM_ts6M,RMSE_LSTM_ts1Y,RMSE_LSTM_ts1.5Y,RMSE_LSTM_ts2Y),
           DAR= c(DAR_LSTM_tr,DAR_LSTM_ts6M,DAR_LSTM_ts1Y,DAR_LSTM_ts1.5Y,DAR_LSTM_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% 
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price LSTM Neural Net Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()


#---------------------------------- Time Neural NET NNAR--------------------------------------------------------------------

set.seed(123)

NNETAr <- forecast::nnetar(y=OIL_MODIFIED_TS[1:504,"Oil_Price"],p=12,size = 60)



NNETAr$fitted

#metrics in train

(RMSE_NNAR_tr<-Metrics::rmse(actual = exp(oil_train$Oil_Price[-c(1:12)]),predicted = exp(NNETAr$fitted[-c(1:12)])))

(MAE_NNAR_tr<-Metrics::mae(actual = exp(oil_train$Oil_Price[-c(1:12)]),predicted = exp(NNETAr$fitted[-c(1:12)])))

(RRSE_NNAR_tr<-Metrics::rrse(actual = exp(oil_train$Oil_Price[-c(1:12)]),predicted = exp(NNETAr$fitted[-c(1:12)])))

(DAR_NNAR_tr<-MDirAcc(actual = exp(oil_train$Oil_Price[-c(1:12)]),predicted = exp(NNETAr$fitted[-c(1:12)])))

# 6Months

(RMSE_NNAR_ts6M<-Metrics::rmse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=6)$mean)))

(MAE_NNAR_ts6M<-Metrics::mae(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=6)$mean)))

(RRSE_NNAR_ts6M<-Metrics::rrse(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=6)$mean)))

(DAR_NNAR_ts6M<-MDirAcc(actual = exp(oil_test_6M$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=6)$mean)))

# 1Y NNAR 

(RMSE_NNAR_ts1Y<-Metrics::rmse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=12)$mean)))

(MAE_NNAR_ts1Y<-Metrics::mae(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=12)$mean)))

(RRSE_NNAR_ts1Y<-Metrics::rrse(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=12)$mean)))

(DAR_NNAR_ts1Y<-MDirAcc(actual = exp(oil_test_1Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=12)$mean)))

#1.5Y NNAR

(RMSE_NNAR_ts1.5Y<-Metrics::rmse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=18)$mean)))

(MAE_NNAR_ts1.5Y<-Metrics::mae(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=18)$mean)))

(RRSE_NNAR_ts1.5Y<-Metrics::rrse(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=18)$mean)))

(DAR_NNAR_ts1.5Y<-MDirAcc(actual = exp(oil_test_1.5Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=18)$mean)))

#2Y NNAR metrics

(RMSE_NNAR_ts2Y<-Metrics::rmse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=24)$mean)))

(MAE_NNAR_ts2Y<-Metrics::mae(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=24)$mean)))

(RRSE_NNAR_ts2Y<-Metrics::rrse(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=24)$mean)))

(DAR_NNAR_ts2Y<-MDirAcc(actual = exp(oil_test_2Y$Oil_Price),predicted = exp(forecast::forecast(NNETAr,h=24)$mean)))


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNAR_tr,RMSE_NNAR_ts6M,RMSE_NNAR_ts1Y,RMSE_NNAR_ts1.5Y,RMSE_NNAR_ts2Y),
           DAR= c(DAR_NNAR_tr,DAR_NNAR_ts6M,DAR_NNAR_ts1Y,DAR_NNAR_ts1.5Y,DAR_NNAR_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>% knitr::kable()


data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNAR_tr,RMSE_NNAR_ts6M,RMSE_NNAR_ts1Y,RMSE_NNAR_ts1.5Y,RMSE_NNAR_ts2Y),
           DAR= c(DAR_NNAR_tr,DAR_NNAR_ts6M,DAR_NNAR_ts1Y,DAR_NNAR_ts1.5Y,DAR_NNAR_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%  
        ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Neural Net MLP Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()

data.frame(Horizon = c(0,6,12,18,24),
           RMSE=c(RMSE_NNAR_tr,RMSE_NNAR_ts6M,RMSE_NNAR_ts1Y,RMSE_NNAR_ts1.5Y,RMSE_NNAR_ts2Y),
           DAR= c(DAR_NNAR_tr,DAR_NNAR_ts6M,DAR_NNAR_ts1Y,DAR_NNAR_ts1.5Y,DAR_NNAR_ts2Y)) %>% 
        mutate(DAR_NUM=round(DAR*Horizon)) %>%
        mutate(DAR=DAR*100) %>%  
        ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Neural Net MLP Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()

#------------------------- Model performance metrics at different time horizons for evaluation -----------------------


TR<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA","NNET","NNAR"),
           RMSE=c(RMSE_lm_tr,RMSE_RF_tr,RMSE_XG_tr,RMSE_SVM_tr,RMSE_ARIMA_tr,RMSE_NNET_tr,RMSE_NNAR_tr),
           MAE=c(MAE_lm_tr,MAE_RF_tr,MAE_XG_tr,MAE_SVM_tr,MAE_ARIMA_tr,MAE_NNET_tr,MAE_NNAR_tr),
           RRSE=c(RRSE_lm_tr,RRSE_RF_tr,RRSE_XG_tr,RRSE_SVM_tr,RRSE_ARIMA_tr,RRSE_NNET_tr,RRSE_NNAR_tr),
           DAR=c(DAR_lm_tr,DAR_RF_tr,DAR_XG_tr,DAR_SVM_tr,DAR_ARIMA_tr,DAR_NNET_tr,DAR_NNAR_tr)) 



half_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA","NNET","NNAR"),
           RMSE=c(RMSE_lm_ts6M,RMSE_RF_ts6M,RMSE_XG_ts6M,RMSE_SVM_ts6M,RMSE_ARIMA_ts6M,RMSE_NNET_ts6M,RMSE_NNAR_ts6M),
           MAE=c(MAE_lm_ts6M,MAE_RF_ts6M,MAE_XG_ts6M,MAE_SVM_ts6M,MAE_ARIMA_ts6M,MAE_NNET_ts6M,MAE_NNAR_ts6M),
           RRSE=c(RRSE_lm_ts6M,RRSE_RF_ts6M,RRSE_XG_ts6M,RRSE_SVM_ts6M,RRSE_ARIMA_ts6M,RRSE_NNET_ts6M,RRSE_NNAR_ts6M),
           DAR=c(DAR_lm_ts6M,DAR_RF_ts6M,DAR_XG_ts6M,DAR_SVM_ts6M,DAR_ARIMA_ts6M,DAR_NNET_ts6M,DAR_NNAR_ts6M)) 


one_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA","NNET","NNAR"),
           RMSE=c(RMSE_lm_ts1Y,RMSE_RF_ts1Y,RMSE_XG_ts1Y,RMSE_SVM_ts1Y,RMSE_ARIMA_ts1Y,RMSE_NNET_ts1Y,RMSE_NNAR_ts1Y),
           MAE=c(MAE_lm_ts1Y,MAE_RF_ts1Y,MAE_XG_ts1Y,MAE_SVM_ts1Y,MAE_ARIMA_ts1Y,MAE_NNET_ts1Y,MAE_NNAR_ts1Y),
           RRSE=c(RRSE_lm_ts1Y,RRSE_RF_ts1Y,RRSE_XG_ts1Y,RRSE_SVM_ts1Y,RRSE_ARIMA_ts1Y,RRSE_NNET_ts1Y,RRSE_NNAR_ts1Y),
           DAR=c(DAR_lm_ts1Y,DAR_RF_ts1Y,DAR_XG_ts1Y,DAR_SVM_ts1Y,DAR_ARIMA_ts1Y,DAR_NNET_ts1Y,DAR_NNAR_ts1Y)) 


one_half_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA","NNET","NNAR"),
           RMSE=c(RMSE_lm_ts1.5Y,RMSE_RF_ts1.5Y,RMSE_XG_ts1.5Y,RMSE_SVM_ts1.5Y,RMSE_ARIMA_ts1.5Y,RMSE_NNET_ts1.5Y,RMSE_NNAR_ts1.5Y),
           MAE=c(MAE_lm_ts1.5Y,MAE_RF_ts1.5Y,MAE_XG_ts1.5Y,MAE_SVM_ts1.5Y,MAE_ARIMA_ts1.5Y,MAE_NNET_ts1.5Y,MAE_NNAR_ts1.5Y),
           RRSE=c(RRSE_lm_ts1.5Y,RRSE_RF_ts1.5Y,RRSE_XG_ts1.5Y,RRSE_SVM_ts1.5Y,RRSE_ARIMA_ts1.5Y,RRSE_NNET_ts1.5Y,RRSE_NNAR_ts1.5Y),
           DAR=c(DAR_lm_ts1.5Y,DAR_RF_ts1.5Y,DAR_XG_ts1.5Y,DAR_SVM_ts1.5Y,DAR_ARIMA_ts1.5Y,DAR_NNET_ts1.5Y,DAR_NNAR_ts1.5Y)) 


two_year<-data.frame(models=c("lm","RF","XGboost","SVM_RBF","ARIMA","NNET","NNAR"),
           RMSE=c(RMSE_lm_ts2Y,RMSE_RF_ts2Y,RMSE_XG_ts2Y,RMSE_SVM_ts2Y,RMSE_ARIMA_ts2Y,RMSE_NNET_ts2Y,RMSE_NNAR_ts2Y),
           MAE=c(MAE_lm_ts2Y,MAE_RF_ts2Y,MAE_XG_ts2Y,MAE_SVM_ts2Y,MAE_ARIMA_ts2Y,MAE_NNET_ts2Y,MAE_NNAR_ts2Y),
           RRSE=c(RRSE_lm_ts2Y,RRSE_RF_ts2Y,RRSE_XG_ts2Y,RRSE_SVM_ts2Y,RRSE_ARIMA_ts2Y,RRSE_NNET_ts2Y,RRSE_NNAR_ts2Y),
           DAR=c(DAR_lm_ts2Y,DAR_RF_ts2Y,DAR_XG_ts2Y,DAR_SVM_ts2Y,DAR_ARIMA_ts2Y,DAR_NNET_ts2Y,DAR_NNAR_ts2Y)) 

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
        ggplot(.,aes(x=factor(Horizon),y=DAR,color=models))+geom_point(size=6,alpha=0.6)+
        geom_hline(yintercept = mean(diff(oil_train$Oil_Price)>0),color="red",linetype="dashed")+
        labs(title = "ML Oil Price Models DAR",
             caption = "Red line corresponds to the DAR of a model that assumes only Growing Oil Prices",
             x="Time Horizon",
             y="Directional Accuracy Ration")+ggthemes::theme_tufte()


bind_rows(results,.id="Horizon") %>% mutate(Horizon = case_when(Horizon == 1 ~ 0,
                                                                Horizon == 2 ~ 6,
                                                                Horizon == 3 ~ 12,
                                                                Horizon == 4 ~ 18,
                                                                Horizon == 5 ~ 24)) %>%
        ggplot(.,aes(x=factor(Horizon),y=RMSE,color=models))+geom_point(size=6,alpha=0.6)+
        geom_hline(yintercept = sqrt(mean((mean(exp(oil_train$Oil_Price))-exp(oil_test_2Y$Oil_Price))^2)),color="red",linetype="dashed")+
        labs(title = "ML Oil Price Models RMSE",
             caption = "Red line corresponds to the RMSE of predicting future Oil Price using the mean Oil Price in the training as Model",
             x="Time Horizon",
             y="Root Mean Squared Error")+ggthemes::theme_tufte()


# line metrics for graphs

sqrt(mean((mean(exp(oil_train$Oil_Price))-exp(oil_test_2Y$Oil_Price))^2))

mean(diff(oil_train$Oil_Price)>0)

mean(abs(mean(oil_train$Oil_Price)-oil_test_2Y$Oil_Price))


sqrt(mean((exp(oil_train$Oil_Price[504])-exp(oil_test_2Y$Oil_Price))^2))


mean(abs(oil_train$Oil_Price[504]-oil_test_2Y$Oil_Price))

mean(oil_train$Oil_Price[493:504])


#----------------------------Ensemble Stacking -----------------------------------------------------------------------

bind_rows(results,.id="Horizon") %>% mutate(Horizon = case_when(Horizon == 1 ~ "0 Months",
                                                                Horizon == 2 ~ "6 Months",
                                                                Horizon == 3 ~ "12 Months",
                                                                Horizon == 4 ~ "18 Months",
                                                                Horizon == 5 ~ "24 Months")) %>% 
        select(-MAE,-RRSE) %>% filter(Horizon=="24 Months") %>% arrange(desc(DAR),RMSE) %>%
        knitr::kable()






Ensemble_DF <- data.frame(Actual_Price= oil_test_2Y$Oil_Price)

Ensemble_DF$lm <- predict(lm_model_price, newdata=oil_test_2Y)

Ensemble_DF$ARIMA <- forecast::forecast(arimaauto,h=24)$mean

Ensemble_DF$RF<- predict(randomF_model_price2, newdata=oil_test_2Y)

Ensemble_DF$XG<- predict(XGboost_model_price_2, newdata=data.matrix(oil_test_2Y))

Ensemble_DF$SVM <- predict(SVM_model_price2, newdata=oil_test_2Y)

Ensemble_DF$NNET <-as.vector(log(pred2Y))

Ensemble_DF$NNAR <- forecast::forecast(NNETAr,h=24)$mean

Ensemble_DF$Date <- oil_test_2Y$Date

Ensemble_DF<-Ensemble_DF %>% mutate(Stack_model=0.35*NNAR+
                               0.4*XG+
                               0.1*ARIMA+
                               0.05*RF+
                               0.05*NNET+
                               0.025*lm+
                               0.025*SVM) 

Ensemble_DF %>% summarise(RMSE_Stack= Metrics::rmse(exp(Actual_Price),exp(Stack_model)),
                          RMSE_NNAR = Metrics::rmse(exp(Actual_Price),exp(NNAR)),
                          RMSE_XG = Metrics::rmse(exp(Actual_Price),exp(XG)),
                          DAR_Stack=MDirAcc(exp(Actual_Price),exp(Stack_model)),
                          DAR_NNAR=MDirAcc(exp(Actual_Price),exp(NNAR)),
                          DAR_XG=MDirAcc(exp(Actual_Price),exp(XG)))


RMSE_Stack_ts6M<-Ensemble_DF[1:6,] %>%
        summarise(RMSE= Metrics::rmse(exp(Actual_Price),exp(Stack_model))) %>% pull() 

DAR_Stack_ts6M<-Ensemble_DF[1:6,] %>%
        summarise(DAR = MDirAcc(exp(Actual_Price),exp(Stack_model))) %>% pull()


RMSE_Stack_ts1Y<-Ensemble_DF[1:12,] %>%
        summarise(RMSE= Metrics::rmse(exp(Actual_Price),exp(Stack_model))) %>% pull()

DAR_Stack_ts1Y<-Ensemble_DF[1:12,] %>%
        summarise(DAR = MDirAcc(exp(Actual_Price),exp(Stack_model))) %>% pull()


RMSE_Stack_ts1.5Y<-Ensemble_DF[1:18,] %>%
        summarise(RMSE= Metrics::rmse(exp(Actual_Price),exp(Stack_model))) %>% pull()

DAR_Stack_ts1.5Y<-Ensemble_DF[1:18,] %>%
        summarise(DAR = MDirAcc(exp(Actual_Price),exp(Stack_model))) %>% pull()



RMSE_Stack_ts2Y<-Ensemble_DF %>%
        summarise(RMSE= Metrics::rmse(exp(Actual_Price),exp(Stack_model))) %>% pull()

DAR_Stack_ts2Y<-Ensemble_DF %>%
        summarise(DAR = MDirAcc(exp(Actual_Price),exp(Stack_model))) %>% pull()


data.frame(Horizon=c(6,12,18,24),
           RMSE=c(RMSE_Stack_ts6M,RMSE_Stack_ts1Y,RMSE_Stack_ts1.5Y,RMSE_Stack_ts2Y),
           DAR=c(DAR_Stack_ts6M,DAR_Stack_ts1Y,DAR_Stack_ts1.5Y,DAR_Stack_ts2Y)) %>% knitr::kable()



data.frame(Horizon=c(6,12,18,24),
           RMSE=c(RMSE_Stack_ts6M,RMSE_Stack_ts1Y,RMSE_Stack_ts1.5Y,RMSE_Stack_ts2Y),
           DAR=c(DAR_Stack_ts6M,DAR_Stack_ts1Y,DAR_Stack_ts1.5Y,DAR_Stack_ts2Y)) %>% 
ggplot(.,aes(x=Horizon,y=RMSE))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = RMSE_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Stacked Model Forecasted RMSE 24 Months",
             y="RMSE ($ USD)",
             x="Time (Months)")+
        ggthemes::theme_tufte()





data.frame(Horizon=c(6,12,18,24),
           RMSE=c(RMSE_Stack_ts6M,RMSE_Stack_ts1Y,RMSE_Stack_ts1.5Y,RMSE_Stack_ts2Y),
           DAR=c(DAR_Stack_ts6M,DAR_Stack_ts1Y,DAR_Stack_ts1.5Y,DAR_Stack_ts2Y)) %>%
        mutate(DAR=DAR*100) %>%
ggplot(.,aes(x=Horizon,y=DAR))+geom_point(color="red",size=5)+geom_smooth()+
        geom_hline(yintercept = DAR_BASELINE,linetype="dashed",color="red")+
        labs(title = "Oil Price Stacked Model Forecasted DAR 24 Months",
             y="DAR %",
             x="Time (Months)")+
        ylim(0,100)+
        ggthemes::theme_tufte()


        
        

        




