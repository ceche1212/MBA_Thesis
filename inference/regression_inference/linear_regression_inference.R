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
library(plotly)

load("Oil_Modified.RData")

#Numeric change to generate a correlation matrix

OIL_MODIFIED$Gov_ctrl_party<-as.factor(OIL_MODIFIED$Gov_ctrl_party)
OIL_MODIFIED$Gov_ctrl_type<-as.factor(OIL_MODIFIED$Gov_ctrl_type)
OIL_MODIFIED$Eco_Rec<-as.factor(OIL_MODIFIED$Eco_Rec)
OIL_MODIFIED$Season<-as.factor(OIL_MODIFIED$Season)

OIL_MODIFIED_NUMERIC<-OIL_MODIFIED

OIL_MODIFIED_NUMERIC<-OIL_MODIFIED_NUMERIC %>% select(-Date)

OIL_MODIFIED_NUMERIC$Gov_ctrl_type<-as.factor(OIL_MODIFIED_NUMERIC$Gov_ctrl_type)
OIL_MODIFIED_NUMERIC$Gov_ctrl_party<-as.factor(OIL_MODIFIED_NUMERIC$Gov_ctrl_party)
OIL_MODIFIED_NUMERIC$Eco_Rec<-as.factor(OIL_MODIFIED_NUMERIC$Eco_Rec)
OIL_MODIFIED_NUMERIC$Season<-as.factor(OIL_MODIFIED_NUMERIC$Season)

OIL_MODIFIED_NUMERIC<-data.frame(map(OIL_MODIFIED_NUMERIC,as.numeric))

#correlation with ignoring NA's and using the spearman method

correlations<-cor(OIL_MODIFIED_NUMERIC,use = "complete.obs",method = "s")

#Oil prices

corr.price = as.matrix(sort(correlations[,'Oil_Price'], decreasing = TRUE))

corr.idx = names(which(apply(corr.price, 1, function(x) (x > 0.3 | x < -0.3))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', 
         addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7,tl.srt = 40)


#Active RIgs

corr.active.rigs = as.matrix(sort(correlations[,'Active_Rigs'], decreasing = TRUE))

corr.idx.rigs = names(which(apply(corr.active.rigs, 1, function(x) (x > 0.3 | x < -0.3))))

corrplot(as.matrix(correlations[corr.idx.rigs,corr.idx.rigs]), type = 'upper', method='color', 
         addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7,tl.srt = 40)


#full model Oil price

OIL_MODIFIED_MODEL<-OIL_MODIFIED %>% select(-Date)

model1_Price<-lm(data = OIL_MODIFIED_MODEL,Oil_Price~.)

model1_Price %>% augment(.) %>%
        ggplot(.,aes(x=Oil_Price,y=.resid))+geom_point(color="darkgreen",alpha=0.6)+
        geom_hline(yintercept = 0,color="red",linetype = 'dashed',size=1)+
        geom_smooth()+
        labs(title = "Residuals vs Oil Price Plot",
             x="Oil Price",
             y="Residuals")+
        ggthemes::theme_tufte()
        


model1_Price %>% augment(.) %>%
        ggplot(.,aes(x=.resid))+geom_histogram(binwidth = 1,fill="darkgreen")+
        geom_vline(xintercept = 0,color="red",linetype = 'dashed',size=1)+
        labs(title = "Residuals Histogram",
             x="Residuals",
             y="Frequency")+
        ggthemes::theme_tufte()
        

par(mfrow = c(1,2))
qqnorm(model1_Price$residuals,col = 'darkgreen')
qqline(model1_Price$residuals,col = 'red')

alias(model1_Price)

#full model Active Rigs

model1_Rigs<-lm(data = OIL_MODIFIED_MODEL,Active_Rigs~.)

summary(model1_Rigs)

anova(model1_Rigs)

model1_Rigs %>% augment(.) %>%
        ggplot(.,aes(x=Active_Rigs,y=.resid))+geom_point(color="darkgreen",alpha=0.6)+
        geom_hline(yintercept = 0,color="red",linetype = 'dashed',size=1)+
        geom_smooth()+
        labs(title = "Residuals vs Active Rigs Plot",
             x="Active Rigs",
             y="Residuals")+
        ggthemes::theme_tufte()



model1_Rigs %>% augment(.) %>%
        ggplot(.,aes(x=.resid))+geom_histogram(binwidth = 50,fill="darkgreen")+
        geom_vline(xintercept = 0,color="red",linetype = 'dashed',size=1)+
        labs(title = "Residuals Histogram",
             x="Residuals",
             y="Frequency")+
        ggthemes::theme_tufte()

qqnorm(model1_Rigs$residuals,col = 'darkgreen')
qqline(model1_Rigs$residuals,col = 'red')

#Multicolinearity Evaluation

alias(model1_Price)

alias(model1_Rigs)

#model 2

OIL_MODIFIED_MODEL2<-OIL_MODIFIED_MODEL %>% select(-Gov_ctrl_type,-Gov_ctrl_party)

model2_Price<-lm(data=OIL_MODIFIED_MODEL2,Oil_Price~.)

model2_Rigs<-lm(data=OIL_MODIFIED_MODEL2,Active_Rigs~.)

car::vif(model2_Price) %>% knitr::kable(.)

car::vif(model2_Rigs) %>% knitr::kable(.)

#model3 AIC Price

model_price_AIC<-MASS::stepAIC(model2_Price,k=2,trace = F)

model_price_AIC$anova


#model3 AIC Rigs

model_Rigs_AIC<-MASS::stepAIC(model2_Rigs,k=2,trace = F)

model_Rigs_AIC$anova

#model4 BIC Price

model_price_BIC<-bas.lm(Oil_Price~.,
                        prior = "BIC",
                        modelprior = uniform(),
                        data = OIL_MODIFIED_MODEL2)

image(model_price_BIC,rotate = FALSE)

summary(model_price_BIC)

#model4 BIC Rigs

model_Rigs_BIC<-bas.lm(Active_Rigs~.,
                        prior = "BIC",
                        modelprior = uniform(),
                        data = OIL_MODIFIED_MODEL2)

image(model_Rigs_BIC,rotate = FALSE)

summary(model_Rigs_BIC)

#model5 Boruta Price

OIL_MODIFIED_MODEL3<-OIL_MODIFIED_MODEL2 %>% select(-Drilled_Wells)

OIL_MODIFIED_MODEL3<-OIL_MODIFIED_MODEL3[complete.cases(OIL_MODIFIED_MODEL3),]

set.seed(123)

model_price_boruta<-Boruta::Boruta(Oil_Price~.,
                                   OIL_MODIFIED_MODEL3,
                                   maxRuns = 100, 
                                   doTrace=0)
print(model_price_boruta)

plot(model_price_boruta)

boruta.median.price <- data.frame(Boruta::attStats(model_price_boruta)[2])

boruta.result.price <- cbind(rownames(boruta.median.price), boruta.median.price)

colnames(boruta.result.price) <- c('features', 'medianImpact')

boruta.result.price <- boruta.result.price %>% arrange(desc(medianImpact))

# add the decision of boruta selection, if a feature is significant to the model
boruta.result.price$decision <- Boruta::attStats(model_price_boruta)['decision']

boruta.result.price[1:19,] %>% knitr::kable(.)


#model5 Boruta Active Rigs

set.seed(123)

model_Rigs_boruta<-Boruta::Boruta(Active_Rigs~.,
                                   OIL_MODIFIED_MODEL3,
                                   maxRuns = 100, 
                                   doTrace=0)
print(model_Rigs_boruta)

plot(model_Rigs_boruta)

boruta.median.rigs <- data.frame(Boruta::attStats(model_Rigs_boruta)[2])

boruta.result.rigs <- cbind(rownames(boruta.median.rigs), boruta.median.rigs)

colnames(boruta.result.rigs) <- c('features', 'medianImpact')

boruta.result.rigs <- boruta.result.rigs %>% arrange(desc(medianImpact))

# add the decision of boruta selection, if a feature is significant to the model
boruta.result.rigs$decision <- Boruta::attStats(model_Rigs_boruta)['decision']

boruta.result.rigs[1:19,] %>% knitr::kable(.)




                                   
