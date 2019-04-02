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

load("Oil_Modified.RData")

#Oil Price and Months

summary(aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Month))

#Active Rigs and Months

aov(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Month) %>% tidy(.) %>% knitr::kable(.)


#Oil Price and Seasons

aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Season) %>% tidy(.) %>% knitr::kable(.)


#Active Rigs and Seasons

aov(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Season) %>% tidy(.) %>% knitr::kable(.)

#Does Oil price varies with the US Presidents political Party

t.test(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$President_Party)

aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$President_Party) %>% tidy(.) %>% knitr::kable(.)

TukeyHSD(aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$President_Party)) %>% tidy(.) %>% knitr::kable(.)

#Does Active Rigs varies with the US Presidents political Party

t.test(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$President_Party)

#Does Oil price varies with the ruling party of US Senate?

t.test(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Senate)

#Does Active Rigs varies with the ruling party of US Senate?

t.test(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Senate)

#Does Oil price varies with the ruling party of US House?

t.test(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$House)

#Does Active Rigs varies with the majority ruling party of US House?

t.test(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$House)

#Does Oil prices varies if the US government is completely dominated by one sigle party at all levels?

t.test(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Gov_ctrl_type)

#Does Active rigs varies if the US government is completely dominated by one sigle party at all levels?

t.test(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Gov_ctrl_type)

#On Scenarios of complete control by one party, does oil prices varies in relationship to the party?

aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Gov_ctrl_party) %>% tidy(.) %>% knitr::kable(.)

TukeyHSD(aov(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Gov_ctrl_party)) %>% tidy(.) %>% knitr::kable(.)

#On Scenarios of complete control by one party, does Active Rigs varies in relationship to the party?

aov(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Gov_ctrl_party) %>% tidy(.) %>% knitr::kable(.)

TukeyHSD(aov(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Gov_ctrl_party)) %>% tidy(.) %>% knitr::kable(.)

#Does Oil prices varies if the US economy is on recession?

t.test(OIL_MODIFIED$Oil_Price~OIL_MODIFIED$Eco_Rec)

#Does Active Rigs varies if the US economy is on recession?

t.test(OIL_MODIFIED$Active_Rigs~OIL_MODIFIED$Eco_Rec)

#Does oil price behavior is independent of price behavior on previous month?

price_oil<-OIL_MODIFIED$Oil_Price

deltaPrice<-c(3.89,price_oil)-price_oil 

deltaPrice<-deltaPrice[-553]

deltaPrice<-deltaPrice*(-1)

#agregar ruido para diferenciar entre valores iguales de un mismo mes

deltaPrice<-deltaPrice+rnorm(552,mean = 0,sd=0.000000001)

deltaPrice<-as.data.frame(deltaPrice)

deltaPrice$oil_price<-price_oil

#convertir la variable en categorica basada en el comportamiento del precio

deltaPrice<-deltaPrice %>% mutate(comportamiento = ifelse(deltaPrice > 0,"UP","DOWN"))

table(deltaPrice$comportamiento)

prop.table(table(deltaPrice$comportamiento))

x<-deltaPrice$comportamiento[-552]

probup<-prop.table(table(x))[[2]] 

#crea un vector lleno de ceros de longitud el vector inicial

y = rep(0, length(x))

# cambia el vector lleno de ceros a dummy dependiendo de los valores del vector inicial en este caso nos interesa la subida

y[x == "UP"] = 1

# le agrega un cero inicial y un cero final al vector dummy

y = c(0, y, 0)

# busca los indices del vector dummy donde la variable es 0

wz = which(y == 0)

# la diference entre los indices nos dice la cantidad de posiciones 0 entre dos valores 1, 
# pero hay que restarle la unidad (1) ya que la posicion 1 es 1 y no 0

price_streak<-diff(wz) - 1

table(price_streak) %>% knitr::kable(.)

prop.table(table(price_streak)) %>% knitr::kable(.)

barplot(prop.table(table(price_streak)),
        col="steelblue",
        xlab = "Days",
        ylab = "Proportions",
        main = "Price Streak Histogram")


data.frame(prop.table(table(price_streak))) %>% mutate(price_streak=as.numeric(price_streak)) %>%
        mutate(price_streak=price_streak-1) %>%
        mutate(price_streak=ifelse(price_streak >5 ,6,price_streak)) %>% 
        group_by(price_streak) %>% 
        summarise(Freq=sum(Freq)) %>%
        mutate(cummulative=cumsum(Freq)) %>% knitr::kable(.)

price_observed<-data.frame(price_streak) %>% 
        mutate(price_streak=ifelse(price_streak>5,6,price_streak)) %>% 
        table(.)

#grafico de la distribucion modificada

GOF1<-data.frame(prop.table(table(price_streak))) %>% mutate(price_streak=as.numeric(price_streak)) %>%
        mutate(price_streak=price_streak-1) %>%
        mutate(price_streak=ifelse(price_streak >5 ,6,price_streak)) %>% 
        group_by(price_streak) %>% 
        summarise(Freq=sum(Freq)) %>%
        mutate(cummulative=cumsum(Freq)) %>% 
        ggplot(.,aes(x=factor(price_streak),y=Freq))+geom_bar(stat = "identity",fill="steelblue",alpha=0.8)+
        geom_point(aes(x=factor(price_streak),y=cummulative),color="red",size=3)+
        geom_hline(yintercept = 1,color="green",size=1.5)+
        labs(title = "Modified price streak histogram",
             x="Price Streak",
             y="Frequency and cummulative frequency")+
        ggthemes::theme_tufte()

#geometric distribution

geom_dist<-data.frame(prop.table(table(rgeom(10000,prob = probup)))) %>% 
        mutate(Var1=as.numeric(Var1)) %>%
        mutate(Var1=Var1-1) %>%
        mutate(Var1=ifelse(Var1 >5 ,6,Var1)) %>% 
        rename(.,Geometric_streak=Var1) %>%
        group_by(Geometric_streak) %>% 
        summarise(Freq=sum(Freq)) %>%
        mutate(cummulative=cumsum(Freq)) %>% knitr::kable(.) 



prop_geom<-c("0"=geom_dist$Freq[1],
             "1"=geom_dist$Freq[2],
             "2"=geom_dist$Freq[3],
             "3"=geom_dist$Freq[4],
             "4"=geom_dist$Freq[5],
             "5"=geom_dist$Freq[6],
             "6"=geom_dist$Freq[7])


GOF2<-data.frame(prop.table(table(rgeom(10000,prob = probup)))) %>% 
        mutate(Var1=as.numeric(Var1)) %>%
        mutate(Var1=Var1-1) %>%
        mutate(Var1=ifelse(Var1 >5 ,6,Var1)) %>%
        group_by(Var1) %>% 
        summarise(Freq=sum(Freq)) %>%
        mutate(cummulative=cumsum(Freq)) %>% 
        ggplot(.,aes(x=factor(Var1),y=Freq))+geom_bar(stat = "identity",fill="steelblue",alpha=0.8)+
        geom_point(aes(x=factor(Var1),y=cummulative),color="red",size=3)+
        geom_hline(yintercept = 1,color="green",size=1.5)+
        labs(title = "Modified Geometric distribution histogram",
             x="Geometric Streak",
             y="Frequency and cummulative frequency")+
        ggthemes::theme_tufte()

grid.arrange(GOF1,GOF2,ncol=2)

chisq.test(price_observed,p=prop_geom) 

round(chisq.test(price_observed,p=prop_geom)$expected)

#performing a simulation of chisquare to calculate the chi statistic and p-values

oil_observed_chi<-chisq.test(price_observed,p=prop_geom)$stat


a2<-data.frame(price_streak) %>% 
        mutate(price_streak=ifelse(price_streak>5,6,price_streak))

null_hyp_oil_price<- a2 %>% 
        infer::specify(response = price_streak) %>%
        infer::hypothesize(null="point",p=prop_geom) %>%
        infer::generate(reps=10000,type = "simulate") %>%
        infer::calculate(stat = "Chisq")

ggplot(null_hyp_oil_price,aes(x=stat))+
        geom_density(fill="black",alpha=0.1)+
        geom_vline(xintercept = oil_observed_chi,color="Red",size=2)+
        stat_function(fun = dchisq, args = list(df = 6), color = "blue",size=1)+
        labs(title = "Chisq Test for Oil Prices Streak",
             x="Chis-sqaure statistic",
             y="Density",
             caption = "Red line corresponds to the observed Chisq observed stat, blue line is the Chisq function")+
        ggthemes::theme_tufte()

null_hyp_oil_price %>% summarise(mean(stat>=oil_observed_chi))
