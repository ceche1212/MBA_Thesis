# data downloaded from the world bank web page http://api.worldbank.org/v2/en/indicator/SP.DYN.LE00.IN?downloadformat=excel

library(dplyr)

data<-read.csv("life_exp.csv")

data<-data[data$ï..year>1972,]

anios_sequencia<-seq(1973,2018,by=1)

#building the month colums

meses<-data.frame()

for(i in seq(1:length(anios_sequencia))){
        
        for(j in seq(1:12)){
                
                meses<-rbind(meses,j)
                
                
                
        }
        
        
}

colnames(meses)<-c("month")

#building the year columns

anios<-data.frame()

for (i in seq(1:length(anios_sequencia))) {
        
        for(j in seq(1:12)){
                
                anios<-rbind(anios,anios_sequencia[i])
                
                
                
        }
        
        
}

colnames(anios)<-c("Year")

#building the Population colum of the data frame

life_exp<-data.frame()

for (i in seq(1:length(data$ï..year))) {
        
        for(j in seq(1:12)){
                
                life_exp<-rbind(life_exp,data$life_exp[i])
                
                
                
        }
        
        
}

colnames(life_exp)<-c("Life_Expectancy")

#combining the data into a new structure

data1<-cbind(anios,meses,life_exp)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Units<-"years"

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(4,2,1,3,5)]

colnames(data1)<-c("Date","Month","Year","Life_Expectancy","Units")

US_life_exp<-data1

save(US_life_exp,file = "US_life_exp.RData")