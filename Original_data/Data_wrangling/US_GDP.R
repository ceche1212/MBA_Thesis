# data downloaded from the world bank web page https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=US&view=chart

library(dplyr)

data<-read.csv("GDP.csv")

data<-data[data$ï..Date>1972,]

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

#building the GDP colum of the data frame

GDP<-data.frame()

for (i in seq(1:length(data$ï..Date))) {
        
        for(j in seq(1:12)){
                
                GDP<-rbind(GDP,data$GDP..current.US..[i])
                
                
                
        }
        
        
}

colnames(GDP)<-c("US_GDP")

#combining the data into a new structure

data1<-cbind(anios,meses,GDP)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Units<-"Trillions USD"

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(4,2,1,3,5)]

colnames(data1)<-c("Date","Month","Year","GDP","Units")

data1$GDP<-data1$GDP/1E12

US_GDP<-data1

save(US_GDP,file = "US_GDP.RData")


