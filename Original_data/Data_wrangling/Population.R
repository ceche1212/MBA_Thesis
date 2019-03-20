# data downloaded from the world bank web page http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel

library(dplyr)

data<-read.csv("Population.csv")

data<-data[data$ï..Year>1972,]

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

Population<-data.frame()

for (i in seq(1:length(data$ï..Year))) {
        
        for(j in seq(1:12)){
                
                Population<-rbind(Population,data$population[i])
                
                
                
        }
        
        
}

colnames(Population)<-c("US_Population")

#combining the data into a new structure

data1<-cbind(anios,meses,Population)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Units<-"millions"

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(4,2,1,3,5)]

colnames(data1)<-c("Date","Month","Year","US_Population","Units")

data1$US_Population<-data1$US_Population/1E6

US_Population<-data1

save(US_Population,file = "US_Population.RData")
