library(readxl)
library(dplyr)

#Downloading the data from the eia website in excel format

data<-read_excel("PET_CRD_PRES_DCU_NUS_A.xls",sheet = 2,skip = 2)

#filtering data above 1972

data$Date<-as.character(data$Date)

extract_year<-function(x){
        
        b<-strsplit(x,"")
        primera<-paste0( b[[1]][1],b[[1]][2],b[[1]][3],b[[1]][4])
        return(primera)
        
}

data$Date_Year<-sapply(data$Date,extract_year)

data$Date_Year<-as.numeric(data$Date_Year)

data<-data %>% filter(data$Date_Year>1972)

years<-seq(1973,2017,by=1)

#building the month colums

meses<-data.frame()

for(i in seq(1:length(years))){
        
        for(j in seq(1:12)){
             
                meses<-rbind(meses,j)
                
                
                
        }
                
                
}

colnames(meses)<-c("month")

#building the year columns

anios<-data.frame()

for (i in seq(1:length(years))) {
        
        for(j in seq(1:12)){
                
                anios<-rbind(anios,years[i])
                
                
                
        }
        
        
}

colnames(anios)<-c("Year")
        
#building the reserves colum of the data frame

reserves<-data.frame()

for (i in seq(1:length(data$Date_Year))) {
        
        for(j in seq(1:12)){
                
                reserves<-rbind(reserves,data$`U.S. Crude Oil Proved Reserves (Million Barrels)`[i])
                
                
                
        }
        
        
}

colnames(reserves)<-c("Oil_Reserves")

#combining the data into a new structure

data1<-cbind(anios,meses,reserves)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Units<-"Million Barrels"

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(1,3,2,4,5)]

colnames(data1)<-c("Date","Month","Year","Oil_Reserves","Units")

Reserves_Oil<-data1

save(Reserves_Oil,file = "Reserves_Oil.RData")



