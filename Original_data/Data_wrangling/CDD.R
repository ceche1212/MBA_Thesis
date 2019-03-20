#Cooling Degree Days:cooling degree
#days are the number of degrees that the daily average temperature falls above 65
#degrees Fahrenheit (°F) ~ 19 (°C).

library(dplyr)

#Downloading the data from the eia website in csv format

data<-read.csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T01.10")

# Filter data for only total of net imports

data1<-subset(data,data$Description=="Cooling Degree-Days, United States")
rm(data)

# Eliminating end of the year summary values

trece<-grepl("13$",data1$YYYYMM)

data1<-data1[!trece,]
rm(trece)

#transforming YYYYMM (date) into character

data1$YYYYMM<-as.character(data1$YYYYMM)

#function to extract year values out of YYYYMM

year<-function(x){
        
        b<-strsplit(x,"")
        anio<-paste0( b[[1]][1],b[[1]][2],b[[1]][3],b[[1]][4])
        return(anio)
        
}

#function to extract month values out of YYYYMM

month<-function(x){
        
        b<-strsplit(x,"")
        mes<-paste0( b[[1]][5],b[[1]][6])
        return(mes)
        
}

#extracting month and year out of YYYYMM

data1$Date_Year<-sapply(data1$YYYYMM,year)
data1$Date_Month<-sapply(data1$YYYYMM,month)

# Selecting only columns of interest

data1<-data1[,c(3,6:8)]

#Creating new data variable with "R date" format

data1$Date<-paste0(data1$Date_Year,"-",data1$Date_Month,"-","01")
data1$Date<-as.Date(data1$Date,format = "%F")

#Changing colum order
data1<-data1[,c(5,4,3,1,2)]

#Changing column names

colnames(data1)<-c("Date","Month","Year","CDD","Units")

CDD<-data1

save(CDD,file = "CDD.RData")



