library(dplyr)

#Downloading the data from the eia website in csv format

data<-read.csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T01.01")

# Filter data for only total of energy consumption

data1<-subset(data,data$Description=="Total Primary Energy Consumption")
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

colnames(data1)<-c("Date","Month","Year","Energy_Consumption","Units")

energy_consumption<-data1

save(energy_consumption,file = "energy_consumption.RData")



