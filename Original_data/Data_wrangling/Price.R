#Coctail Oil price purchase by US at the average domestic market value without charges of shipping

library(dplyr)

#Downloading the data from the eia website in csv format

data<-read.csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T09.01")

# Filter data for only total of active rotary rigs

data1<-subset(data,data$Description=="Crude Oil Domestic First Purchase Price")
rm(data)

# Eliminating end of the year summary values

trece<-grepl("13$",data1$YYYYMM)

data1<-data1[!trece,]
rm(trece)

#transforming YYYYMM (date) into character

data1$YYYYMM<-as.character(data1$YYYYMM)

#Prices values were imported as factors with trailing whitespace which impedes a propper conversion to numeric
#The conversion process will start with a transformation into character

data1$Value<-as.character(data1$Value)

#removal of white space

data1$Value<-sapply(data1$Value,trimws)

#conversion to numeric

data1$Value<-as.numeric(data1$Value)

#some NA values were introduced into the conversion process they need to be fixed these are all values from 1973
# and the las value available from 2018. 1973 price values are going to be substitued by the 1973 year average and
#2018 will be substituted by the previous month last value. First dates needs to be modified to filter

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

#Fixing missing price values of 1973

data1$Value[data1$Date_Year=="1973"]<-3.89

#Fixing missing price values of november 2018

data1$Value[data1$Date_Year=="2018" & data1$Date_Month=="11"]<-data1$Value[data1$Date_Year=="2018" & data1$Date_Month=="10"]

# Selecting only columns of interest

data1<-data1[,c(3,6:8)]

#Creating new data variable with "R date" format

data1$Date<-paste0(data1$Date_Year,"-",data1$Date_Month,"-","01")
data1$Date<-as.Date(data1$Date,format = "%F")

#Changing colum order
data1<-data1[,c(5,4,3,1,2)]

#Changing column names

colnames(data1)<-c("Date","Month","Year","Oil_Price","Units")

Oil_Price<-data1

save(Oil_Price,file = "Oil_Price.RData")
