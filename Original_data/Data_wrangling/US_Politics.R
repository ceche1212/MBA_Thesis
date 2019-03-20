# data downloaded manually from Wilkipedia https://en.wikipedia.org/wiki/Party_divisions_of_United_States_Congresses

library(dplyr)

data<-read.csv("US_Politics.csv")

#unlisting variables, if republican = 1

Rep_conversion_num<-function(x){
        
        if(x=="Republican"){
                
                pol<-1
                
        }else{
                pol<-0
        }
        
        return(pol)
}

data_unlisted<-data

data_unlisted$President.P<-sapply(data_unlisted$President.P,Rep_conversion_num)
data_unlisted$Senate<-sapply(data_unlisted$Senate,Rep_conversion_num)
data_unlisted$House<-sapply(data_unlisted$House,Rep_conversion_num)



#building the month colums

anios_sequencia<-seq(1973,2018,by=1)

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

# building the Presidents party colum

Presidents_party<-data.frame()

for (i in seq(1:length(data_unlisted$ï..Year))) {
        
        for(j in seq(1:12)){
                
                Presidents_party<-rbind(Presidents_party,data_unlisted$President.P[i])
                
                
                
        }
        
        
}

colnames(Presidents_party)<-c("Presidents_party")

# building the Senates party colum

Senate_party<-data.frame()

for (i in seq(1:length(data_unlisted$ï..Year))) {
        
        for(j in seq(1:12)){
                
                Senate_party<-rbind(Senate_party,data_unlisted$Senate[i])
                
                
                
        }
        
        
}

colnames(Senate_party)<-c("Senate")

# building the House party colum

House_party<-data.frame()

for (i in seq(1:length(data_unlisted$ï..Year))) {
        
        for(j in seq(1:12)){
                
                House_party<-rbind(House_party,data_unlisted$House[i])
                
                
                
        }
        
        
}

colnames(House_party)<-c("House")

#combining the data into a new structure

data1<-cbind(anios,meses,Presidents_party,Senate_party,House_party)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(6,2,1,3,4,5)]

# Reconverting numeric variables into factors 1 = "Republican"

Reconversion_num<-function(x){
        
        if(x==1){
                
                pol<-"Republican"
                
        }else{
                pol<-"Democrat"
        }
        
        return(pol)
}

data1$Presidents_party<-sapply(data1$Presidents_party,Reconversion_num)
data1$Senate<-sapply(data1$Senate,Reconversion_num)
data1$House<-sapply(data1$House,Reconversion_num)

US_Politics<-data1

save(US_Politics,file = "US_Politics.RData")



